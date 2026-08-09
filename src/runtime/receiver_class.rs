//! ADR-0019 Phase E box E1a: the shadow-mode receiver classifier.
//!
//! `receiver_dispatch_class`/`dispatch_mro` compute the "who owns this method for this
//! receiver" decision from a single place, using [`crate::type_id::TypeId`] and the
//! raku-adjudicated [`crate::builtins::builtin_type_catalog`] instead of the four
//! divergent MRO tables and the alias logic baked into `value_type_name`. See
//! `todo/deep/adr0019-e1-typeid-receiver-owner.md` for the full design and the
//! verification items (V1-V5) referenced in the comments below.
//!
//! **E1a is shadow-only.** Nothing here drives dispatch yet — [`Interpreter::shadow_check_owner`]
//! is the only way this module is reached from the interpreter, and it only records a
//! `MUTSU_VM_STATS` comparison against the *existing* owner decision. Making the
//! classifier authoritative is E1b.

use super::*;
use crate::builtins::builtin_type_catalog::builtin_type_info;
use crate::type_id::{TypeId, well_known_types};
use crate::value::ValueView;
use std::sync::Arc;

/// Whether a receiver is a concrete value or a type object (`Foo` vs `Foo.new`).
/// Mirrors the `:D`/`:U` smiley distinction; consulted by E4, not by any E1a probe.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub(crate) enum Definedness {
    Concrete,
    TypeObject,
}

/// A hint for how the *execution* layer (E4/E5/E6) should run a resolved candidate
/// against this receiver — distinct from the MRO chain itself. E1 only classifies; it
/// does not touch the ~14 delegation sites `ArrayStorageDelegate` describes.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub(crate) enum ReceiverExec {
    /// Ordinary dispatch: the resolved candidate runs directly against the receiver.
    Direct,
    /// A user class `is Array`/`is List`: native Positional methods run against the
    /// `__mutsu_array_storage` backing attribute, not the `Instance` itself.
    ArrayStorageDelegate,
    /// A role-mixed value (`but`/`does`): the role layer is tried before the inner
    /// value's own chain.
    MixinLayered,
}

/// The classifier's answer for one receiver: its canonical [`TypeId`], whether it is a
/// concrete value or a type object, and an execution hint for how a resolved candidate
/// should run against it.
#[derive(Copy, Clone, Debug)]
pub(crate) struct ReceiverClass {
    pub(crate) type_id: TypeId,
    pub(crate) definedness: Definedness,
    pub(crate) exec: ReceiverExec,
}

impl Interpreter {
    /// Classify `value`'s dispatch receiver: its canonical owner type, definedness, and
    /// execution hint. E1a-shadow only — see the module doc.
    pub(crate) fn receiver_dispatch_class(&mut self, value: &Value) -> ReceiverClass {
        let definedness = match value.view() {
            ValueView::Package(_) | ValueView::ParametricRole { .. } => Definedness::TypeObject,
            _ => Definedness::Concrete,
        };
        let exec = match value.view() {
            ValueView::Mixin(..) => ReceiverExec::MixinLayered,
            _ => ReceiverExec::Direct,
        };
        let chain = self.dispatch_mro(value);
        let type_id = chain
            .first()
            .copied()
            .unwrap_or_else(|| well_known_types().any);
        // Array/List-storage delegation only applies to a user `Instance` subclass
        // whose chain reaches Array/List *below* its own class (the class itself is
        // never literally "Array"/"List" — real Array/List values are their own
        // ValueView variant, not an Instance).
        let exec = if matches!(value.view(), ValueView::Instance { .. })
            && chain
                .iter()
                .any(|t| *t == well_known_types().array || *t == well_known_types().list)
        {
            ReceiverExec::ArrayStorageDelegate
        } else {
            exec
        };
        ReceiverClass {
            type_id,
            definedness,
            exec,
        }
    }

    /// The full ordered owner chain (self first, `Mu` last except `Junction`, which
    /// raku itself skips `Any` for — see the catalog's `Junction` row) for `value`'s
    /// dispatch receiver.
    pub(crate) fn dispatch_mro(&mut self, value: &Value) -> Vec<TypeId> {
        match value.view() {
            // Transient wrappers: classify the held value.
            ValueView::VarRef { value: inner, .. } => self.dispatch_mro(inner),
            ValueView::Scalar(inner) => self.dispatch_mro(inner),
            ValueView::ContainerRef(_) => value.with_deref(|inner| self.dispatch_mro(inner)),
            ValueView::HashEntryRef { .. } => {
                let inner = value.hash_entry_read();
                self.dispatch_mro(&inner)
            }
            ValueView::LazyThunk(thunk_data) => {
                let cached = thunk_data.cache.lock().unwrap().clone();
                match cached {
                    Some(cached) => self.dispatch_mro(&cached),
                    // Mirrors `value_type_name`'s uncached-thunk answer ("Scalar"),
                    // which is not itself a raku type — best-effort fallback only.
                    None => vec![
                        TypeId::intern("Scalar"),
                        well_known_types().any,
                        well_known_types().mu,
                    ],
                }
            }

            // Instance/Package: the registry's own class MRO IS the chain (design
            // decision 3). A type object's chain is identical to an instance of the
            // same class — only `definedness` differs, computed separately in
            // `receiver_dispatch_class`.
            ValueView::Instance { class_name, .. } => self.class_chain(class_name.as_str()),
            ValueView::Package(name) => self.class_chain(name.as_str()),
            ValueView::ParametricRole { base_name, .. } => self.class_chain(base_name.as_str()),

            // Enum (V3): raku puts the enum type itself ahead of Int, unlike
            // `value_type_name`'s "Int" answer for every enum value.
            ValueView::Enum { enum_type, .. } => {
                let mut chain = vec![TypeId::from_symbol(enum_type)];
                chain.extend(self.catalog_chain_for_name("Int"));
                chain
            }

            // Role mixins / allomorphs (V2, V4): see `mixin_chain`.
            ValueView::Mixin(inner, mixins) => self.mixin_chain(inner, mixins),

            // Every other variant is a builtin concrete value. Reuse
            // `value_type_name`'s existing alias resolution (Map-declared Hash,
            // gather-Seq, Array-vs-List itemization, Set/Bag/Mix mutability,
            // BigRat->Rat/FatRat, ...) so E1a's shadow probes compare against
            // EXACTLY the name the current dispatch sites already compute — the
            // design doc's "aliases resolved here and only here" rule, applied by
            // delegating to the one place that already resolves them correctly
            // rather than re-deriving a second copy.
            _ => {
                let name = crate::runtime::utils::value_type_name(value);
                self.catalog_chain_for_name(name)
            }
        }
    }

    /// Look up a builtin-type catalog chain by name, trying the raw name first and
    /// then the Buf/Blob short-alias normalization (V5: `buf8` -> `Buf[uint8]`, etc.).
    /// Falls back to a best-effort `[name, Any, Mu]` chain for names the catalog does
    /// not model (mutsu-internal types with no raku equivalent, e.g. `CustomType`) —
    /// never hit for a catalog-covered type.
    fn catalog_chain_for_name(&self, name: &str) -> Vec<TypeId> {
        if let Some(info) = builtin_type_info(name) {
            return info.mro.iter().map(|s| TypeId::intern(s)).collect();
        }
        let normalized = crate::runtime::utils::normalize_buf_type_name(name);
        if normalized != name
            && let Some(info) = builtin_type_info(&normalized)
        {
            return info.mro.iter().map(|s| TypeId::intern(s)).collect();
        }
        vec![
            TypeId::intern(name),
            well_known_types().any,
            well_known_types().mu,
        ]
    }

    /// The dispatch chain for an Instance/Package/ParametricRole named `name`: a
    /// builtin-catalog chain if `name` (or its Buf/Blob-normalized form) is a catalog
    /// type, otherwise the registry's class MRO with a catalog tail spliced on where
    /// a builtin ancestor (e.g. a user class `is Array`) does not already carry one.
    fn class_chain(&mut self, name: &str) -> Vec<TypeId> {
        if let Some(info) = builtin_type_info(name) {
            return info.mro.iter().map(|s| TypeId::intern(s)).collect();
        }
        let normalized = crate::runtime::utils::normalize_buf_type_name(name);
        if normalized != name
            && let Some(info) = builtin_type_info(&normalized)
        {
            return info.mro.iter().map(|s| TypeId::intern(s)).collect();
        }
        self.class_chain_with_catalog_tail(name)
    }

    /// The registry MRO for `class_name`, with a builtin catalog's tail spliced in the
    /// moment a builtin ancestor appears that the registry MRO does not already
    /// continue past — e.g. `class Foo is Array {}` registers as `[Foo, Array]` (the
    /// registry has no model of Array's own List/Cool/Any/Mu ancestry for a class it
    /// never registered), so the catalog's `Array` row supplies the rest:
    /// `[Foo, Array, List, Cool, Any, Mu]`.
    fn class_chain_with_catalog_tail(&mut self, class_name: &str) -> Vec<TypeId> {
        let reg_mro = self.class_mro(class_name);
        let mut chain: Vec<TypeId> = Vec::with_capacity(reg_mro.len());
        for (i, sym) in reg_mro.iter().enumerate() {
            chain.push(TypeId::from_symbol(*sym));
            if let Some(info) = builtin_type_info(sym.as_str()) {
                let continues = info
                    .mro
                    .get(1)
                    .is_some_and(|next| reg_mro.get(i + 1).is_some_and(|s| s.as_str() == *next));
                if !continues {
                    for tail_name in &info.mro[1..] {
                        let tid = TypeId::intern(tail_name);
                        if !chain.contains(&tid) {
                            chain.push(tid);
                        }
                    }
                }
                break;
            }
        }
        if chain.is_empty() {
            chain.push(TypeId::intern(class_name));
        }
        chain
    }

    /// The chain for a `Mixin(inner, mixins)` value: either the allomorph shortcut
    /// (V4, mirroring `value_type_name`'s Mixin arm exactly) or the general role-mixin
    /// case (role TypeIds first, then the inner value's own chain).
    ///
    /// V2 finding: raku's `(0 but A) but B` has the LATER-applied role (`B`) win, but
    /// `MixinOverrides` (`crate::value::MixinOverrides`, a plain `HashMap<String,
    /// Value>` keyed by role name) carries no application-order information at all —
    /// only role names as map keys — so true "later wins" cannot be reconstructed from
    /// a `Mixin` value today. This mirrors `dispatch_mixin_method_call`'s existing
    /// order (alphabetical by role name) so the chain is at least deterministic, which
    /// is all E1a requires; the representation gap is filed as its own ticket rather
    /// than fixed here (fixing it means adding an order field to `MixinOverrides`, a
    /// bigger change than this shadow-mode classifier should carry).
    fn mixin_chain(
        &mut self,
        inner: &Arc<Value>,
        mixins: &crate::gc::Gc<crate::value::MixinOverrides>,
    ) -> Vec<TypeId> {
        if mixins.contains_key("Str") {
            let allomorph_name = match inner.view() {
                ValueView::Int(_) | ValueView::BigInt(_) => Some("IntStr"),
                ValueView::Num(_) => Some("NumStr"),
                ValueView::Rat(_, _) | ValueView::FatRat(_, _) | ValueView::BigRat(_, _) => {
                    Some("RatStr")
                }
                ValueView::Complex(_, _) => Some("ComplexStr"),
                _ => None,
            };
            if let Some(name) = allomorph_name {
                return self.catalog_chain_for_name(name);
            }
        }
        let mut role_names: Vec<&str> = mixins
            .keys()
            .filter_map(|k| k.strip_prefix("__mutsu_role__"))
            .collect();
        role_names.sort_unstable();
        let mut chain: Vec<TypeId> = role_names.into_iter().map(TypeId::intern).collect();
        chain.extend(self.dispatch_mro(inner.as_ref()));
        chain
    }

    /// Shadow-mode comparison for ADR-0019 E1a (`MUTSU_VM_STATS`-gated, a no-op
    /// otherwise): compute the classifier's owner for `target` and compare it against
    /// `old_owner` (the name the *existing* dispatch-path logic at `site` already
    /// picked). Purely observational — `old_owner` continues to drive dispatch
    /// unchanged. See `todo/deep/adr0019-e1-typeid-receiver-owner.md`.
    pub(crate) fn shadow_check_owner(
        &mut self,
        site: &'static str,
        target: &Value,
        old_owner: &str,
    ) {
        if !crate::vm::vm_stats::enabled() {
            return;
        }
        let rc = self.receiver_dispatch_class(target);
        let matched = rc.type_id.as_str() == old_owner;
        crate::vm::vm_stats::record_owner_shadow_check(site, matched, || {
            format!(
                "old={old_owner} new={} definedness={:?} exec={:?}",
                rc.type_id, rc.definedness, rc.exec
            )
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn interp() -> Interpreter {
        Interpreter::new()
    }

    #[test]
    fn int_chain_matches_catalog() {
        let mut i = interp();
        let rc = i.receiver_dispatch_class(&Value::int(1));
        assert_eq!(rc.type_id.as_str(), "Int");
        assert_eq!(rc.definedness, Definedness::Concrete);
        let chain = i.dispatch_mro(&Value::int(1));
        let names: Vec<&str> = chain.iter().map(|t| t.as_str()).collect();
        assert_eq!(names, vec!["Int", "Cool", "Any", "Mu"]);
    }

    #[test]
    fn package_type_object_is_type_object_definedness_with_same_chain_as_instance() {
        let mut i = interp();
        let package = Value::package(crate::symbol::Symbol::intern("Int"));
        let rc = i.receiver_dispatch_class(&package);
        assert_eq!(rc.type_id.as_str(), "Int");
        assert_eq!(rc.definedness, Definedness::TypeObject);
    }

    #[test]
    fn nil_chain_is_not_collapsed_to_any() {
        // Unlike `value_type_name(Nil)` ("Any"), the classifier reports Nil's own
        // catalog chain -- one of E1's deliberate, ledgered divergences.
        let mut i = interp();
        let chain = i.dispatch_mro(&Value::NIL);
        let names: Vec<&str> = chain.iter().map(|t| t.as_str()).collect();
        assert_eq!(names, vec!["Nil", "Cool", "Any", "Mu"]);
    }

    #[test]
    fn hash_chain_includes_map() {
        let mut i = interp();
        let chain = i.dispatch_mro(&Value::hash(std::collections::HashMap::new()));
        let names: Vec<&str> = chain.iter().map(|t| t.as_str()).collect();
        assert_eq!(names, vec!["Hash", "Map", "Cool", "Any", "Mu"]);
    }
}
