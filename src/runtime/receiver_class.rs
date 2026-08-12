//! ADR-0019 Phase E box E1: the receiver classifier.
//!
//! `receiver_dispatch_class`/`dispatch_mro` compute the "who owns this method for this
//! receiver" decision from a single place, using [`crate::type_id::TypeId`] and the
//! raku-adjudicated [`crate::builtins::builtin_type_catalog`] instead of the four
//! divergent MRO tables and the alias logic baked into `value_type_name`. See
//! `todo/deep/adr0019-e1-typeid-receiver-owner.md` for the full design and the
//! verification items (V1-V5) referenced in the comments below.
//!
//! **E1a** (landed) wired the classifier in shadow mode only, comparing its answer
//! against each site's existing string-based decision under `MUTSU_VM_STATS` counters
//! without changing behavior.
//!
//! **E1b** (this slice) makes the classifier authoritative at the dispatch/fallback
//! sites enumerated in the design doc's E1b bullet, via [`Interpreter::dispatch_owner_chain`]
//! / [`Interpreter::dispatch_owner_name`] (a `dispatch_mro` variant that skips a role
//! `Mixin`'s role-TypeId prefix — see its doc comment for why). The lone exception is
//! `multi_arg_type_keys` (`vm_call_method_compiled_cache.rs`), whose cutover is
//! deliberately deferred to `todo/tickets/multi-arg-type-keys-package-collision.md`:
//! unlike the other three original E1a sites, making it authoritative there is not a
//! shadow-mode-safe refactor but IS the fix for that ticket's Package-collision bug, so
//! it stays on `shadow_check_owner` until that ticket is picked up on its own. MOP
//! fallback consolidation (E1c) is still out of scope here.

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
        // A parametrized name (`Array[Int]`, `array[int32]`, `CArray[uint8]`)
        // not itself in the catalog: strip the `[...]` argument and splice the
        // BASE type's own catalog chain ahead of it (mirrors `registry.rs`'s
        // `class_mro`/`class_mro_readonly` fix for the same pattern, ADR-0019
        // E2b twelfth slice) -- without this, every typed-array VALUE's chain
        // dead-ended at itself, never reaching `Array`/`List`/`Any`/`Mu`.
        if let Some((base, _)) = name.split_once('[')
            && name.ends_with(']')
            && let Some(info) = builtin_type_info(base)
        {
            let mut chain = vec![TypeId::intern(name)];
            chain.extend(info.mro.iter().map(|s| TypeId::intern(s)));
            return chain;
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
                if continues {
                    // `reg_mro` already carries the catalog's own continuation
                    // (ADR-0019 E2b twelfth slice: `class_mro`'s parametrized-name
                    // fallback for `Array[Int]`/`array[int32]`/`CArray[uint8]`
                    // splices the full catalog tail itself) -- push the rest of
                    // `reg_mro` verbatim instead of stopping here, or the chain
                    // would silently drop everything past this builtin ancestor.
                    chain.extend(reg_mro[i + 1..].iter().map(|s| TypeId::from_symbol(*s)));
                } else {
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

    /// ADR-0019 **E1b**: the dispatch-owner chain for a **non-Instance,
    /// non-Package** receiver, authoritative at the fallback/qualified-dispatch
    /// sites enumerated in `todo/deep/adr0019-e1-typeid-receiver-owner.md`'s E1b
    /// slice. This is [`Self::dispatch_mro`] with one deliberate difference: for a
    /// role `Mixin` it skips the role-`TypeId` prefix `dispatch_mro` puts first,
    /// returning the *inner* value's own chain instead.
    ///
    /// Why the skip is required, not optional: every call site that consults this
    /// chain runs strictly *after* a dedicated, role-registry-aware path has
    /// already tried role methods for the same receiver
    /// (`dispatch_mixin_method_call` before `call_method_with_values`'s augment
    /// gate; `dispatch_qualified_mixin_method` before
    /// `dispatch_qualified_non_instance_method`). Re-deriving a role owner here
    /// would at best repeat that lookup, and at worst regress: using the role
    /// name as the SOLE owner for a role mixed onto a builtin value (`@a but R`)
    /// stops any lookup keyed on that single name from ever reaching the inner
    /// value's real builtin ancestry. Confirmed by direct repro before this cutover
    /// landed: `augment class Array { method my-foo {...} }; (@a but R).my-foo`
    /// resolved fine under the old `value_type_name`-based owner (which unwraps a
    /// Mixin to its inner value, same as this skip); switching the owner to
    /// `dispatch_mro`'s raw role-first chain's first element made it
    /// unresolvable, since `"R"` has no `augment`-recorded method of that name.
    /// Allomorphs (`<1/3>`, `IntStr` et al.) are exempted from the skip: their
    /// classifier chain already starts with the allomorph type itself, not a
    /// role, so `dispatch_mro`'s answer is already correct.
    pub(crate) fn dispatch_owner_chain(&mut self, value: &Value) -> Vec<TypeId> {
        if let ValueView::Mixin(inner, mixins) = value.view()
            && !mixins.contains_key("Str")
        {
            return self.dispatch_mro(inner.as_ref());
        }
        self.dispatch_mro(value)
    }

    /// The single canonical owner name for [`Self::dispatch_owner_chain`] — the
    /// classifier's authoritative answer for a non-Instance, non-Package receiver,
    /// replacing `value_type_name` at the E1b-cutover sites (fallback/augment
    /// gates that key a lookup on ONE name, not a full MRO walk).
    pub(crate) fn dispatch_owner_name(&mut self, value: &Value) -> &'static str {
        self.dispatch_owner_chain(value)
            .first()
            .map(|t| t.as_str())
            .unwrap_or_else(|| well_known_types().any.as_str())
    }

    /// ADR-0019 **E1c**: the owner name a `Metamodel` MOP entry (`.^ver`,
    /// `.^auth`, `.^attributes`, `.^concretization`, ...) keys its lookup on. A
    /// type object or instance reports its own declared name directly, exactly
    /// like the sites this replaces; every other receiver (concrete builtins,
    /// Enum values, role mixins) resolves through [`Self::dispatch_owner_name`]
    /// instead of `value_type_name` — the two agree except for the E1a-ledger
    /// cases (Enum, role mixin) where the classifier is the *correct* answer.
    /// Collapses the 13+8 duplicated `_ => value_type_name(&args[0]).to_string()`
    /// fallback arms surveyed across the MOP dispatch modules
    /// (`todo/deep/adr0019-e1-typeid-receiver-owner.md`) into one call.
    pub(crate) fn mop_receiver_owner(&mut self, value: &Value) -> String {
        match value.view() {
            ValueView::Package(name) => name.resolve(),
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            _ => self.dispatch_owner_name(value).to_string(),
        }
    }

    /// Shadow-mode comparison for ADR-0019 E1a (`MUTSU_VM_STATS`-gated, a no-op
    /// otherwise): compute the classifier's owner for `target` and compare it against
    /// `old_owner` (the name the *existing* dispatch-path logic at `site` already
    /// picked). Purely observational — `old_owner` continues to drive dispatch
    /// unchanged. Retained in E1b only at `multi_arg_type_keys`
    /// (`vm_call_method_compiled_cache.rs`), whose cutover is deliberately deferred
    /// to its own ticket — see `todo/tickets/multi-arg-type-keys-package-collision.md`.
    /// See `todo/deep/adr0019-e1-typeid-receiver-owner.md`.
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

    /// ADR-0019 Phase E box E2a shadow check (`MUTSU_VM_STATS`-gated, a no-op
    /// otherwise): when a native arity cascade at `site` just recognized
    /// `name` on `target` at `arity` (returned `Some`), verify the
    /// [`crate::builtins::native_method_row`] catalog already accounts for
    /// it, bumping `native_call_unmodeled` when it does not. `name` must be a
    /// `Symbol::as_str()` result (interned, `'static`) so the row lookup does
    /// not need to allocate. See `todo/deep/adr0019-e2-e4-resolver-core.md`
    /// decision 2's counter-to-zero discipline; nothing reads this catalog to
    /// make a real dispatch decision yet.
    ///
    /// **E2b**: walks the full [`Self::dispatch_owner_chain`], not just its
    /// first (most-derived) element. A row lives at the owner that actually
    /// *declares* the method (e.g. `so`/`not`/`defined` are `Any`-declared
    /// and recognized for every concrete receiver by the shared arity-0
    /// cascade arms), the same way [`Self::resolve_sequence`] walks the whole
    /// chain rather than probing only the receiver's own concrete type. A
    /// flat point lookup at the concrete owner alone over-counted every
    /// inherited-and-recognized method as unmodeled even though a row for it
    /// already existed further up the chain.
    ///
    /// Each chain element is also tried through
    /// [`canonical_builtin_owner`](crate::builtins::builtin_type_methods::canonical_builtin_owner),
    /// which folds a handful of builtin families to the single owner whose
    /// native table actually serves them (`Sub`/`Method`/`Block`/`Routine`/
    /// `Code` -> `Code`; the whole `Buf`/`Blob`/`utf8`/... family -> `Blob`;
    /// `FatRat` -> `Rat`). raku's own `.^mro` for these types does NOT
    /// include the folded owner (`Buf.new.^mro` is `Buf, Any, Mu`, not
    /// `Buf, Blob, Any, Mu`) -- confirmed against real `raku`, 2026-08-10 --
    /// so [`Self::dispatch_owner_chain`] correctly omits it too, and a plain
    /// chain walk can never find the folded owner's rows. The row catalog
    /// itself is generated keyed by this same folded owner (via
    /// [`super::builtin_type_methods::builtin_method_entries`]), so without
    /// this second lookup every Buf/Blob/FatRat-family method reads as
    /// permanently unmodeled no matter how many rows are added.
    pub(crate) fn record_native_row_coverage(
        &mut self,
        site: &str,
        target: &Value,
        name: &'static str,
        arity: usize,
    ) {
        if !crate::vm::vm_stats::enabled() {
            return;
        }
        let chain = self.dispatch_owner_chain(target);
        let mask = crate::builtins::native_method_row::NativeArityMask::for_arity(arity);
        let covered = chain_owner_probe(&chain, |owner| {
            crate::builtins::native_method_row::native_method_row(owner, name)
                .0
                .contains(mask)
        });
        let owner = chain
            .first()
            .map(|t| t.as_str())
            .unwrap_or_else(|| crate::type_id::well_known_types().any.as_str());
        crate::vm::vm_stats::record_native_call_recognition(site, owner, name, covered);
    }

    /// ADR-0019 Phase E box E7 step 4 (`.^can`,
    /// `todo/deep/adr0019-e5-e7-entry-routing.md` "E7 step 4"): does the E2
    /// native-method-row catalog have an explicit recognition row for `name`
    /// at ANY level of `target`'s dispatch chain -- the "does Raku consider
    /// this name a method on this type at all" existence question `.^can`
    /// asks. This is deliberately a DIFFERENT question from
    /// [`crate::runtime::resolution_sequence::native_row_servable`]'s
    /// call-shape-specific "is this row reachable for THIS call" (E4b's
    /// `Native` resolver candidate): unlike that function, this does NOT
    /// exclude `SPECIAL`/`MUTATES_RECEIVER` rows (a mutating method like
    /// `List.push` still IS a method `.can` should find) and ignores
    /// arity/definedness entirely (a method existing at any arity, or
    /// requiring definedness, still means `.can` is true) -- confirmed
    /// against real Raku behavior (`raku -e 'say List.can("push")'` is
    /// `(&push)`, true, on an INDEFINITE type object). Shares the same
    /// chain-walk-plus-`canonical_builtin_owner`-fold traversal as
    /// [`Self::record_native_row_coverage`] via [`chain_owner_probe`].
    pub(crate) fn e2_native_method_exists(&mut self, target: &Value, name: &'static str) -> bool {
        let chain = self.dispatch_owner_chain(target);
        chain_owner_probe(&chain, |owner| {
            crate::builtins::native_method_row::native_method_row_exists(owner, name)
        })
    }
}

/// Shared chain-walk-plus-fold traversal for [`Interpreter::record_native_row_coverage`]
/// and [`Interpreter::e2_native_method_exists`]: try `probe` at each level of
/// `chain` (most-derived first), and, when it fails, retry through
/// [`crate::builtins::builtin_type_methods::canonical_builtin_owner`]'s fold
/// (`Buf`/`Blob`/... -> `Blob`, `Sub`/`Method`/... -> `Code`, etc.) -- the
/// catalog is keyed by the folded owner, so a plain per-level lookup would
/// otherwise never find a Buf/Blob-family (or Sub/Method/...-family) row.
fn chain_owner_probe(chain: &[TypeId], mut probe: impl FnMut(&'static str) -> bool) -> bool {
    chain.iter().any(|owner| {
        let owner = owner.as_str();
        if probe(owner) {
            return true;
        }
        let folded = crate::builtins::builtin_type_methods::canonical_builtin_owner(owner);
        !folded.is_empty() && folded != owner && probe(folded)
    })
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

    /// ADR-0019 E2b: `Failure` is never declared as a real class (built
    /// purely via `Value::make_instance`), so before the `builtin_type_catalog`
    /// row was added its chain was just `["Failure"]` -- no continuation to
    /// `Any`/`Mu` at all, meaning the `Any`-declared universal-method rows
    /// (`so`/`defined`/`sink`/...) could never be found via the chain walk in
    /// `record_native_row_coverage` no matter how many rows were added.
    #[test]
    fn failure_chain_reaches_any_and_mu_via_nil() {
        let mut i = interp();
        i.run("my $f = Failure.new(\"oops\");").unwrap();
        let f = i.env().get("f").cloned().unwrap();
        let chain = i.dispatch_owner_chain(&f);
        let names: Vec<&str> = chain.iter().map(|t| t.as_str()).collect();
        assert_eq!(names, vec!["Failure", "Nil", "Cool", "Any", "Mu"]);
    }

    /// ADR-0019 E2b (twelfth slice, 2026-08-10): a bare parametrized TYPE
    /// OBJECT (`Array[Int]`, `array[int32].WHAT`, ...) is a `Package` whose
    /// name is the literal parametrized string -- never a user-declared
    /// class, so `class_mro("Array[Int]")` used to treat it as parentless
    /// (`compute_class_mro`'s fallback for an unregistered class with no
    /// parents), yielding just `["Array[Int]"]` with no continuation at
    /// all, unlike raku's real `Array[Int].^mro` (`Array[Int], Array, List,
    /// Cool, Any, Mu`). Confirmed against raku (`Array[Int].gist` was
    /// `native_call_unmodeled`-flagged before this fix, per the full `t/`
    /// sweep), then fixed in two steps: (1) `class_mro`/`class_mro_readonly`
    /// strip the `[...]` argument and splice the base's own catalog chain
    /// when the base is a catalog builtin (not just a registered class,
    /// which the existing `Blob[uint32]`-style handling already covered),
    /// and (2) `class_chain_with_catalog_tail`'s `continues` branch, which
    /// matched the now-already-spliced tail and `break`-ed WITHOUT pushing
    /// the rest of `reg_mro` -- silently truncating the chain right back
    /// down to `[Array[Int], Array]`. Fixed by extending `chain` with the
    /// remaining `reg_mro` elements on the `continues` branch instead of
    /// dropping them.
    #[test]
    fn parametrized_type_object_chain_is_not_truncated() {
        let mut i = interp();
        let package = Value::package(crate::symbol::Symbol::intern("Array[Int]"));
        let chain = i.dispatch_owner_chain(&package);
        let names: Vec<&str> = chain.iter().map(|t| t.as_str()).collect();
        assert_eq!(
            names,
            vec!["Array[Int]", "Array", "List", "Cool", "Any", "Mu"]
        );
    }

    /// ADR-0019 E2b (twelfth slice): the NativeCall-facing sibling of the
    /// test above, using the newly-added `array` catalog row (previously
    /// only `Array`, the boxed collection type, had one).
    #[test]
    fn typed_native_array_type_object_chain_is_not_truncated() {
        let mut i = interp();
        let package = Value::package(crate::symbol::Symbol::intern("array[int32]"));
        let chain = i.dispatch_owner_chain(&package);
        let names: Vec<&str> = chain.iter().map(|t| t.as_str()).collect();
        assert_eq!(names, vec!["array[int32]", "array", "Cool", "Any", "Mu"]);
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

    /// ADR-0019 E7 step 4: a plain 1-arg-recognized row is found at the
    /// receiver's own (most-derived) chain level.
    #[test]
    fn e2_native_method_exists_finds_own_level_row() {
        let mut i = interp();
        assert!(i.e2_native_method_exists(&Value::str_from("abc"), "chars"));
    }

    /// The dummy-`Value::NIL`-arg probe this box replaces only ever calls
    /// `native_method_0arg`/`native_method_1arg` -- never a 2-arg cascade --
    /// so a 2-arg-only method is invisible to it. `Str.substr-eq(pos, needle)`
    /// is exactly such a row (`("Str", "substr-eq", NativeArityMask::A2, ...)`
    /// in the generated table); the E2-row lookup finds it because it asks
    /// about EXISTENCE, not about whether a 0/1-arg cascade call happens to
    /// answer `Some`.
    #[test]
    fn e2_native_method_exists_finds_a_two_arg_only_method() {
        let mut i = interp();
        assert!(i.e2_native_method_exists(&Value::str_from("abc"), "substr-eq"));
    }

    /// A method that IS recognized, but only via a special/mutating path
    /// outside the pure arity cascades (`List.push`), still exists.
    #[test]
    fn e2_native_method_exists_finds_a_mutating_only_method() {
        let mut i = interp();
        let arr = Value::array(vec![Value::int(1)]);
        assert!(i.e2_native_method_exists(&arr, "push"));
    }

    /// A name the catalog never claims for this chain at all is correctly
    /// reported absent.
    #[test]
    fn e2_native_method_exists_is_false_for_an_unknown_name() {
        let mut i = interp();
        assert!(!i.e2_native_method_exists(&Value::int(1), "no-such-method-at-all"));
    }
}
