//! `$obj ~~ SomeType` for an instance of a user class, remembered per
//! (class, type) for one registry write generation.
//!
//! Whether an instance of a user class matches a nominal class or role type
//! is decided by the class's MRO and composed roles alone: the instance's
//! attributes play no part. The general check (`type_object_accepts_uncached`)
//! re-derives that answer on every call through the type-matching gauntlet,
//! ~20k instructions for `$obj ~~ R` — Monad's `is-some` / `is-right` and
//! every chain check in its `t/14_concurrent_types.t` are this shape (#17331).
//!
//! The memo is consulted only where the answer cannot depend on anything
//! else:
//!
//! - the receiver is a plain instance whose class decides its type checks: a
//!   class the program declared, a registered class (a runtime mixin such as
//!   AttrX::Mooish's `Attribute+{AttrX::Mooish::Attribute}`), a core catalog
//!   class, or a core meta-object (`.HOW`, `Perl6::Metamodel::ClassHOW+{R}`)
//!   -- never a Match, a BOOT object or a `BOOTSTRAPATTR` (#17335);
//! - the type is a registered class or role, a core role or catalog class, or
//!   a core metamodel name (`Metamodel::GenericHOW`), named without a smiley
//!   or a parameterization (a parameterized role's pun class, a registered
//!   class of its own, is admitted), and is not a subset (whose `where` reads
//!   the value) or an enum;
//! - at this point of the program the name is not re-bound: no type capture
//!   or package alias resolves it (`try_resolved_type_capture_name`), and
//!   the name is bound in scope to nothing but that very type object
//!   (`type_name_binding`; a constant or term of the name shadows it).
//!
//! A built-in scalar (`Str`, `Int`, `Bool`, ...) checked against a nominal
//! type is remembered the same way, keyed by its built-in type: a plain scalar
//! has no MRO or roles of its own, so for a type whose answer does not read the
//! value (not a native type, `UInt`, a subset or an enum) the built-in type
//! decides it (#17876). SBOM::CycloneDX's `Map` tests every attribute value
//! with `~~ Positional`, `~~ Str:D`, `~~ Enumify`, ...
//!
//! Every registry write may change an MRO or a composition, so the table is
//! dropped whenever the registry's write generation moves.

use super::super::*;

pub(crate) use crate::runtime::resolution_caches::MemoCheck;

impl Interpreter {
    /// Whether `left` matches the type object named `type_name`
    /// (`left ~~ type_name`). See [`Self::type_object_accepts_uncached`] for
    /// the rules; this front answers a repeat check of a user-class instance
    /// or a built-in scalar against a nominal type from the memo described in
    /// the module docs.
    // Cost: O(1) expected on a memo hit (two hash probes and a scope lookup
    // of the name); a miss is the O(d) check, d = MRO length.
    pub(crate) fn type_object_accepts(&mut self, left: &Value, type_name: Symbol) -> bool {
        if let Some(kind) = Self::builtin_scalar_kind(left) {
            return self.scalar_type_object_accepts(left, kind, type_name);
        }
        let Some((class, _)) = self.instance_nominal_memo_key(left, type_name.as_str(), false)
        else {
            return self.type_object_accepts_uncached(left, type_name);
        };
        self.memoized_instance_type_check((class, type_name, MemoCheck::Accepts), |vm| {
            vm.type_object_accepts_uncached(left, type_name)
        })
    }

    /// [`Self::type_object_accepts`] for a built-in scalar of type `kind`
    /// (#17876). A key in the memo was admitted under the current registry
    /// generation, and admission reads only the registry and the static core
    /// tables, so a hit re-asks only the scope-dependent half: whether the
    /// name still means its own type here.
    // Cost: O(1) expected on a hit (one memo probe, one capture lookup and one
    // scope lookup of the name); a miss is the full check.
    fn scalar_type_object_accepts(&mut self, left: &Value, kind: Symbol, type_name: Symbol) -> bool {
        let ty = type_name.as_str();
        let base = ty.strip_suffix(":D").unwrap_or(ty);
        if !Self::scalar_memo_name_shape(base) {
            return self.type_object_accepts_uncached(left, type_name);
        }
        // A user class may be named like a built-in (`class Str`), so a
        // scalar's answer lives under its own check kind.
        let key = (kind, type_name, MemoCheck::ScalarAccepts);
        let admitted = match self.remembered_type_check(&key) {
            Some(hit) => {
                if self.type_name_names_itself(base) {
                    return hit;
                }
                false
            }
            None => self.scalar_memo_type_admissible(base) && self.type_name_names_itself(base),
        };
        if !admitted {
            return self.type_object_accepts_uncached(left, type_name);
        }
        self.memoized_instance_type_check(key, |vm| {
            vm.type_object_accepts_uncached(left, type_name)
        })
    }

    /// The memo's answer for `key` under the current registry generation.
    // Cost: O(1) expected.
    fn remembered_type_check(&self, key: &(Symbol, Symbol, MemoCheck)) -> Option<bool> {
        let memo = &self.caches.instance_type_accepts;
        if memo.0 != self.registry_write_generation() {
            return None;
        }
        memo.1.get(key).copied()
    }

    /// The memo key for `type_matches_value(constraint, value)` (a parameter,
    /// return or assignment type check), or `None` when the answer may depend
    /// on more than the instance's class and the type. A `:D` smiley is
    /// admitted: an instance is always definite.
    // Cost: as [`Self::instance_nominal_memo_key`], plus one intern.
    pub(crate) fn type_matches_memo_key(
        &self,
        value: &Value,
        constraint: &str,
    ) -> Option<(Symbol, Symbol, MemoCheck)> {
        let (class, _) = self.instance_nominal_memo_key(value, constraint, true)?;
        Some((class, Symbol::intern(constraint), MemoCheck::Matches))
    }

    /// Whether `value` is an instance of a user class whose MRO reaches the
    /// core `Any`, memoized per class for one registry write generation.
    /// `false` also when the memo does not apply (not a plain user-class
    /// instance, or the program declares its own `Any`); the caller then
    /// takes the full check.
    // Cost: O(1) expected on a hit; the core `Any` check on a miss.
    pub(crate) fn instance_matches_core_any(&mut self, value: &Value) -> bool {
        if self.types.user_declared_classes.contains("Any") {
            return false;
        }
        let Some(receiver) = self.memo_receiver_key(value) else {
            return false;
        };
        let key = (receiver, crate::symbol::wk::any(), MemoCheck::CoreAny);
        self.memoized_instance_type_check(key, |vm| {
            vm.type_matches_value_why_core("Any", value, &mut None)
        })
    }

    /// Answer `key` from the memo, or run `check` and remember its answer
    /// under the registry generation it was computed for.
    // Cost: O(1) expected on a hit; `check` on a miss.
    pub(crate) fn memoized_instance_type_check(
        &mut self,
        key: (Symbol, Symbol, MemoCheck),
        check: impl FnOnce(&mut Self) -> bool,
    ) -> bool {
        let generation = self.registry_write_generation();
        let memo = &mut self.caches.instance_type_accepts;
        if memo.0 != generation {
            memo.1.clear();
            memo.0 = generation;
        }
        if let Some(&hit) = memo.1.get(&key) {
            return hit;
        }
        let answer = check(self);
        // The check may have written the registry (a published mixin class
        // adopted on this thread): remember it only under the generation the
        // answer was computed for.
        if self.registry_write_generation() == generation {
            self.caches.instance_type_accepts.1.insert(key, answer);
        }
        answer
    }

    /// The instance's class and the type's name when `left` checked against
    /// the nominal type `ty` can be memoized (see the module docs), else
    /// `None`. With `allow_definite`, a `:D` smiley is admitted (the type
    /// returned is the name without it).
    // Cost: O(n) in the type name's length plus three registry probes and
    // one scope lookup of the name.
    fn instance_nominal_memo_key<'t>(
        &self,
        left: &Value,
        ty: &'t str,
        allow_definite: bool,
    ) -> Option<(Symbol, &'t str)> {
        let class_name = self.memo_receiver_key(left)?;
        let ty = match ty.strip_suffix(":D") {
            Some(base) if allow_definite => base,
            _ => ty,
        };
        // A smiley (`R:D`) keeps the full check.
        if ty.is_empty() || ty.ends_with(":D") || ty.ends_with(":U") || ty.ends_with(":_") {
            return None;
        }
        {
            let registry = self.registry();
            // A parameterization (`R[Int]`) keeps the full check, unless the
            // name is itself a registered class: the class a parameterized
            // role puns into (`R\0default[Any]`, what `::?CLASS` names in
            // its methods), which is as nominal as any other class.
            if ty.contains('[') && !registry.classes.contains_key(ty) {
                return None;
            }
            if registry.subsets.contains_key(ty) || registry.enum_types.contains_key(ty) {
                return None;
            }
            if !(registry.classes.contains_key(ty)
                || registry.roles.contains_key(ty)
                || Self::memo_core_nominal_type(ty))
            {
                return None;
            }
        }
        if !self.type_name_names_itself(ty) {
            return None;
        }
        Some((class_name, ty))
    }

    /// The memo's receiver key for `left`: the class of an instance whose
    /// class decides its type checks (`memo_receiver_class`), or, for such
    /// an instance with roles mixed in (`$obj does R`, the shape of a `.HOW`
    /// or an `Attribute` a trait composed a role into), a key naming the
    /// class and every marker of the composition. A mixin's check reads only
    /// those: its inner value's type and its override keys (role markers,
    /// allomorph and value-mixin markers). Parameterized constraints, which
    /// also read the role type arguments, are not admitted by the callers. A
    /// `Capture` is keyed by its kind alone. `None` for any other value.
    // Cost: O(1) for an instance; O(k log k) for a mixin, k = its markers
    // (sorted and interned into one key).
    fn memo_receiver_key(&self, left: &Value) -> Option<Symbol> {
        if left.is_lazy_match_value() {
            return None;
        }
        match left.view() {
            ValueView::Instance { class_name, .. } => {
                self.memo_receiver_class(left, class_name).then_some(class_name)
            }
            ValueView::Mixin(inner, mixins) => {
                let ValueView::Instance { class_name, .. } = inner.view() else {
                    return None;
                };
                if !self.memo_receiver_class(inner, class_name) {
                    return None;
                }
                let mut markers: Vec<&str> = mixins.keys().map(String::as_str).collect();
                markers.sort_unstable();
                let mut key = String::with_capacity(
                    16 + class_name.as_str().len() + markers.iter().map(|m| m.len() + 1).sum::<usize>(),
                );
                // `\u{1}` never occurs in a class name, so a composite key
                // cannot collide with a plain instance's class.
                key.push_str("\u{1}mixin\u{1}");
                key.push_str(class_name.as_str());
                for marker in markers {
                    key.push('\u{1}');
                    key.push_str(marker);
                }
                Some(Symbol::intern(&key))
            }
            // A Capture's checks read only its kind: it is always concrete,
            // and the roles it does are the class's (`Capture:D $params` on
            // AttrX::Mooish's `store-with-cb` / `invoke-opt`, #17335).
            ValueView::Capture { .. } => {
                static CAPTURE_KEY: std::sync::OnceLock<Symbol> = std::sync::OnceLock::new();
                Some(*CAPTURE_KEY.get_or_init(|| Symbol::intern("\u{1}view\u{1}Capture")))
            }
            _ => None,
        }
    }

    /// Whether an instance of `class` (the value `left`) has a type-check
    /// answer decided by its class alone, so the memo may key it by the
    /// class: a class the program declared, a registered class (a runtime
    /// mixin such as `Attribute+{R}`), a core catalog class, or a core meta
    /// object (`Perl6::Metamodel::ClassHOW+{R}`, a `.HOW`). A Match (whose
    /// checks read its cursor), a BOOT object or `IterationBuffer` view (which
    /// smartmatch only their own `BOOT*` type) and a `BOOTSTRAPATTR` (an
    /// `Attribute` whose identity is in its attributes) keep the full check.
    // Cost: O(1) expected: a few hash probes and one attribute-map probe.
    fn memo_receiver_class(&self, left: &Value, class: Symbol) -> bool {
        if left.is_match_instance()
            || left.is_iteration_buffer_view()
            || crate::runtime::boot_collection::is_boot_collection_invocant(left)
        {
            return false;
        }
        let name = class.as_str();
        let known = self.types.user_declared_classes.contains(name)
            || self.registry().classes.contains_key(name)
            || crate::builtin_types::catalog::builtin_type_info(name).is_some()
            || Self::is_core_meta_object_class(name);
        known && !crate::runtime::attribute_identity::is_bootstrapattr(left)
    }

    /// Whether `name` is the class of a core meta-object (`.HOW`):
    /// `Perl6::Metamodel::ClassHOW`, or one with roles mixed in
    /// (`Perl6::Metamodel::ClassHOW+{AttrX::Mooish::ClassHOW}`).
    // Cost: O(n) in the name's length plus a binary search of the core names.
    fn is_core_meta_object_class(name: &str) -> bool {
        let base = name.split_once("+{").map_or(name, |(base, _)| base);
        base.strip_prefix("Perl6::Metamodel::")
            .is_some_and(|bare| bare.ends_with("HOW") && crate::value::is_core_metamodel_name(bare))
    }

    /// Whether `ty` names a core nominal type whose answer for an instance
    /// is decided by the instance's class: a core role, a core catalog class
    /// or a core metamodel name (`Metamodel::GenericHOW`). Never a native
    /// type, `UInt`/`NativeInt` (value-dependent subsets) or a spelling with
    /// a smiley, parameterization or lexical-site suffix.
    // Cost: O(n) in the name's length plus a few table lookups.
    fn memo_core_nominal_type(ty: &str) -> bool {
        if !ty.starts_with(|c: char| c.is_ascii_uppercase())
            || matches!(ty, "UInt" | "NativeInt")
            || ty.contains(['[', '(', '\0', ' '])
        {
            return false;
        }
        crate::runtime::types::is_builtin_role_name(ty)
            || crate::builtin_types::catalog::builtin_type_info(ty).is_some()
            || ty
                .strip_prefix("Perl6::")
                .unwrap_or(ty)
                .strip_prefix("Metamodel::")
                .is_some_and(crate::value::is_core_metamodel_name)
    }

    /// The built-in type of a plain scalar receiver whose nominal type checks
    /// the memo may answer, else `None` (#17876). A plain scalar has no MRO or
    /// roles of its own: whether it matches a nominal class or role is decided
    /// by its built-in type alone.
    // Cost: O(1).
    fn builtin_scalar_kind(left: &Value) -> Option<Symbol> {
        use crate::symbol::wk;
        Some(match left.view() {
            ValueView::Int(_) | ValueView::BigInt(_) => wk::int_type(),
            ValueView::Num(_) => wk::num_type(),
            ValueView::Str(_) => wk::str_type(),
            ValueView::Bool(_) => wk::bool_type(),
            ValueView::Rat(..) => wk::rat_type(),
            ValueView::FatRat(..) => wk::fat_rat(),
            ValueView::Complex(..) => wk::complex_type(),
            _ => return None,
        })
    }

    /// Whether the spelling `base` (a type name with any `:D` removed) can
    /// name a type the scalar memo admits: not a native type (`int8`, `uint`,
    /// `num32`, `str`, ... -- all spelled lowercase), not `UInt`, and not a
    /// smiley or parameterization. A package-qualified name (`A::B`) and a
    /// lexical type's site key (`AttrProxy\0<id>`, what a `my class` in a
    /// role body is checked under) are admitted: the type side is then
    /// decided by `scalar_memo_type_admissible`, which wants a registered
    /// class or role for them. A `:D` smiley is admitted by the caller (a
    /// scalar is definite) and kept in the key.
    // Cost: O(n) in the name's length.
    fn scalar_memo_name_shape(base: &str) -> bool {
        base.starts_with(|c: char| c.is_ascii_uppercase())
            && base != "UInt"
            && !base.contains('[')
            && !has_lone_colon(base)
    }

    /// Whether the registry and the core tables make `base` a nominal type
    /// whose answer for a built-in scalar reads nothing but the scalar's
    /// type: a core role, a core class of the catalog, or a registered class
    /// or role -- never a subset or an enum. Stable for one registry write
    /// generation.
    // Cost: up to six hash probes.
    fn scalar_memo_type_admissible(&self, base: &str) -> bool {
        let registry = self.registry();
        if registry.subsets.contains_key(base) || registry.enum_types.contains_key(base) {
            return false;
        }
        registry.classes.contains_key(base)
            || registry.roles.contains_key(base)
            || crate::runtime::types::is_builtin_role_name(base)
            || crate::builtin_types::catalog::builtin_type_info(base).is_some()
    }

    /// Whether the type name `ty` means that very type here: no type capture
    /// or package alias resolves it, and the name is bound in scope to
    /// nothing but its own type object (a lexical type declaration binds its
    /// name to its type object; a constant or term of the name shadows it).
    // Cost: one capture lookup and one scope lookup of the name.
    fn type_name_names_itself(&self, ty: &str) -> bool {
        if self.try_resolved_type_capture_name(ty).is_some() {
            return false;
        }
        match self.type_name_binding(ty) {
            None => true,
            Some(bound) => matches!(bound.view(), ValueView::Package(p) if p == ty),
        }
    }
}

/// Whether `name` has a `:` that is not part of a `::` package separator (a
/// smiley such as `:U`, or an adverb such as `:ver<1>`).
// Cost: O(n) in the name's length.
fn has_lone_colon(name: &str) -> bool {
    let bytes = name.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b':' {
            if bytes.get(i + 1) == Some(&b':') {
                i += 2;
                continue;
            }
            return true;
        }
        i += 1;
    }
    false
}
