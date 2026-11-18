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
//! - the receiver is a plain instance of a class the program declared (not a
//!   lazy Match, a built-in class or a wrapper value);
//! - the type is a registered class or role named without a smiley or a
//!   parameterization (a parameterized role's pun class, a registered class
//!   of its own, is admitted), and is not a subset (whose `where` reads the
//!   value);
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
        let ValueView::Instance { class_name, .. } = value.view() else {
            return false;
        };
        if value.is_lazy_match_value()
            || value.is_match_instance()
            || !self.types.user_declared_classes.contains(class_name.as_str())
            || self.types.user_declared_classes.contains("Any")
        {
            return false;
        }
        let key = (class_name, crate::symbol::wk::any(), MemoCheck::CoreAny);
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
        let ValueView::Instance { class_name, .. } = left.view() else {
            return None;
        };
        if left.is_lazy_match_value() || left.is_match_instance() {
            return None;
        }
        let ty = match ty.strip_suffix(":D") {
            Some(base) if allow_definite => base,
            _ => ty,
        };
        // A smiley (`R:D`) keeps the full check.
        if ty.is_empty() || ty.ends_with(":D") || ty.ends_with(":U") || ty.ends_with(":_") {
            return None;
        }
        if !self.types.user_declared_classes.contains(class_name.as_str()) {
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
            if !(registry.classes.contains_key(ty) || registry.roles.contains_key(ty))
                || registry.subsets.contains_key(ty)
                || registry.enum_types.contains_key(ty)
            {
                return None;
            }
        }
        if !self.type_name_names_itself(ty) {
            return None;
        }
        Some((class_name, ty))
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
    /// smiley, parameterization or lexical-site key. A `:D` smiley is admitted
    /// by the caller (a scalar is definite) and kept in the key.
    // Cost: O(n) in the name's length.
    fn scalar_memo_name_shape(base: &str) -> bool {
        base.starts_with(|c: char| c.is_ascii_uppercase())
            && base != "UInt"
            && !base.contains(['[', ':', '\0'])
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
