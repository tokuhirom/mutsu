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
//! Every registry write may change an MRO or a composition, so the table is
//! dropped whenever the registry's write generation moves.

use super::super::*;

pub(crate) use crate::runtime::resolution_caches::MemoCheck;

impl Interpreter {
    /// Whether `left` matches the type object named `type_name`
    /// (`left ~~ type_name`). See [`Self::type_object_accepts_uncached`] for
    /// the rules; this front answers a repeat check of a user-class instance
    /// against a nominal user type from the memo described in the module docs.
    // Cost: O(1) expected on a memo hit (two hash probes and a scope lookup
    // of the name); a miss is the O(d) check, d = MRO length.
    pub(crate) fn type_object_accepts(&mut self, left: &Value, type_name: Symbol) -> bool {
        let Some((class, _)) = self.instance_nominal_memo_key(left, type_name.as_str(), false)
        else {
            return self.type_object_accepts_uncached(left, type_name);
        };
        self.memoized_instance_type_check((class, type_name, MemoCheck::Accepts), |vm| {
            vm.type_object_accepts_uncached(left, type_name)
        })
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
        if self.try_resolved_type_capture_name(ty).is_some() {
            return None;
        }
        // A lexical type declaration binds its own name to its type object;
        // anything else bound under the name (a constant, a term) shadows it.
        match self.type_name_binding(ty) {
            None => {}
            Some(bound) if matches!(bound.view(), ValueView::Package(p) if p == ty) => {}
            Some(_) => return None,
        }
        Some((class_name, ty))
    }
}
