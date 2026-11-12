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
//!   parameterization, and is not a subset (whose `where` reads the value);
//! - at this point of the program the name is not re-bound: no type capture
//!   or package alias resolves it (`try_resolved_type_capture_name`) and no
//!   lexical term or constant of that name is in scope (`type_name_binding`).
//!
//! Every registry write may change an MRO or a composition, so the table is
//! dropped whenever the registry's write generation moves.

use super::super::*;

impl Interpreter {
    /// Whether `left` matches the type object named `type_name`
    /// (`left ~~ type_name`). See [`Self::type_object_accepts_uncached`] for
    /// the rules; this front answers a repeat check of a user-class instance
    /// against a nominal user type from the memo described in the module docs.
    // Cost: O(1) expected on a memo hit (two hash probes and a scope lookup
    // of the name); a miss is the O(d) check, d = MRO length.
    pub(crate) fn type_object_accepts(&mut self, left: &Value, type_name: Symbol) -> bool {
        let Some(key) = self.type_accepts_memo_key(left, type_name) else {
            return self.type_object_accepts_uncached(left, type_name);
        };
        let generation = self.registry_write_generation();
        let memo = &mut self.caches.instance_type_accepts;
        if memo.0 != generation {
            memo.1.clear();
            memo.0 = generation;
        }
        if let Some(&hit) = memo.1.get(&key) {
            return hit;
        }
        let answer = self.type_object_accepts_uncached(left, type_name);
        // The check may have written the registry (a published mixin class
        // adopted on this thread): remember it only under the generation the
        // answer was computed for.
        if self.registry_write_generation() == generation {
            self.caches.instance_type_accepts.1.insert(key, answer);
        }
        answer
    }

    /// The memo key `(class, type)` for `left ~~ type_name`, or `None` when
    /// the answer may depend on more than the two (see the module docs).
    // Cost: O(n) in the type name's length plus two registry probes and one
    // scope lookup of the name.
    fn type_accepts_memo_key(&self, left: &Value, type_name: Symbol) -> Option<(Symbol, Symbol)> {
        let ValueView::Instance { class_name, .. } = left.view() else {
            return None;
        };
        if left.is_lazy_match_value() || left.is_match_instance() {
            return None;
        }
        let ty = type_name.as_str();
        // A smiley (`R:D`) or a parameterization (`R[Int]`) keeps the full check.
        if ty.is_empty()
            || ty.ends_with(":D")
            || ty.ends_with(":U")
            || ty.ends_with(":_")
            || ty.contains('[')
        {
            return None;
        }
        if !self.types.user_declared_classes.contains(class_name.as_str()) {
            return None;
        }
        {
            let registry = self.registry();
            if !(registry.classes.contains_key(ty) || registry.roles.contains_key(ty))
                || registry.subsets.contains_key(ty)
                || registry.enum_types.contains_key(ty)
            {
                return None;
            }
        }
        if self.try_resolved_type_capture_name(ty).is_some() || self.type_name_binding(ty).is_some()
        {
            return None;
        }
        Some((class_name, type_name))
    }
}
