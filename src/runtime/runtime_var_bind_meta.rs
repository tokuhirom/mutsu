//! The declared vs. bound element type of an `@`/`%` variable after `:=`.
//!
//! A `:=` bind aliases the RHS container, and element operations through the
//! variable must then obey the *bound container's* element type
//! (`my @a := Array[Int].new; @a[0] = "x"` dies). The by-name lane
//! (`__mutsu_type::<name>`) is what those operations consult, so the bind
//! paths propagate the container's type into it.
//!
//! A later rebind, however, is checked against the variable's *declaration*,
//! not against whatever it happens to be bound to right now: an untyped
//! `my @a` only constrains to `Positional`, so `@a := Array[Str].new` after
//! `@a := Array[Int].new` is fine, while `my Cool @c` still refuses
//! `Array[Any]` whatever it was bound to before.
//!
//! When a bind makes the two differ, the lane value becomes
//! `Pair(declared => current)` instead of the plain `Str` constraint:
//! `declared` is the declaration's element type (`""` for an untyped
//! declaration) and `current` the bound container's (a `Str`, or `Nil` when
//! the bound container is untyped). Keeping both halves in the one env entry
//! means every mechanism that scopes, saves, restores or captures the lane
//! (block exit, closure capture, routine return) carries the declaration with
//! it, and a re-declaration — which writes a plain `Str` or removes the key —
//! drops the override for free.

use super::*;

impl Interpreter {
    /// The element type `name` was DECLARED with, for checking a `:=` rebind.
    ///
    /// Differs from [`Self::var_type_constraint`] only after a bind replaced
    /// the in-effect constraint with the bound container's element type.
    // Cost: O(1).
    pub(crate) fn var_declared_type_constraint(&self, name: &str) -> Option<String> {
        if !Self::env_type_constraint_seen() {
            return None;
        }
        let name_sym = Symbol::intern(name);
        if !Self::env_type_constraint_seen_for(name_sym) {
            return None;
        }
        let stored = self.env.get_sym(Self::type_meta_key_for_sym(name_sym))?;
        match stored.view() {
            ValueView::Str(tc) => Some(tc.as_str().to_owned()),
            ValueView::Pair(declared, _) => (!declared.is_empty()).then(|| declared.clone()),
            _ => None,
        }
    }

    /// Install the element type of the container `name` was just `:=`-bound
    /// to as its in-effect constraint, preserving its declared one (see the
    /// module docs). `constraint` is in [`Self::set_var_type_constraint`]'s
    /// format; `None` means the bound container is untyped.
    // Cost: O(1).
    pub(crate) fn set_var_bound_type_constraint(&mut self, name: &str, constraint: Option<String>) {
        let declared = self.var_declared_type_constraint(name);
        self.set_var_type_constraint(name, constraint);
        let current = self.var_type_constraint(name);
        if current == declared {
            // The plain `Str` (or absent) lane already says both.
            return;
        }
        let name_sym = Symbol::intern(name);
        let current = match current {
            Some(c) => crate::runtime::constraint_meta::constraint_meta_value_str(&c),
            None => Value::NIL,
        };
        self.env.insert_sym(
            Self::type_meta_key_for_sym(name_sym),
            Value::pair(declared.unwrap_or_default(), current),
        );
        Self::mark_env_type_constraint_seen_for(name_sym);
    }
}
