//! The lexical `strict` pragma's mark, and what `no strict` makes of an
//! undeclared variable.
//!
//! Under `no strict` an undeclared `$x` / `@x` / `%x` is the current
//! package's `our` variable (`$OUR::x`): rakudo auto-declares it where it is
//! first used, and every other undeclared use of the same name in that
//! package aliases the same variable. mutsu stores such a write under the
//! bare env key and persists it in `our_vars`; the env key is block-scoped,
//! so a read after the block that made the write has to reach the package
//! store instead (#10622).
//!
//! `strict_mode` is the run-time switch the undeclared-write check reads,
//! and it is off unless a `use strict` ran, so it cannot tell an explicit
//! `no strict` apart. The `no strict` is recorded lexically in the env as
//! well (`__mutsu_pragma::strict`, as `MONKEY-SEE-NO-EVAL` is), and only a
//! scope under that mark falls back to the package store.

use super::*;
use crate::meta_ns::MetaNs;
use crate::symbol::Symbol;
use crate::value::ValueView;

const PRAGMA: &str = "strict";

impl Interpreter {
    /// Record `use strict` (`on`) / `no strict` in this scope.
    // Cost: O(1) expected, one env insert.
    pub(crate) fn mark_strict_pragma(&mut self, on: bool) {
        self.env_mut().insert_sym(
            MetaNs::Pragma.key_for_str(PRAGMA),
            if on { Value::TRUE } else { Value::FALSE },
        );
    }

    /// The package variable an undeclared `name` stands for under `no
    /// strict`, read on a miss of every other store. `None` when the scope is
    /// not under an explicit `no strict`, when `name` is not a plain
    /// unqualified `$`/`@`/`%` variable (bare scalars carry no sigil), or when
    /// nothing has been stored in it.
    // Cost: O(1) expected: one env probe, one `our_vars` probe.
    pub(crate) fn no_strict_package_var(&self, name: &str) -> Option<Value> {
        if !Self::is_auto_declarable_name(name) {
            return None;
        }
        if Self::env_is_no_strict(self.env()) {
            self.get_our_var(name).cloned()
        } else {
            None
        }
    }

    /// Whether `env` is under an explicit `no strict`.
    // Cost: O(1) expected, one env probe.
    pub(crate) fn env_is_no_strict(env: &crate::env::Env) -> bool {
        matches!(
            env.get_sym(MetaNs::Pragma.key_for_str(PRAGMA))
                .map(Value::view),
            Some(ValueView::Bool(false))
        )
    }

    /// Keep a variable a block auto-declared under `no strict` in the package
    /// store when the block's env scope ends. Most writes already put it there
    /// (`SetGlobal` persists every by-name store), but an autovivifying
    /// element store (`%h<k> = v` on an undeclared `%h`) creates the container
    /// in the env only. Returns whether `key` names such a variable (the
    /// caller then keeps its env binding too).
    // Cost: O(n) for the name check, n = name length, plus one `our_vars` insert.
    pub(crate) fn persist_no_strict_package_var(&mut self, key: Symbol, value: &Value) -> bool {
        let name = key.resolve();
        if !Self::is_auto_declarable_name(&name) {
            return false;
        }
        self.set_our_var(name, value.clone());
        true
    }

    /// A user variable name `no strict` auto-declares: an identifier, after an
    /// optional `@`/`%` sigil, with no twigil, no package qualifier, and not a
    /// compiler-internal `__` temporary or the topic.
    // Cost: O(n), n = name length.
    fn is_auto_declarable_name(name: &str) -> bool {
        let bare = name.strip_prefix(['@', '%']).unwrap_or(name);
        bare.chars()
            .next()
            .is_some_and(|c| c.is_alphabetic() || c == '_')
            && bare != "_"
            && !bare.starts_with("__")
            && !bare.contains("::")
    }
}
