//! A method call on a `Failure` (ADR-11276): its own rows first, then
//! Rakudo's `FALLBACK`, which throws an unhandled `Failure`.
//!
//! The one entry for the interpreter's call dispatch and the VM's two
//! method-call opcodes, which each carried their own copy of the "an unhandled
//! Failure explodes" check (with drifted exemption lists).

use super::*;

/// The methods an unhandled `Failure` answers without throwing even though it
/// has no row for them: the `Mu`/`Any` introspection a `Failure` inherits
/// (its identity, type and truth), and the `Exception`/`Failure` control
/// methods, which are rows of their own (`exception_control.rs`).
const ANSWERED_UNHANDLED: &[&str] = &[
    "defined",
    "so",
    "not",
    "WHICH",
    "WHERE",
    "HOW",
    "WHY",
    "WHO",
    "WHAT",
    "DEFINITE",
    "VAR",
    "^name",
    "isa",
    "does",
    "can",
    "ACCEPTS",
    "is-handling",
    "perl",
    "rethrow",
    "die",
    "fail",
    "resume",
];

impl Interpreter {
    /// The answer of `method` called on `target` when it is a `Failure`:
    /// `Failure`'s row for the call, else, for an unhandled Failure and a
    /// method it does not answer as an object, its exception thrown (the
    /// Failure marked handled). `None` for any other receiver, and for a call
    /// the rest of the dispatch answers.
    // Cost: O(k + n), k = rows named `method`, n = names of
    // `ANSWERED_UNHANDLED`, plus the row's own cost.
    pub(crate) fn failure_own_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !crate::builtins::method_table::failure::is_failure(target) {
            return None;
        }
        if let Some(answer) =
            crate::builtins::method_table::failure::own_method(self, target, method, args)
        {
            return Some(answer);
        }
        if target.is_failure_handled() || ANSWERED_UNHANDLED.contains(&method) {
            return None;
        }
        Some(Err(crate::builtins::method_table::failure::explode(target)))
    }
}
