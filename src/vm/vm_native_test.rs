//! VM-side dispatch for `Test` / `Test::Util` functions.
//!
//! `is`/`ok`/`plan`/`is-deeply`/`subtest`/… are the bulk of the residual
//! function-dispatch fallback when running the roast suite (see
//! docs/vm-decoupling.md, lever A). They are implemented as typed Rust methods
//! (`Interpreter::call_test_function`) rather than user AST, and are routed to
//! the interpreter on purpose (`is_interpreter_handled_function`).
//!
//! Until now the VM reached them only by delegating to `Interpreter::call_function`
//! — the generic, giant-`match` function dispatcher — which re-runs argument
//! sanitisation and name resolution before finally landing on
//! `call_test_function`. This routes the VM straight to the typed Test handler
//! instead, the same "VM owns the native dispatch table" pattern already used
//! for `sprintf` and the pure builtins.
//!
//! Scope note: the *bodies* of these functions are inherently interpreter
//! coupled — they own the TAP counter/plan (`test_state`), emit TAP output, and
//! call back into method dispatch (`.gist`/`.raku`), `EVAL`, and subprocess
//! spawning. So this decouples the *dispatch layer*, not the bodies; moving the
//! TAP state itself out of the interpreter is the separate lever-B work.
//!
//! Safety: both call sites that invoke this run *after* the VM's user-function /
//! proto / multi-candidate / on-the-fly-compile resolution has already failed to
//! match, so a user-defined `sub ok { … }` still shadows the Test routine (it is
//! resolved earlier and never reaches here).

use super::*;
use crate::value::Value;

impl VM {
    /// Dispatch a `Test` module function straight to its typed handler when the
    /// name is a known Test function and test mode is active. Returns
    /// `Some(result)` when handled, `None` to let the caller fall back to
    /// `Interpreter::call_function` unchanged.
    pub(super) fn try_native_test_function(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !crate::runtime::Interpreter::is_test_function_name(name)
            || !self.interpreter.test_module_loaded()
        {
            return None;
        }
        // Some Test names collide with core builtins (notably `run`, which is the
        // `Proc` spawner far more often than `Test::Util::run`). The interpreter
        // resolves the builtin first (its `call_function` match runs before the
        // Test fallback), so never hijack a name that is also a builtin — let it
        // take the normal path.
        if crate::runtime::Interpreter::is_builtin_function(name) {
            return None;
        }
        // Replicate the pre-dispatch setup that `Interpreter::exec_call` /
        // `call_function` do before reaching `call_test_function`: strip the
        // synthetic `__test_callsite_line` argument and stash it so TAP
        // diagnostics report the right source line.
        let (clean_args, callsite_line) = self.interpreter.sanitize_call_args(args);
        loan_env!(self, set_pending_callsite_line(callsite_line));
        match loan_env!(self, call_test_function(name, &clean_args)) {
            // Matched and handled by the typed Test dispatcher.
            Ok(Some(value)) => Some(self.interpreter.maybe_fetch_rw_proxy(value, true)),
            // `is_test_function_name` said yes but the dispatcher declined: fall
            // back to the generic path so behaviour is never lost.
            Ok(None) => None,
            Err(e) => Some(Err(e)),
        }
    }
}
