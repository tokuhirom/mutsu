//! The routine-call forms of `fail` (`fail(...)` in an expression, `.fail`,
//! and the `...` stub), which raise through builtins rather than through
//! `OpCode::Fail`, decide like the `fail` statement whether the failure
//! returns from a routine past that routine's `CATCH` (#17913).

use super::*;

impl Interpreter {
    /// `fail(...)` / `$exception.fail`.
    // Cost: as `builtin_fail_signal`, plus O(1) to read the calling chunk.
    pub(super) fn builtin_fail(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_fail_signal(args)
            .map_err(|err| self.mark_fail_from_calling_code(err))
    }

    /// The `...` stub: `fail` semantics with an `X::StubCode`.
    // Cost: as `builtin_stub_die_signal`, plus O(1) to read the calling chunk.
    pub(super) fn builtin_stub_die(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_stub_die_signal(args)
            .map_err(|err| self.mark_fail_from_calling_code(err))
    }

    /// Mark a fail signal raised from code that lies lexically in a routine
    /// (or is one) as returning from it, so the routine's own `CATCH` lets it
    /// pass and the call answers the Failure -- what the `fail` statement's
    /// `OpCode::Fail` records from the same compile-time facts. The calling
    /// chunk decides: a subset's `where` block declared outside the routine
    /// is not in it, so its `fail` stays a catchable exception there.
    // Cost: O(1).
    fn mark_fail_from_calling_code(&self, mut err: RuntimeError) -> RuntimeError {
        if !err.is_fail() || self.current_code == 0 {
            return err;
        }
        // SAFETY: `current_code` is the address of the live bytecode frame's
        // `CompiledCode` that is calling this builtin, kept alive for the whole
        // frame by `vm_call_*`; it is only read here, synchronously.
        let code = unsafe { &*(self.current_code as *const crate::opcode::CompiledCode) };
        if code.is_routine || code.lexically_in_routine {
            err.set_fail_in_routine_body();
        }
        err
    }
}
