//! Lazy-gather suspension for opcodes that `take` several times per execution.
//!
//! A bounded pull of a lazy `gather` (`force_lazy_list_vm_n`) suspends the
//! body by unwinding with [`Interpreter::LAZY_GATHER_TAKE_LIMIT_SIGNAL`] and
//! resumes it from a saved ip/stack. That is exact only at an instruction
//! boundary: an opcode that runs its own element loop — a hyper method call
//! `@a».take` — cannot record how far that loop got, so a signal raised by
//! its first `take` unwound the op and the remaining elements were lost
//! (`(gather { @a».take }).map({ $_ })` yielded only the first value, #9785).
//!
//! Such an op therefore runs with [`AsyncState::take_defer_to_op_end`](crate::runtime::async_state::AsyncState::take_defer_to_op_end) set:
//! a take-limit hit inside it only parks `gather_suspend_pending` (the op's
//! iteration is finite, so letting it finish just over-produces into the
//! gather's cache), and the op suspends right after it completes — at its own
//! instruction boundary, exactly like an `OpCode::Take` at the same ip.

use super::*;

impl Interpreter {
    /// Run `f` (the body of a multi-take opcode) with take-limit suspension
    /// deferred to the end of the op.
    // Cost: O(1) plus `f`.
    pub(super) fn run_take_deferring_op<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<T, RuntimeError>,
    ) -> Result<T, RuntimeError> {
        let saved = std::mem::replace(&mut self.async_state.take_defer_to_op_end, true);
        let result = f(self);
        self.async_state.take_defer_to_op_end = saved;
        result
    }

    /// Called after a multi-take opcode at `ip` completed successfully: when a
    /// take inside it reached the lazy pull's limit, suspend now, stamping the
    /// op's location the way `OpCode::Take` does so an enclosing `for` loop
    /// re-enters the same iteration right after this op.
    ///
    /// Inside a condition-driven loop (`lazy_take_boundary_defer`) the pending
    /// flag is left for that loop's iteration boundary instead — a suspension
    /// there re-enters from the condition, which would skip the rest of the
    /// body. A take reached through a nested routine call is likewise left to
    /// the driver frame (`gather_suspend_boundary_reached`).
    // Cost: O(1).
    pub(super) fn suspend_after_take_deferring_op(
        &mut self,
        code: &CompiledCode,
        ip: usize,
    ) -> Result<(), RuntimeError> {
        if self.async_state.take_defer_to_op_end
            || self.async_state.lazy_take_boundary_defer
            || !self.gather_suspend_boundary_reached()
        {
            return Ok(());
        }
        self.async_state.gather_suspend_pending = false;
        let mut e = RuntimeError::new(Self::LAZY_GATHER_TAKE_LIMIT_SIGNAL);
        e.set_take_suspend_site(Some((code.ops.as_ptr() as usize, ip)));
        Err(e)
    }
}
