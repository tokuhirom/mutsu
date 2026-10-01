//! `OpCode::LoopExitGuard`: run a loop iteration's exit phasers when a loop
//! control signal unwinds out of the iteration.
//!
//! The phasers are queued on the *dynamic* path, the way rakudo's loop
//! handlers run them: whether a `next` leaves this loop is only known when the
//! signal arrives here, not where it is written. A `next` in a closure the body
//! calls leaves this loop, while the same closure handed to `.map` ends the
//! map's iteration instead (and so never reaches this guard).

use super::*;

impl Interpreter {
    /// Run the guarded body `[ip+1..body_end)`. On a `next` aimed at this loop
    /// run the NEXT queue `[body_end..exit_start)`; on any `next`/`last`/`redo`/
    /// `return` leaving the iteration run the UNDO+LEAVE queue
    /// `[exit_start..end)`; then re-raise the signal for the loop runner.
    ///
    /// The queue order (NEXT, then UNDO, then LEAVE) is the one rakudo uses for
    /// an interrupted iteration, the reverse of the fall-through order
    /// (KEEP/UNDO, LEAVE, NEXT) the compiler lays out after the body. An
    /// interrupted iteration's value is undefined, so UNDO runs, never KEEP.
    // Cost: O(1) per execution plus the body and, on an early exit, the phaser
    // queues.
    pub(super) fn exec_loop_exit_guard_op(
        &mut self,
        code: &CompiledCode,
        (body_end, exit_start, end): (u32, u32, u32),
        label: &Option<String>,
        ip: &mut usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let guard_ip = *ip;
        let body_end = body_end as usize;
        let stack_depth = self.stack.len();
        // A lazy-gather pull that suspended inside the body re-enters here:
        // continue where it stopped instead of replaying the body.
        let run_start = self
            .take_try_catch_gather_resume(code, guard_ip)
            .unwrap_or(guard_ip + 1);
        let err = match self.run_range(code, run_start, body_end, compiled_fns) {
            Ok(()) => {
                *ip = end as usize;
                return Ok(());
            }
            Err(e) => e,
        };
        if Self::is_gather_take_limit_signal(&err) {
            // A coroutine suspension, not an exit: the iteration goes on.
            return Err(self.park_try_catch_gather_suspend(code, guard_ip, body_end, err));
        }
        let queue_start = if err.is_next() && Self::label_matches(&err.label, label) {
            body_end
        } else if err.is_next() || err.is_last() || err.is_redo() || err.is_return() {
            exit_start as usize
        } else {
            return Err(err);
        };
        self.stack.truncate(stack_depth);
        // A phaser that itself dies replaces the signal, as in rakudo.
        self.run_range(code, queue_start, end as usize, compiled_fns)?;
        Err(err)
    }
}
