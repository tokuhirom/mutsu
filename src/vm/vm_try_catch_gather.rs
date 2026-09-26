//! Lazy-gather suspension through a `try`/CATCH/CONTROL region.
//!
//! A bounded pull of a lazy `gather` (`force_lazy_list_vm_n`) suspends the
//! body at a `take` by unwinding with the internal
//! [`Interpreter::LAZY_GATHER_TAKE_LIMIT_SIGNAL`]. That is a coroutine
//! suspension, not an exception: a user `CATCH` (even `default`) must never
//! see it, and the region it passes through must not end. So `OpCode::TryCatch`
//! parks a [`ForLoopResumeState::TryCatch`] continuation instead of dispatching
//! the signal to its handlers, and consumes it when the pull re-enters the op.

use super::*;
use crate::value::ForLoopResumeState;

impl Interpreter {
    /// Whether `e` is the lazy-gather take-limit suspension signal.
    // Cost: O(m), m = length of the error message (a string compare).
    pub(super) fn is_gather_take_limit_signal(e: &RuntimeError) -> bool {
        e.message == Self::LAZY_GATHER_TAKE_LIMIT_SIGNAL
    }

    /// On (re-)entry to the `TryCatch` op at `try_ip`: when a suspended pull
    /// parked a continuation for THIS region, consume it, restore the chained
    /// inner state into the resume slot, and return the ip to continue the
    /// protected body at. `None` is a fresh entry (run the body from its start).
    // Cost: O(1).
    pub(super) fn take_try_catch_gather_resume(
        &mut self,
        code: &CompiledCode,
        try_ip: usize,
    ) -> Option<usize> {
        let code_id = code.ops.as_ptr() as usize;
        if !matches!(
            self.gather_for_loop_resume,
            Some(ForLoopResumeState::TryCatch { code_id: cid, loop_ip, .. })
                if cid == code_id && loop_ip == try_ip
        ) {
            return None;
        }
        let Some(ForLoopResumeState::TryCatch {
            resume_ip, inner, ..
        }) = self.gather_for_loop_resume.take()
        else {
            unreachable!("checked above");
        };
        self.gather_for_loop_resume = inner.map(|b| *b);
        Some(resume_ip)
    }

    /// The protected body `[try_ip + 1, body_end)` of the `TryCatch` op at
    /// `try_ip` unwound with the take-limit signal `e`. Park where the body
    /// must continue on resume — right after a `take` directly in the body,
    /// or at a nested loop/region op whose own state is chained as `inner` —
    /// and hand the signal back to propagate to the pull driver, which keeps
    /// its ip on this op. A suspension this region cannot locate (one raised
    /// in another code object) propagates untouched, still unseen by CATCH.
    // Cost: O(1).
    pub(super) fn park_try_catch_gather_suspend(
        &mut self,
        code: &CompiledCode,
        try_ip: usize,
        body_end: usize,
        mut e: RuntimeError,
    ) -> RuntimeError {
        let code_id = code.ops.as_ptr() as usize;
        let body = (try_ip + 1)..body_end;
        let nested_site = self
            .gather_for_loop_resume
            .as_ref()
            .map(ForLoopResumeState::resume_op_site)
            .filter(|(cid, op)| *cid == code_id && body.contains(op));
        let (resume_ip, inner) = if let Some((_, op)) = nested_site {
            (op, self.gather_for_loop_resume.take().map(Box::new))
        } else if let Some((_, take_ip)) = e
            .take_suspend_site()
            .filter(|(cid, t)| *cid == code_id && body.contains(t))
        {
            e.set_take_suspend_site(None);
            (take_ip + 1, None)
        } else {
            return e;
        };
        self.gather_for_loop_resume = Some(ForLoopResumeState::TryCatch {
            code_id,
            loop_ip: try_ip,
            resume_ip,
            inner,
        });
        e
    }
}
