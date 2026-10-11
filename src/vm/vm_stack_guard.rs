//! Native-stack headroom guard (ADR-0100).
//!
//! A Raku call is a Rust call in mutsu: the `exec_one` dispatch frame, the
//! call-op handler, signature binding and (for a re-entrant construct) a whole
//! `eval_block_value` frame all sit on the native stack, and the callee's body
//! loop runs nested inside them. So Raku recursion is native recursion, and
//! when it runs out the thread hits its guard page and the Rust runtime
//! **aborts** — no unwinding, nothing for `try`/`CATCH` to catch, no `END`
//! phaser, no backtrace.
//!
//! This module turns that into an ordinary catchable exception by checking, at
//! the VM's call boundary, how much stack is left. The measurement is the
//! stack pointer rather than a frame count on purpose: the per-call cost is
//! not a constant (a plain compiled call is cheap, a re-entrant one is not),
//! so a depth limit generous enough for one program is unsafe for another,
//! while the stack pointer needs no maintenance because it *is* the state.
//!
//! Each thread that runs user VM code records its floor once at start-up
//! ([`init_thread_stack_floor`]). A thread that never does — an `Interpreter`
//! driven from a `#[test]`, or the wasm library build — leaves the floor at
//! zero, which disables the check and keeps that thread's pre-ADR-0100
//! behaviour; there is no portable way to discover a stack bound mutsu did not
//! choose itself (`pthread_getattr_np` is glibc-only and `libc` is an optional
//! dependency here).

pub use crate::native_stack::init_thread_stack_floor;
pub(crate) use crate::native_stack::{DEEP_RECURSION_MESSAGE, headroom_exhausted};

/// Calls made between two headroom checks.
///
/// Checking at *every* call boundary cost ~4% on a call-dominated workload
/// (`fib(32)`): reading the stack pointer needs the address of a local, which
/// forces a stack slot and acts as an optimization barrier in the hottest
/// functions mutsu has. Amortizing it over an interval puts a decrement and a
/// predictable branch on the hot path instead, at the price of overshooting
/// the floor by at most one interval's worth of frames — which is what
/// [`crate::native_stack::STACK_RESERVE_BYTES`] is sized to absorb: 32 frames of the fattest kind
/// observed (~156 KiB each, an interpreted non-JIT frame in a debug build) is
/// ~5 MiB of the 16 MiB reserve (or of the 8 MiB one on the smallest stack
/// tier).
pub(crate) const STACK_CHECK_INTERVAL: u32 = 32;

/// Refuse a Raku call that this thread no longer has the stack to make.
///
/// Called at the VM's call boundary — the same seven entry points that carry
/// the `SafepointKind::Call` GC safepoint — so the error unwinds through the
/// ordinary `Result` path and reaches `try` / `CATCH` / `END` like any other
/// exception, instead of the guard page aborting the process.
#[cold]
#[inline(never)]
pub(crate) fn check_now() -> Result<(), crate::value::RuntimeError> {
    // #11217: a renderer that ran out of stack since the last check, and whose
    // truncated text no output or method-row path has reported yet.
    if crate::value::render_guard::take_render_overflow() {
        return Err(crate::value::render_guard::render_overflow_error());
    }
    if headroom_exhausted() {
        return Err(crate::value::RuntimeError::typed_msg(
            "X::AdHoc",
            DEEP_RECURSION_MESSAGE.to_string(),
        ));
    }
    Ok(())
}

impl crate::interpreter::Interpreter {
    /// The ADR-0100 call-boundary guard: raise rather than let deep recursion
    /// walk this thread off the end of its native stack. One decrement on the
    /// hot path; the actual stack read happens once per
    /// [`STACK_CHECK_INTERVAL`] calls.
    #[inline]
    pub(crate) fn guard_native_stack(&mut self) -> Result<(), crate::value::RuntimeError> {
        match self.stack_check_countdown.checked_sub(1) {
            Some(left) => {
                self.stack_check_countdown = left;
                Ok(())
            }
            None => {
                self.stack_check_countdown = STACK_CHECK_INTERVAL;
                check_now()
            }
        }
    }
}
