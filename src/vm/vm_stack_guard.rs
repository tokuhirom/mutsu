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

use std::cell::Cell;

/// Stack kept below the guard, out of the 256 MiB every VM-running thread
/// gets (`main.rs`'s `mutsu-main`, and
/// `builtins_system::USER_THREAD_STACK_SIZE` for `start`/Promise/Supply
/// workers). It has to cover three things once the guard fires: building the
/// error, unwinding, and running the `END` phasers that unwind reaches — plus
/// the native recursion a *single* Raku call can do between two checks (a deep
/// regex match, dropping a deeply nested value), which the call-boundary check
/// by construction cannot see.
const STACK_RESERVE_BYTES: usize = 16 * 1024 * 1024;

/// A stack at most this large is left unguarded: the reserve would eat most of
/// it, so the guard would refuse calls a thread of that size handles fine.
const MIN_GUARDABLE_STACK_BYTES: usize = STACK_RESERVE_BYTES * 4;

thread_local! {
    /// Lowest stack address this thread may still make a call from, or 0 when
    /// this thread is unguarded. Stacks grow down, so "headroom left" is
    /// `stack pointer - floor`.
    static STACK_FLOOR: Cell<usize> = const { Cell::new(0) };
}

/// An address on the current thread's stack, close enough to the stack pointer
/// for a megabyte-scale reserve. `black_box` keeps the local from being
/// optimized into a register with no address of its own.
#[inline(always)]
fn approx_stack_pointer() -> usize {
    let anchor = 0u8;
    std::hint::black_box(&anchor) as *const u8 as usize
}

/// Arm the guard for the calling thread, which must have `stack_size` bytes of
/// stack and must call this near the *top* of that stack (the first thing in a
/// spawned thread's closure, or in the process's real entry point) — the floor
/// is computed from where this is called.
///
/// Idempotent in effect but not in value: a later call from deeper in the same
/// thread would lower the floor, so call it once.
pub fn init_thread_stack_floor(stack_size: usize) {
    if stack_size < MIN_GUARDABLE_STACK_BYTES {
        return;
    }
    let floor = approx_stack_pointer()
        .saturating_sub(stack_size)
        .saturating_add(STACK_RESERVE_BYTES);
    STACK_FLOOR.with(|c| c.set(floor));
}

/// Whether this thread is too close to the bottom of its stack to make another
/// Raku call. Always false on an unguarded thread.
#[inline]
pub(crate) fn headroom_exhausted() -> bool {
    let floor = STACK_FLOOR.with(|c| c.get());
    floor != 0 && approx_stack_pointer() < floor
}

/// The message a user sees. rakudo has no dedicated `X::Recursion` to match
/// (it grows its call stack on the heap and fails with an out-of-memory
/// exception instead), so this rides `X::AdHoc`, which every generic handler
/// already catches — see ADR-0100.
pub(crate) const DEEP_RECURSION_MESSAGE: &str = "Too deep recursion (out of stack space)";

/// Calls made between two headroom checks.
///
/// Checking at *every* call boundary cost ~4% on a call-dominated workload
/// (`fib(32)`): reading the stack pointer needs the address of a local, which
/// forces a stack slot and acts as an optimization barrier in the hottest
/// functions mutsu has. Amortizing it over an interval puts a decrement and a
/// predictable branch on the hot path instead, at the price of overshooting
/// the floor by at most one interval's worth of frames — which is what
/// [`STACK_RESERVE_BYTES`] is sized to absorb: 32 frames of the fattest kind
/// observed (~156 KiB each, an interpreted non-JIT frame in a debug build) is
/// ~5 MiB of the 16 MiB reserve.
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_unarmed_thread_is_never_reported_exhausted() {
        assert!(!headroom_exhausted());
    }

    #[test]
    fn a_stack_too_small_to_hold_the_reserve_is_left_unguarded() {
        std::thread::spawn(|| {
            init_thread_stack_floor(1024);
            assert!(!headroom_exhausted());
        })
        .join()
        .unwrap();
    }

    #[test]
    fn an_armed_thread_has_headroom_right_after_arming() {
        std::thread::Builder::new()
            .stack_size(MIN_GUARDABLE_STACK_BYTES * 2)
            .spawn(|| {
                init_thread_stack_floor(MIN_GUARDABLE_STACK_BYTES * 2);
                assert!(!headroom_exhausted());
            })
            .unwrap()
            .join()
            .unwrap();
    }

    #[test]
    fn the_floor_sits_one_reserve_above_the_bottom_of_the_stack() {
        std::thread::Builder::new()
            .stack_size(MIN_GUARDABLE_STACK_BYTES * 2)
            .spawn(|| {
                let top = approx_stack_pointer();
                init_thread_stack_floor(MIN_GUARDABLE_STACK_BYTES * 2);
                let floor = STACK_FLOOR.with(|c| c.get());
                // Usable headroom is the stack minus the reserve, give or take
                // the few bytes between `top` and the arming call's own frame.
                let usable = top - floor;
                let expected = MIN_GUARDABLE_STACK_BYTES * 2 - STACK_RESERVE_BYTES;
                assert!(
                    usable.abs_diff(expected) < 64 * 1024,
                    "usable {usable} vs expected {expected}"
                );
            })
            .unwrap()
            .join()
            .unwrap();
    }
}
