//! The native-stack floor (ADR-0100), below every layer that needs it.
//!
//! The VM's call-boundary guard (`vm::vm_stack_guard`) and the value
//! renderers (`.Str` / `.gist` / `.raku` of a deeply nested value, #11217)
//! both ask the same question -- is this thread close to the bottom of its
//! stack? -- so the per-thread floor and the probe live here, in a leaf
//! module that names no upper layer.

use std::cell::Cell;

/// Stack kept below the guard on a full-size (256 MiB) stack -- the one every
/// VM-running thread normally gets (`main.rs`'s `mutsu-main`, and
/// `stack_budget::STACK_TIERS[0]` for `start`/Promise/Supply workers). It has
/// to cover three things once the guard fires: building the error, unwinding,
/// and running the `END` phasers that unwind reaches — plus the native
/// recursion a *single* Raku call can do between two checks (a deep regex
/// match, dropping a deeply nested value), which the call-boundary check by
/// construction cannot see.
///
/// A worker the address-space budget stepped down to a smaller stack
/// (ADR-0123) keeps a quarter of it instead, see [`reserve_for`].
pub(crate) const STACK_RESERVE_BYTES: usize = 16 * 1024 * 1024;

/// A stack smaller than this is left unguarded: its reserve would no longer
/// cover one check interval's worth of overshoot plus the unwind. The
/// smallest stack tier (`stack_budget::STACK_TIERS`) is exactly this.
pub(crate) const MIN_GUARDABLE_STACK_BYTES: usize = 32 * 1024 * 1024;

/// The reserve kept below the guard on a stack of `stack_size` bytes:
/// [`STACK_RESERVE_BYTES`], or a quarter of a smaller stack (8 MiB on the
/// 32 MiB tier -- still above the ~5 MiB worst-case interval overshoot of a
/// debug build, see `vm::vm_stack_guard::STACK_CHECK_INTERVAL`).
fn reserve_for(stack_size: usize) -> usize {
    STACK_RESERVE_BYTES.min(stack_size / 4)
}

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
    let top = approx_stack_pointer();
    // `stack_size` is a claim about the caller's stack, and a wrong one must
    // leave the thread unguarded rather than arm it into an already-exhausted
    // state. Saturating the subtraction instead of rejecting it did exactly
    // that on wasm32, where `usize` is 32 bits and the stack pointer sits a
    // megabyte or two into linear memory: `sp - 256 MiB` clamped to 0, and the
    // reserve then placed the floor ABOVE `sp`, so the very first check fired
    // and every `start {}` block died with "Too deep recursion".
    let Some(floor) = top
        .checked_sub(stack_size)
        .and_then(|bottom| bottom.checked_add(reserve_for(stack_size)))
        .filter(|floor| *floor < top)
    else {
        return;
    };
    STACK_FLOOR.with(|c| c.set(floor));
}

/// Whether this thread is too close to the bottom of its stack to make another
/// Raku call. Always false on an unguarded thread.
#[inline]
pub(crate) fn headroom_exhausted() -> bool {
    let floor = STACK_FLOOR.with(|c| c.get());
    floor != 0 && approx_stack_pointer() < floor
}

/// Whether this thread has a native stack floor to enforce.
#[inline]
pub(crate) fn stack_floor_armed() -> bool {
    STACK_FLOOR.with(|c| c.get() != 0)
}

/// The message a user sees. rakudo has no dedicated `X::Recursion` to match
/// (it grows its call stack on the heap and fails with an out-of-memory
/// exception instead), so this rides `X::AdHoc`, which every generic handler
/// already catches — see ADR-0100.
pub(crate) const DEEP_RECURSION_MESSAGE: &str = "Too deep recursion (out of stack space)";

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
    fn a_stack_size_bigger_than_the_address_below_us_leaves_the_thread_unguarded() {
        // The wasm32 shape: a 32-bit `usize` whose stack pointer is far below
        // the claimed stack size. Arming must decline, not compute a floor
        // above the current stack pointer — that made `headroom_exhausted()`
        // true immediately and killed every `start {}` block in the wasm
        // build with "Too deep recursion (out of stack space)".
        init_thread_stack_floor(usize::MAX);
        assert!(!headroom_exhausted());
        assert_eq!(STACK_FLOOR.with(|c| c.get()), 0);
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

    #[test]
    fn the_smallest_stack_tier_keeps_a_quarter_as_its_reserve() {
        // ADR-0123: a worker the address-space budget stepped down to the
        // smallest tier is still guarded, with a proportionally smaller
        // reserve.
        let size = *crate::runtime::stack_budget::STACK_TIERS.last().unwrap();
        assert_eq!(size, MIN_GUARDABLE_STACK_BYTES);
        std::thread::Builder::new()
            .stack_size(size)
            .spawn(move || {
                let top = approx_stack_pointer();
                init_thread_stack_floor(size);
                let floor = STACK_FLOOR.with(|c| c.get());
                assert_ne!(floor, 0, "the smallest tier must be guarded");
                let usable = top - floor;
                let expected = size - size / 4;
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
