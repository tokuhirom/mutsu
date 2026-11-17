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
/// `stack_budget::initial_stack_bytes` for `start`/Promise/Supply workers
/// when the address space is unlimited). It has
/// to cover three things once the guard fires: building the error, unwinding,
/// and running the `END` phasers that unwind reaches — plus the native
/// recursion a *single* Raku call can do between two checks (a deep regex
/// match, dropping a deeply nested value), which the call-boundary check by
/// construction cannot see.
///
/// A smaller stack -- a worker's initial stack under an address-space budget
/// (ADR-17392) -- keeps a quarter of it instead, see [`reserve_for`]. So does
/// a stack segment ([`SEGMENT_BYTES`]).
pub(crate) const STACK_RESERVE_BYTES: usize = 16 * 1024 * 1024;

/// The smallest reserve that still covers one check interval's worst-case
/// overshoot plus the unwind (ADR-0123 §6). A debug build needs 8 MiB (over a
/// ~5 MiB worst-case interval overshoot). A release frame is about a tenth of
/// a debug one (measured 6-17 KiB per Raku call level: plain sub, method,
/// `.map` closure), so one interval overshoots well under 1 MiB and 1 MiB
/// suffices.
#[cfg(debug_assertions)]
const MIN_RESERVE_BYTES: usize = 8 * 1024 * 1024;
#[cfg(not(debug_assertions))]
const MIN_RESERVE_BYTES: usize = 1024 * 1024;

/// A stack smaller than this is left unguarded: half of it is
/// [`MIN_RESERVE_BYTES`], and below that no headroom would be left above the
/// reserve. A worker's initial stack under an address-space budget
/// (`stack_budget::initial_stack_bytes`, ADR-17392) is exactly this: 16 MiB in
/// a debug build, 2 MiB in a release one. The thread goes deeper on segments,
/// so only what a parked worker costs depends on it.
pub(crate) const MIN_GUARDABLE_STACK_BYTES: usize = 2 * MIN_RESERVE_BYTES;

/// The reserve kept below the guard on a stack of `stack_size` bytes:
/// [`STACK_RESERVE_BYTES`], or a quarter of a smaller stack, but never less
/// than [`MIN_RESERVE_BYTES`] (see `vm::vm_stack_guard::STACK_CHECK_INTERVAL`
/// for the overshoot it covers). So the smallest guardable stack keeps half of
/// itself, 1 MiB in a release build; a 64 MiB segment keeps 16 MiB.
fn reserve_for(stack_size: usize) -> usize {
    STACK_RESERVE_BYTES
        .min(stack_size / 4)
        .max(MIN_RESERVE_BYTES)
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
///
/// On a thread that may still grow ([`new_segment_if_low`]) this is the
/// bottom of the current *segment*: a site that can continue on a fresh one
/// asks [`new_segment_if_low`] first and only treats this as the end when no
/// segment is to be had.
#[inline]
pub(crate) fn headroom_exhausted() -> bool {
    let floor = STACK_FLOOR.with(|c| c.get());
    floor != 0 && approx_stack_pointer() < floor
}

/// The stack left above this thread's floor, or `None` on an unguarded
/// thread (whose depth nothing measures). On a growing thread this is the
/// headroom of the current segment.
// Cost: O(1).
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn headroom_bytes() -> Option<usize> {
    let floor = STACK_FLOOR.with(|c| c.get());
    (floor != 0).then(|| approx_stack_pointer().saturating_sub(floor))
}

/// The stack a freshly started user-code thread can use before its guard
/// fires, on the smallest stack one is ever given (`MIN_GUARDABLE_STACK_BYTES`,
/// ADR-17392): 8 MiB in a debug build, 1 MiB in a release one.
// Cost: O(1).
#[cfg(not(target_arch = "wasm32"))]
pub(crate) const fn fresh_stack_usable_bytes() -> usize {
    MIN_GUARDABLE_STACK_BYTES - MIN_RESERVE_BYTES
}

/// The logical stack a user-code thread may recurse through (ADR-17392): the
/// size of the one stack every such thread had before stacks grew in
/// segments, so a program reaches the same depth whichever thread it runs on.
pub(crate) const LOGICAL_STACK_BYTES: usize = 256 * 1024 * 1024;

/// The size of one stack segment a growing thread maps when it runs low.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) const SEGMENT_BYTES: usize = 64 * 1024 * 1024;

thread_local! {
    /// How many more segments this thread may map. 0 on a thread that was
    /// given its whole logical stack up front (the main thread, and every
    /// worker when the address space is unlimited) or that is unguarded.
    static SEGMENTS_LEFT: Cell<u32> = const { Cell::new(0) };
}

/// The segments a thread that starts on `initial` bytes needs to reach the
/// usable depth of a [`LOGICAL_STACK_BYTES`] stack.
// Cost: O(1).
#[cfg(not(target_arch = "wasm32"))]
fn segments_for(initial: usize) -> u32 {
    let target = LOGICAL_STACK_BYTES - reserve_for(LOGICAL_STACK_BYTES);
    let have = initial.saturating_sub(reserve_for(initial));
    let per = SEGMENT_BYTES - reserve_for(SEGMENT_BYTES);
    u32::try_from(target.saturating_sub(have).div_ceil(per)).unwrap_or(u32::MAX)
}

/// Arm the guard for a user-code thread that starts on `initial` bytes of
/// stack (see [`init_thread_stack_floor`]) and may grow in segments up to the
/// depth of a [`LOGICAL_STACK_BYTES`] stack (ADR-17392).
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn init_growable_thread_stack(initial: usize) {
    init_thread_stack_floor(initial);
    if stack_floor_armed() && cfg!(all(feature = "native", not(target_arch = "wasm32"))) {
        SEGMENTS_LEFT.with(|c| c.set(segments_for(initial)));
    }
}

/// A stack segment this thread may run on, taken by [`new_segment_if_low`].
/// Holds its share of the address-space budget until it is used.
pub(crate) struct Segment {
    #[cfg(all(feature = "native", not(target_arch = "wasm32")))]
    reservation: crate::runtime::stack_budget::StackReservation,
}

/// A fresh segment, when this thread is below the floor of its current one
/// and may still grow: it has segments left, and the address-space budget
/// (ADR-0123) has room for one. `None` means "carry on here", which past the
/// floor means the caller raises its "Too deep recursion" as before.
#[inline]
pub(crate) fn new_segment_if_low() -> Option<Segment> {
    if !headroom_exhausted() {
        return None;
    }
    take_segment()
}

/// A fresh segment for a thread that may still grow, whatever its headroom:
/// [`new_segment_if_low`] without the headroom test, for a caller that made
/// it already.
#[cold]
#[inline(never)]
pub(crate) fn take_segment() -> Option<Segment> {
    #[cfg(all(feature = "native", not(target_arch = "wasm32")))]
    {
        let left = SEGMENTS_LEFT.with(|c| c.get());
        if left == 0 {
            return None;
        }
        let reservation = crate::runtime::stack_budget::try_reserve(SEGMENT_BYTES)?;
        Some(Segment { reservation })
    }
    #[cfg(not(all(feature = "native", not(target_arch = "wasm32"))))]
    None
}

impl Segment {
    /// Run `f` on this segment and return to the current stack after it,
    /// with this thread's floor and segment count restored -- also when `f`
    /// unwinds.
    // Cost: O(1) plus `f`; maps and unmaps one segment.
    pub(crate) fn run<R>(self, f: impl FnOnce() -> R) -> R {
        #[cfg(all(feature = "native", not(target_arch = "wasm32")))]
        {
            struct Restore {
                floor: usize,
                left: u32,
            }
            impl Drop for Restore {
                fn drop(&mut self) {
                    STACK_FLOOR.with(|c| c.set(self.floor));
                    SEGMENTS_LEFT.with(|c| c.set(self.left));
                }
            }
            let _reservation = self.reservation;
            let left = SEGMENTS_LEFT.with(|c| c.get());
            let _restore = Restore {
                floor: STACK_FLOOR.with(|c| c.get()),
                left,
            };
            SEGMENTS_LEFT.with(|c| c.set(left - 1));
            stacker::grow(SEGMENT_BYTES, move || {
                let top = approx_stack_pointer();
                let floor = top - SEGMENT_BYTES + reserve_for(SEGMENT_BYTES);
                STACK_FLOOR.with(|c| c.set(floor));
                f()
            })
        }
        #[cfg(not(all(feature = "native", not(target_arch = "wasm32"))))]
        {
            f()
        }
    }
}

/// When this thread is below its floor and may grow, run `$call` again on a
/// fresh stack segment and `return` its result from the enclosing function.
/// Otherwise do nothing, so the site's own "Too deep recursion" check that
/// follows applies as before (ADR-17392).
///
/// `$call` is the enclosing function called again with the arguments it
/// received; it must sit before anything that consumes or changes them.
#[macro_export]
#[doc(hidden)]
macro_rules! grow_and_reenter {
    ($call:expr) => {
        if let Some(segment) = $crate::native_stack::new_segment_if_low() {
            return segment.run(|| $call);
        }
    };
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
                let expected =
                    MIN_GUARDABLE_STACK_BYTES * 2 - reserve_for(MIN_GUARDABLE_STACK_BYTES * 2);
                assert!(
                    usable.abs_diff(expected) < 64 * 1024,
                    "usable {usable} vs expected {expected}"
                );
            })
            .unwrap()
            .join()
            .unwrap();
    }

    /// A recursion that uses ~4 KiB of stack per level and continues on a
    /// fresh segment whenever it runs low. Returns how deep it got.
    #[cfg(all(feature = "native", not(target_arch = "wasm32")))]
    fn segmented_depth(levels: usize) -> usize {
        crate::grow_and_reenter!(segmented_depth(levels));
        if levels == 0 || headroom_exhausted() {
            return 0;
        }
        let pad = [0u8; 4096];
        std::hint::black_box(&pad);
        segmented_depth(levels - 1) + 1
    }

    #[cfg(all(feature = "native", not(target_arch = "wasm32")))]
    #[test]
    fn a_growable_thread_recurses_past_its_initial_stack() {
        // ADR-17392: a thread on the smallest stack goes three times as deep
        // as that stack, on segments, and is back on its own stack after.
        std::thread::Builder::new()
            .stack_size(MIN_GUARDABLE_STACK_BYTES)
            .spawn(|| {
                init_growable_thread_stack(MIN_GUARDABLE_STACK_BYTES);
                let levels = 3 * MIN_GUARDABLE_STACK_BYTES / 4096;
                let floor = STACK_FLOOR.with(|c| c.get());
                let left = SEGMENTS_LEFT.with(|c| c.get());
                assert!(left > 0);
                assert_eq!(segmented_depth(levels), levels);
                assert_eq!(STACK_FLOOR.with(|c| c.get()), floor);
                assert_eq!(SEGMENTS_LEFT.with(|c| c.get()), left);
            })
            .unwrap()
            .join()
            .unwrap();
    }

    #[cfg(all(feature = "native", not(target_arch = "wasm32")))]
    #[test]
    fn a_growable_thread_stops_at_its_logical_stack() {
        // The segments add up to the logical stack and no further, so a
        // runaway recursion still ends in the floor check, not the guard page.
        std::thread::Builder::new()
            .stack_size(MIN_GUARDABLE_STACK_BYTES)
            .spawn(|| {
                init_growable_thread_stack(MIN_GUARDABLE_STACK_BYTES);
                let depth = segmented_depth(usize::MAX);
                let logical = LOGICAL_STACK_BYTES / 4096;
                assert!(depth > logical / 2, "only {depth} levels");
                assert!(depth < 2 * logical, "{depth} levels");
            })
            .unwrap()
            .join()
            .unwrap();
    }

    #[test]
    fn a_full_stack_gets_no_segments() {
        assert_eq!(segments_for(LOGICAL_STACK_BYTES), 0);
        assert!(segments_for(MIN_GUARDABLE_STACK_BYTES) > 0);
    }

    #[test]
    fn the_smallest_stack_keeps_half_as_its_reserve() {
        // ADR-17392: a worker's small initial stack under an address-space
        // budget is still guarded, with the smallest reserve that covers one
        // check interval.
        let size = MIN_GUARDABLE_STACK_BYTES;
        std::thread::Builder::new()
            .stack_size(size)
            .spawn(move || {
                let top = approx_stack_pointer();
                init_thread_stack_floor(size);
                let floor = STACK_FLOOR.with(|c| c.get());
                assert_ne!(floor, 0, "the smallest stack must be guarded");
                let usable = top - floor;
                let expected = size - MIN_RESERVE_BYTES;
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
