//! Stack guard for the recursive value renderers (#11217).
//!
//! `.Str`, `.gist` and `.raku` of a nested `Array` / `Hash` / `Pair` recurse
//! once per level, as plain `fn(&Value) -> String` functions with no
//! interpreter and no `Result`. A structure built at run time
//! (`$a = 1 => $a` a hundred thousand times) walked the thread off its native
//! stack and aborted the process, inside a single method call -- out of reach
//! of the ADR-0100 call-boundary guard.
//!
//! The renderers call [`render_too_deep`] on entering a container. When the
//! thread is within its ADR-0100 reserve, the renderer stops descending
//! (yielding [`TRUNCATED`] for that subtree) and the overflow is recorded on
//! the thread. Whoever turns the rendered string into a Raku result -- the
//! method-row dispatcher, `say` / `put` / `print` -- calls
//! [`take_render_overflow`] and raises the catchable "Too deep recursion"
//! error instead of using the truncated text. rakudo grows its stack on the
//! heap and eventually fails with an out-of-memory exception; a catchable
//! error is the closest equivalent that never crashes.

use std::cell::Cell;
use std::sync::atomic::{AtomicBool, Ordering};

/// Placeholder for a subtree the renderer did not descend into. Never reaches
/// the user: the overflow it marks is raised as an error first.
pub(crate) const TRUNCATED: &str = "...";

/// Set once any thread has recorded an overflow, so the common
/// [`take_render_overflow`] costs one relaxed load and no thread-local access.
static ANY_OVERFLOW: AtomicBool = AtomicBool::new(false);

thread_local! {
    /// This thread truncated a render that nobody has reported yet.
    static OVERFLOWED: Cell<bool> = const { Cell::new(false) };
}

/// Whether a renderer about to descend into a container must stop: the thread
/// is inside its native-stack reserve. Records the overflow when it is.
/// Always false on a thread with no armed stack floor.
// Cost: O(1).
#[inline]
pub(crate) fn render_too_deep() -> bool {
    if crate::native_stack::headroom_exhausted() {
        OVERFLOWED.with(|c| c.set(true));
        ANY_OVERFLOW.store(true, Ordering::Relaxed);
        return true;
    }
    false
}

/// Consume this thread's pending render overflow, if any.
// Cost: O(1).
#[inline]
pub(crate) fn take_render_overflow() -> bool {
    ANY_OVERFLOW.load(Ordering::Relaxed) && OVERFLOWED.with(|c| c.replace(false))
}

/// The catchable error a truncated render is reported as.
pub(crate) fn render_overflow_error() -> crate::value::RuntimeError {
    crate::value::RuntimeError::typed_msg(
        "X::AdHoc",
        crate::native_stack::DEEP_RECURSION_MESSAGE.to_string(),
    )
}

/// `result`, or the overflow error when the render that produced it was
/// truncated.
// Cost: O(1).
#[inline]
pub(crate) fn check_render<T>(
    result: Result<T, crate::value::RuntimeError>,
) -> Result<T, crate::value::RuntimeError> {
    if take_render_overflow() {
        return Err(render_overflow_error());
    }
    result
}
