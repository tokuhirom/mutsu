//! Parse-time diagnostics rakudo treats as a "sorry": reported when the compile
//! phase ends rather than aborting the parse, so the unit's `CHECK` code still
//! runs (a negative literal subscript, #15742).
//!
//! mutsu keeps them fatal unless the unit has compile-phase code that could
//! observe the difference: a parse that finishes with pending sorries and no
//! `CHECK` phaser returns the first one exactly as it always did.

use std::cell::RefCell;

use super::parse_result::PError;

thread_local! {
    static PENDING: RefCell<Vec<PError>> = const { RefCell::new(Vec::new()) };
}

/// Queue `error`, found `remaining` bytes before the end of the parsed text.
/// A construct parsed again by a retried alternative is queued once.
// Cost: O(p), p = sorries already queued.
pub(crate) fn push(mut error: PError, remaining: usize) {
    error.remaining_len = Some(remaining);
    PENDING.with(|pending| {
        let mut pending = pending.borrow_mut();
        if !pending.iter().any(|e| e.remaining_len == Some(remaining)) {
            pending.push(error);
        }
    });
}

/// Drop whatever an earlier (or abandoned) parse queued.
// Cost: O(p), p = sorries queued.
pub(crate) fn clear() {
    PENDING.with(|pending| pending.borrow_mut().clear());
}

/// The queued sorries, in the order the text holds them (the parse is
/// left to right), emptying the queue.
// Cost: O(p log p), p = sorries queued.
pub(crate) fn take() -> Vec<PError> {
    let mut taken = PENDING.with(|pending| std::mem::take(&mut *pending.borrow_mut()));
    taken.sort_by_key(|e| std::cmp::Reverse(e.remaining_len));
    taken
}
