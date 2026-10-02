//! Statement attempts of a fragment that is parsed from a copy of the unit's
//! text: the body of a regex code block (`{ ... }`, `<?{ ... }>`,
//! `<{ ... }>`). The regex parser hands `parse_fragment` a `String`, so the
//! statement positions in that copy are outside the unit's source buffer and
//! would not be recorded. Rakudo numbers those statements like any other
//! statement list (see `stmt::trace`), so the fragment's parse borrows the
//! enclosing unit's attempt list instead. It records each position shifted by
//! the unit offset the fragment was copied from.

use std::cell::RefCell;
use std::rc::Rc;

use super::{ORIGINAL_SOURCE, StatementAttempts, attempt_offset};

thread_local! {
    /// The attempt list (and fragment base offset) that the next
    /// `set_original_source` adopts; see [`lend_attempts`].
    static LENT: RefCell<Option<(Rc<RefCell<StatementAttempts>>, usize)>> =
        const { RefCell::new(None) };
}

/// The unit offset of `input`, for a unit that records statement attempts:
/// where a copy of the text starting at `input` would have to record its own
/// attempts. Follows a lent list, so it also works inside a nested fragment.
pub(in crate::parser) fn unit_offset(input: &str) -> Option<usize> {
    attempt_offset(input).map(|(_, offset)| offset)
}

/// The unit offset of `copy`, a copy of a stretch of `region` (the unit's own
/// text): a `token` body taken out of its braces and trimmed.
// Cost: O(n), n = region.len() (one substring search), when the unit records
// attempts; O(1) otherwise.
pub(in crate::parser) fn unit_offset_of_copy(region: &str, copy: &str) -> Option<usize> {
    let start = unit_offset(region)?;
    Some(start + region.find(copy)?)
}

/// Lend the current unit's attempt list to the next parse, which parses a
/// copy of the unit's text starting at unit offset `base`. A no-op when the
/// unit records no attempts. Undo it with [`clear_lent_attempts`] once that
/// parse returns.
pub(in crate::parser) fn lend_attempts(base: usize) {
    let attempts = ORIGINAL_SOURCE.with(|s| s.borrow().attempts.clone());
    LENT.with(|lent| *lent.borrow_mut() = attempts.map(|attempts| (attempts, base)));
}

/// Drop a loan that no parse adopted.
pub(in crate::parser) fn clear_lent_attempts() {
    LENT.with(|lent| *lent.borrow_mut() = None);
}

/// Called right after a parse installed its own source origin: take over a
/// lent attempt list.
pub(super) fn adopt_lent_attempts() {
    if let Some((attempts, base)) = LENT.with(|lent| lent.borrow_mut().take()) {
        ORIGINAL_SOURCE.with(|s| {
            let mut origin = s.borrow_mut();
            origin.attempts = Some(attempts);
            origin.attempt_base = Some(base);
        });
    }
}
