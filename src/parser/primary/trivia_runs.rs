//! Where the last token of a statement ended, for its origin (ADR-10723 §4).
//!
//! A statement parser returns the input after the whitespace and comments
//! that follow the statement, so the consumed text can end in a comment
//! (`1 # note`). Rakudo's statement range stops at the last token. Reading a
//! comment back from the end of the text cannot be done reliably (`"a # b"`
//! holds a `#` too), so `ws` records each run of whitespace it consumed that
//! held a comment or Pod, and the statement's end is moved back over such a
//! run only when one ends exactly there.

use std::cell::RefCell;

/// Runs kept: a statement's trailing run is among the last few `ws` calls.
const KEPT: usize = 16;

#[derive(Clone, Copy, Default)]
pub(in crate::parser) struct TriviaRuns {
    /// `(start, end)` addresses of the runs, newest at `next - 1`.
    runs: [(usize, usize); KEPT],
    next: usize,
}

thread_local! {
    static RUNS: RefCell<TriviaRuns> = RefCell::new(TriviaRuns::default());
}

/// Record that `ws` consumed `start` up to `end` and met a comment or Pod.
// Cost: O(1).
pub(in crate::parser) fn note(start: &str, end: &str) {
    RUNS.with(|r| {
        let mut runs = r.borrow_mut();
        let slot = runs.next % KEPT;
        runs.runs[slot] = (start.as_ptr() as usize, end.as_ptr() as usize);
        runs.next = runs.next.wrapping_add(1);
    });
}

/// Forget the runs of a previous source (a new unit starts parsing).
// Cost: O(1).
pub(in crate::parser) fn clear() {
    RUNS.with(|r| *r.borrow_mut() = TriviaRuns::default());
}

/// The runs, to keep across a nested parse.
// Cost: O(1).
pub(in crate::parser) fn snapshot() -> TriviaRuns {
    RUNS.with(|r| *r.borrow())
}

// Cost: O(1).
pub(in crate::parser) fn restore(runs: TriviaRuns) {
    RUNS.with(|r| *r.borrow_mut() = runs);
}

/// The start of the recorded run that ends at address `end`, the earliest
/// one when several do (a lookahead may `ws` from inside the run again).
// Cost: O(1) (`KEPT` entries).
pub(in crate::parser) fn start_of_run_ending_at(end: usize) -> Option<usize> {
    RUNS.with(|r| {
        let runs = r.borrow();
        let kept = runs.next.min(KEPT);
        runs.runs[..kept]
            .iter()
            .filter(|&&(s, e)| e == end && s < e)
            .map(|&(s, _)| s)
            .min()
    })
}
