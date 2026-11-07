//! A log of the line comments a parse skips, for `Str.AST(:actions)`.
//!
//! Rakudo's grammar reports each comment to the actions object
//! (`comment:sym<#>`); mutsu's parser drops them in the whitespace skipper,
//! so while a log is open the skipper writes down where each one was.

use std::cell::RefCell;

thread_local! {
    static LOG: RefCell<Option<Vec<(usize, usize)>>> = const { RefCell::new(None) };
}

/// Start logging; the comments skipped from now on are kept until [`finish`].
// Cost: O(1).
pub fn start() {
    LOG.with(|l| *l.borrow_mut() = Some(Vec::new()));
}

/// Stop logging and answer the byte ranges of the comments seen, ascending
/// and without repeats (a backtracking parse skips one more than once).
// Cost: O(c log c), c = comments logged.
pub fn finish() -> Vec<(usize, usize)> {
    let mut log = LOG.with(|l| l.borrow_mut().take()).unwrap_or_default();
    log.sort_unstable();
    log.dedup();
    log
}

/// The whitespace skipper passed the comment `input` starts, which ends where
/// `after` begins.
// Cost: O(1) when no log is open.
pub(super) fn note(input: &str, after: &str) {
    if LOG.with(|l| l.borrow().is_none()) {
        return;
    }
    let Some(at) = super::primary::source_offset(input) else {
        return;
    };
    let end = at + (input.len() - after.len());
    LOG.with(|l| {
        if let Some(log) = l.borrow_mut().as_mut() {
            log.push((at, end));
        }
    });
}
