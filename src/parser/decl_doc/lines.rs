//! Rakudo's line rule for a trailing `#=` comment.
//!
//! A `#=` documents the declaration before it only while it stays next to it,
//! line by line: it must start on the line the declaration's text ends on (or,
//! inside a package body, the line the declaration starts on), or on the line
//! after that, or on the line after the start of the previous `#=` that
//! documented the same declaration. So a blank line, an ordinary comment or a
//! multi-line `#=[...]` block cuts the chain, and the comment past it has no
//! declarand: rakudo warns "Missing declarand for trailing declarator doc" and
//! attaches it to nothing.
//!
//! Routines and grammar rules are exempt (rakudo keeps them as the declarand
//! until the next statement, blank lines or not), and so are parameters and
//! anonymous code, whose `#=` placement [`super::attach_param`] and
//! [`super::attach`] already settle.

use super::{SiteIdent, source_offset, with_table, ws};
use crate::parser::primary::current_line_number;

/// The lines a recorded declaration spans, for the line rule.
#[derive(Clone, Copy, Debug)]
pub(super) struct SiteLines {
    /// Whether the line rule applies to this declaration at all.
    bound: bool,
    start_line: i64,
    /// The line the declaration's own text ends on (trailing whitespace and
    /// comments left out).
    end_line: i64,
    /// The source offset just past the declaration's own text. A `#=` before
    /// it is inside the declaration (a package body).
    body_end: usize,
}

impl SiteLines {
    const UNBOUND: SiteLines = SiteLines {
        bound: false,
        start_line: 0,
        end_line: 0,
        body_end: 0,
    };

    /// The lines of the declaration parsed from `start` to `end`.
    // Cost: O(n + log l), n = length of the declaration, l = lines in the unit.
    pub(super) fn of(start: &str, end: &str, ident: &SiteIdent, extends_to_eof: bool) -> Self {
        let bound = matches!(
            ident,
            SiteIdent::Package { .. } | SiteIdent::Attr { .. } | SiteIdent::Variable
        );
        let Some(from) = source_offset(start).filter(|_| bound) else {
            return Self::UNBOUND;
        };
        let start_line = current_line_number(start);
        if extends_to_eof {
            return SiteLines {
                bound,
                start_line,
                end_line: start_line,
                body_end: usize::MAX,
            };
        }
        let cut = trailing_doc_start(start, end).unwrap_or(end);
        let text = strip_trailing_trivia(&start[..start.len() - cut.len()]);
        let end_line = match text.char_indices().next_back() {
            Some((last, _)) => current_line_number(&start[last..]),
            None => start_line,
        };
        SiteLines {
            bound,
            start_line,
            end_line,
            body_end: from + text.len(),
        }
    }

    /// The line a `#=` at offset `at` may follow by at most one line, before
    /// any earlier `#=` of the same declaration extends it; `None` when the
    /// line rule does not apply.
    // Cost: O(1).
    pub(super) fn base_line(&self, at: usize) -> Option<i64> {
        self.bound.then(|| {
            if at < self.body_end {
                self.start_line
            } else {
                self.end_line
            }
        })
    }
}

/// `text` without the whitespace and whole comment lines after its last line
/// of code. The first line is never dropped: it is the declaration itself.
// Cost: O(n), n = length of `text`.
fn strip_trailing_trivia(mut text: &str) -> &str {
    loop {
        let trimmed = text.trim_end();
        let line_start = trimmed.rfind('\n').map_or(0, |i| i + 1);
        if line_start > 0 && trimmed[line_start..].trim_start().starts_with('#') {
            text = &trimmed[..line_start];
            continue;
        }
        return trimmed;
    }
}

/// The lines a declarator comment starting at `input` (whose text, as
/// written, is `written`) starts and ends on.
// Cost: O(n + log l), n = length of the comment, l = lines in the unit.
pub(super) fn comment_lines(input: &str, written: &str) -> (i64, i64) {
    let line = current_line_number(input);
    let end_line = match written.trim_end().char_indices().next_back() {
        Some((last, _)) => current_line_number(&input[last..]),
        None => line,
    };
    (line, end_line)
}

/// The first `#=` comment inside `start..end` that only whitespace (and
/// further comments) follows up to `end`, as a suffix of `start`. Only
/// comments `ws` actually recorded count, so a `#` inside a string default
/// cannot be mistaken for one.
// Cost: O(c + n), c = comments in the range, n = length of `start..end`.
pub(super) fn trailing_doc_start<'a>(start: &'a str, end: &str) -> Option<&'a str> {
    let (s, e) = (source_offset(start)?, source_offset(end)?);
    // Collected first: `ws` below records the comments it skips in the table.
    let candidates: Vec<usize> = with_table(|t| {
        t.comments
            .range(s..e)
            .filter(|(_, comment)| comment.trailing)
            .map(|(&at, _)| at)
            .collect()
    })?;
    candidates
        .into_iter()
        .map(|at| &start[at - s..])
        .find(|from| ws(from).is_ok_and(|(r, _)| r.len() == end.len()))
}
