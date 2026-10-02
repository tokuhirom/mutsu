//! A finished match's two spans: the `<(` / `)>`-narrowed `.from`/`.to`, and
//! the cursor's own `(start, end)`.
//!
//! Raku keeps them apart: `)>` moves `.to` but not `.pos`, the position the
//! cursor reached (`'<abc>' ~~ /'<' <( \w+ )> '>'/` has `.to` 4, `.pos` 5), and
//! `Grammar.parse` asks whether the *cursor* covered the subject, not the
//! narrowed span (#10570).

use crate::runtime::RegexCaptures;

impl RegexCaptures {
    /// Settle a match that ran from `start` to `end`: `.from`/`.to` take the
    /// `<(` / `)>` markers when any fired, and the cursor span is kept
    /// alongside them in that case. Numbered captures move to the positional
    /// slots they name (#10895).
    // Cost: O(n + p), n = the distinct capture names, p = the positional slots.
    pub(crate) fn finish_span(&mut self, start: usize, end: usize) {
        self.settle_numbered_captures();
        self.from = self.capture_start.unwrap_or(start);
        self.to = self.capture_end.unwrap_or(end);
        if (self.from, self.to) != (start, end) {
            self.rare_mut().cursor_span = Some((start, end));
        }
    }

    /// Where the cursor started (`.from` unless `<(` moved it).
    // Cost: O(1).
    pub(crate) fn cursor_from(&self) -> usize {
        self.cursor_span().map_or(self.from, |(start, _)| start)
    }

    /// Where the cursor ended — Raku's `.pos` (`.to` unless `)>` moved it).
    // Cost: O(1).
    pub(crate) fn cursor_pos(&self) -> usize {
        self.cursor_span().map_or(self.to, |(_, end)| end)
    }

    /// `.pos` when it differs from `.to`, for a Match built from this span.
    // Cost: O(1).
    pub(crate) fn narrowed_pos(&self) -> Option<usize> {
        Some(self.cursor_pos()).filter(|&pos| pos != self.to)
    }

    fn cursor_span(&self) -> Option<(usize, usize)> {
        self.rare().and_then(|rare| rare.cursor_span)
    }
}
