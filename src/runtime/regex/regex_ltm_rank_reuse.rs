//! Ranking a `|` branch from the walk that collected its ends, when that walk
//! already was a declarative-prefix measurement
//! ([#9617](https://github.com/tokuhirom/mutsu/issues/9617)).
//!
//! The plural alternation producer (`ltm_rank_and_collect_branches`) walks
//! every branch for its ends and then ranks it by
//! [`Interpreter::ltm_branch_rank_key`]. Inside a measurement both are the
//! same walk: `ltm_prefix_len_at` on a branch walks it with
//! `regex_match_ends_from_caps_in_pkg` under `LTM_DECLARATIVE_MODE`, from a
//! fresh fate frame, keeping the subrule stack, and takes the furthest end or
//! fate. So in `regex A { '{' [ <A> | . ]*? '}' }` every step of the loop
//! walked each branch twice, and the second walk was most of the time a
//! ranking took.
//!
//! [`ltm_branch_walk_open`] / [`LtmBranchWalk::close`] bracket the collecting
//! walk in a fate frame of its own, so the prefix comes out of that one walk.
//! The frame's fate is folded back into the enclosing frame without touching
//! `LTM_PREFIX_TERMINATED`, so the enclosing measurement sees exactly the
//! state the unbracketed walk would have left.

use super::super::*;
use super::regex_helpers::LTM_DECLARATIVE_MODE;
use super::regex_ltm_fate::{ltm_fate_frame_close, ltm_fate_frame_open};
use std::cell::Cell;
use std::collections::HashSet;

/// A branch walk bracketed while measuring (see the module docs).
pub(super) struct LtmBranchWalk {
    /// The enclosing frame's fate, while the branch's own frame is open.
    enclosing: Option<Option<usize>>,
}

/// Open a fate frame for one branch walk, when a measurement is in progress.
// Cost: O(1).
pub(super) fn ltm_branch_walk_open() -> LtmBranchWalk {
    let measuring = LTM_DECLARATIVE_MODE.with(Cell::get);
    LtmBranchWalk {
        enclosing: measuring.then(ltm_fate_frame_open),
    }
}

impl LtmBranchWalk {
    /// Close the frame and fold its fate into the enclosing one. Returns the
    /// branch's prefix length at `pos` — the furthest of `ends` and the
    /// frame's fate, as `ltm_prefix_len_at` would measure it — or `None`
    /// when no measurement is in progress.
    // Cost: O(e), e = ends of the branch.
    pub(super) fn close(self, ends: &[(usize, RegexCaptures)], pos: usize) -> Option<usize> {
        let enclosing = self.enclosing?;
        let fate = ltm_fate_frame_close(enclosing);
        if let Some(fate) = fate {
            // Merge, not `ltm_record_fate`: the walk already set (or a nested
            // alternation scope already settled) `LTM_PREFIX_TERMINATED`.
            let merged = enclosing.map_or(fate, |outer| outer.max(fate));
            ltm_fate_frame_close(Some(merged));
        }
        let furthest = ends.iter().map(|&(end, _)| end).chain(fate).max()?;
        Some(furthest - pos)
    }
}

impl Interpreter {
    /// [`Self::ltm_branch_rank_key`] for a branch whose prefix length is
    /// already known: only the `litlen` walk is left to do.
    // Cost: O(l), l = the branch's leading-literal walk (`ltm_litlen_at`).
    pub(super) fn ltm_branch_rank_key_with_prefix(
        &mut self,
        alt: &RegexPattern,
        chars: &[char],
        pos: usize,
        pkg: Symbol,
        plen: usize,
    ) -> (usize, usize) {
        let mut seen = HashSet::new();
        let litlen = self.ltm_litlen_at(alt, chars, pos, pkg, &mut seen, 0);
        (plen.max(litlen), litlen)
    }
}
