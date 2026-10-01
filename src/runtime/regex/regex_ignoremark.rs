//! `:m` (ignoremark) over the published subject: the pattern runs in its
//! mark-stripped form over the target's stripped view, and every end, marker
//! and capture span is mapped back to subject space. Shared by the tree walk
//! and the compiled engine (ADR-0135 D4), which differ only in how they run
//! the stripped pattern.

use super::super::*;
use super::regex_helpers::{remap_caps_spans_derived_offset, strip_marks_pattern};
use super::regex_ltm_fate::{ltm_fate_frame_close_into, ltm_fate_frame_open};

/// Runs a mark-stripped pattern over a stripped subject slice from its start,
/// returning the ends it found (in stripped-slice coordinates).
pub(super) type StrippedRun<'f> =
    dyn FnMut(&mut Interpreter, &RegexPattern, &[char]) -> Vec<(usize, RegexCaptures)> + 'f;

impl Interpreter {
    /// `pattern` (a `:m` pattern) at `start` of `target`, run by `run` on the
    /// stripped forms, with every result mapped back to `target`'s positions.
    // Cost: O(r + c), r = the run itself, c = the capture spans remapped.
    pub(super) fn ignoremark_on_target(
        &mut self,
        pattern: &RegexPattern,
        target: &MatchTarget,
        start: usize,
        run: &mut StrippedRun<'_>,
    ) -> Vec<(usize, RegexCaptures)> {
        let stripped = target.stripped();
        let derived_start = stripped.original_to_stripped(start);
        let stripped_pattern = strip_marks_pattern(pattern);
        let enclosing_fate = ltm_fate_frame_open();
        let mut results = run(self, &stripped_pattern, &stripped.chars()[derived_start..]);
        ltm_fate_frame_close_into(enclosing_fate, |fate| {
            stripped.stripped_to_original(fate + derived_start)
        });
        let orig_len = target.chars().len();
        // `start` is the position this call was offered, which may be
        // a character `:ignoremark` strips away entirely (a bare
        // combining mark, say). `derived_start` already skipped past
        // it to reach the first surviving character the inner match
        // actually starts consuming from, so when that differs from
        // `start`, `start` was never a real consumed position and
        // `true_start` is this match's real beginning. Record it as
        // `capture_start` (the top-level caller's `caps.from = caps
        // .capture_start.unwrap_or(start)` fallback) so it propagates
        // up through `group_merge_delta` for a *scoped* `[:m ...]`
        // group too, not only a whole-pattern `:m`.
        //
        // Only do this when a skip actually happened: `start` is also
        // where THIS group was entered when it is not the very first
        // atom in the pattern (e.g. `'q'? [:m 'x']`, `<:Lu> [:m
        // 'AFE']`) -- there, nothing at `start` was stripped, so
        // `true_start == start` and setting `capture_start` here
        // would wrongly overwrite the outer match's real (earlier)
        // start once merged up, dropping whatever a preceding atom
        // already consumed.
        let true_start = stripped.stripped_to_original(derived_start);
        for (end, caps) in &mut results {
            *end = stripped.stripped_to_original(*end + derived_start);
            if let Some(cs) = caps.capture_start.as_mut() {
                *cs = stripped.stripped_to_original(*cs + derived_start);
            } else if true_start != start {
                caps.capture_start = Some(true_start);
            }
            if let Some(ce) = caps.capture_end.as_mut() {
                *ce = stripped.stripped_to_original(*ce + derived_start);
            }
            remap_caps_spans_derived_offset(caps, stripped.stripped_map(), orig_len, derived_start);
        }
        results
    }
}
