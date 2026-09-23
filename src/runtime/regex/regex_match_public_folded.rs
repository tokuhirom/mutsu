//! The two public-match entry paths that match on a transformed copy of the
//! subject: `:m` (combining marks stripped) and `:i` with a multi-character
//! case fold (`ß` -> `ss`). Both map every recorded position back to the
//! original subject, including the fates of a declarative-prefix measurement
//! (`regex_ltm_fate`).

use super::super::*;
use super::regex_casefold::{casefold_pattern, casefold_text};
use super::regex_helpers::{map_pos, strip_marks_pattern};
use super::regex_ltm_fate::{ltm_fate_frame_close_into, ltm_fate_frame_open};
use super::regex_prefilter::regex_scan_positions;

impl Interpreter {
    /// `:m`: match on the mark-stripped subject. `measuring` limits the match
    /// to start 0 (see `regex_match_with_parsed_captures`).
    pub(super) fn regex_match_ignoremark_captures(
        &mut self,
        parsed: &RegexPattern,
        target: &MatchTarget,
        measuring: bool,
        pkg: Symbol,
    ) -> Option<RegexCaptures> {
        let stripped = target.stripped();
        let stripped_chars = stripped.chars();
        let stripped_parsed = strip_marks_pattern(parsed);
        let orig_len = target.chars().len();
        let enclosing_fate = ltm_fate_frame_open();
        let mut result = None;
        if stripped_parsed.anchor_start {
            result = self
                .regex_match_end_from_caps_in_pkg(&stripped_parsed, stripped_chars, 0, pkg)
                .map(|(end, mut caps)| {
                    caps.from = caps.capture_start.unwrap_or(0);
                    caps.to = caps.capture_end.unwrap_or(end);
                    super::regex_helpers::remap_caps_spans(
                        &mut caps,
                        stripped.stripped_map(),
                        orig_len,
                    );
                    caps.set_target(Some(target.clone()));
                    caps
                });
        } else {
            for start in regex_scan_positions(self, &stripped_parsed, stripped_chars, 0, pkg) {
                if measuring && start > 0 {
                    break;
                }
                if let Some((end, mut caps)) = self.regex_match_end_from_caps_in_pkg(
                    &stripped_parsed,
                    stripped_chars,
                    start,
                    pkg,
                ) {
                    caps.from = caps.capture_start.unwrap_or(start);
                    caps.to = caps.capture_end.unwrap_or(end);
                    super::regex_helpers::remap_caps_spans(
                        &mut caps,
                        stripped.stripped_map(),
                        orig_len,
                    );
                    caps.set_target(Some(target.clone()));
                    result = Some(caps);
                    break;
                }
            }
        }
        ltm_fate_frame_close_into(enclosing_fate, |fate| {
            map_pos(fate, stripped.stripped_map(), orig_len)
        });
        result
    }

    /// `:i` with a multi-character case fold: match on the case-folded
    /// subject. Matches must start and end at "fold boundaries" -- positions
    /// in folded space that correspond to the start of an original
    /// character's fold expansion -- so `t` is never matched out of the
    /// expansion of `ﬆ` -> `st`.
    pub(super) fn regex_match_casefold_captures(
        &mut self,
        parsed: &RegexPattern,
        target: &MatchTarget,
        measuring: bool,
        pkg: Symbol,
    ) -> Option<RegexCaptures> {
        let orig_chars = target.chars();
        let (folded_chars, pos_map) = casefold_text(orig_chars);
        let folded_parsed = casefold_pattern(parsed);
        let orig_len = orig_chars.len();

        // Helper: check if a position in folded space is at a fold boundary
        // (i.e., the start of an original character's expansion).
        let is_fold_boundary = |pos: usize| -> bool {
            pos == 0 || pos >= folded_chars.len() || pos_map[pos] != pos_map[pos - 1]
        };

        let enclosing_fate = ltm_fate_frame_open();
        let mut result = None;
        if folded_parsed.anchor_start {
            result = self
                .regex_match_end_from_caps_in_pkg(&folded_parsed, &folded_chars, 0, pkg)
                .and_then(|(end, mut caps)| {
                    let end_pos = caps.capture_end.unwrap_or(end);
                    if !is_fold_boundary(end_pos) {
                        return None;
                    }
                    caps.from = caps.capture_start.unwrap_or(0);
                    caps.to = end_pos;
                    super::regex_helpers::remap_caps_spans(&mut caps, &pos_map, orig_len);
                    caps.set_target(Some(target.clone()));
                    Some(caps)
                });
        } else {
            let last_start = if measuring { 0 } else { folded_chars.len() };
            for start in 0..=last_start {
                // Only try start positions at fold boundaries
                if !is_fold_boundary(start) {
                    continue;
                }
                if let Some((end, mut caps)) =
                    self.regex_match_end_from_caps_in_pkg(&folded_parsed, &folded_chars, start, pkg)
                {
                    let end_pos = caps.capture_end.unwrap_or(end);
                    // Only accept matches that end at fold boundaries
                    if !is_fold_boundary(end_pos) {
                        continue;
                    }
                    caps.from = caps.capture_start.unwrap_or(start);
                    caps.to = end_pos;
                    super::regex_helpers::remap_caps_spans(&mut caps, &pos_map, orig_len);
                    caps.set_target(Some(target.clone()));
                    result = Some(caps);
                    break;
                }
            }
        }
        ltm_fate_frame_close_into(enclosing_fate, |fate| map_pos(fate, &pos_map, orig_len));
        result
    }
}
