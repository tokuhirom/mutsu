//! Matching one subject repeatedly: the search entry point a walk over a
//! single string (`split`, `subst(:g)`) calls once per match, and the
//! pattern it can parse once for the whole walk.

use super::super::*;
use super::regex_helpers::strip_marks_pattern;
use super::regex_prefilter::regex_scan_positions;
use crate::runtime::regex_types::RegexPattern;
use std::sync::Arc;

impl Interpreter {
    /// The parsed form of `pattern` when one parse can serve every match of a
    /// walk over one subject, or `None` when each match must look the pattern
    /// up again ([`Self::regex_match_with_captures_from_target`]).
    ///
    /// A static pattern (after its match-time scalars are marked, see
    /// [`Self::with_match_time_scalars`]) parses to the same tree on every
    /// lookup: the parse cache is keyed only by the text, the package and the
    /// token registry's generation, none of which a walk changes. Looking it
    /// up per match cost the scalar scan, the static scan and a hash probe,
    /// about 700 instructions a separator on `.split(/\s+/)`. A dynamic
    /// pattern (interpolation, `<$var>`) keeps the per-match lookup, since a
    /// code block in it may change what it interpolates.
    // Cost: O(p) once, p = the pattern's length, plus the (cached) parse.
    pub(crate) fn regex_walk_pattern(&self, pattern: &str) -> Option<Arc<RegexPattern>> {
        let marked = self.with_match_time_scalars(pattern, Vec::new());
        if !crate::runtime::regex_parse::regex_pattern_is_static(&marked) {
            return None;
        }
        self.parse_regex(&marked)
    }

    /// [`Self::regex_match_with_captures_from`] against a subject already
    /// materialized as a [`MatchTarget`].
    ///
    /// A caller that scans the SAME subject repeatedly — `split` walks one
    /// string separator by separator — must build the target once and call this.
    /// Building it costs a copy of the whole subject (an `Arc<String>` plus an
    /// `Arc<[char]>`, ~5 bytes per character), so paying it per match made
    /// `.split(/rx/)` O(separators x subject): 11.9 s on an 80 KB subject where
    /// rakudo takes 0.26 s (#8247).
    // Cost: the (cached) parse lookup, plus the search from `from_pos`.
    pub(crate) fn regex_match_with_captures_from_target(
        &mut self,
        pattern: &str,
        target: &MatchTarget,
        from_pos: usize,
    ) -> Option<RegexCaptures> {
        let parsed = self.parse_regex_for_match(pattern)?;
        let mut caps = self.regex_match_parsed_from_target(&parsed, target, from_pos)?;
        caps.set_target(Some(target.clone()));
        Some(caps)
    }

    /// [`Self::regex_match_with_captures_from_target`] with the pattern
    /// already parsed ([`Self::regex_walk_pattern`]). The captures do NOT
    /// carry `target`: a caller that builds a `Match` from them attaches it
    /// with [`RegexCaptures::set_target`] (an allocation per match that a
    /// plain `split` never needs).
    // Cost: the search from `from_pos`: the prefilter scan plus the engine's
    // work at each admitted start.
    pub(crate) fn regex_match_parsed_from_target(
        &mut self,
        parsed: &RegexPattern,
        target: &MatchTarget,
        from_pos: usize,
    ) -> Option<RegexCaptures> {
        let pkg = self.current_package_sym();
        let _target_scope = super::regex_helpers::MatchTargetScope::enter(target.clone());
        let orig_chars = target.chars();
        if from_pos > orig_chars.len() {
            return None;
        }
        if parsed.anchor_start && from_pos != 0 {
            return None;
        }
        if parsed.ignore_mark {
            let stripped = target.stripped();
            let stripped_chars = stripped.chars();
            let stripped_parsed = strip_marks_pattern(parsed);
            let orig_len = orig_chars.len();
            let stripped_from = stripped.original_to_stripped(from_pos);
            let start_pos = if stripped_parsed.anchor_start {
                0
            } else {
                stripped_from
            };
            for start in
                regex_scan_positions(self, &stripped_parsed, stripped_chars, start_pos, pkg)
            {
                if let Some((end, mut caps)) = self.regex_match_end_from_caps_in_pkg(
                    &stripped_parsed,
                    stripped_chars,
                    start,
                    pkg,
                ) {
                    caps.finish_span(start, end);
                    super::regex_helpers::remap_caps_spans(
                        &mut caps,
                        stripped.stripped_map(),
                        orig_len,
                    );
                    return Some(caps);
                }
            }
            return None;
        }
        let start_pos = if parsed.anchor_start { 0 } else { from_pos };
        let starts = regex_scan_positions(self, parsed, orig_chars, start_pos, pkg);
        let (start, end, mut caps) = self.rx_scan_first(parsed, orig_chars, starts, pkg)?;
        caps.finish_span(start, end);
        Some(caps)
    }
}
