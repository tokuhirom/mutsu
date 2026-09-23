use super::super::*;
use super::regex_helpers::strip_marks_pattern;

impl Interpreter {
    /// Match regex anchored at a specific character position.
    /// Returns captures only if the match starts exactly at `pos`.
    // Cost: O(n), n = chars of `text`, to build the MatchTarget, plus one
    // anchored attempt at `pos`. Rakudo: O(1) setup -- see #9144.
    pub(crate) fn regex_match_with_captures_at(
        &mut self,
        pattern: &str,
        text: &str,
        pos: usize,
    ) -> Option<RegexCaptures> {
        let target = MatchTarget::new(text);
        self.regex_match_with_captures_at_target(pattern, &target, pos)
    }

    /// [`Self::regex_match_with_captures_at`] against a subject already
    /// materialized as a [`MatchTarget`].
    ///
    /// A caller that tries the same regex at many positions of ONE subject —
    /// `Str.trans` with a Regex key probes every character — must build the
    /// target once and call this; building it per position copies the whole
    /// subject each time and makes the scan quadratic (#9142).
    // Cost: O(1) setup plus one anchored attempt at `pos`.
    pub(crate) fn regex_match_with_captures_at_target(
        &mut self,
        pattern: &str,
        target: &MatchTarget,
        pos: usize,
    ) -> Option<RegexCaptures> {
        let parsed = self.parse_regex(pattern)?;
        let pkg = self.current_package_sym();
        let _target_scope = super::regex_helpers::MatchTargetScope::enter(target.clone());
        let orig_chars = target.chars();
        if pos > orig_chars.len() {
            return None;
        }
        if parsed.anchor_start && pos != 0 {
            return None;
        }
        if parsed.ignore_mark {
            let stripped = target.stripped();
            let stripped_chars = stripped.chars();
            let stripped_parsed = strip_marks_pattern(&parsed);
            let orig_len = orig_chars.len();
            // Find the stripped position corresponding to `pos`
            let stripped_pos = stripped.original_to_stripped(pos);
            if stripped_pos > stripped_chars.len() {
                return None;
            }
            return self
                .regex_match_end_from_caps_in_pkg(
                    &stripped_parsed,
                    stripped_chars,
                    stripped_pos,
                    pkg,
                )
                .map(|(end, mut caps)| {
                    caps.from = caps.capture_start.unwrap_or(stripped_pos);
                    caps.to = caps.capture_end.unwrap_or(end);
                    super::regex_helpers::remap_caps_spans(
                        &mut caps,
                        stripped.stripped_map(),
                        orig_len,
                    );
                    caps.set_target(Some(target.clone()));
                    caps
                });
        }
        self.regex_match_end_from_caps_in_pkg(&parsed, orig_chars, pos, pkg)
            .map(|(end, mut caps)| {
                caps.from = caps.capture_start.unwrap_or(pos);
                caps.to = caps.capture_end.unwrap_or(end);
                caps.set_target(Some(target.clone()));
                caps
            })
    }
}
