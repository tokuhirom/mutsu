use super::super::*;
use super::regex_helpers::{map_pos, strip_marks_pattern};
use super::regex_prefilter::regex_scan_positions;

/// One match's span plus every capture text it produced: the positional list
/// (`$0`, `$1`, ...) and the per-name list (`$<name>`, which is a list because a
/// quantified named capture matches repeatedly).
pub(crate) type MatchWithAllCaptures = (usize, usize, Vec<String>, HashMap<String, Vec<String>>);

impl Interpreter {
    /// Ranking key for selecting the best full (anchored) match: prefer the
    /// longest end, then more captures. Equal keys are left to the caller's
    /// STABLE sort, which preserves DFS priority order (highest priority first).
    fn full_match_rank(m: &(usize, RegexCaptures)) -> (usize, usize, usize) {
        let (end, caps) = m;
        let total_named: usize = caps.named.values().map(|v| v.nodes.len()).sum();
        (*end, caps.positional.len(), total_named)
    }

    /// Match `pattern` anchored at the start of `text` and require it to cover the
    /// whole text (what `Grammar.parse` needs). When it only matches a PREFIX —
    /// so the parse fails — the best-ranked partial match is written to `partial`.
    /// `Grammar.parse(:actions(...))` needs that: Rakudo dispatches the start
    /// rule's action whenever the start rule matched, even though the parse as a
    /// whole then fails on the leftover text.
    pub(in crate::runtime) fn regex_match_with_captures_full_from_start_tracking_partial(
        &mut self,
        pattern: &str,
        text: &str,
        partial: &mut Option<RegexCaptures>,
    ) -> Option<RegexCaptures> {
        let parsed = self.parse_regex(pattern)?;
        let pkg = self.current_package_sym();
        let target = MatchTarget::new(text);
        let _target_scope = super::regex_helpers::MatchTargetScope::enter(target.clone());
        let orig_chars = target.chars();

        if parsed.ignore_mark {
            let stripped = target.stripped();
            let stripped_chars = stripped.chars();
            let stripped_parsed = strip_marks_pattern(&parsed);
            let orig_len = orig_chars.len();
            let mut matches =
                self.regex_match_ends_stop_at_full(&stripped_parsed, stripped_chars, 0, pkg);
            if matches.is_empty() {
                return None;
            }
            // Sort DESCENDING by (end, captures…) with a STABLE sort so that
            // equal-key matches keep their incoming priority order (the DFS
            // returns highest-priority first). Then take the FIRST full match.
            // Using ascending `sort_by_key` + `.rev().find(...)` was wrong: `.rev()`
            // inverts the priority order of equal-key matches, so the LOWEST-priority
            // alternative of a `||`/`|` tie won (`^ [ <a> || <b> ] $` picked `<b>`).
            matches.sort_by(|a, b| Self::full_match_rank(b).cmp(&Self::full_match_rank(a)));
            let (end, mut caps) = matches
                .into_iter()
                .find(|(end, _)| *end == stripped_chars.len())?;
            caps.from = caps.capture_start.unwrap_or(0);
            caps.to = caps.capture_end.unwrap_or(end);
            super::regex_helpers::remap_caps_spans(&mut caps, stripped.stripped_map(), orig_len);
            caps.set_target(Some(target));
            return Some(caps);
        }

        let mut matches = self.regex_match_ends_stop_at_full(&parsed, orig_chars, 0, pkg);
        if matches.is_empty() {
            return None;
        }
        // See the comment in the ignore_mark branch above: DESCENDING stable sort
        // then take the first full match, so equal-key `||`/`|` ties keep the
        // highest-priority alternative instead of inverting it via `.rev()`.
        matches.sort_by(|a, b| Self::full_match_rank(b).cmp(&Self::full_match_rank(a)));
        let Some(full_idx) = matches.iter().position(|(end, _)| *end == orig_chars.len()) else {
            // No full match, so `.parse` fails — but hand the longest partial
            // match back so an action-driven parse can still dispatch what the
            // start rule DID match, the way Rakudo's reduce-time dispatch does.
            if !matches.is_empty() {
                let (end, mut caps) = matches.swap_remove(0);
                caps.from = caps.capture_start.unwrap_or(0);
                caps.to = caps.capture_end.unwrap_or(end);
                caps.set_target(Some(target));
                *partial = Some(caps);
            }
            return None;
        };
        let (end, mut caps) = matches.swap_remove(full_idx);
        caps.from = caps.capture_start.unwrap_or(0);
        caps.to = caps.capture_end.unwrap_or(end);
        caps.set_target(Some(target));
        Some(caps)
    }

    /// Match regex anchored at a specific character position.
    /// Returns captures only if the match starts exactly at `pos`.
    pub(crate) fn regex_match_with_captures_at(
        &mut self,
        pattern: &str,
        text: &str,
        pos: usize,
    ) -> Option<RegexCaptures> {
        let parsed = self.parse_regex(pattern)?;
        let pkg = self.current_package_sym();
        let target = MatchTarget::new(text);
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

    /// Match regex searching from a specific character position (non-anchored).
    /// Unlike `regex_match_with_captures_at` which only matches starting exactly
    /// at `pos`, this tries each position from `from_pos` onwards until a match
    /// is found (like `:c(N)` / `:continue(N)` in Raku).
    pub(crate) fn regex_match_with_captures_from(
        &mut self,
        pattern: &str,
        text: &str,
        from_pos: usize,
    ) -> Option<RegexCaptures> {
        let target = MatchTarget::new(text);
        self.regex_match_with_captures_from_target(pattern, &target, from_pos)
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
    pub(crate) fn regex_match_with_captures_from_target(
        &mut self,
        pattern: &str,
        target: &MatchTarget,
        from_pos: usize,
    ) -> Option<RegexCaptures> {
        let parsed = self.parse_regex(pattern)?;
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
            let stripped_parsed = strip_marks_pattern(&parsed);
            let orig_len = orig_chars.len();
            let stripped_from = stripped.original_to_stripped(from_pos);
            let start_pos = if stripped_parsed.anchor_start {
                0
            } else {
                stripped_from
            };
            for start in regex_scan_positions(&stripped_parsed, stripped_chars, start_pos) {
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
                    return Some(caps);
                }
            }
            return None;
        }
        let start_pos = if parsed.anchor_start { 0 } else { from_pos };
        for start in regex_scan_positions(&parsed, orig_chars, start_pos) {
            if let Some((end, mut caps)) =
                self.regex_match_end_from_caps_in_pkg(&parsed, orig_chars, start, pkg)
            {
                caps.from = caps.capture_start.unwrap_or(start);
                caps.to = caps.capture_end.unwrap_or(end);
                caps.set_target(Some(target.clone()));
                return Some(caps);
            }
        }
        None
    }

    pub(in crate::runtime) fn regex_match_all_with_captures(
        &mut self,
        pattern: &str,
        text: &str,
    ) -> Vec<RegexCaptures> {
        self.regex_match_captures_impl(pattern, text, false, false)
    }

    /// Like [`Self::regex_match_all_with_captures`], but at each start position
    /// keeps ONLY the highest-DFS-priority (canonical greedy/frugal) match end —
    /// the one the single-match engine would pick — instead of every possible
    /// end, and skips a start a previously accepted match already covers.
    ///
    /// Both halves matter for the plain `:g` path. Collecting every end and then
    /// keeping the longest per start (as `select_non_overlapping_matches` does)
    /// forces a top-level frugal quantifier (`.*?`) to its greedy length, so
    /// `"aXbXcX" ~~ m:g/.*?X/` answered one match ("aXbXcX") instead of three.
    /// And running the pattern at a position that is discarded afterwards still
    /// runs a `{ ... }` block inside it, so
    /// `"aa bb" ~~ m:g/( \w* { $c++ } )/` left `$c` at 12 where raku leaves it
    /// at 4 — raku finds one match, commits to it and resumes the scan after it.
    ///
    /// A zero-width match does not advance the barrier, so a `\w*` matching
    /// empty right after a previous match is still reported — exactly the
    /// `from >= last_end` test the post-filter applied.
    ///
    /// The `:overlap` / `:exhaustive` paths genuinely need every end at every
    /// start and keep using [`Self::regex_match_all_with_captures`].
    pub(in crate::runtime) fn regex_match_non_overlapping(
        &mut self,
        pattern: &str,
        text: &str,
    ) -> Vec<RegexCaptures> {
        self.regex_match_captures_impl(pattern, text, true, true)
    }

    fn regex_match_captures_impl(
        &mut self,
        pattern: &str,
        text: &str,
        canonical_only: bool,
        skip_covered: bool,
    ) -> Vec<RegexCaptures> {
        let Some(parsed) = self.parse_regex(pattern) else {
            return Vec::new();
        };
        let pkg = self.current_package_sym();
        let target = MatchTarget::new(text);
        let _target_scope = super::regex_helpers::MatchTargetScope::enter(target.clone());
        let orig_chars = target.chars();

        if parsed.ignore_mark {
            let stripped = target.stripped();
            let stripped_chars = stripped.chars();
            let stripped_parsed = strip_marks_pattern(&parsed);
            let orig_len = orig_chars.len();
            let mut out = Vec::new();
            let mut starts = Vec::new();
            if stripped_parsed.anchor_start {
                starts.push(0usize);
            } else {
                starts.extend(regex_scan_positions(&stripped_parsed, stripped_chars, 0));
            }
            let mut last_end = 0usize;
            for start in starts {
                if skip_covered && start < last_end {
                    continue;
                }
                // `canonical_only` keeps only the first end anyway, and the
                // DFS walk finds it first -- so ask for one and let the walk
                // stop, instead of exploring the whole backtracking tree and
                // running a `{ ... }` block in the pattern once per discarded
                // end.
                let ends = if canonical_only {
                    self.regex_match_end_from_caps_in_pkg(
                        &stripped_parsed,
                        stripped_chars,
                        start,
                        pkg,
                    )
                    .into_iter()
                    .collect()
                } else {
                    self.regex_match_ends_from_caps_in_pkg(
                        &stripped_parsed,
                        stripped_chars,
                        start,
                        pkg,
                    )
                };
                for (end, mut caps) in ends {
                    caps.from = caps.capture_start.unwrap_or(start);
                    caps.to = caps.capture_end.unwrap_or(end);
                    super::regex_helpers::remap_caps_spans(
                        &mut caps,
                        stripped.stripped_map(),
                        orig_len,
                    );
                    caps.set_target(Some(target.clone()));
                    if skip_covered {
                        if caps.from < last_end {
                            break;
                        }
                        last_end = caps.to;
                    }
                    out.push(caps);
                    if canonical_only {
                        break;
                    }
                }
            }
            out.sort_by_key(|caps| (caps.from, caps.to, caps.positional.len(), caps.named.len()));
            return out;
        }

        let mut out = Vec::new();
        let mut starts = Vec::new();
        if parsed.anchor_start {
            starts.push(0usize);
        } else {
            starts.extend(regex_scan_positions(&parsed, orig_chars, 0));
        }
        let mut last_end = 0usize;
        for start in starts {
            if skip_covered && start < last_end {
                continue;
            }
            // See the `ignore_mark` twin above: one end is all `canonical_only`
            // keeps, and the walk finds it first.
            let ends: Vec<_> = if canonical_only {
                self.regex_match_end_from_caps_in_pkg(&parsed, orig_chars, start, pkg)
                    .into_iter()
                    .collect()
            } else {
                self.regex_match_ends_from_caps_in_pkg(&parsed, orig_chars, start, pkg)
            };
            for (end, mut caps) in ends {
                caps.from = caps.capture_start.unwrap_or(start);
                caps.to = caps.capture_end.unwrap_or(end);
                caps.set_target(Some(target.clone()));
                // A capture group can report a span starting BEFORE this start
                // position (`caps.capture_start`), so the barrier is re-tested
                // against the reported span, not against `start`.
                if skip_covered {
                    if caps.from < last_end {
                        break;
                    }
                    last_end = caps.to;
                }
                out.push(caps);
                if canonical_only {
                    break;
                }
            }
        }
        out.sort_by_key(|caps| (caps.from, caps.to, caps.positional.len(), caps.named.len()));
        out
    }

    /// Find the first match from `min_pos` against a subject already
    /// materialized as a [`MatchTarget`], returning its span plus every capture
    /// text: the positional list and the per-name lists (a substitution needs
    /// the named ones to bind `$<name>` in its replacement and in the `$/` it
    /// leaves behind). Unlike `regex_find_first`, this preserves full-text
    /// context for zero-width assertions.
    ///
    /// Taking a target rather than a `&str` is the point: this is the scan step
    /// of every global substitution (`.subst(:g)`, `s:g///`), called once per
    /// match over one unchanging subject. Deriving the subject's char vector
    /// per call made those O(matches x subject) -- 23.2 s to substitute in a
    /// 640 KB string where rakudo takes 0.79 s, with 66% of the instructions of
    /// an ordinary 32 KB substitution inside that one `collect` (#8247). Build
    /// the target once, outside the loop.
    pub(crate) fn regex_find_first_from_with_all_captures_in(
        &mut self,
        pattern: &str,
        target: &MatchTarget,
        min_pos: usize,
    ) -> Option<MatchWithAllCaptures> {
        let parsed = self.parse_regex(pattern)?;
        let pkg = self.current_package_sym();
        let _target_scope = super::regex_helpers::MatchTargetScope::enter(target.clone());
        let orig_chars = target.chars();
        if parsed.anchor_start && min_pos > 0 {
            return None;
        }
        if parsed.ignore_mark {
            let stripped = target.stripped();
            let stripped_chars = stripped.chars();
            let stripped_parsed = strip_marks_pattern(&parsed);
            let orig_len = orig_chars.len();
            let stripped_min = stripped.original_to_stripped(min_pos);
            let search_start = if stripped_parsed.anchor_start {
                0
            } else {
                stripped_min
            };
            for start in regex_scan_positions(&stripped_parsed, stripped_chars, search_start) {
                if let Some((end, mut caps)) = self.regex_match_end_from_caps_in_pkg(
                    &stripped_parsed,
                    stripped_chars,
                    start,
                    pkg,
                ) {
                    // Remap the capture spans back to original-subject space so
                    // the derived texts keep their combining marks (pre-P4 the
                    // stored text axis returned mark-stripped text here).
                    for slot in caps.positional.iter_mut() {
                        super::regex_helpers::remap_pos_slot(
                            slot,
                            stripped.stripped_map(),
                            orig_len,
                            0,
                        );
                    }
                    return Some((
                        map_pos(
                            caps.capture_start.unwrap_or(start),
                            stripped.stripped_map(),
                            orig_len,
                        ),
                        map_pos(
                            caps.capture_end.unwrap_or(end),
                            stripped.stripped_map(),
                            orig_len,
                        ),
                        super::regex_helpers::pos_slot_texts(&caps.positional, orig_chars),
                        super::regex_helpers::named_slot_texts(&caps.named, orig_chars),
                    ));
                }
            }
            return None;
        }
        let search_start = if parsed.anchor_start { 0 } else { min_pos };
        for start in regex_scan_positions(&parsed, orig_chars, search_start) {
            if let Some((end, caps)) =
                self.regex_match_end_from_caps_in_pkg(&parsed, orig_chars, start, pkg)
            {
                // `<( … )>` narrows the reported match to the marked region even
                // though the pattern consumed more, exactly as the other match
                // entry points do. `.subst`'s native fast path spans a
                // replacement with what this returns, so ignoring the markers
                // would overwrite the context the pattern only looked at
                // (`'xaby'.subst(/a <( b )>/, 'Z')` is `xaZy`, not `xZy`).
                return Some((
                    caps.capture_start.unwrap_or(start),
                    caps.capture_end.unwrap_or(end),
                    super::regex_helpers::pos_slot_texts(&caps.positional, orig_chars),
                    super::regex_helpers::named_slot_texts(&caps.named, orig_chars),
                ));
            }
        }
        None
    }

    pub(crate) fn regex_find_first(&mut self, pattern: &str, text: &str) -> Option<(usize, usize)> {
        let parsed = self.parse_regex(pattern)?;
        let pkg = self.current_package_sym();
        let target = MatchTarget::new(text);
        let _target_scope = super::regex_helpers::MatchTargetScope::enter(target.clone());

        // When :m (ignoremark) is set, strip combining marks from both text and
        // pattern literals, match on stripped forms, then map positions back.
        if parsed.ignore_mark {
            let orig_chars = target.chars();
            let stripped = target.stripped();
            let stripped_chars = stripped.chars();
            let stripped_parsed = strip_marks_pattern(&parsed);
            let orig_len = orig_chars.len();

            if stripped_parsed.anchor_start {
                return self
                    .regex_match_end_from_in_pkg(&stripped_parsed, stripped_chars, 0, pkg)
                    .map(|end| {
                        (
                            map_pos(0, stripped.stripped_map(), orig_len),
                            map_pos(end, stripped.stripped_map(), orig_len),
                        )
                    });
            }
            for start in regex_scan_positions(&stripped_parsed, stripped_chars, 0) {
                if let Some(end) =
                    self.regex_match_end_from_in_pkg(&stripped_parsed, stripped_chars, start, pkg)
                {
                    return Some((
                        map_pos(start, stripped.stripped_map(), orig_len),
                        map_pos(end, stripped.stripped_map(), orig_len),
                    ));
                }
            }
            return None;
        }

        let chars: Vec<char> = text.chars().collect();
        if parsed.anchor_start {
            return self
                .regex_match_end_from_in_pkg(&parsed, &chars, 0, pkg)
                .map(|end| (0, end));
        }
        for start in regex_scan_positions(&parsed, &chars, 0) {
            if let Some(end) = self.regex_match_end_from_in_pkg(&parsed, &chars, start, pkg) {
                return Some((start, end));
            }
        }
        None
    }

    pub(in crate::runtime) fn regex_match_len_at_start(
        &mut self,
        pattern: &str,
        text: &str,
    ) -> Option<usize> {
        let captures = self.regex_match_with_captures(pattern, text)?;
        if captures.from == 0 {
            Some(captures.to)
        } else {
            None
        }
    }

    pub(super) fn regex_match_len_at_start_in_pkg(
        &mut self,
        pattern: &str,
        text: &str,
        pkg: Symbol,
    ) -> Option<usize> {
        let mut interp = Interpreter {
            env: self.env.clone(),
            // The scratch runs in this package. Both the string and its interned
            // mirror are set: `current_package_sym()` reads the mirror, and a
            // scratch that overrode only the string answered for the wrong
            // package ([#7576](https://github.com/tokuhirom/mutsu/issues/7576)).
            current_package: Arc::new(RwLock::new(pkg.as_str().to_owned())),
            current_package_sym: std::sync::Arc::new(std::sync::atomic::AtomicU32::new(pkg.id())),
            var_dynamic_flags: self.var_dynamic_flags.clone(),
            state_vars: self.state_vars.clone(),
            ..self.new_regex_scratch_sharing_io()
        };
        self.copy_decl_registry_into(&mut interp);
        interp.regex_match_len_at_start(pattern, text)
    }
}
