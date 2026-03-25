use super::super::*;
use super::regex_helpers::{map_pos, strip_marks_pattern, strip_marks_text};

impl Interpreter {
    pub(in crate::runtime) fn regex_match_with_captures_full_from_start(
        &self,
        pattern: &str,
        text: &str,
    ) -> Option<RegexCaptures> {
        let parsed = self.parse_regex(pattern)?;
        let pkg = self.current_package.clone();
        let orig_chars: Vec<char> = text.chars().collect();

        if parsed.ignore_mark {
            let (stripped_chars, pos_map) = strip_marks_text(&orig_chars);
            let stripped_parsed = strip_marks_pattern(&parsed);
            let orig_len = orig_chars.len();
            let mut matches =
                self.regex_match_ends_from_caps_in_pkg(&stripped_parsed, &stripped_chars, 0, &pkg);
            if matches.is_empty() {
                return None;
            }
            matches.sort_by_key(|(end, caps)| (*end, caps.positional.len(), caps.named.len()));
            let (end, mut caps) = matches
                .into_iter()
                .rev()
                .find(|(end, _)| *end == stripped_chars.len())?;
            let from = map_pos(caps.capture_start.unwrap_or(0), &pos_map, orig_len);
            let to = map_pos(caps.capture_end.unwrap_or(end), &pos_map, orig_len);
            caps.from = from;
            caps.to = to;
            caps.matched = orig_chars[from..to].iter().collect();
            return Some(caps);
        }

        let mut matches = self.regex_match_ends_from_caps_in_pkg(&parsed, &orig_chars, 0, &pkg);
        if matches.is_empty() {
            return None;
        }
        matches.sort_by_key(|(end, caps)| (*end, caps.positional.len(), caps.named.len()));
        let (end, mut caps) = matches
            .into_iter()
            .rev()
            .find(|(end, _)| *end == orig_chars.len())?;
        caps.from = caps.capture_start.unwrap_or(0);
        caps.to = caps.capture_end.unwrap_or(end);
        caps.matched = orig_chars[caps.from..caps.to].iter().collect();
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
        let pkg = self.current_package.clone();
        let orig_chars: Vec<char> = text.chars().collect();
        if pos > orig_chars.len() {
            return None;
        }
        if parsed.anchor_start && pos != 0 {
            return None;
        }
        if parsed.ignore_mark {
            let (stripped_chars, pos_map) = strip_marks_text(&orig_chars);
            let stripped_parsed = strip_marks_pattern(&parsed);
            let orig_len = orig_chars.len();
            // Find the stripped position corresponding to `pos`
            let stripped_pos = pos_map
                .iter()
                .position(|&p| p >= pos)
                .unwrap_or(stripped_chars.len());
            if stripped_pos > stripped_chars.len() {
                return None;
            }
            return self
                .regex_match_end_from_caps_in_pkg(
                    &stripped_parsed,
                    &stripped_chars,
                    stripped_pos,
                    &pkg,
                )
                .map(|(end, mut caps)| {
                    let from = map_pos(
                        caps.capture_start.unwrap_or(stripped_pos),
                        &pos_map,
                        orig_len,
                    );
                    let to = map_pos(caps.capture_end.unwrap_or(end), &pos_map, orig_len);
                    caps.from = from;
                    caps.to = to;
                    caps.matched = orig_chars[from..to].iter().collect();
                    caps
                });
        }
        self.regex_match_end_from_caps_in_pkg(&parsed, &orig_chars, pos, &pkg)
            .map(|(end, mut caps)| {
                caps.from = caps.capture_start.unwrap_or(pos);
                caps.to = caps.capture_end.unwrap_or(end);
                caps.matched = orig_chars[caps.from..caps.to].iter().collect();
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
        let parsed = self.parse_regex(pattern)?;
        let pkg = self.current_package.clone();
        let orig_chars: Vec<char> = text.chars().collect();
        if from_pos > orig_chars.len() {
            return None;
        }
        if parsed.anchor_start && from_pos != 0 {
            return None;
        }
        if parsed.ignore_mark {
            let (stripped_chars, pos_map) = strip_marks_text(&orig_chars);
            let stripped_parsed = strip_marks_pattern(&parsed);
            let orig_len = orig_chars.len();
            let stripped_from = pos_map
                .iter()
                .position(|&p| p >= from_pos)
                .unwrap_or(stripped_chars.len());
            let start_pos = if stripped_parsed.anchor_start {
                0
            } else {
                stripped_from
            };
            for start in start_pos..=stripped_chars.len() {
                if let Some((end, mut caps)) = self.regex_match_end_from_caps_in_pkg(
                    &stripped_parsed,
                    &stripped_chars,
                    start,
                    &pkg,
                ) {
                    let from = map_pos(caps.capture_start.unwrap_or(start), &pos_map, orig_len);
                    let to = map_pos(caps.capture_end.unwrap_or(end), &pos_map, orig_len);
                    caps.from = from;
                    caps.to = to;
                    caps.matched = orig_chars[from..to].iter().collect();
                    return Some(caps);
                }
            }
            return None;
        }
        let start_pos = if parsed.anchor_start { 0 } else { from_pos };
        for start in start_pos..=orig_chars.len() {
            if let Some((end, mut caps)) =
                self.regex_match_end_from_caps_in_pkg(&parsed, &orig_chars, start, &pkg)
            {
                caps.from = caps.capture_start.unwrap_or(start);
                caps.to = caps.capture_end.unwrap_or(end);
                caps.matched = orig_chars[caps.from..caps.to].iter().collect();
                return Some(caps);
            }
        }
        None
    }

    pub(in crate::runtime) fn regex_match_all_with_captures(
        &self,
        pattern: &str,
        text: &str,
    ) -> Vec<RegexCaptures> {
        let Some(parsed) = self.parse_regex(pattern) else {
            return Vec::new();
        };
        let pkg = self.current_package.clone();
        let orig_chars: Vec<char> = text.chars().collect();

        if parsed.ignore_mark {
            let (stripped_chars, pos_map) = strip_marks_text(&orig_chars);
            let stripped_parsed = strip_marks_pattern(&parsed);
            let orig_len = orig_chars.len();
            let mut out = Vec::new();
            let mut starts = Vec::new();
            if stripped_parsed.anchor_start {
                starts.push(0usize);
            } else {
                starts.extend(0..=stripped_chars.len());
            }
            for start in starts {
                for (end, mut caps) in self.regex_match_ends_from_caps_in_pkg(
                    &stripped_parsed,
                    &stripped_chars,
                    start,
                    &pkg,
                ) {
                    let from = map_pos(caps.capture_start.unwrap_or(start), &pos_map, orig_len);
                    let to = map_pos(caps.capture_end.unwrap_or(end), &pos_map, orig_len);
                    caps.from = from;
                    caps.to = to;
                    caps.matched = orig_chars[from..to].iter().collect();
                    out.push(caps);
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
            starts.extend(0..=orig_chars.len());
        }
        for start in starts {
            for (end, mut caps) in
                self.regex_match_ends_from_caps_in_pkg(&parsed, &orig_chars, start, &pkg)
            {
                caps.from = caps.capture_start.unwrap_or(start);
                caps.to = caps.capture_end.unwrap_or(end);
                caps.matched = orig_chars[caps.from..caps.to].iter().collect();
                out.push(caps);
            }
        }
        out.sort_by_key(|caps| (caps.from, caps.to, caps.positional.len(), caps.named.len()));
        out
    }

    /// Find first regex match in text, starting search from `min_pos` (char index).
    /// Returns (from, to) as char indices in the full text.
    /// Unlike `regex_find_first`, this preserves full-text context for zero-width assertions.
    pub(crate) fn regex_find_first_from(
        &self,
        pattern: &str,
        text: &str,
        min_pos: usize,
    ) -> Option<(usize, usize)> {
        let parsed = self.parse_regex(pattern)?;
        let pkg = self.current_package.clone();
        let orig_chars: Vec<char> = text.chars().collect();
        if parsed.anchor_start && min_pos > 0 {
            return None;
        }
        if parsed.ignore_mark {
            let (stripped_chars, pos_map) = strip_marks_text(&orig_chars);
            let stripped_parsed = strip_marks_pattern(&parsed);
            let orig_len = orig_chars.len();
            let stripped_min = pos_map
                .iter()
                .position(|&p| p >= min_pos)
                .unwrap_or(stripped_chars.len());
            let search_start = if stripped_parsed.anchor_start {
                0
            } else {
                stripped_min
            };
            for start in search_start..=stripped_chars.len() {
                if let Some(end) =
                    self.regex_match_end_from_in_pkg(&stripped_parsed, &stripped_chars, start, &pkg)
                {
                    return Some((
                        map_pos(start, &pos_map, orig_len),
                        map_pos(end, &pos_map, orig_len),
                    ));
                }
            }
            return None;
        }
        let search_start = if parsed.anchor_start { 0 } else { min_pos };
        for start in search_start..=orig_chars.len() {
            if let Some(end) = self.regex_match_end_from_in_pkg(&parsed, &orig_chars, start, &pkg) {
                return Some((start, end));
            }
        }
        None
    }

    /// Like `regex_find_first_from` but also returns positional captures.
    pub(crate) fn regex_find_first_from_with_captures(
        &self,
        pattern: &str,
        text: &str,
        min_pos: usize,
    ) -> Option<(usize, usize, Vec<String>)> {
        let parsed = self.parse_regex(pattern)?;
        let pkg = self.current_package.clone();
        let orig_chars: Vec<char> = text.chars().collect();
        if parsed.anchor_start && min_pos > 0 {
            return None;
        }
        if parsed.ignore_mark {
            let (stripped_chars, pos_map) = strip_marks_text(&orig_chars);
            let stripped_parsed = strip_marks_pattern(&parsed);
            let orig_len = orig_chars.len();
            let stripped_min = pos_map
                .iter()
                .position(|&p| p >= min_pos)
                .unwrap_or(stripped_chars.len());
            let search_start = if stripped_parsed.anchor_start {
                0
            } else {
                stripped_min
            };
            for start in search_start..=stripped_chars.len() {
                if let Some((end, caps)) = self.regex_match_end_from_caps_in_pkg(
                    &stripped_parsed,
                    &stripped_chars,
                    start,
                    &pkg,
                ) {
                    return Some((
                        map_pos(start, &pos_map, orig_len),
                        map_pos(end, &pos_map, orig_len),
                        caps.positional,
                    ));
                }
            }
            return None;
        }
        let search_start = if parsed.anchor_start { 0 } else { min_pos };
        for start in search_start..=orig_chars.len() {
            if let Some((end, caps)) =
                self.regex_match_end_from_caps_in_pkg(&parsed, &orig_chars, start, &pkg)
            {
                return Some((start, end, caps.positional));
            }
        }
        None
    }

    pub(crate) fn regex_find_first(&self, pattern: &str, text: &str) -> Option<(usize, usize)> {
        let parsed = self.parse_regex(pattern)?;
        let pkg = self.current_package.clone();

        // When :m (ignoremark) is set, strip combining marks from both text and
        // pattern literals, match on stripped forms, then map positions back.
        if parsed.ignore_mark {
            let orig_chars: Vec<char> = text.chars().collect();
            let (stripped_chars, pos_map) = strip_marks_text(&orig_chars);
            let stripped_parsed = strip_marks_pattern(&parsed);
            let orig_len = orig_chars.len();

            if stripped_parsed.anchor_start {
                return self
                    .regex_match_end_from_in_pkg(&stripped_parsed, &stripped_chars, 0, &pkg)
                    .map(|end| {
                        (
                            map_pos(0, &pos_map, orig_len),
                            map_pos(end, &pos_map, orig_len),
                        )
                    });
            }
            for start in 0..=stripped_chars.len() {
                if let Some(end) =
                    self.regex_match_end_from_in_pkg(&stripped_parsed, &stripped_chars, start, &pkg)
                {
                    return Some((
                        map_pos(start, &pos_map, orig_len),
                        map_pos(end, &pos_map, orig_len),
                    ));
                }
            }
            return None;
        }

        let chars: Vec<char> = text.chars().collect();
        if parsed.anchor_start {
            return self
                .regex_match_end_from_in_pkg(&parsed, &chars, 0, &pkg)
                .map(|end| (0, end));
        }
        for start in 0..=chars.len() {
            if let Some(end) = self.regex_match_end_from_in_pkg(&parsed, &chars, start, &pkg) {
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
        &self,
        pattern: &str,
        text: &str,
        pkg: &str,
    ) -> Option<usize> {
        let mut interp = Interpreter {
            env: self.env.clone(),
            functions: self.functions.clone(),
            proto_functions: self.proto_functions.clone(),
            token_defs: self.token_defs.clone(),
            current_package: pkg.to_string(),
            var_dynamic_flags: self.var_dynamic_flags.clone(),
            var_type_constraints: self.var_type_constraints.clone(),
            state_vars: self.state_vars.clone(),
            ..Default::default()
        };
        interp.regex_match_len_at_start(pattern, text)
    }
}
