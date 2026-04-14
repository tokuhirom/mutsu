use super::*;
use crate::symbol::Symbol;

/// Detect the case pattern of a word.
/// Returns one of: "uc" (all upper), "lc" (all lower), "ucfirst" (first upper, rest lower),
/// "lcfirst" (first lower, rest upper).
fn detect_word_case(word: &str) -> &'static str {
    let chars: Vec<char> = word.chars().filter(|c| c.is_alphabetic()).collect();
    if chars.is_empty() {
        return "lc";
    }
    let all_upper = chars.iter().all(|c| c.is_uppercase());
    let all_lower = chars.iter().all(|c| c.is_lowercase());
    if all_upper {
        return "uc";
    }
    if all_lower {
        return "lc";
    }
    if chars[0].is_uppercase() && chars[1..].iter().all(|c| c.is_lowercase()) {
        return "ucfirst";
    }
    if chars[0].is_lowercase() && chars[1..].iter().all(|c| c.is_uppercase()) {
        return "lcfirst";
    }
    // Mixed case - fall back to char-by-char
    "mixed"
}

/// Apply a case function to a word.
fn apply_case_function(word: &str, case_fn: &str) -> String {
    match case_fn {
        "uc" => word.to_uppercase(),
        "lc" => word.to_lowercase(),
        "ucfirst" => {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => {
                    let mut s = first.to_uppercase().to_string();
                    for ch in chars {
                        for c in ch.to_lowercase() {
                            s.push(c);
                        }
                    }
                    s
                }
            }
        }
        "lcfirst" => {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => {
                    let mut s = first.to_lowercase().to_string();
                    for ch in chars {
                        for c in ch.to_uppercase() {
                            s.push(c);
                        }
                    }
                    s
                }
            }
        }
        _ => word.to_string(),
    }
}

/// Apply samecase on a per-word basis: detect the case pattern of each word in the
/// matched text, then apply that pattern to each corresponding word in the replacement.
fn samecase_per_word(replacement: &str, matched: &str) -> String {
    let matched_words: Vec<&str> = matched.split_whitespace().collect();
    if matched_words.is_empty() {
        return replacement.to_string();
    }

    // Detect case function for each matched word
    let case_fns: Vec<&str> = matched_words.iter().map(|w| detect_word_case(w)).collect();

    // Check if all words have the same case function (non-wordcase mode)
    // Only use the "entire string" shortcut for uc/lc where it makes sense.
    // For ucfirst/lcfirst, we still need per-word application.
    let all_same = case_fns.windows(2).all(|w| w[0] == w[1]);
    if all_same && (case_fns[0] == "uc" || case_fns[0] == "lc") {
        return apply_case_function(replacement, case_fns[0]);
    }

    // Word-case mode: apply per-word case functions
    let mut result = String::new();
    let mut word_idx = 0;
    let mut chars = replacement.chars().peekable();
    while chars.peek().is_some() {
        // Collect leading whitespace
        while let Some(&ch) = chars.peek() {
            if ch.is_whitespace() {
                result.push(ch);
                chars.next();
            } else {
                break;
            }
        }
        // Collect word
        let mut word = String::new();
        while let Some(&ch) = chars.peek() {
            if ch.is_whitespace() {
                break;
            }
            word.push(ch);
            chars.next();
        }
        if !word.is_empty() {
            let case_fn = if word_idx < case_fns.len() {
                case_fns[word_idx]
            } else {
                case_fns.last().unwrap()
            };
            result.push_str(&apply_case_function(&word, case_fn));
            word_idx += 1;
        }
    }
    result
}

/// Apply samemark on a per-word basis: split both source and target by whitespace,
/// apply samemark to each word pair, then reassemble with the replacement's whitespace.
fn samemark_per_word(target: &str, source: &str) -> String {
    let src_words: Vec<&str> = source.split_whitespace().collect();
    if src_words.is_empty() {
        return target.to_string();
    }

    // Split target into words and whitespace segments
    let mut result = String::new();
    let mut word_idx = 0;
    let mut chars = target.chars().peekable();
    while chars.peek().is_some() {
        // Collect leading whitespace
        let mut ws = String::new();
        while let Some(&ch) = chars.peek() {
            if ch.is_whitespace() {
                ws.push(ch);
                chars.next();
            } else {
                break;
            }
        }
        result.push_str(&ws);
        // Collect word
        let mut word = String::new();
        while let Some(&ch) = chars.peek() {
            if ch.is_whitespace() {
                break;
            }
            word.push(ch);
            chars.next();
        }
        if !word.is_empty() {
            let src_word = if word_idx < src_words.len() {
                src_words[word_idx]
            } else {
                src_words.last().unwrap()
            };
            result.push_str(&crate::builtins::samemark_string(&word, src_word));
            word_idx += 1;
        }
    }
    result
}

/// Apply samespace: replace whitespace runs in the replacement with corresponding
/// whitespace runs from the matched text.
fn samespace_replace(replacement: &str, matched: &str) -> String {
    // Split matched text into whitespace runs
    let mut ws_runs: Vec<&str> = Vec::new();
    let mut i = 0;
    let bytes = matched.as_bytes();
    while i < bytes.len() {
        // Skip non-whitespace
        while i < bytes.len() && !matched[i..].starts_with(|c: char| c.is_whitespace()) {
            i += matched[i..].chars().next().map_or(1, |c| c.len_utf8());
        }
        if i >= bytes.len() {
            break;
        }
        let start = i;
        while i < bytes.len() && matched[i..].starts_with(|c: char| c.is_whitespace()) {
            i += matched[i..].chars().next().map_or(1, |c| c.len_utf8());
        }
        ws_runs.push(&matched[start..i]);
    }

    // Now replace whitespace runs in the replacement with runs from the matched text
    let mut result = String::new();
    let mut ws_idx = 0;
    let mut j = 0;
    let repl_bytes = replacement.as_bytes();
    while j < repl_bytes.len() {
        let ch = replacement[j..].chars().next().unwrap();
        if ch.is_whitespace() {
            // Skip the whitespace run in the replacement
            while j < repl_bytes.len() && replacement[j..].starts_with(|c: char| c.is_whitespace())
            {
                j += replacement[j..].chars().next().map_or(1, |c| c.len_utf8());
            }
            // Insert the corresponding whitespace run from the matched text
            if ws_idx < ws_runs.len() {
                result.push_str(ws_runs[ws_idx]);
                ws_idx += 1;
            } else {
                result.push(' ');
            }
        } else {
            result.push(ch);
            j += ch.len_utf8();
        }
    }
    result
}

/// Expand positional capture references ($0, $1, ...) in a substitution
/// replacement string.  Called after the regex match so capture values are known.
pub(crate) fn expand_capture_refs(template: &str, captures: &[String]) -> String {
    let mut out = String::new();
    let bytes = template.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'$' && i + 1 < bytes.len() && bytes[i + 1].is_ascii_digit() {
            // Parse the digit(s) following $
            let mut j = i + 1;
            while j < bytes.len() && bytes[j].is_ascii_digit() {
                j += 1;
            }
            if let Ok(idx) = template[i + 1..j].parse::<usize>()
                && let Some(cap) = captures.get(idx)
            {
                out.push_str(cap);
            }
            i = j;
        } else {
            out.push(template[i..].chars().next().unwrap());
            i += template[i..].chars().next().unwrap().len_utf8();
        }
    }
    out
}

/// Extract a variable name (identifier chars: alpha, digit, _, -, ::)
fn take_var_name(input: &str) -> Option<usize> {
    let mut chars = input.char_indices();
    let (_, first) = chars.next()?;
    if !first.is_ascii_alphabetic()
        && first != '_'
        && first != '*'
        && first != '?'
        && first != '!'
        && first != '^'
    {
        return None;
    }
    let mut end = first.len_utf8();
    for (idx, ch) in chars {
        if ch.is_ascii_alphanumeric() || ch == '_' || ch == '-' || ch == ':' {
            end = idx + ch.len_utf8();
        } else {
            break;
        }
    }
    Some(end)
}

fn normalize_subst_replacement(template: &str) -> String {
    let mut out = String::new();
    let mut chars = template.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch != '\\' {
            out.push(ch);
            continue;
        }
        let Some(next) = chars.peek().copied() else {
            out.push('\\');
            continue;
        };
        match next {
            '\\' => {
                out.push('\\');
                chars.next();
            }
            '&' => {
                out.push('&');
                chars.next();
            }
            _ => out.push('\\'),
        }
    }
    out
}

impl VM {
    fn canonical_infix_lookup_name(name: &str) -> std::borrow::Cow<'_, str> {
        if name == "(+)" {
            return std::borrow::Cow::Borrowed("+");
        }
        std::borrow::Cow::Borrowed(name)
    }

    fn should_retry_with_canonical_infix_name(name: &str) -> bool {
        matches!(
            name,
            "(<=)" | "⊆" | "(>=)" | "⊇" | "(<)" | "⊂" | "(>)" | "⊃" | "⊈" | "⊉" | "⊄" | "⊅"
        )
    }

    /// Create a Match object for a substitution match.
    fn make_subst_match(text: &str, start: usize, end: usize) -> Value {
        let chars: Vec<char> = text.chars().collect();
        let matched: String = chars[start..end].iter().collect();
        Value::make_match_object_full(
            matched,
            start as i64,
            end as i64,
            &[],
            &std::collections::HashMap::new(),
            &std::collections::HashMap::new(),
            &[],
            &[],
            Some(text),
        )
    }

    #[allow(clippy::too_many_arguments)]
    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_subst_op(
        &mut self,
        code: &CompiledCode,
        pattern_idx: u32,
        replacement_idx: u32,
        samecase: bool,
        sigspace: bool,
        samemark: bool,
        samespace: bool,
        global: bool,
        nth_idx: Option<u32>,
        x_count: Option<u32>,
        perl5: bool,
    ) -> Result<(), RuntimeError> {
        let pattern = Self::const_str(code, pattern_idx).to_string();
        let raw_replacement = normalize_subst_replacement(Self::const_str(code, replacement_idx));
        let replacement = self.interpolate_subst_replacement_with_closures(&raw_replacement);

        let nth_spec = nth_idx.map(|idx| Self::const_str(code, idx).to_string());
        let x_count = x_count.map(|n| n as usize);
        let target = self
            .interpreter
            .env()
            .get("_")
            .cloned()
            .unwrap_or(Value::Nil);
        let text = target.to_string_value();

        if nth_spec.is_none() && x_count.is_none() && !global {
            if perl5 {
                let found = self.interpreter.regex_find_first_p5(&pattern, &text);
                if let Some((start, end)) = found {
                    let out = Self::apply_substitutions(
                        &text,
                        &[(start, end)],
                        &replacement,
                        samecase,
                        sigspace,
                        samemark,
                        samespace,
                    );
                    let result = Value::str(out);
                    self.interpreter
                        .env_mut()
                        .insert("_".to_string(), result.clone());
                    self.interpreter
                        .env_mut()
                        .insert("$_".to_string(), result.clone());
                    self.interpreter
                        .env_mut()
                        .insert("__mutsu_rw_map_topic__".to_string(), result);
                    // Create Match object and set $/
                    let match_obj = Self::make_subst_match(&text, start, end);
                    self.interpreter
                        .env_mut()
                        .insert("/".to_string(), match_obj.clone());
                    self.substitution_in_smartmatch = self.in_smartmatch_rhs;
                    self.stack.push(match_obj);
                } else {
                    self.interpreter
                        .env_mut()
                        .insert("/".to_string(), Value::Nil);
                    self.substitution_in_smartmatch = self.in_smartmatch_rhs;
                    self.stack.push(Value::Bool(false));
                }
            } else {
                let found = self
                    .interpreter
                    .regex_find_first_from_with_captures(&pattern, &text, 0);
                if let Some((start, end, captures)) = found {
                    // Expand $0, $1, ... in the replacement with captured groups
                    let expanded = expand_capture_refs(&replacement, &captures);
                    let out = Self::apply_substitutions(
                        &text,
                        &[(start, end)],
                        &expanded,
                        samecase,
                        sigspace,
                        samemark,
                        samespace,
                    );
                    let result = Value::str(out);
                    self.interpreter
                        .env_mut()
                        .insert("_".to_string(), result.clone());
                    self.interpreter
                        .env_mut()
                        .insert("$_".to_string(), result.clone());
                    self.interpreter
                        .env_mut()
                        .insert("__mutsu_rw_map_topic__".to_string(), result);
                    // Create Match object and set $/
                    let match_obj = Self::make_subst_match(&text, start, end);
                    self.interpreter
                        .env_mut()
                        .insert("/".to_string(), match_obj.clone());
                    self.substitution_in_smartmatch = self.in_smartmatch_rhs;
                    self.stack.push(match_obj);
                } else {
                    self.interpreter
                        .env_mut()
                        .insert("/".to_string(), Value::Nil);
                    self.substitution_in_smartmatch = self.in_smartmatch_rhs;
                    self.stack.push(Value::Bool(false));
                }
            }
            return Ok(());
        }

        // For Raku regex with capture references, collect per-match captures
        // so each match can expand $0, $1 etc. independently.
        let (ranges, per_match_captures) = if perl5 {
            let all_matches = self.interpreter.regex_find_all_p5(&pattern, &text);
            let selected = if global && nth_spec.is_none() && x_count.is_none() {
                all_matches
            } else {
                Self::select_substitution_ranges(&all_matches, nth_spec.as_deref(), x_count)?
            };
            (selected, Vec::new())
        } else {
            // Find all matches with captures using iterative find_first_from
            let mut matches_with_caps: Vec<(usize, usize, Vec<String>)> = Vec::new();
            let mut pos = 0;
            while let Some((start, end, caps)) = self
                .interpreter
                .regex_find_first_from_with_captures(&pattern, &text, pos)
            {
                matches_with_caps.push((start, end, caps));
                pos = if end > start { end } else { start + 1 };
            }
            let all_ranges: Vec<(usize, usize)> =
                matches_with_caps.iter().map(|(s, e, _)| (*s, *e)).collect();
            let selected = if global && nth_spec.is_none() && x_count.is_none() {
                all_ranges
            } else {
                Self::select_substitution_ranges(&all_ranges, nth_spec.as_deref(), x_count)?
            };
            // Extract captures for selected ranges
            let captures: Vec<Vec<String>> = selected
                .iter()
                .filter_map(|r| {
                    matches_with_caps
                        .iter()
                        .find(|(s, e, _)| *s == r.0 && *e == r.1)
                        .map(|(_, _, c)| c.clone())
                })
                .collect();
            (selected, captures)
        };
        if ranges.is_empty() {
            self.interpreter
                .env_mut()
                .insert("/".to_string(), Value::Nil);
            self.substitution_in_smartmatch = self.in_smartmatch_rhs;
            self.stack.push(Value::Bool(false));
            return Ok(());
        }

        let out = if !per_match_captures.is_empty() {
            // Build output with per-match capture expansion
            Self::apply_substitutions_with_captures(
                &text,
                &ranges,
                &replacement,
                &per_match_captures,
                samecase,
                sigspace,
                samemark,
                samespace,
            )
        } else {
            Self::apply_substitutions(
                &text,
                &ranges,
                &replacement,
                samecase,
                sigspace,
                samemark,
                samespace,
            )
        };
        let result = Value::str(out);
        self.interpreter
            .env_mut()
            .insert("_".to_string(), result.clone());
        self.interpreter
            .env_mut()
            .insert("$_".to_string(), result.clone());
        // Track topic mutation for map rw writeback
        self.interpreter
            .env_mut()
            .insert("__mutsu_rw_map_topic__".to_string(), result);
        // Create Match object from first match range and set $/
        let (first_start, first_end) = ranges[0];
        let match_obj = Self::make_subst_match(&text, first_start, first_end);
        self.interpreter
            .env_mut()
            .insert("/".to_string(), match_obj.clone());
        self.substitution_in_smartmatch = self.in_smartmatch_rhs;
        self.stack.push(match_obj);
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_non_destructive_subst_op(
        &mut self,
        code: &CompiledCode,
        pattern_idx: u32,
        replacement_idx: u32,
        samecase: bool,
        sigspace: bool,
        samemark: bool,
        samespace: bool,
        global: bool,
        nth_idx: Option<u32>,
        x_count: Option<u32>,
        perl5: bool,
    ) -> Result<(), RuntimeError> {
        let pattern = Self::const_str(code, pattern_idx).to_string();
        let raw_replacement = normalize_subst_replacement(Self::const_str(code, replacement_idx));
        let replacement = self.interpolate_subst_replacement_with_closures(&raw_replacement);

        let nth_spec = nth_idx.map(|idx| Self::const_str(code, idx).to_string());
        let x_count = x_count.map(|n| n as usize);
        let target = self
            .interpreter
            .env()
            .get("_")
            .cloned()
            .unwrap_or(Value::Nil);
        let text = target.to_string_value();

        if nth_spec.is_none() && x_count.is_none() && !global {
            if perl5 {
                let found = self.interpreter.regex_find_first_p5(&pattern, &text);
                if let Some((start, end)) = found {
                    let out = Self::apply_substitutions(
                        &text,
                        &[(start, end)],
                        &replacement,
                        samecase,
                        sigspace,
                        samemark,
                        samespace,
                    );
                    self.stack.push(Value::str(out));
                } else {
                    self.stack.push(Value::str(text));
                }
            } else {
                let found = self
                    .interpreter
                    .regex_find_first_from_with_captures(&pattern, &text, 0);
                if let Some((start, end, captures)) = found {
                    let expanded = expand_capture_refs(&replacement, &captures);
                    let out = Self::apply_substitutions(
                        &text,
                        &[(start, end)],
                        &expanded,
                        samecase,
                        sigspace,
                        samemark,
                        samespace,
                    );
                    self.stack.push(Value::str(out));
                } else {
                    self.stack.push(Value::str(text));
                }
            }
            return Ok(());
        }

        let (ranges, per_match_captures) = if perl5 {
            let all_matches = self.interpreter.regex_find_all_p5(&pattern, &text);
            let selected = if global && nth_spec.is_none() && x_count.is_none() {
                all_matches
            } else {
                Self::select_substitution_ranges(&all_matches, nth_spec.as_deref(), x_count)?
            };
            (selected, Vec::new())
        } else {
            let mut matches_with_caps: Vec<(usize, usize, Vec<String>)> = Vec::new();
            let mut pos = 0;
            while let Some((start, end, caps)) = self
                .interpreter
                .regex_find_first_from_with_captures(&pattern, &text, pos)
            {
                matches_with_caps.push((start, end, caps));
                pos = if end > start { end } else { start + 1 };
            }
            let all_ranges: Vec<(usize, usize)> =
                matches_with_caps.iter().map(|(s, e, _)| (*s, *e)).collect();
            let selected = if global && nth_spec.is_none() && x_count.is_none() {
                all_ranges
            } else {
                Self::select_substitution_ranges(&all_ranges, nth_spec.as_deref(), x_count)?
            };
            let captures: Vec<Vec<String>> = selected
                .iter()
                .filter_map(|r| {
                    matches_with_caps
                        .iter()
                        .find(|(s, e, _)| *s == r.0 && *e == r.1)
                        .map(|(_, _, c)| c.clone())
                })
                .collect();
            (selected, captures)
        };
        if ranges.is_empty() {
            self.stack.push(Value::str(text));
            return Ok(());
        }
        let out = if !per_match_captures.is_empty() {
            Self::apply_substitutions_with_captures(
                &text,
                &ranges,
                &replacement,
                &per_match_captures,
                samecase,
                sigspace,
                samemark,
                samespace,
            )
        } else {
            Self::apply_substitutions(
                &text,
                &ranges,
                &replacement,
                samecase,
                sigspace,
                samemark,
                samespace,
            )
        };
        self.stack.push(Value::str(out));
        Ok(())
    }

    fn select_substitution_ranges(
        all_matches: &[(usize, usize)],
        nth_spec: Option<&str>,
        x_count: Option<usize>,
    ) -> Result<Vec<(usize, usize)>, RuntimeError> {
        if let Some(raw) = nth_spec {
            let n = Self::parse_subst_nth_spec(raw)?;
            if n == 0 {
                return Err(RuntimeError::new("Invalid :nth index (must be >= 1)"));
            }
            if n > all_matches.len() {
                return Ok(Vec::new());
            }
            return Ok(vec![all_matches[n - 1]]);
        }
        if let Some(n) = x_count {
            if n == 0 {
                return Ok(Vec::new());
            }
            if all_matches.len() < n {
                return Ok(Vec::new());
            }
            return Ok(all_matches.iter().copied().take(n).collect());
        }
        Ok(all_matches.first().copied().into_iter().collect())
    }

    fn parse_subst_nth_spec(raw: &str) -> Result<usize, RuntimeError> {
        let token = raw.trim();
        if token.eq_ignore_ascii_case("-Inf") {
            return Err(RuntimeError::new("Invalid :nth index (-Inf)"));
        }
        let n = token
            .parse::<i64>()
            .map_err(|_| RuntimeError::new(format!("Invalid :nth index ({token})")))?;
        if n <= 0 {
            return Err(RuntimeError::new(format!("Invalid :nth index ({token})")));
        }
        Ok(n as usize)
    }

    fn apply_substitutions(
        text: &str,
        ranges: &[(usize, usize)],
        replacement: &str,
        samecase: bool,
        sigspace: bool,
        samemark: bool,
        samespace: bool,
    ) -> String {
        let mut out = String::new();
        let mut prev_end_b = 0usize;
        for (start, end) in ranges {
            let start_b = runtime::char_idx_to_byte(text, *start);
            let end_b = runtime::char_idx_to_byte(text, *end);
            out.push_str(&text[prev_end_b..start_b]);
            let matched_text = &text[start_b..end_b];
            let mut repl = if samecase {
                if sigspace {
                    samecase_per_word(replacement, matched_text)
                } else {
                    crate::builtins::samecase_string(replacement, matched_text)
                }
            } else if samemark {
                if matched_text.contains(char::is_whitespace)
                    && replacement.contains(char::is_whitespace)
                {
                    samemark_per_word(replacement, matched_text)
                } else {
                    crate::builtins::samemark_string(replacement, matched_text)
                }
            } else {
                replacement.to_string()
            };
            if samespace {
                repl = samespace_replace(&repl, matched_text);
            }
            out.push_str(&repl);
            prev_end_b = end_b;
        }
        out.push_str(&text[prev_end_b..]);
        out
    }

    /// Like `apply_substitutions` but expands `$0`, `$1`, etc. per match
    /// using the corresponding capture groups.
    #[allow(clippy::too_many_arguments)]
    fn apply_substitutions_with_captures(
        text: &str,
        ranges: &[(usize, usize)],
        replacement: &str,
        per_match_captures: &[Vec<String>],
        samecase: bool,
        sigspace: bool,
        samemark: bool,
        samespace: bool,
    ) -> String {
        let mut out = String::new();
        let mut prev_end_b = 0usize;
        for (i, (start, end)) in ranges.iter().enumerate() {
            let start_b = runtime::char_idx_to_byte(text, *start);
            let end_b = runtime::char_idx_to_byte(text, *end);
            out.push_str(&text[prev_end_b..start_b]);
            let matched_text = &text[start_b..end_b];
            let expanded = if let Some(caps) = per_match_captures.get(i) {
                expand_capture_refs(replacement, caps)
            } else {
                replacement.to_string()
            };
            let mut repl = if samecase {
                if sigspace {
                    samecase_per_word(&expanded, matched_text)
                } else {
                    crate::builtins::samecase_string(&expanded, matched_text)
                }
            } else if samemark {
                if matched_text.contains(char::is_whitespace)
                    && expanded.contains(char::is_whitespace)
                {
                    samemark_per_word(&expanded, matched_text)
                } else {
                    crate::builtins::samemark_string(&expanded, matched_text)
                }
            } else {
                expanded
            };
            if samespace {
                repl = samespace_replace(&repl, matched_text);
            }
            out.push_str(&repl);
            prev_end_b = end_b;
        }
        out.push_str(&text[prev_end_b..]);
        out
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_transliterate_op(
        &mut self,
        code: &CompiledCode,
        from_idx: u32,
        to_idx: u32,
        delete: bool,
        complement: bool,
        squash: bool,
        non_destructive: bool,
    ) -> Result<(), RuntimeError> {
        let from = Self::const_str(code, from_idx);
        let to = Self::const_str(code, to_idx);
        let target = self
            .interpreter
            .env()
            .get("_")
            .cloned()
            .unwrap_or(Value::Nil);
        // If $_ is bound to a read-only topic (e.g. `with 'literal' { ... }`),
        // tr/// must throw X::Assignment::RO. The `with` desugaring marks the
        // topic value with a Mixin override `__mutsu_topic_ro__` when the
        // condition expression is a literal.
        if !non_destructive
            && let Value::Mixin(_, overrides) = &target
            && overrides.contains_key("__mutsu_topic_ro__")
        {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "message".to_string(),
                Value::str(format!(
                    "Cannot modify an immutable Str ({})",
                    target.to_string_value()
                )),
            );
            attrs.insert("value".to_string(), target);
            return Err(RuntimeError::typed("X::Assignment::RO", attrs));
        }
        let text = target.to_string_value();

        let translated = crate::builtins::transliterate::transliterate(
            &text, from, to, delete, squash, complement,
        );
        let translated_value = Value::str(translated.clone());

        // tr/// (lowercase) always modifies $_; TR/// (uppercase) only modifies
        // $_ in smartmatch context (so that $var ~~ TR/// writes back to $var).
        if !non_destructive || self.in_smartmatch_rhs {
            self.interpreter
                .env_mut()
                .insert("_".to_string(), translated_value);
        }
        // Signal to the smartmatch handler that this is a transliterate result
        // so it returns the result directly (as StrDistance) instead of comparing.
        if self.in_smartmatch_rhs {
            self.transliterate_in_smartmatch = true;
        }
        // tr/// returns a StrDistance object holding both the original and the
        // transliterated string. It stringifies to the `after` value.
        let mut sd_attrs = std::collections::HashMap::new();
        sd_attrs.insert("before".to_string(), Value::str(text));
        sd_attrs.insert("after".to_string(), Value::str(translated));
        let sd = Value::make_instance(Symbol::intern("StrDistance"), sd_attrs);
        self.stack.push(sd);
        Ok(())
    }

    /// Check if a value is "listy" (array, hash, range, seq, etc.) for hyper op purposes.
    /// Scalars return false, meaning the hyper result should not be wrapped in an array.
    fn is_listy(val: &Value) -> bool {
        matches!(
            val,
            Value::Array(..)
                | Value::Seq(..)
                | Value::Hash(..)
                | Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::GenericRange { .. }
                | Value::LazyList(..)
                | Value::Set(..)
                | Value::Bag(..)
                | Value::Mix(..)
        )
    }

    pub(super) fn exec_hyper_op(
        &mut self,
        code: &CompiledCode,
        op_idx: u32,
        dwim_left: bool,
        dwim_right: bool,
    ) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap_or(Value::Nil);
        let left = self.stack.pop().unwrap_or(Value::Nil);
        let op = Self::const_str(code, op_idx).to_string();
        let both_scalar = !Self::is_listy(&left) && !Self::is_listy(&right);
        let left_list = Interpreter::value_to_list(&left);
        let right_list = Interpreter::value_to_list(&right);
        let left_len = left_list.len();
        let right_len = right_list.len();
        if left_len == 0 && right_len == 0 {
            self.stack.push(Value::array(Vec::new()));
            return Ok(());
        }
        let result_len = if !dwim_left && !dwim_right {
            if left_len != right_len {
                return Err(RuntimeError::new(format!(
                    "Non-dwimmy hyper operator: left has {} elements, right has {}",
                    left_len, right_len
                )));
            }
            left_len
        } else if dwim_left && dwim_right {
            std::cmp::max(left_len, right_len)
        } else if dwim_right {
            left_len
        } else {
            right_len
        };
        let is_smartmatch = op == "~~";
        let mut results = Vec::with_capacity(result_len);
        for i in 0..result_len {
            let l = if left_len == 0 {
                &Value::Int(0)
            } else {
                &left_list[i % left_len]
            };
            let r = if right_len == 0 {
                &Value::Int(0)
            } else {
                &right_list[i % right_len]
            };
            if is_smartmatch {
                results.push(Value::Bool(self.vm_smart_match(l, r)));
            } else {
                // Try user-defined infix dispatch first when either operand
                // is an instance, to avoid built-in ops silently coercing objects.
                let mut resolved = false;
                if matches!(l, Value::Instance { .. }) || matches!(r, Value::Instance { .. }) {
                    let infix_name = format!("infix:<{}>", op);
                    if let Some(v) = self.try_user_infix(&infix_name, l, r)? {
                        results.push(v);
                        resolved = true;
                    }
                }
                if !resolved {
                    results.push(self.eval_reduction_operator_values(&op, l, r)?);
                }
            }
        }
        let result = if both_scalar && results.len() == 1 {
            results.into_iter().next().unwrap()
        } else {
            // Preserve List kind when inputs are Lists (not Arrays)
            let left_is_array = matches!(&left, Value::Array(_, crate::value::ArrayKind::Array));
            let right_is_array = matches!(&right, Value::Array(_, crate::value::ArrayKind::Array));
            if !left_is_array && !right_is_array {
                Value::Array(std::sync::Arc::new(results), crate::value::ArrayKind::List)
            } else {
                Value::real_array(results)
            }
        };
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_hyper_func_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        dwim_left: bool,
        dwim_right: bool,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap_or(Value::Nil);
        let left = self.stack.pop().unwrap_or(Value::Nil);
        let name = Self::const_str(code, name_idx).to_string();
        let both_scalar = !Self::is_listy(&left) && !Self::is_listy(&right);
        let left_list = Interpreter::value_to_list(&left);
        let right_list = Interpreter::value_to_list(&right);
        let left_len = left_list.len();
        let right_len = right_list.len();
        if left_len == 0 && right_len == 0 {
            self.stack.push(Value::array(Vec::new()));
            return Ok(());
        }
        let result_len = if !dwim_left && !dwim_right {
            if left_len != right_len {
                return Err(RuntimeError::new(format!(
                    "Non-dwimmy hyper operator: left has {} elements, right has {}",
                    left_len, right_len
                )));
            }
            left_len
        } else if dwim_left && dwim_right {
            std::cmp::max(left_len, right_len)
        } else if dwim_right {
            left_len
        } else {
            right_len
        };
        // Look up the function by name (&name variable or compiled function)
        let mut results = Vec::with_capacity(result_len);
        for i in 0..result_len {
            let l = if left_len == 0 {
                &Value::Int(0)
            } else {
                &left_list[i % left_len]
            };
            let r = if right_len == 0 {
                &Value::Int(0)
            } else {
                &right_list[i % right_len]
            };
            let call_args = vec![l.clone(), r.clone()];
            let result =
                self.call_function_compiled_first(&name, call_args.clone(), compiled_fns)?;
            results.push(result);
        }
        let result = if both_scalar && results.len() == 1 {
            results.into_iter().next().unwrap()
        } else {
            // Preserve List kind when inputs are Lists (not Arrays)
            let left_is_array = matches!(&left, Value::Array(_, crate::value::ArrayKind::Array));
            let right_is_array = matches!(&right, Value::Array(_, crate::value::ArrayKind::Array));
            if !left_is_array && !right_is_array {
                Value::Array(std::sync::Arc::new(results), crate::value::ArrayKind::List)
            } else {
                Value::real_array(results)
            }
        };
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_meta_op(
        &mut self,
        code: &CompiledCode,
        meta_idx: u32,
        op_idx: u32,
    ) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap_or(Value::Nil);
        let left = self.stack.pop().unwrap_or(Value::Nil);
        let meta = Self::const_str(code, meta_idx).to_string();
        let op = Self::const_str(code, op_idx).to_string();
        let result = match meta.as_str() {
            "R" => {
                if op == "..." || op == "...^" {
                    let exclude_end = op == "...^";
                    self.interpreter
                        .eval_sequence_values(right, left, exclude_end)?
                } else if op == "~~" {
                    Value::Bool(self.vm_smart_match(&right, &left))
                } else {
                    self.eval_reduction_operator_values(&op, &right, &left)?
                }
            }
            "X" => {
                let value_is_lazy = |v: &Value| match v {
                    Value::LazyList(_) => true,
                    Value::Range(_, end)
                    | Value::RangeExcl(_, end)
                    | Value::RangeExclStart(_, end)
                    | Value::RangeExclBoth(_, end) => *end == i64::MAX,
                    Value::GenericRange { end, .. } => {
                        let end_f = end.to_f64();
                        end_f.is_infinite() && end_f.is_sign_positive()
                    }
                    _ => false,
                };
                let lazy_inputs = value_is_lazy(&left) || value_is_lazy(&right);
                let lazy_limit = 256usize;
                let materialize_side = |v: &Value| -> Vec<Value> {
                    if value_is_lazy(v) {
                        let iter = ZipIter::from_value(v);
                        let len = iter.len().min(lazy_limit);
                        (0..len).map(|i| iter.nth(i)).collect()
                    } else {
                        runtime::value_to_list(v)
                    }
                };
                let left_list = materialize_side(&left);
                let right_list = materialize_side(&right);
                let mut results = Vec::new();
                if op.is_empty() || op == "," {
                    for l in &left_list {
                        for r in &right_list {
                            results.push(Value::array(vec![l.clone(), r.clone()]));
                        }
                    }
                } else if op == "~~" {
                    for l in &left_list {
                        for r in &right_list {
                            results.push(Value::Bool(self.vm_smart_match(l, r)));
                        }
                    }
                } else {
                    for l in &left_list {
                        for r in &right_list {
                            results.push(self.eval_reduction_operator_values(&op, l, r)?);
                        }
                    }
                }
                if lazy_inputs {
                    Value::LazyList(std::sync::Arc::new(crate::value::LazyList::new_cached(
                        results,
                    )))
                } else if results.is_empty() {
                    Value::Seq(std::sync::Arc::new(Vec::new()))
                } else {
                    Value::array(results)
                }
            }
            "Z" => {
                // Use lazy index-based iteration for ranges to avoid
                // materializing huge/infinite lists like 1..*.
                let left_iter = ZipIter::from_value(&left);
                let right_iter = ZipIter::from_value(&right);
                let all_lazy = left_iter.is_lazy() && right_iter.is_lazy();
                let len = left_iter.len().min(right_iter.len()).min(MAX_ZIP_EXPAND);
                let mut results = Vec::new();
                if op.is_empty() || op == "," {
                    for i in 0..len {
                        results.push(Value::array(vec![left_iter.nth(i), right_iter.nth(i)]));
                    }
                } else if op == "=>" {
                    for i in 0..len {
                        let key = left_iter.nth(i).to_string_value();
                        results.push(Value::Pair(key, Box::new(right_iter.nth(i))));
                    }
                } else {
                    // Check for 3-way zip reduction case ([Z+] a, b, c)
                    // where left has exactly 2 elements and the second is a list.
                    let nested_left = if left_iter.len() == 2 {
                        let second = left_iter.nth(1);
                        match &second {
                            Value::Array(..) | Value::Seq(_) | Value::Slip(_) => {
                                Some((left_iter.nth(0), runtime::value_to_list(&second)))
                            }
                            _ => None,
                        }
                    } else {
                        None
                    };
                    for i in 0..len {
                        if let Some((ref first, ref extra)) = nested_left {
                            let mut v = self.eval_reduction_operator_values(
                                &op,
                                first,
                                &right_iter.nth(i),
                            )?;
                            if let Some(extra_i) = extra.get(i) {
                                v = self.eval_reduction_operator_values(&op, &v, extra_i)?;
                            }
                            results.push(v);
                        } else {
                            results.push(self.eval_reduction_operator_values(
                                &op,
                                &left_iter.nth(i),
                                &right_iter.nth(i),
                            )?);
                        }
                    }
                }
                if all_lazy {
                    Value::LazyList(std::sync::Arc::new(crate::value::LazyList::new_cached(
                        results,
                    )))
                } else {
                    Value::array(results)
                }
            }
            "!" => {
                let inner = self.eval_reduction_operator_values(&op, &left, &right)?;
                Value::Bool(!inner.truthy())
            }
            _ => {
                return Err(RuntimeError::new(format!(
                    "Unknown meta operator: {}",
                    meta
                )));
            }
        };
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_infix_func_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        right_arity: u32,
        modifier_idx: &Option<u32>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let arity = right_arity as usize;
        let mut right_vals: Vec<Value> = Vec::with_capacity(arity);
        for _ in 0..arity {
            right_vals.push(self.stack.pop().unwrap_or(Value::Nil));
        }
        right_vals.reverse();
        let left_val = self.stack.pop().unwrap_or(Value::Nil);
        let name = Self::const_str(code, name_idx).to_string();
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx).to_string());
        let result = if name == "atan2" {
            let mut x = right_vals
                .first()
                .and_then(runtime::to_float_value)
                .unwrap_or(0.0);
            let mut y = runtime::to_float_value(&left_val).unwrap_or(0.0);
            if modifier.as_deref() == Some("R") {
                std::mem::swap(&mut x, &mut y);
            }
            Value::Num(y.atan2(x))
        } else if name == "sprintf" {
            let fmt = match &left_val {
                Value::Str(s) => s.to_string(),
                _ => String::new(),
            };
            if modifier.as_deref() == Some("X") {
                let mut parts = Vec::new();
                for val in &right_vals {
                    parts.push(runtime::format_sprintf(&fmt, Some(val)));
                }
                Value::str(parts.join(" "))
            } else {
                let arg = right_vals.first();
                let rendered = runtime::format_sprintf(&fmt, arg);
                Value::str(rendered)
            }
        } else {
            let mut call_args = vec![left_val.clone()];
            call_args.extend(right_vals.clone());
            if modifier.as_deref() == Some("R") && call_args.len() == 2 {
                call_args.swap(0, 1);
            }
            let lookup_name = Self::canonical_infix_lookup_name(&name);
            let infix_name = format!("infix:<{}>", lookup_name.as_ref());
            let assoc = self
                .interpreter
                .infix_associativity(&infix_name)
                .unwrap_or_else(|| "left".to_string());
            if assoc == "chain" && call_args.len() > 2 {
                let mut all_true = true;
                for pair in call_args.windows(2) {
                    let left = pair[0].clone();
                    let right = pair[1].clone();
                    let pair_result =
                        if let Some(v) = self.try_user_infix(&infix_name, &left, &right)? {
                            v
                        } else {
                            self.call_infix_fallback(
                                lookup_name.as_ref(),
                                Some(&infix_name),
                                vec![left, right],
                                compiled_fns,
                            )?
                        };
                    if !pair_result.truthy() {
                        all_true = false;
                        break;
                    }
                }
                Value::Bool(all_true)
            } else if call_args.len() == 2 {
                let right_val = right_vals.first().cloned().unwrap_or(Value::Nil);
                if let Some(result) = self.try_user_infix(&infix_name, &left_val, &right_val)? {
                    result
                } else {
                    self.call_infix_fallback(
                        lookup_name.as_ref(),
                        Some(&infix_name),
                        call_args,
                        compiled_fns,
                    )?
                }
            } else {
                // For multi-arg calls (list-associative flattened chains),
                // try the user-defined function first before falling back
                // to built-in reduction.
                if let Some(def) = self
                    .interpreter
                    .resolve_function_with_types(&infix_name, &call_args)
                {
                    self.compile_and_call_function_def(&def, call_args.clone(), compiled_fns)?
                } else {
                    self.call_infix_fallback(
                        lookup_name.as_ref(),
                        Some(&infix_name),
                        call_args,
                        compiled_fns,
                    )?
                }
            }
        };
        self.stack.push(result);
        Ok(())
    }

    fn flip_flop_scope_key(&self) -> String {
        if let Some(Value::Int(id)) = self.interpreter.env().get("__mutsu_callable_id") {
            return format!("callable:{id}");
        }
        if let Some((package, name)) = self.interpreter.routine_stack_top() {
            return format!("routine:{package}::{name}");
        }
        "top".to_string()
    }

    fn flip_flop_operand_truthy(&mut self, value: &Value, is_rhs: bool) -> bool {
        match value {
            Value::Whatever => !is_rhs,
            Value::Regex(_)
            | Value::RegexWithAdverbs { .. }
            | Value::Routine { is_regex: true, .. } => {
                let topic = self
                    .interpreter
                    .env()
                    .get("_")
                    .cloned()
                    .unwrap_or(Value::Nil);
                self.vm_smart_match(&topic, value)
            }
            _ => value.truthy(),
        }
    }

    fn eval_expr_range(
        &mut self,
        code: &CompiledCode,
        start: usize,
        end: usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        let saved_depth = self.stack.len();
        self.run_range(code, start, end, compiled_fns)?;
        let value = self.stack.pop().unwrap_or(Value::Nil);
        self.stack.truncate(saved_depth);
        Ok(value)
    }

    fn flip_flop_step(
        &mut self,
        key: &str,
        lhs: bool,
        rhs: bool,
        exclude_start: bool,
        exclude_end: bool,
        is_fff: bool,
    ) -> Value {
        let seq = self
            .interpreter
            .get_state_var(key)
            .and_then(|v| match v {
                Value::Int(i) if *i > 0 => Some(*i),
                _ => None,
            })
            .unwrap_or(0);

        if seq > 0 {
            let current = seq;
            if rhs {
                self.interpreter
                    .set_state_var(key.to_string(), Value::Int(0));
                if exclude_end {
                    Value::Nil
                } else {
                    Value::Int(current)
                }
            } else {
                self.interpreter
                    .set_state_var(key.to_string(), Value::Int(current + 1));
                Value::Int(current)
            }
        } else if lhs {
            if !is_fff && rhs {
                self.interpreter
                    .set_state_var(key.to_string(), Value::Int(0));
                if exclude_start || exclude_end {
                    Value::Nil
                } else {
                    Value::Int(1)
                }
            } else {
                self.interpreter
                    .set_state_var(key.to_string(), Value::Int(2));
                if exclude_start {
                    Value::Nil
                } else {
                    Value::Int(1)
                }
            }
        } else {
            Value::Nil
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_flip_flop_expr_op(
        &mut self,
        code: &CompiledCode,
        ip: &mut usize,
        lhs_end: u32,
        rhs_end: u32,
        site_id: u64,
        exclude_start: bool,
        exclude_end: bool,
        is_fff: bool,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let lhs_start = *ip + 1;
        let lhs_end = lhs_end as usize;
        let rhs_start = lhs_end;
        let rhs_end = rhs_end as usize;

        if self.in_smartmatch_rhs {
            let lhs_pattern = self.eval_expr_range(code, lhs_start, lhs_end, compiled_fns)?;
            let rhs_pattern = self.eval_expr_range(code, rhs_start, rhs_end, compiled_fns)?;
            let scope = self.flip_flop_scope_key();
            let matcher_key = format!("__mutsu_ff_state::{scope}::{site_id}");
            let mut map = std::collections::HashMap::new();
            map.insert("__mutsu_ff_matcher".to_string(), Value::Bool(true));
            map.insert("key".to_string(), Value::str(matcher_key));
            map.insert("lhs".to_string(), lhs_pattern);
            map.insert("rhs".to_string(), rhs_pattern);
            map.insert("exclude_start".to_string(), Value::Bool(exclude_start));
            map.insert("exclude_end".to_string(), Value::Bool(exclude_end));
            map.insert("is_fff".to_string(), Value::Bool(is_fff));
            self.stack.push(Value::hash(map));
            *ip = rhs_end;
            return Ok(());
        }

        let scope = self.flip_flop_scope_key();
        let state_key = format!("__mutsu_ff_state::{scope}::{site_id}");
        let seq = self
            .interpreter
            .get_state_var(&state_key)
            .and_then(|v| match v {
                Value::Int(i) if *i > 0 => Some(*i),
                _ => None,
            })
            .unwrap_or(0);

        let (lhs, rhs) = if seq > 0 {
            if !is_fff {
                let _ = self.eval_expr_range(code, lhs_start, lhs_end, compiled_fns)?;
            }
            let rhs_value = self.eval_expr_range(code, rhs_start, rhs_end, compiled_fns)?;
            (false, self.flip_flop_operand_truthy(&rhs_value, true))
        } else {
            let lhs_value = self.eval_expr_range(code, lhs_start, lhs_end, compiled_fns)?;
            if !self.flip_flop_operand_truthy(&lhs_value, false) {
                (false, false)
            } else if is_fff {
                (true, false)
            } else {
                let rhs_value = self.eval_expr_range(code, rhs_start, rhs_end, compiled_fns)?;
                (true, self.flip_flop_operand_truthy(&rhs_value, true))
            }
        };

        let value = self.flip_flop_step(&state_key, lhs, rhs, exclude_start, exclude_end, is_fff);
        self.stack.push(value);
        *ip = rhs_end;
        Ok(())
    }

    fn call_infix_fallback(
        &mut self,
        name: &str,
        infix_name: Option<&str>,
        call_args: Vec<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        // When an infix operator is called with a single Iterable argument,
        // flatten it into elements (like a +@foo slurpy) and reduce over them.
        let call_args = if call_args.len() == 1 {
            match &call_args[0] {
                Value::Hash(map) => {
                    // Break Hash into Pairs
                    map.iter()
                        .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                        .collect::<Vec<_>>()
                }
                Value::Array(items, ..) => items.iter().cloned().collect(),
                _ => call_args,
            }
        } else {
            call_args
        };
        if call_args.len() >= 2 {
            let mut acc = call_args[0].clone();
            let mut reduced = true;
            for rhs in &call_args[1..] {
                match crate::runtime::Interpreter::apply_reduction_op(name, &acc, rhs) {
                    Ok(value) => acc = value,
                    Err(_) => {
                        reduced = false;
                        break;
                    }
                }
            }
            if reduced {
                return Ok(acc);
            }
        }
        if let Some(op_name) = infix_name
            && let Ok(v) = self
                .interpreter
                .call_user_routine_direct(op_name, call_args.clone())
        {
            return Ok(v);
        }
        if Self::should_retry_with_canonical_infix_name(name)
            && let Some(op_name) = infix_name
            && let Ok(v) =
                self.call_function_compiled_first(op_name, call_args.clone(), compiled_fns)
        {
            return Ok(v);
        }
        match self.call_function_compiled_first(name, call_args.clone(), compiled_fns) {
            Ok(v) => Ok(v),
            Err(err) => {
                // `for foo-bar() -> ...` currently produces an infix AST fallback call.
                // If `foo-bar` has explicit empty signature `:()`, retry zero-arg dispatch.
                let is_empty_sig_rejection = err
                    .message
                    .starts_with("Too many positionals passed; expected 0 arguments but got more")
                    || err.message.starts_with("Unexpected named argument '");
                if is_empty_sig_rejection {
                    if let Ok(v) = self.call_function_compiled_first(name, Vec::new(), compiled_fns)
                    {
                        return Ok(v);
                    }
                    if Self::should_retry_with_canonical_infix_name(name)
                        && let Some(op_name) = infix_name
                        && let Ok(v) =
                            self.call_function_compiled_first(op_name, Vec::new(), compiled_fns)
                    {
                        return Ok(v);
                    }
                    if let Some(op_name) = infix_name {
                        let op_env_name = format!("&{}", op_name);
                        if let Some(code_val) = self.interpreter.env().get(&op_env_name).cloned() {
                            return self.vm_call_on_value(code_val, Vec::new(), None);
                        }
                    }
                    let bare_env_name = format!("&{}", name);
                    if let Some(code_val) = self.interpreter.env().get(&bare_env_name).cloned() {
                        return self.vm_call_on_value(code_val, Vec::new(), None);
                    }
                    let method_name = name
                        .strip_prefix("infix:<")
                        .and_then(|s| s.strip_suffix('>'))
                        .unwrap_or(name);
                    if !method_name.is_empty()
                        && !call_args.is_empty()
                        && call_args[0].to_string_value() == "method"
                    {
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("name".to_string(), Value::str(method_name.to_string()));
                        attrs.insert("is_dispatcher".to_string(), Value::Bool(false));
                        let mut sig_attrs = std::collections::HashMap::new();
                        sig_attrs.insert("params".to_string(), Value::array(Vec::new()));
                        attrs.insert(
                            "signature".to_string(),
                            Value::make_instance(Symbol::intern("Signature"), sig_attrs),
                        );
                        attrs.insert("returns".to_string(), Value::Package(Symbol::intern("Mu")));
                        attrs.insert("of".to_string(), Value::Package(Symbol::intern("Mu")));
                        return Ok(Value::make_instance(Symbol::intern("Method"), attrs));
                    }
                    Err(RuntimeError::new(format!(
                        "Confused: two terms in a row (unknown infix function: {})",
                        name
                    )))
                } else {
                    if let Some(op_name) = infix_name {
                        let op_env_name = format!("&{}", op_name);
                        if let Some(code_val) = self.interpreter.env().get(&op_env_name).cloned() {
                            return self.vm_call_on_value(code_val, call_args, None);
                        }
                    }
                    let bare_env_name = format!("&{}", name);
                    if let Some(code_val) = self.interpreter.env().get(&bare_env_name).cloned() {
                        self.vm_call_on_value(code_val, call_args, None)
                    } else {
                        let method_name = name
                            .strip_prefix("infix:<")
                            .and_then(|s| s.strip_suffix('>'))
                            .unwrap_or(name);
                        if !method_name.is_empty()
                            && !call_args.is_empty()
                            && call_args[0].to_string_value() == "method"
                        {
                            let mut attrs = std::collections::HashMap::new();
                            attrs.insert("name".to_string(), Value::str(method_name.to_string()));
                            attrs.insert("is_dispatcher".to_string(), Value::Bool(false));
                            let mut sig_attrs = std::collections::HashMap::new();
                            sig_attrs.insert("params".to_string(), Value::array(Vec::new()));
                            attrs.insert(
                                "signature".to_string(),
                                Value::make_instance(Symbol::intern("Signature"), sig_attrs),
                            );
                            attrs.insert(
                                "returns".to_string(),
                                Value::Package(Symbol::intern("Mu")),
                            );
                            attrs.insert("of".to_string(), Value::Package(Symbol::intern("Mu")));
                            return Ok(Value::make_instance(Symbol::intern("Method"), attrs));
                        }
                        Err(RuntimeError::new(format!(
                            "Confused: two terms in a row (unknown infix function: {})",
                            name
                        )))
                    }
                }
            }
        }
    }

    /// Interpolate a substitution replacement string, evaluating `{...}` blocks
    /// as closures (Raku double-quoted string semantics).
    fn interpolate_subst_replacement_with_closures(&mut self, template: &str) -> String {
        let mut out = String::new();
        let mut i = 0usize;
        let bytes = template.as_bytes();
        while i < bytes.len() {
            // Handle escape sequences
            if bytes[i] == b'\\' && i + 1 < bytes.len() {
                match bytes[i + 1] {
                    b'n' => {
                        out.push('\n');
                        i += 2;
                    }
                    b't' => {
                        out.push('\t');
                        i += 2;
                    }
                    b'\\' => {
                        out.push('\\');
                        i += 2;
                    }
                    b'$' => {
                        out.push('$');
                        i += 2;
                    }
                    b'@' => {
                        out.push('@');
                        i += 2;
                    }
                    b'{' => {
                        out.push('{');
                        i += 2;
                    }
                    _ => {
                        out.push('\\');
                        i += 1;
                    }
                }
                continue;
            }
            // Handle {expr} code blocks
            if bytes[i] == b'{' {
                // Find matching closing brace (respecting nesting)
                let mut depth = 1;
                let mut j = i + 1;
                while j < bytes.len() && depth > 0 {
                    if bytes[j] == b'{' {
                        depth += 1;
                    }
                    if bytes[j] == b'}' {
                        depth -= 1;
                    }
                    if depth > 0 {
                        j += 1;
                    }
                }
                if depth == 0 {
                    let code_str = &template[i + 1..j];
                    let parsed = crate::parse_dispatch::parse_source(code_str);
                    if let Ok((stmts, _)) = parsed {
                        // Save $_ before evaluating the code block, as
                        // eval_block_value may clobber the topic variable.
                        let saved_topic = self.interpreter.env().get("_").cloned();
                        let val = self
                            .interpreter
                            .eval_block_value(&stmts)
                            .unwrap_or(Value::Nil);
                        // Restore $_ so the substitution can find the target
                        if let Some(topic) = saved_topic {
                            self.interpreter.env_mut().insert("_".to_string(), topic);
                        }
                        out.push_str(&val.to_string_value());
                    } else {
                        // If parsing fails, treat as literal text
                        out.push('{');
                        out.push_str(code_str);
                        out.push('}');
                    }
                    i = j + 1;
                    continue;
                }
                // Handle ${...}
                if i + 1 < bytes.len()
                    && bytes[i] == b'$'
                    && bytes[i + 1] == b'{'
                    && let Some(close) = template[i + 2..].find('}')
                {
                    let name = &template[i + 2..i + 2 + close];
                    let value = self
                        .interpreter
                        .env()
                        .get(name)
                        .cloned()
                        .unwrap_or(Value::Nil);
                    out.push_str(&value.to_string_value());
                    i += 2 + close + 1;
                    continue;
                }
            }
            // Handle $var and $var[idx]
            if bytes[i] == b'$' && i + 1 < bytes.len() {
                let after = &template[i + 1..];
                if let Some(name_len) = take_var_name(after) {
                    let name = &after[..name_len];
                    let after_name = &after[name_len..];
                    // Check for postcircumfix [idx]
                    if after_name.starts_with('[')
                        && let Some(close) = after_name.find(']')
                    {
                        let idx_str = &after_name[1..close];
                        let value = self
                            .interpreter
                            .env()
                            .get(name)
                            .cloned()
                            .unwrap_or(Value::Nil);
                        if let Ok(idx) = idx_str.parse::<i64>() {
                            match &value {
                                Value::Array(arr, _) => {
                                    let idx = if idx < 0 {
                                        (arr.len() as i64 + idx) as usize
                                    } else {
                                        idx as usize
                                    };
                                    let elem = arr.get(idx).cloned().unwrap_or(Value::Nil);
                                    out.push_str(&elem.to_string_value());
                                }
                                _ => {
                                    out.push_str(&value.to_string_value());
                                }
                            }
                        } else {
                            out.push_str(&value.to_string_value());
                        }
                        i += 1 + name_len + close + 1;
                        continue;
                    }
                    let value = self
                        .interpreter
                        .env()
                        .get(name)
                        .cloned()
                        .unwrap_or(Value::Nil);
                    out.push_str(&value.to_string_value());
                    i += 1 + name_len;
                    continue;
                }
            }
            // Handle @var[]
            if bytes[i] == b'@' && i + 1 < bytes.len() {
                let after = &template[i + 1..];
                if let Some(name_len) = take_var_name(after) {
                    let name = &after[..name_len];
                    let after_name = &after[name_len..];
                    if after_name.starts_with('[')
                        && let Some(close) = after_name.find(']')
                    {
                        let arr_name = format!("@{}", name);
                        let value = self
                            .interpreter
                            .env()
                            .get(&arr_name)
                            .cloned()
                            .unwrap_or(Value::Nil);
                        out.push_str(&value.to_string_value());
                        i += 1 + name_len + close + 1;
                        continue;
                    }
                    let arr_name = format!("@{}", name);
                    let value = self
                        .interpreter
                        .env()
                        .get(&arr_name)
                        .cloned()
                        .unwrap_or(Value::Nil);
                    out.push_str(&value.to_string_value());
                    i += 1 + name_len;
                    continue;
                }
            }
            out.push(template[i..].chars().next().unwrap());
            i += template[i..].chars().next().unwrap().len_utf8();
        }
        out
    }
}

/// Maximum elements to produce from Z (zip) when iterating over ranges.
/// This caps the output for infinite ranges (e.g., `1..* Z** 1..*`).
/// Kept small because the meta-operator (e.g. `**`) may be expensive
/// for large values. The caller (e.g., `.[^5]`) will further limit.
// TODO: Ideally Z should return a lazy Seq and only compute elements on demand.
const MAX_ZIP_EXPAND: usize = 1_000;

/// Helper for lazy index-based iteration over values in Z (zip) operations.
/// Avoids materializing huge ranges like `1..*` into million-element Vecs.
enum ZipIter {
    /// Inclusive integer range: elements are start, start+1, ..., end
    IntRange { start: i64, count: usize },
    /// Exclusive-end integer range: elements are start, start+1, ..., end-1
    IntRangeExcl { start: i64, count: usize },
    /// Already-materialized list
    List(Vec<Value>),
    /// A list that ends with `*` (Whatever): the last real element is repeated
    /// to extend the list to any requested length.
    ExtendedList {
        items: Vec<Value>,
        /// The last real element (before `*`), used for extension
        fill: Value,
    },
    /// A lazy list (preserves laziness for is-lazy propagation)
    Lazy(Vec<Value>),
}

impl ZipIter {
    fn from_value(val: &Value) -> Self {
        match val {
            Value::Range(a, b) => {
                let count = if *b >= *a {
                    ((*b - *a + 1) as usize).min(MAX_ZIP_EXPAND)
                } else {
                    0
                };
                ZipIter::IntRange { start: *a, count }
            }
            Value::RangeExcl(a, b) => {
                let count = if *b > *a {
                    ((*b - *a) as usize).min(MAX_ZIP_EXPAND)
                } else {
                    0
                };
                ZipIter::IntRangeExcl { start: *a, count }
            }
            Value::RangeExclStart(a, b) => {
                let start = *a + 1;
                let count = if *b >= start {
                    ((*b - start + 1) as usize).min(MAX_ZIP_EXPAND)
                } else {
                    0
                };
                ZipIter::IntRange { start, count }
            }
            Value::RangeExclBoth(a, b) => {
                let start = *a + 1;
                let count = if *b > start {
                    ((*b - start) as usize).min(MAX_ZIP_EXPAND)
                } else {
                    0
                };
                ZipIter::IntRangeExcl { start, count }
            }
            // Nil in zip context is a 1-element list (not empty), matching Raku behavior
            // where `Nil Z+ 2` yields `(2)` (Nil coerces to 0).
            Value::Nil => ZipIter::List(vec![Value::Nil]),
            Value::LazyList(_) => {
                let list = runtime::value_to_list(val);
                let len = list.len().min(MAX_ZIP_EXPAND);
                ZipIter::Lazy(list[..len].to_vec())
            }
            _ => {
                let list = runtime::value_to_list(val);
                // Check for trailing Whatever (*) — extends the list by
                // repeating the last real element.
                if list.len() >= 2 && matches!(list.last(), Some(Value::Whatever)) {
                    let items: Vec<Value> = list[..list.len() - 1].to_vec();
                    let fill = items.last().cloned().unwrap_or(Value::Nil);
                    ZipIter::ExtendedList { items, fill }
                } else {
                    ZipIter::List(list)
                }
            }
        }
    }

    /// Returns true if this side represents an infinite / lazy source.
    fn is_lazy(&self) -> bool {
        match self {
            ZipIter::IntRange { count, .. } | ZipIter::IntRangeExcl { count, .. } => {
                *count >= MAX_ZIP_EXPAND
            }
            ZipIter::ExtendedList { .. } | ZipIter::Lazy(_) => true,
            ZipIter::List(_) => false,
        }
    }

    fn len(&self) -> usize {
        match self {
            ZipIter::IntRange { count, .. } | ZipIter::IntRangeExcl { count, .. } => *count,
            ZipIter::List(v) | ZipIter::Lazy(v) => v.len(),
            // Extended lists can match any length from the other side
            ZipIter::ExtendedList { .. } => usize::MAX,
        }
    }

    fn nth(&self, i: usize) -> Value {
        match self {
            ZipIter::IntRange { start, .. } | ZipIter::IntRangeExcl { start, .. } => {
                Value::Int(*start + i as i64)
            }
            ZipIter::List(v) | ZipIter::Lazy(v) => v[i].clone(),
            ZipIter::ExtendedList { items, fill } => {
                if i < items.len() {
                    items[i].clone()
                } else {
                    fill.clone()
                }
            }
        }
    }
}
