use super::super::*;
use crate::symbol::Symbol;
use std::collections::HashMap;

impl Interpreter {
    /// Clear `$/` and all numeric capture variables (`$0`, `$1`, ...) after a failed match.
    pub(in crate::runtime) fn clear_match_state(&mut self) {
        self.env.insert("/".to_string(), Value::Nil);
        let numeric_keys: Vec<String> = self
            .env
            .keys()
            .filter(|k| !k.is_empty() && k.chars().all(|ch| ch.is_ascii_digit()))
            .cloned()
            .collect();
        for key in numeric_keys {
            self.env.insert(key, Value::Nil);
        }
    }

    pub(in crate::runtime) fn apply_single_regex_captures(&mut self, captures: &RegexCaptures) {
        let make_capture_match = |capture: &str, from: usize, to: usize| {
            let mut attrs = HashMap::new();
            attrs.insert("str".to_string(), Value::str(capture.to_string()));
            attrs.insert("from".to_string(), Value::Int(from as i64));
            attrs.insert("to".to_string(), Value::Int(to as i64));
            attrs.insert("list".to_string(), Value::array(Vec::new()));
            attrs.insert("named".to_string(), Value::hash(HashMap::new()));
            Value::make_instance(Symbol::intern("Match"), attrs)
        };

        let mut attrs = HashMap::new();
        attrs.insert("str".to_string(), Value::str(captures.matched.clone()));
        attrs.insert("from".to_string(), Value::Int(captures.from as i64));
        attrs.insert("to".to_string(), Value::Int(captures.to as i64));
        let positional: Vec<Value> = if !captures.positional_slots.is_empty() {
            captures
                .positional_slots
                .iter()
                .map(|slot| match slot {
                    Some((s, from, to)) => make_capture_match(s, *from, *to),
                    None => Value::Nil,
                })
                .collect()
        } else {
            captures
                .positional
                .iter()
                .enumerate()
                .map(|(i, s)| {
                    let (from, to) = captures
                        .positional_offsets
                        .get(i)
                        .copied()
                        .unwrap_or((0, s.chars().count()));
                    make_capture_match(s, from, to)
                })
                .collect()
        };
        attrs.insert("list".to_string(), Value::array(positional));
        let mut named = HashMap::new();
        for (k, v) in &captures.named {
            let vals: Vec<Value> = v
                .iter()
                .map(|s| make_capture_match(s, 0, s.chars().count()))
                .collect();
            if vals.len() == 1 {
                named.insert(k.clone(), vals[0].clone());
            } else {
                named.insert(k.clone(), Value::array(vals));
            }
        }
        attrs.insert("named".to_string(), Value::hash(named));
        let match_obj = Value::make_instance(Symbol::intern("Match"), attrs);
        self.env.insert("/".to_string(), match_obj.clone());

        // Reset stale numeric captures before applying new ones.
        let numeric_keys: Vec<String> = self
            .env
            .keys()
            .filter(|k| !k.is_empty() && k.chars().all(|ch| ch.is_ascii_digit()))
            .cloned()
            .collect();
        for key in numeric_keys {
            self.env.insert(key, Value::Nil);
        }

        for (i, slot) in captures.positional_slots.iter().enumerate() {
            let value = match slot {
                Some((capture, from, to)) => make_capture_match(capture, *from, *to),
                None => Value::Nil,
            };
            self.env.insert(i.to_string(), value);
        }
        if captures.positional_slots.is_empty() {
            self.env.insert("0".to_string(), Value::Nil);
        }
        // Set named capture env vars from the match object's named hash
        if let Value::Instance { ref attributes, .. } = match_obj
            && let Some(Value::Hash(named_hash)) = attributes.get("named")
        {
            for (k, v) in named_hash.iter() {
                self.env.insert(format!("<{}>", k), v.clone());
            }
        }
    }

    /// Select non-overlapping matches from all matches (for :g/global).
    /// Takes the longest match at each position, then greedily selects
    /// matches that don't overlap with previously selected ones.
    pub(in crate::runtime) fn select_non_overlapping_matches(
        &self,
        all: Vec<RegexCaptures>,
    ) -> Vec<RegexCaptures> {
        if all.is_empty() {
            return Vec::new();
        }
        // First, pick the longest match at each starting position
        let mut best_by_start: std::collections::BTreeMap<usize, RegexCaptures> =
            std::collections::BTreeMap::new();
        for capture in all {
            let key = capture.from;
            match best_by_start.get(&key) {
                Some(existing) if capture.to <= existing.to => {}
                _ => {
                    best_by_start.insert(key, capture);
                }
            }
        }
        // Then greedily select non-overlapping: skip if starts before previous end
        let mut result = Vec::new();
        let mut last_end = 0usize;
        for (_, capture) in best_by_start {
            if capture.from >= last_end {
                last_end = capture.to;
                result.push(capture);
            }
        }
        result
    }

    /// Parse :x(...) style repeat bounds.
    /// Returns (min_required, max_to_return). `max_to_return = None` means unbounded.
    pub(in crate::runtime) fn parse_match_repeat_bounds(
        value: &Value,
    ) -> Option<(usize, Option<usize>)> {
        fn parse_non_negative_int(v: &Value) -> Option<i64> {
            match v {
                Value::Int(i) => Some((*i).max(0)),
                Value::Num(n) if n.is_finite() && n.fract() == 0.0 => Some((*n as i64).max(0)),
                Value::Str(s) => s.trim().parse::<i64>().ok().map(|i| i.max(0)),
                Value::Whatever => Some(i64::MAX),
                _ => None,
            }
        }

        fn adjust_range_bounds(
            start: i64,
            end: i64,
            excl_start: bool,
            excl_end: bool,
        ) -> Option<(usize, Option<usize>)> {
            let mut min = start;
            let mut max = end;
            if excl_start {
                min = min.saturating_add(1);
            }
            if excl_end && max != i64::MAX {
                max = max.saturating_sub(1);
            }
            min = min.max(0);
            if max != i64::MAX {
                max = max.max(0);
                if max < min {
                    return None;
                }
                Some((min as usize, Some(max as usize)))
            } else {
                Some((min as usize, None))
            }
        }

        match value {
            Value::Range(start, end) => adjust_range_bounds(*start, *end, false, false),
            Value::RangeExcl(start, end) => adjust_range_bounds(*start, *end, false, true),
            Value::RangeExclStart(start, end) => adjust_range_bounds(*start, *end, true, false),
            Value::RangeExclBoth(start, end) => adjust_range_bounds(*start, *end, true, true),
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                let min = parse_non_negative_int(start.as_ref())?;
                let max = parse_non_negative_int(end.as_ref())?;
                adjust_range_bounds(min, max, *excl_start, *excl_end)
            }
            _ => {
                let n = parse_non_negative_int(value)?;
                if n == i64::MAX {
                    Some((0, None))
                } else {
                    Some((n as usize, Some(n as usize)))
                }
            }
        }
    }

    /// Apply :x bounds to already-ordered matches.
    pub(in crate::runtime) fn select_matches_by_repeat_bounds(
        matches: Vec<RegexCaptures>,
        min_required: usize,
        max_to_return: Option<usize>,
    ) -> Option<Vec<RegexCaptures>> {
        if matches.len() < min_required {
            return None;
        }
        let take_n = max_to_return.unwrap_or(matches.len()).min(matches.len());
        Some(matches.into_iter().take(take_n).collect())
    }

    /// Apply multiple regex captures (for :g, :ov, :ex) -- set $/ to list of Match objects.
    pub(in crate::runtime) fn apply_multi_regex_captures(&mut self, selected: &[RegexCaptures]) {
        let slash_list = selected
            .iter()
            .map(|c| {
                Value::make_match_object_with_captures(
                    c.matched.clone(),
                    c.from as i64,
                    c.to as i64,
                    &c.positional,
                    &c.named,
                )
            })
            .collect::<Vec<_>>();
        self.env.insert("/".to_string(), Value::array(slash_list));
        if let Some(first) = selected.first() {
            for (i, cap) in first.positional.iter().enumerate() {
                self.env.insert(i.to_string(), Value::str(cap.clone()));
            }
        }
    }

    pub(in crate::runtime) fn set_pending_nth_error(message: String) {
        crate::runtime::regex_parse::PENDING_REGEX_ERROR.with(|e| {
            *e.borrow_mut() = Some(RuntimeError::new(message));
        });
    }

    pub(in crate::runtime) fn parse_nth_token(token: &str, total: usize) -> Result<usize, String> {
        let t = token.trim();
        if t.is_empty() {
            return Err("Invalid :nth index ()".to_string());
        }
        if t.eq_ignore_ascii_case("-Inf") {
            return Err("Invalid :nth index (-Inf)".to_string());
        }
        if t == "*" {
            if total == 0 {
                return Err("Invalid :nth index (*)".to_string());
            }
            return Ok(total);
        }
        if let Some(rest) = t.strip_prefix("*-") {
            let n = rest
                .trim()
                .parse::<usize>()
                .map_err(|_| format!("Invalid :nth index ({t})"))?;
            if n >= total {
                return Err(format!("Invalid :nth index ({t})"));
            }
            return Ok(total - n);
        }
        let n = t
            .parse::<i64>()
            .map_err(|_| format!("Invalid :nth index ({t})"))?;
        if n <= 0 {
            return Err(format!("Invalid :nth index ({t})"));
        }
        Ok(n as usize)
    }

    pub(in crate::runtime) fn collect_nth_indices_from_value(
        &self,
        value: &Value,
        total: usize,
        out: &mut Vec<usize>,
    ) -> Result<(), String> {
        match value {
            Value::Int(i) => out.push(Self::parse_nth_token(&i.to_string(), total)?),
            Value::Num(n) => {
                if n.fract() != 0.0 {
                    return Err(format!("Invalid :nth index ({n})"));
                }
                out.push(Self::parse_nth_token(&format!("{}", *n as i64), total)?);
            }
            Value::Str(s) => {
                for piece in s.split(',') {
                    out.push(Self::parse_nth_token(piece, total)?);
                }
            }
            Value::Whatever => out.push(Self::parse_nth_token("*", total)?),
            Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => {
                for item in items.iter() {
                    self.collect_nth_indices_from_value(item, total, out)?;
                }
            }
            _ => {
                return Err(format!("Invalid :nth index ({})", value.to_string_value()));
            }
        }
        Ok(())
    }

    pub(in crate::runtime) fn resolve_nth_indices(
        &self,
        raw: &str,
        total: usize,
    ) -> Result<Vec<usize>, String> {
        let raw = raw.trim();
        if raw.starts_with('$') {
            let var_name = raw.trim_start_matches('$');
            let value = self.env.get(var_name).cloned().unwrap_or(Value::Nil);
            let mut out = Vec::new();
            self.collect_nth_indices_from_value(&value, total, &mut out)?;
            return Ok(out);
        }
        let mut out = Vec::new();
        for token in raw.split(',') {
            out.push(Self::parse_nth_token(token, total)?);
        }
        Ok(out)
    }

    /// Get the match continuation position from `$/.to`, defaulting to 0.
    pub(in crate::runtime) fn get_match_to_position(&self) -> usize {
        if let Some(Value::Instance { attributes, .. }) = self.env.get("/")
            && let Some(Value::Int(to)) = attributes.get("to")
        {
            return *to as usize;
        }
        0
    }

    #[cfg(feature = "pcre2")]
    pub(in crate::runtime) fn regex_match_with_captures_p5(
        &self,
        pattern: &str,
        text: &str,
    ) -> Option<RegexCaptures> {
        let re = self.compile_p5_regex(pattern)?;
        let mut locs = re.capture_locations();
        let m0 = re.captures_read(&mut locs, text.as_bytes()).ok()??;
        let names = re.capture_names();
        let mut out = RegexCaptures {
            matched: text.get(m0.start()..m0.end())?.to_string(),
            from: m0.start(),
            to: m0.end(),
            ..RegexCaptures::default()
        };
        for idx in 1..locs.len() {
            if names.get(idx).is_some_and(Option::is_none) {
                if let Some((start, end)) = locs.get(idx) {
                    let captured = text.get(start..end)?.to_string();
                    out.positional.push(captured.clone());
                    out.positional_offsets.push((start, end));
                    out.positional_slots.push(Some((captured, start, end)));
                } else {
                    out.positional_slots.push(None);
                }
                continue;
            }
            if let (Some(Some(name)), Some((start, end))) = (names.get(idx), locs.get(idx)) {
                let captured = text.get(start..end)?.to_string();
                out.named
                    .entry(name.to_string())
                    .or_default()
                    .push(captured);
            }
        }
        Some(out)
    }

    #[cfg(feature = "pcre2")]
    pub(in crate::runtime) fn regex_match_all_with_captures_p5(
        &self,
        pattern: &str,
        text: &str,
    ) -> Vec<RegexCaptures> {
        let Some(re) = self.compile_p5_regex(pattern) else {
            return Vec::new();
        };
        let names = re.capture_names();
        let mut out = Vec::new();
        let mut start = 0usize;
        let bytes = text.as_bytes();
        let mut locs = re.capture_locations();
        while start <= bytes.len() {
            let Ok(Some(m0)) = re.captures_read_at(&mut locs, bytes, start) else {
                break;
            };
            let Some(matched) = text.get(m0.start()..m0.end()) else {
                break;
            };
            let mut item = RegexCaptures {
                matched: matched.to_string(),
                from: m0.start(),
                to: m0.end(),
                ..RegexCaptures::default()
            };
            for idx in 1..locs.len() {
                if names.get(idx).is_some_and(Option::is_none) {
                    if let Some((c_start, c_end)) = locs.get(idx) {
                        let Some(captured) = text.get(c_start..c_end) else {
                            continue;
                        };
                        item.positional.push(captured.to_string());
                        item.positional_offsets.push((c_start, c_end));
                        item.positional_slots
                            .push(Some((captured.to_string(), c_start, c_end)));
                    } else {
                        item.positional_slots.push(None);
                    }
                    continue;
                }
                if let (Some(Some(name)), Some((c_start, c_end))) = (names.get(idx), locs.get(idx))
                {
                    let Some(captured) = text.get(c_start..c_end) else {
                        continue;
                    };
                    item.named
                        .entry(name.to_string())
                        .or_default()
                        .push(captured.to_string());
                }
            }
            // Advance past the match (at least 1 byte to avoid infinite loop)
            if m0.end() == start {
                start += 1;
            } else {
                start = m0.end();
            }
            out.push(item);
        }
        out
    }

    /// Find first P5 regex match, returning (byte_start, byte_end) converted to
    /// char-index pairs for compatibility with `apply_substitutions`.
    #[cfg(feature = "pcre2")]
    pub(crate) fn regex_find_first_p5(&self, pattern: &str, text: &str) -> Option<(usize, usize)> {
        let re = self.compile_p5_regex(pattern)?;
        let bytes = text.as_bytes();
        let mut locs = re.capture_locations();
        let m0 = re.captures_read(&mut locs, bytes).ok()??;
        let byte_start = m0.start();
        let byte_end = m0.end();
        // Convert byte offsets to char indices
        let char_start = text[..byte_start].chars().count();
        let char_end = text[..byte_end].chars().count();
        Some((char_start, char_end))
    }

    #[cfg(not(feature = "pcre2"))]
    pub(crate) fn regex_find_first_p5(&self, pattern: &str, text: &str) -> Option<(usize, usize)> {
        // Fallback: convert P5 pattern and use Raku regex engine
        self.regex_find_first(pattern, text)
    }

    /// Find all non-overlapping P5 regex matches, returning char-index pairs.
    #[cfg(feature = "pcre2")]
    pub(crate) fn regex_find_all_p5(&self, pattern: &str, text: &str) -> Vec<(usize, usize)> {
        let Some(re) = self.compile_p5_regex(pattern) else {
            return Vec::new();
        };
        let bytes = text.as_bytes();
        let mut results = Vec::new();
        let mut start = 0usize;
        let mut locs = re.capture_locations();
        while start <= bytes.len() {
            let Ok(Some(m0)) = re.captures_read_at(&mut locs, bytes, start) else {
                break;
            };
            let byte_start = m0.start();
            let byte_end = m0.end();
            let char_start = text[..byte_start].chars().count();
            let char_end = text[..byte_end].chars().count();
            results.push((char_start, char_end));
            if m0.end() == start {
                start += 1;
            } else {
                start = m0.end();
            }
        }
        results
    }

    #[cfg(not(feature = "pcre2"))]
    pub(crate) fn regex_find_all_p5(&self, pattern: &str, text: &str) -> Vec<(usize, usize)> {
        // Fallback: use Raku regex engine
        self.regex_find_all(pattern, text)
    }

    /// Extract the regex pattern string from a named token/regex definition.
    /// Returns `Some(pattern)` if the token body contains a single regex literal.
    pub(in crate::runtime) fn extract_token_regex_pattern(&self, name: &str) -> Option<String> {
        let defs = self.resolve_token_defs(name)?;
        let def = defs.first()?;
        // Look for a body consisting of a single Expr(Literal(Regex(pat)))
        if def.body.len() == 1
            && let Stmt::Expr(Expr::Literal(Value::Regex(pat))) = &def.body[0]
        {
            return Some(pat.to_string());
        }
        None
    }
}
