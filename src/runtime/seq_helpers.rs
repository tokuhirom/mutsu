use super::*;
use crate::value::signature::{SigInfo, SigParam, extract_sig_info, signature_smartmatch};

impl Interpreter {
    fn signature_capture_like(value: &Value) -> Option<(Vec<Value>, HashMap<String, Value>)> {
        match value {
            Value::Capture { positional, named } => Some((positional.clone(), named.clone())),
            Value::Hash(map) => Some((Vec::new(), (**map).clone())),
            Value::Set(items) => Some((
                Vec::new(),
                items
                    .iter()
                    .map(|k| (k.clone(), Value::Bool(true)))
                    .collect(),
            )),
            Value::Bag(items) => Some((
                Vec::new(),
                items
                    .iter()
                    .map(|(k, v)| (k.clone(), Value::Int(*v)))
                    .collect(),
            )),
            Value::Mix(items) => Some((
                Vec::new(),
                items
                    .iter()
                    .map(|(k, v)| (k.clone(), Value::Num(*v)))
                    .collect(),
            )),
            Value::Rat(n, d) | Value::FatRat(n, d) => {
                let mut named = HashMap::new();
                named.insert("numerator".to_string(), Value::Int(*n));
                named.insert("denominator".to_string(), Value::Int(*d));
                Some((Vec::new(), named))
            }
            Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => {
                let mut positional = Vec::new();
                let mut named = HashMap::new();
                for item in items.iter() {
                    if let Value::Pair(k, v) = item {
                        named.insert(k.clone(), *v.clone());
                    } else {
                        positional.push(item.clone());
                    }
                }
                Some((positional, named))
            }
            Value::Pair(k, v) => {
                let mut named = HashMap::new();
                named.insert(k.clone(), *v.clone());
                Some((Vec::new(), named))
            }
            Value::Instance { attributes, .. } => Some((Vec::new(), (**attributes).clone())),
            Value::Mixin(inner, _) => Self::signature_capture_like(inner),
            _ => None,
        }
    }

    fn signature_where_ok(&mut self, candidate: &Value, where_expr: &Expr) -> bool {
        let saved = self.env.clone();
        self.env.insert("_".to_string(), candidate.clone());
        let ok = match where_expr {
            Expr::AnonSub { body, .. } => self
                .eval_block_value(body)
                .map(|v| v.truthy())
                .unwrap_or(false),
            expr => self
                .eval_block_value(&[Stmt::Expr(expr.clone())])
                .map(|v| self.smart_match(candidate, &v))
                .unwrap_or(false),
        };
        self.env = saved;
        ok
    }

    fn sig_param_optional(p: &SigParam) -> bool {
        p.has_default || p.optional_marker
    }

    fn sig_param_matches_value(&mut self, candidate: &Value, param: &SigParam) -> bool {
        if let Some(constraint) = &param.type_constraint
            && !self.type_matches_value(constraint, candidate)
        {
            return false;
        }
        if let Some(where_expr) = &param.where_constraint
            && !self.signature_where_ok(candidate, where_expr)
        {
            return false;
        }
        if let Some(sub_params) = &param.sub_signature {
            let sub = SigInfo {
                params: sub_params.clone(),
                return_type: None,
            };
            if !self.signature_accepts_value(candidate, &sub) {
                return false;
            }
        }
        true
    }

    fn signature_accepts_value(&mut self, left: &Value, signature: &SigInfo) -> bool {
        let Some((positional, named)) = Self::signature_capture_like(left) else {
            return false;
        };

        let mut pos_idx = 0usize;
        let mut consumed_named: HashSet<String> = HashSet::new();
        let has_capture = signature.params.iter().any(|p| p.is_capture);
        let has_slurpy_positional = signature
            .params
            .iter()
            .any(|p| p.slurpy && !(p.named || p.sigil == '%'));
        let has_slurpy_named = signature
            .params
            .iter()
            .any(|p| p.slurpy && (p.named || p.sigil == '%'));

        if has_capture {
            return true;
        }

        for param in &signature.params {
            if param.is_capture {
                return true;
            }

            let is_named_space = param.named || (param.slurpy && param.sigil == '%');
            if is_named_space {
                if param.slurpy {
                    if let Some(constraint) = &param.type_constraint {
                        for (key, value) in &named {
                            if consumed_named.contains(key) {
                                continue;
                            }
                            if !self.type_matches_value(constraint, value) {
                                return false;
                            }
                        }
                    }
                    consumed_named.extend(named.keys().cloned());
                    continue;
                }

                let mut candidate = named.get(&param.name).cloned();
                if candidate.is_none() {
                    candidate = self
                        .call_method_with_values(left.clone(), &param.name, Vec::new())
                        .ok();
                }
                let Some(candidate) = candidate else {
                    if Self::sig_param_optional(param) {
                        continue;
                    }
                    return false;
                };
                consumed_named.insert(param.name.clone());
                if !self.sig_param_matches_value(&candidate, param) {
                    return false;
                }
                continue;
            }

            if param.slurpy {
                if let Some(constraint) = &param.type_constraint {
                    for value in &positional[pos_idx..] {
                        if !self.type_matches_value(constraint, value) {
                            return false;
                        }
                    }
                }
                pos_idx = positional.len();
                continue;
            }

            let Some(candidate) = positional.get(pos_idx).cloned() else {
                if Self::sig_param_optional(param) {
                    continue;
                }
                return false;
            };
            pos_idx += 1;
            if !self.sig_param_matches_value(&candidate, param) {
                return false;
            }
        }

        if !has_slurpy_positional && pos_idx < positional.len() {
            return false;
        }
        if !has_slurpy_named
            && named
                .keys()
                .any(|key| !consumed_named.contains(key.as_str()))
        {
            return false;
        }
        true
    }

    fn p5_pattern_to_rust_regex(pattern: &str) -> String {
        let chars: Vec<char> = pattern.chars().collect();
        let mut i = 0usize;
        let mut out = String::new();
        while i < chars.len() {
            if chars[i] == '(' && i + 3 < chars.len() && chars[i + 1] == '?' {
                if chars[i + 2] == '<' {
                    let mut j = i + 3;
                    let mut name = String::new();
                    while j < chars.len() && chars[j] != '>' {
                        name.push(chars[j]);
                        j += 1;
                    }
                    if j < chars.len() && !name.is_empty() {
                        out.push_str("(?P<");
                        out.push_str(&name);
                        out.push('>');
                        i = j + 1;
                        continue;
                    }
                } else if chars[i + 2] == '\'' {
                    let mut j = i + 3;
                    let mut name = String::new();
                    while j < chars.len() && chars[j] != '\'' {
                        name.push(chars[j]);
                        j += 1;
                    }
                    if j < chars.len() && !name.is_empty() {
                        out.push_str("(?P<");
                        out.push_str(&name);
                        out.push('>');
                        i = j + 1;
                        continue;
                    }
                }
            }
            out.push(chars[i]);
            i += 1;
        }
        out
    }

    fn expand_p5_interpolation(&self, pattern: &str) -> String {
        let chars: Vec<char> = pattern.chars().collect();
        let mut out = String::new();
        let mut i = 0usize;
        while i < chars.len() {
            if chars[i] == '\\' {
                out.push(chars[i]);
                i += 1;
                if i < chars.len() {
                    out.push(chars[i]);
                    i += 1;
                }
                continue;
            }
            if chars[i] == '$'
                && i + 1 < chars.len()
                && (chars[i + 1] == '_' || chars[i + 1].is_ascii_alphabetic())
            {
                let mut j = i + 1;
                while j < chars.len() && (chars[j] == '_' || chars[j].is_ascii_alphanumeric()) {
                    j += 1;
                }
                let name: String = chars[i + 1..j].iter().collect();
                if let Some(value) = self.env.get(&name) {
                    out.push_str(&value.to_string_value());
                }
                i = j;
                continue;
            }
            out.push(chars[i]);
            i += 1;
        }
        out
    }

    fn compile_p5_regex(&self, pattern: &str) -> Option<fancy_regex::Regex> {
        let interpolated = self.expand_p5_interpolation(pattern);
        let converted = Self::p5_pattern_to_rust_regex(&interpolated);
        let transformed = Self::transform_p5_pattern(&converted);
        fancy_regex::Regex::new(&transformed).ok()
    }

    /// Transform P5 regex patterns to handle Perl 5 semantics that
    /// fancy-regex doesn't support directly:
    /// - `\Z` → `(?=\n?\z)` (match before optional final newline)
    /// - `$` without `(?m)` → `(?=\n?\z)` (P5 $ matches before optional trailing newline)
    /// - Fix `[[` inside character classes for compatibility
    /// - `(?(?=X)yes|no)` → `(?:(?=X)yes|(?!X)no)` (conditional with lookahead)
    /// - `(?(?!X)yes|no)` → `(?:(?!X)yes|(?=X)no)` (conditional with neg lookahead)
    /// - `(?(?{0})yes|no)` → `(?:no)` (code eval returning false)
    /// - `(?(?{1})yes|no)` → `(?:yes)` (code eval returning true)
    /// - `(?(N)yes|no)` with undefined group N → `(?:no)`
    fn transform_p5_pattern(pattern: &str) -> String {
        // First pass: count defined capture groups
        let num_groups = Self::count_capture_groups(pattern);
        // Second pass: transform
        let chars: Vec<char> = pattern.chars().collect();
        let mut out = String::new();
        let mut i = 0;
        let mut in_char_class = false;
        let mut multiline_active = false;
        while i < chars.len() {
            if !in_char_class && chars[i] == '\\' && i + 1 < chars.len() {
                if chars[i + 1] == 'Z' {
                    out.push_str("(?=\\n?\\z)");
                    i += 2;
                    continue;
                }
                out.push(chars[i]);
                out.push(chars[i + 1]);
                i += 2;
                continue;
            }
            if chars[i] == '[' && !in_char_class {
                in_char_class = true;
                out.push(chars[i]);
                i += 1;
                if i < chars.len() && chars[i] == '^' {
                    out.push(chars[i]);
                    i += 1;
                }
                if i < chars.len() && chars[i] == ']' {
                    out.push('\\');
                    out.push(']');
                    i += 1;
                }
                continue;
            }
            if in_char_class && chars[i] == '[' {
                out.push('\\');
                out.push('[');
                i += 1;
                continue;
            }
            if in_char_class && chars[i] == ']' {
                in_char_class = false;
                out.push(chars[i]);
                i += 1;
                continue;
            }
            // Handle P5 conditional patterns: (?(COND)yes|no)
            if !in_char_class && chars[i] == '(' && i + 1 < chars.len() && chars[i + 1] == '?' {
                let rest: String = chars[i + 2..].iter().collect();
                // Track (?m) flag
                if rest.starts_with('m') {
                    multiline_active = true;
                }
                // Conditional with lookahead/lookbehind: (?(?=X)yes|no)
                if (rest.starts_with("(?=") || rest.starts_with("(?!"))
                    && let Some(transformed) =
                        Self::transform_conditional_lookahead(&chars, i, false)
                {
                    out.push_str(&transformed.0);
                    i = transformed.1;
                    continue;
                }
                if (rest.starts_with("(?<=") || rest.starts_with("(?<!"))
                    && let Some(transformed) =
                        Self::transform_conditional_lookahead(&chars, i, true)
                {
                    out.push_str(&transformed.0);
                    i = transformed.1;
                    continue;
                }
                // Conditional with code: (?(?{0})yes|no) or (?(?{1})yes|no)
                if rest.starts_with("(?{")
                    && let Some(transformed) = Self::transform_conditional_code(&chars, i)
                {
                    out.push_str(&transformed.0);
                    i = transformed.1;
                    continue;
                }
                // Conditional with backreference: (?(N)yes|no)
                if rest.starts_with('(')
                    && let Some(transformed) =
                        Self::transform_conditional_backref(&chars, i, num_groups)
                {
                    out.push_str(&transformed.0);
                    i = transformed.1;
                    continue;
                }
            }
            if !in_char_class && chars[i] == '$' && !multiline_active {
                let next = chars.get(i + 1);
                let is_end_anchor = next.is_none()
                    || *next.unwrap() == ')'
                    || *next.unwrap() == '|'
                    || *next.unwrap() == '/';
                if is_end_anchor {
                    out.push_str("(?=\\n?\\z)");
                    i += 1;
                    continue;
                }
            }
            out.push(chars[i]);
            i += 1;
        }
        out
    }

    /// Count the number of capture groups in a P5 regex pattern.
    fn count_capture_groups(pattern: &str) -> usize {
        let chars: Vec<char> = pattern.chars().collect();
        let mut count = 0;
        let mut i = 0;
        let mut in_class = false;
        while i < chars.len() {
            if chars[i] == '\\' {
                i += 2;
                continue;
            }
            if chars[i] == '[' && !in_class {
                in_class = true;
                i += 1;
                continue;
            }
            if chars[i] == ']' && in_class {
                in_class = false;
                i += 1;
                continue;
            }
            if !in_class && chars[i] == '(' {
                if i + 1 < chars.len() && chars[i + 1] == '?' {
                    // Check for conditional: (?(N)...) or (?(...) ...)
                    // The inner (N) or (...) is NOT a capture group
                    if i + 2 < chars.len() && chars[i + 2] == '(' {
                        // Skip the condition part: (?(...)
                        i += 3; // skip (?(
                        let mut depth = 1;
                        while i < chars.len() && depth > 0 {
                            if chars[i] == '(' {
                                depth += 1;
                            } else if chars[i] == ')' {
                                depth -= 1;
                            }
                            i += 1;
                        }
                        continue;
                    }
                    // Non-capturing or special group — don't count
                } else {
                    count += 1;
                }
            }
            i += 1;
        }
        count
    }

    /// Transform `(?(?=X)yes|no)` or `(?(?!X)yes|no)` into
    /// `(?:(?=X)yes|(?!X)no)` or `(?:(?!X)yes|(?=X)no)`.
    /// Returns (replacement_string, new_index_after_the_conditional).
    fn transform_conditional_lookahead(
        chars: &[char],
        start: usize,
        _is_lookbehind: bool,
    ) -> Option<(String, usize)> {
        // start points to '(' of '(?(...'
        // chars[start] = '('
        // chars[start+1] = '?'
        // chars[start+2] = '(' of the condition
        let cond_start = start + 2;
        // Find matching ')' for the condition
        let cond_end = Self::find_matching_paren(chars, cond_start)?;
        let condition: String = chars[cond_start..=cond_end].iter().collect();

        // After condition, parse yes and optional no branches
        let mut pos = cond_end + 1;
        let (yes_branch, no_branch, end) = Self::parse_conditional_branches(chars, pos)?;
        pos = end;

        // Determine the negated condition
        let negated = if condition.starts_with("(?=") {
            condition.replacen("(?=", "(?!", 1)
        } else if condition.starts_with("(?!") {
            condition.replacen("(?!", "(?=", 1)
        } else if condition.starts_with("(?<=") {
            condition.replacen("(?<=", "(?<!", 1)
        } else if condition.starts_with("(?<!") {
            condition.replacen("(?<!", "(?<=", 1)
        } else {
            return None;
        };

        let result = format!("(?:{condition}{yes_branch}|{negated}{no_branch})");
        Some((result, pos))
    }

    /// Transform `(?(?{0})yes|no)` → `(?:no)` and `(?(?{1})yes|no)` → `(?:yes)`.
    fn transform_conditional_code(chars: &[char], start: usize) -> Option<(String, usize)> {
        let cond_start = start + 2;
        let cond_end = Self::find_matching_paren(chars, cond_start)?;
        let condition: String = chars[cond_start + 1..cond_end].iter().collect();
        // condition is like "?{0}" or "?{1}"
        let code_value = if condition == "?{0}" {
            false
        } else if condition == "?{1}" {
            true
        } else {
            // Unknown code — treat as false (safe default)
            false
        };

        let pos = cond_end + 1;
        let (yes_branch, no_branch, end) = Self::parse_conditional_branches(chars, pos)?;

        let result = if code_value {
            format!("(?:{yes_branch})")
        } else {
            format!("(?:{no_branch})")
        };
        Some((result, end))
    }

    /// Transform `(?(N)yes|no)` with undefined group N → `(?:no)`.
    /// If group N IS defined, leave it as-is for fancy-regex to handle.
    fn transform_conditional_backref(
        chars: &[char],
        start: usize,
        num_groups: usize,
    ) -> Option<(String, usize)> {
        // chars[start] = '(', chars[start+1] = '?', chars[start+2] = '('
        let mut pos = start + 3;
        let mut num_str = String::new();
        while pos < chars.len() && chars[pos].is_ascii_digit() {
            num_str.push(chars[pos]);
            pos += 1;
        }
        if num_str.is_empty() || pos >= chars.len() || chars[pos] != ')' {
            return None;
        }
        let group_num: usize = num_str.parse().ok()?;
        pos += 1; // skip ')'

        if group_num <= num_groups {
            // Group exists, fancy-regex can handle it — don't transform
            return None;
        }

        // Group doesn't exist — always take the "no" branch
        let (_, no_branch, end) = Self::parse_conditional_branches(chars, pos)?;
        Some((format!("(?:{no_branch})"), end))
    }

    /// Find the matching ')' for a '(' at the given position.
    fn find_matching_paren(chars: &[char], start: usize) -> Option<usize> {
        if start >= chars.len() || chars[start] != '(' {
            return None;
        }
        let mut depth = 1;
        let mut i = start + 1;
        while i < chars.len() {
            if chars[i] == '\\' {
                i += 2;
                continue;
            }
            if chars[i] == '(' {
                depth += 1;
            } else if chars[i] == ')' {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            i += 1;
        }
        None
    }

    /// Parse `yes|no)` or `yes)` branches of a conditional pattern.
    /// Returns (yes_branch, no_branch, position_after_closing_paren).
    fn parse_conditional_branches(chars: &[char], start: usize) -> Option<(String, String, usize)> {
        let mut depth = 1; // We're inside the outer (? ... )
        let mut i = start;
        let mut pipe_pos = None;
        while i < chars.len() {
            if chars[i] == '\\' {
                i += 2;
                continue;
            }
            if chars[i] == '(' {
                depth += 1;
            } else if chars[i] == ')' {
                depth -= 1;
                if depth == 0 {
                    let yes_branch: String = if let Some(pp) = pipe_pos {
                        chars[start..pp].iter().collect()
                    } else {
                        chars[start..i].iter().collect()
                    };
                    let no_branch: String = if let Some(pp) = pipe_pos {
                        chars[pp + 1..i].iter().collect()
                    } else {
                        String::new()
                    };
                    return Some((yes_branch, no_branch, i + 1));
                }
            } else if chars[i] == '|' && depth == 1 && pipe_pos.is_none() {
                pipe_pos = Some(i);
            }
            i += 1;
        }
        None
    }

    fn apply_single_regex_captures(&mut self, captures: &RegexCaptures) {
        let make_capture_match = |capture: &str, from: usize, to: usize| {
            let mut attrs = HashMap::new();
            attrs.insert("str".to_string(), Value::Str(capture.to_string()));
            attrs.insert("from".to_string(), Value::Int(from as i64));
            attrs.insert("to".to_string(), Value::Int(to as i64));
            attrs.insert("list".to_string(), Value::array(Vec::new()));
            attrs.insert("named".to_string(), Value::hash(HashMap::new()));
            Value::make_instance("Match".to_string(), attrs)
        };

        let mut attrs = HashMap::new();
        attrs.insert("str".to_string(), Value::Str(captures.matched.clone()));
        attrs.insert("from".to_string(), Value::Int(captures.from as i64));
        attrs.insert("to".to_string(), Value::Int(captures.to as i64));
        let positional: Vec<Value> = captures
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
            .collect();
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
        let match_obj = Value::make_instance("Match".to_string(), attrs);
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

    /// Get the match continuation position from `$/.to`, defaulting to 0.
    fn get_match_to_position(&self) -> usize {
        if let Some(Value::Instance { attributes, .. }) = self.env.get("/")
            && let Some(Value::Int(to)) = attributes.get("to")
        {
            return *to as usize;
        }
        0
    }

    fn regex_match_with_captures_p5(&self, pattern: &str, text: &str) -> Option<RegexCaptures> {
        let re = self.compile_p5_regex(pattern)?;
        let captures = re.captures(text).ok()??;
        let m0 = captures.get(0)?;
        let names: Vec<Option<&str>> = re.capture_names().collect();
        let mut out = RegexCaptures {
            matched: m0.as_str().to_string(),
            from: m0.start(),
            to: m0.end(),
            ..RegexCaptures::default()
        };
        for idx in 1..captures.len() {
            if names.get(idx).is_some_and(|n| n.is_none()) {
                if let Some(m) = captures.get(idx) {
                    out.positional.push(m.as_str().to_string());
                    out.positional_offsets.push((m.start(), m.end()));
                    out.positional_slots
                        .push(Some((m.as_str().to_string(), m.start(), m.end())));
                } else {
                    out.positional_slots.push(None);
                }
                continue;
            }
            if let (Some(Some(name)), Some(m)) = (names.get(idx), captures.get(idx)) {
                out.named
                    .entry((*name).to_string())
                    .or_default()
                    .push(m.as_str().to_string());
            }
        }
        Some(out)
    }

    fn regex_match_all_with_captures_p5(&self, pattern: &str, text: &str) -> Vec<RegexCaptures> {
        let Some(re) = self.compile_p5_regex(pattern) else {
            return Vec::new();
        };
        let names: Vec<Option<&str>> = re.capture_names().collect();
        let mut out = Vec::new();
        let mut start = 0;
        while start <= text.len() {
            let Ok(Some(captures)) = re.captures_from_pos(text, start) else {
                break;
            };
            let Some(m0) = captures.get(0) else {
                break;
            };
            let mut item = RegexCaptures {
                matched: m0.as_str().to_string(),
                from: m0.start(),
                to: m0.end(),
                ..RegexCaptures::default()
            };
            for idx in 1..captures.len() {
                if names.get(idx).is_some_and(|n| n.is_none()) {
                    if let Some(m) = captures.get(idx) {
                        item.positional.push(m.as_str().to_string());
                        item.positional_offsets.push((m.start(), m.end()));
                        item.positional_slots.push(Some((
                            m.as_str().to_string(),
                            m.start(),
                            m.end(),
                        )));
                    } else {
                        item.positional_slots.push(None);
                    }
                    continue;
                }
                if let (Some(Some(name)), Some(m)) = (names.get(idx), captures.get(idx)) {
                    item.named
                        .entry((*name).to_string())
                        .or_default()
                        .push(m.as_str().to_string());
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

    /// Extract the regex pattern string from a named token/regex definition.
    /// Returns `Some(pattern)` if the token body contains a single regex literal.
    fn extract_token_regex_pattern(&self, name: &str) -> Option<String> {
        let defs = self.resolve_token_defs(name)?;
        let def = defs.first()?;
        // Look for a body consisting of a single Expr(Literal(Regex(pat)))
        if def.body.len() == 1
            && let Stmt::Expr(Expr::Literal(Value::Regex(pat))) = &def.body[0]
        {
            return Some(pat.clone());
        }
        None
    }

    pub(super) fn smart_match(&mut self, left: &Value, right: &Value) -> bool {
        match (left, right) {
            // Whatever on RHS always matches (ACCEPTS returns True for any value)
            (_, Value::Whatever) => true,
            (Value::Version { .. }, Value::Version { parts, plus, minus }) => {
                Self::version_smart_match(left, parts, *plus, *minus)
            }
            // When RHS is a callable (Sub), invoke it with LHS as argument and
            // return truthiness of the result.  If the sub accepts no parameters,
            // call it with no arguments (simple closure truth).
            (_, Value::Sub(data)) => {
                let func = right.clone();
                let _ = data; // keep pattern match shape explicit for callable RHS
                if let Ok(result) = self.call_sub_value(func.clone(), vec![left.clone()], false) {
                    return result.truthy();
                }
                match self.call_sub_value(func, vec![], false) {
                    Ok(result) => result.truthy(),
                    Err(_) => false,
                }
            }
            // Smartmatch against a flip-flop matcher object produced by ff/fff
            // in SmartMatchExpr RHS context.
            (_, Value::Hash(map))
                if matches!(map.get("__mutsu_ff_matcher"), Some(Value::Bool(true))) =>
            {
                let key = map
                    .get("key")
                    .map(Value::to_string_value)
                    .unwrap_or_else(|| "__mutsu_ff_matcher::default".to_string());
                let lhs_pat = map.get("lhs").cloned().unwrap_or(Value::Nil);
                let rhs_pat = map.get("rhs").cloned().unwrap_or(Value::Nil);
                let exclude_start = matches!(map.get("exclude_start"), Some(Value::Bool(true)));
                let exclude_end = matches!(map.get("exclude_end"), Some(Value::Bool(true)));
                let is_fff = matches!(map.get("is_fff"), Some(Value::Bool(true)));

                let seq = self
                    .get_state_var(&key)
                    .and_then(|v| match v {
                        Value::Int(i) if *i > 0 => Some(*i),
                        _ => None,
                    })
                    .unwrap_or(0);

                let (lhs_hit, rhs_hit) = if seq > 0 {
                    (false, self.smart_match(left, &rhs_pat))
                } else {
                    let lhs_match = self.smart_match(left, &lhs_pat);
                    if !lhs_match {
                        (false, false)
                    } else if is_fff {
                        (true, false)
                    } else {
                        (true, self.smart_match(left, &rhs_pat))
                    }
                };

                let out = if seq > 0 {
                    let current = seq;
                    if rhs_hit {
                        self.set_state_var(key.clone(), Value::Int(0));
                        if exclude_end {
                            Value::Nil
                        } else {
                            Value::Int(current)
                        }
                    } else {
                        self.set_state_var(key.clone(), Value::Int(current + 1));
                        Value::Int(current)
                    }
                } else if lhs_hit {
                    if !is_fff && rhs_hit {
                        self.set_state_var(key.clone(), Value::Int(0));
                        if exclude_start || exclude_end {
                            Value::Nil
                        } else {
                            Value::Int(1)
                        }
                    } else {
                        self.set_state_var(key.clone(), Value::Int(2));
                        if exclude_start {
                            Value::Nil
                        } else {
                            Value::Int(1)
                        }
                    }
                } else {
                    Value::Nil
                };
                out.truthy()
            }
            // Named regex/token used as smartmatch RHS — perform regex match
            (
                _,
                Value::Routine {
                    is_regex: true,
                    name,
                    package,
                },
            ) => {
                // Look up the token def and extract the regex pattern from its body
                let qualified = format!("{}::{}", package, name);
                if let Some(pat) = self
                    .extract_token_regex_pattern(&qualified)
                    .or_else(|| self.extract_token_regex_pattern(name))
                {
                    let text = left.to_string_value();
                    if let Some(captures) = self.regex_match_with_captures(&pat, &text) {
                        let match_obj = Value::make_match_object_with_captures(
                            captures.matched.clone(),
                            captures.from as i64,
                            captures.to as i64,
                            &captures.positional,
                            &captures.named,
                        );
                        self.env.insert("/".to_string(), match_obj);
                        for (i, v) in captures.positional.iter().enumerate() {
                            self.env.insert(i.to_string(), Value::Str(v.clone()));
                        }
                        for (k, v) in &captures.named {
                            let value = if v.len() == 1 {
                                Value::Str(v[0].clone())
                            } else {
                                Value::array(v.iter().cloned().map(Value::Str).collect())
                            };
                            self.env.insert(format!("<{}>", k), value);
                        }
                        return true;
                    }
                    self.env.insert("/".to_string(), Value::Nil);
                    return false;
                }
                // Fallback: call as sub
                let func = right.clone();
                match self.call_sub_value(func, vec![left.clone()], false) {
                    Ok(result) => result.truthy(),
                    Err(_) => false,
                }
            }
            // Built-in routines used as callables in smartmatch
            (_, Value::Routine { .. }) => {
                let func = right.clone();
                match self.call_sub_value(func, vec![left.clone()], false) {
                    Ok(result) => result.truthy(),
                    Err(_) => false,
                }
            }
            // :pos/:p anchored match (non-exhaustive) — match at $/.to (or 0)
            (
                _,
                Value::RegexWithAdverbs {
                    pattern: pat,
                    pos: true,
                    exhaustive: false,
                    ..
                },
            ) => {
                let text = left.to_string_value();
                let start_pos = self.get_match_to_position();
                if let Some(captures) = self.regex_match_with_captures_at(pat, &text, start_pos) {
                    self.apply_single_regex_captures(&captures);
                    return true;
                }
                self.env.insert("/".to_string(), Value::Nil);
                false
            }
            (_, Value::Regex(pat))
            | (
                _,
                Value::RegexWithAdverbs {
                    pattern: pat,
                    exhaustive: false,
                    perl5: false,
                    ..
                },
            ) => {
                let text = left.to_string_value();
                if let Some(captures) = self.regex_match_with_captures(pat, &text) {
                    for (i, v) in captures.positional.iter().enumerate() {
                        self.env.insert(i.to_string(), Value::Str(v.clone()));
                    }
                    // Execute code blocks from regex for side effects
                    self.execute_regex_code_blocks(&captures.code_blocks);
                    let match_obj = Value::make_match_object_full(
                        captures.matched.clone(),
                        captures.from as i64,
                        captures.to as i64,
                        &captures.positional,
                        &captures.named,
                        &captures.named_subcaps,
                        Some(&text),
                    );
                    // Set named capture env vars from the match object's named hash
                    // so subcapture-aware Match objects are used (not plain strings)
                    if let Value::Instance { ref attributes, .. } = match_obj
                        && let Some(Value::Hash(named_hash)) = attributes.get("named")
                    {
                        for (k, v) in named_hash.iter() {
                            self.env.insert(format!("<{}>", k), v.clone());
                        }
                    }
                    self.env.insert("/".to_string(), match_obj);
                    return true;
                }
                self.env.insert("/".to_string(), Value::Nil);
                false
            }
            (
                _,
                Value::RegexWithAdverbs {
                    pattern: pat,
                    exhaustive: false,
                    perl5: true,
                    ..
                },
            ) => {
                let text = left.to_string_value();
                if let Some(captures) = self.regex_match_with_captures_p5(pat, &text) {
                    self.apply_single_regex_captures(&captures);
                    return true;
                }
                self.env.insert("/".to_string(), Value::Nil);
                false
            }
            (
                _,
                Value::RegexWithAdverbs {
                    pattern,
                    exhaustive: true,
                    repeat,
                    perl5,
                    ..
                },
            ) => {
                let text = left.to_string_value();
                let mut all = if *perl5 {
                    self.regex_match_all_with_captures_p5(pattern, &text)
                } else {
                    self.regex_match_all_with_captures(pattern, &text)
                };
                if all.is_empty() {
                    self.env.insert("/".to_string(), Value::Nil);
                    return false;
                }
                let selected = if let Some(needed) = *repeat {
                    let earliest = all.iter().map(|c| c.from).min().unwrap_or(0);
                    all.retain(|c| c.from == earliest);
                    if all.len() < needed {
                        self.env.insert("/".to_string(), Value::Nil);
                        return false;
                    }
                    all.into_iter().take(needed).collect::<Vec<_>>()
                } else {
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
                    best_by_start.into_values().collect::<Vec<_>>()
                };
                if selected.is_empty() {
                    self.env.insert("/".to_string(), Value::Nil);
                    return false;
                }
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
                        self.env.insert(i.to_string(), Value::Str(cap.clone()));
                    }
                }
                true
            }
            (_, Value::Junction { kind, values }) => match kind {
                crate::value::JunctionKind::Any => values.iter().any(|v| self.smart_match(left, v)),
                crate::value::JunctionKind::All => values.iter().all(|v| self.smart_match(left, v)),
                crate::value::JunctionKind::One => {
                    values.iter().filter(|v| self.smart_match(left, v)).count() == 1
                }
                crate::value::JunctionKind::None => {
                    values.iter().all(|v| !self.smart_match(left, v))
                }
            },
            // IO::Path/Str ~~ Pair(:e), :d, :f, :r, :w, :x file tests
            (_, Value::Pair(key, val))
                if matches!(val.as_ref(), Value::Bool(true))
                    && matches!(key.as_str(), "e" | "d" | "f" | "r" | "w" | "x" | "s" | "z") =>
            {
                let path_str = match left {
                    Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } if class_name == "IO::Path" => {
                        attributes.get("path").map(|v| v.to_string_value())
                    }
                    Value::Str(s) => Some(s.clone()),
                    _ => None,
                };
                if let Some(p) = path_str {
                    let path = std::path::Path::new(&p);
                    match key.as_str() {
                        "e" => path.exists(),
                        "d" => path.is_dir(),
                        "f" => path.is_file(),
                        "r" => path.exists(), // simplified: exists = readable
                        "w" => path.exists(), // simplified
                        "x" => {
                            #[cfg(unix)]
                            {
                                use std::os::unix::fs::PermissionsExt;
                                std::fs::metadata(&p)
                                    .map(|m| m.permissions().mode() & 0o111 != 0)
                                    .unwrap_or(false)
                            }
                            #[cfg(not(unix))]
                            {
                                false
                            }
                        }
                        "s" => std::fs::metadata(&p).map(|m| m.len() > 0).unwrap_or(false),
                        "z" => std::fs::metadata(&p).map(|m| m.len() == 0).unwrap_or(false),
                        _ => false,
                    }
                } else {
                    false
                }
            }
            // Hash ~~ Pair: check that key exists in hash and value smartmatches
            (Value::Hash(map), Value::Pair(key, val)) => {
                if let Some(hash_val) = map.get(key.as_str()) {
                    self.smart_match(hash_val, val)
                } else {
                    // Key not in hash: compare against an undefined type object.
                    self.smart_match(&Value::Package("Mu".to_string()), val)
                }
            }
            // Hash ~~ Hash: structural equality (eqv)
            (Value::Hash(lmap), Value::Hash(rmap)) => {
                if lmap.len() != rmap.len() {
                    return false;
                }
                for (k, lv) in lmap.iter() {
                    match rmap.get(k) {
                        Some(rv) => {
                            if !self.smart_match(lv, rv) {
                                return false;
                            }
                        }
                        None => return false,
                    }
                }
                true
            }
            // Array ~~ Hash: check if any element exists as a key in the hash
            (Value::Array(items, ..), Value::Hash(map)) => items.iter().any(|item| {
                let key = item.to_string_value();
                map.contains_key(&key)
            }),
            // Regex ~~ Hash: check if any key matches the regex
            (
                Value::Regex(pat) | Value::RegexWithAdverbs { pattern: pat, .. },
                Value::Hash(map),
            ) => {
                for key in map.keys() {
                    if self.regex_find_first(pat, key).is_some() {
                        return true;
                    }
                }
                false
            }
            // Scalar ~~ Hash: check key existence
            (_, Value::Hash(map)) => {
                let key = left.to_string_value();
                map.contains_key(&key)
            }
            // List/Array ~~ List/Array: element-wise smartmatch with ** support
            (_, r) if Self::is_list_like(r) => {
                // Non-iterable LHS: return False (don't treat scalars as a list)
                if !Self::is_iterable(left) {
                    return false;
                }
                // Lazy LHS or lazy RHS: return False (unless same object, handled by PartialEq)
                if Self::is_lazy(left) || Self::is_lazy(right) {
                    return Self::same_object(left, right);
                }
                let lhs = Self::extract_list_items(left);
                let rhs = Self::extract_list_items(right);
                self.list_smartmatch(&lhs, &rhs)
            }
            // Parametric role smartmatch: R1[C2] ~~ R1[C1] (subtyping)
            (
                Value::ParametricRole {
                    base_name: lhs_base,
                    type_args: lhs_args,
                },
                Value::ParametricRole {
                    base_name: rhs_base,
                    type_args: rhs_args,
                },
            ) => {
                if lhs_args.len() != rhs_args.len() {
                    return false;
                }
                if lhs_base != rhs_base && !self.role_is_subtype(lhs_base, rhs_base) {
                    return false;
                }
                // Each LHS type arg must be a subtype of (or equal to) the corresponding RHS type arg
                for (l_arg, r_arg) in lhs_args.iter().zip(rhs_args.iter()) {
                    if !self.parametric_arg_subtypes(l_arg, r_arg) {
                        return false;
                    }
                }
                true
            }
            // Parametric role ~~ base role: R1[C1] ~~ R1
            (
                Value::ParametricRole {
                    base_name: lhs_base,
                    ..
                },
                Value::Package(rhs_name),
            ) => lhs_base == rhs_name,
            // Value instance/mixin ~~ parametric role: check composed role + type arguments.
            (
                left_value,
                Value::ParametricRole {
                    base_name: rhs_base,
                    type_args: rhs_args,
                },
            ) => {
                if !left_value.does_check(rhs_base) {
                    return false;
                }
                let lhs_args = if let Value::Mixin(_, mixins) = left_value {
                    match mixins.get(&format!("__mutsu_role_typeargs__{}", rhs_base)) {
                        Some(Value::Array(items, ..)) => Some(items.as_ref().clone()),
                        _ => None,
                    }
                } else {
                    None
                };
                let Some(lhs_args) = lhs_args else {
                    return false;
                };
                if lhs_args.len() != rhs_args.len() {
                    return false;
                }
                lhs_args
                    .iter()
                    .zip(rhs_args.iter())
                    .all(|(lhs, rhs)| self.parametric_arg_subtypes(lhs, rhs))
            }
            // When RHS is a CustomType, use Raku type checking protocol
            (_, Value::CustomType { id, how, .. }) => self.custom_type_check(left, *id, how),
            // When LHS is a CustomType (type object), check type cache or HOW.type_check
            (Value::CustomType { how, id, .. }, _) => {
                if let Value::Package(_) = right {
                    // After compose: check the type check cache
                    let data = self.custom_type_data.get(id).cloned();
                    if let Some(ref data) = data
                        && let Some(ref cache) = data.type_check_cache
                    {
                        // Check if the RHS type is in our cache
                        for cached_type in cache {
                            if right == cached_type {
                                return true;
                            }
                        }
                        if data.authoritative {
                            return false;
                        }
                    }
                    // Before compose or no cache: call HOW.type_check
                    if let Ok(result) = self.call_method_with_values(
                        *how.clone(),
                        "type_check",
                        vec![Value::Nil, right.clone()],
                    ) {
                        result.truthy()
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            // When RHS is a type/Package, check type membership
            (_, Value::Package(type_name)) => {
                // Handle type smileys (:U, :D, :_)
                let (base_type, smiley) = super::types::strip_type_smiley(type_name);

                // A Package on the LHS is a type object - check type hierarchy
                if let Value::Package(_) = left {
                    let type_ok = self.type_matches_value(base_type, left);
                    if !type_ok {
                        return false;
                    }
                    // Check definedness constraint
                    return match smiley {
                        Some(":U") => true,  // Package is undefined
                        Some(":D") => false, // Package is not defined
                        _ => true,
                    };
                }
                self.type_matches_value(type_name, left)
            }
            // Enum type objects are currently represented as Str values in env.
            (_, Value::Str(type_name)) if self.enum_types.contains_key(type_name) => {
                matches!(left, Value::Enum { enum_type, .. } if enum_type == type_name)
            }
            // Mu instances smartmatch only the Mu type object (Mu ~~ Mu.new is True).
            (
                Value::Package(lhs_type),
                Value::Instance {
                    class_name: rhs_class,
                    ..
                },
            ) if rhs_class == "Mu" => lhs_type == "Mu",
            // When LHS is a type object (Package), only match same type or type hierarchy
            (Value::Package(_), _) => false,
            // When RHS is NaN, check if LHS is also NaN
            (_, Value::Num(b)) if b.is_nan() => Self::value_is_nan(left),
            (Value::Num(a), _) if a.is_nan() => Self::value_is_nan(right),
            // Complex comparison (NaN-aware: any NaN component means NaN smartmatch)
            (Value::Complex(ar, ai), Value::Complex(br, bi)) => {
                let a_nan = ar.is_nan() || ai.is_nan();
                let b_nan = br.is_nan() || bi.is_nan();
                if a_nan && b_nan {
                    true
                } else if a_nan || b_nan {
                    false
                } else {
                    ar == br && ai == bi
                }
            }
            (Value::Int(a), Value::Complex(br, bi)) => (*a as f64) == *br && *bi == 0.0,
            (Value::Complex(ar, ai), Value::Int(b)) => *ar == (*b as f64) && *ai == 0.0,
            (Value::Num(a), Value::Complex(br, bi)) => {
                if a.is_nan() && (br.is_nan() || bi.is_nan()) {
                    true
                } else {
                    *a == *br && *bi == 0.0
                }
            }
            (Value::Complex(ar, ai), Value::Num(b)) => {
                if b.is_nan() && (ar.is_nan() || ai.is_nan()) {
                    true
                } else {
                    *ar == *b && *ai == 0.0
                }
            }
            (Value::Complex(ar, ai), Value::Rat(n, d)) => {
                if *d != 0 {
                    *ar == (*n as f64 / *d as f64) && *ai == 0.0
                } else {
                    false
                }
            }
            (Value::Rat(n, d), Value::Complex(br, bi)) => {
                if *d != 0 {
                    (*n as f64 / *d as f64) == *br && *bi == 0.0
                } else {
                    false
                }
            }
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Num(a), Value::Num(b)) => a == b,
            (Value::Int(a), Value::Num(b)) => (*a as f64) == *b,
            (Value::Num(a), Value::Int(b)) => *a == (*b as f64),
            (Value::Rat(an, ad), Value::Rat(bn, bd)) => an * bd == bn * ad,
            (Value::Int(a), Value::Rat(n, d)) => *a * d == *n,
            (Value::Rat(n, d), Value::Int(b)) => *n == *b * d,
            (Value::Str(a), Value::Str(b)) => a == b,
            // Str ~~ Numeric: numify LHS and compare
            (Value::Str(a), Value::Int(b)) => a.trim().parse::<f64>() == Ok(*b as f64),
            (Value::Str(a), Value::Num(b)) => a.trim().parse::<f64>().is_ok_and(|v| {
                if v.is_nan() && b.is_nan() {
                    true
                } else {
                    v == *b
                }
            }),
            (Value::Str(a), Value::Rat(n, d)) => {
                if *d != 0 {
                    a.trim()
                        .parse::<f64>()
                        .is_ok_and(|v| v == *n as f64 / *d as f64)
                } else {
                    false
                }
            }
            (Value::Int(a), Value::Str(b)) => b.trim().parse::<f64>() == Ok(*a as f64),
            (Value::Nil, Value::Str(s)) => s.is_empty(),
            // IO::Path ~~ IO::Path: compare by path string value
            (
                Value::Instance {
                    class_name: cn_a,
                    attributes: attrs_a,
                    ..
                },
                Value::Instance {
                    class_name: cn_b,
                    attributes: attrs_b,
                    ..
                },
            ) if cn_a == "IO::Path" && cn_b == "IO::Path" => {
                let path_a = attrs_a.get("path").map(|v| v.to_string_value());
                let path_b = attrs_b.get("path").map(|v| v.to_string_value());
                path_a == path_b
            }
            // Signature ~~ Signature: s1 ACCEPTS s2
            (
                Value::Instance {
                    class_name: cn_a,
                    id: id_a,
                    ..
                },
                Value::Instance {
                    class_name: cn_b,
                    id: id_b,
                    ..
                },
            ) if cn_a == "Signature" && cn_b == "Signature" => {
                if let (Some(info_a), Some(info_b)) =
                    (extract_sig_info(left), extract_sig_info(right))
                {
                    signature_smartmatch(&info_b, &info_a)
                } else {
                    id_a == id_b
                }
            }
            // Value ~~ Signature: signature ACCEPTS value
            (_, Value::Instance { class_name: cn, .. }) if cn == "Signature" => {
                if let Some(info) = extract_sig_info(right) {
                    self.signature_accepts_value(left, &info)
                } else {
                    false
                }
            }
            // Instance identity: two instances match iff they have the same id
            (Value::Instance { id: id_a, .. }, Value::Instance { id: id_b, .. }) => id_a == id_b,
            // When RHS is a Bool, result is that Bool
            (_, Value::Bool(b)) => *b,
            // Instance identity
            (Value::Instance { .. }, _) | (_, Value::Instance { .. }) => false,
            // Range ~~ Range: LHS is subset of RHS.
            // Uses raw bound values. Exclusivity is compared pairwise:
            // if RHS excludes a bound, LHS must either also exclude it
            // or have a strictly interior value.
            (l, r) if l.is_range() && r.is_range() => {
                let r_str = Self::range_has_string_endpoints(r);
                let (_, _, l_es, l_ee) = Self::range_exclusivity(l);
                let (_, _, r_es, r_ee) = Self::range_exclusivity(r);
                if r_str {
                    let (l_min_s, l_max_s) = Self::range_raw_string_bounds(l);
                    let (r_min_s, r_max_s) = Self::range_raw_string_bounds(r);
                    let min_ok = if r_es {
                        l_min_s > r_min_s || (l_min_s == r_min_s && l_es)
                    } else {
                        l_min_s >= r_min_s
                    };
                    let max_ok = if r_ee {
                        l_max_s < r_max_s || (l_max_s == r_max_s && l_ee)
                    } else {
                        l_max_s <= r_max_s
                    };
                    min_ok && max_ok
                } else {
                    let (l_min, l_max) = Self::range_raw_bounds_f64(l);
                    let (r_min, r_max) = Self::range_raw_bounds_f64(r);
                    let min_ok = if r_es {
                        l_min > r_min || (l_min == r_min && l_es)
                    } else {
                        l_min >= r_min
                            || (l_min.is_nan() && r_min.is_nan())
                            || (l_min.is_nan() && r_min.is_infinite() && r_min < 0.0)
                    };
                    let max_ok = if r_ee {
                        l_max < r_max || (l_max == r_max && l_ee)
                    } else {
                        l_max <= r_max
                            || (l_max.is_nan() && r_max.is_nan())
                            || (l_max.is_nan() && r_max.is_infinite() && r_max > 0.0)
                    };
                    min_ok && max_ok
                }
            }
            // Range ~~ Numeric: numify range (element count) and compare with ==
            (l, r) if l.is_range() && r.is_numeric() => {
                let elems = Self::range_elems_f64(l);
                let rval = r.to_f64();
                elems == rval
            }
            // Value ~~ Range: check if value is contained in the range
            (l, r) if r.is_range() => Self::value_in_range(l, r),
            // Default: compare equality
            _ => left.to_string_value() == right.to_string_value(),
        }
    }

    fn role_is_subtype(&self, lhs_role: &str, rhs_role: &str) -> bool {
        if lhs_role == rhs_role {
            return true;
        }
        let mut stack = vec![lhs_role.to_string()];
        let mut seen = HashSet::new();
        while let Some(role) = stack.pop() {
            if !seen.insert(role.clone()) {
                continue;
            }
            if let Some(parents) = self.role_parents.get(&role) {
                for parent in parents {
                    if parent == rhs_role {
                        return true;
                    }
                    stack.push(parent.clone());
                }
            }
        }
        false
    }

    /// Check if `lhs` type arg is a subtype of `rhs` type arg for parametric role subtyping.
    /// E.g., Package("C2") subtypes Package("C1") if C2 isa C1.
    fn parametric_arg_subtypes(&self, lhs: &Value, rhs: &Value) -> bool {
        match (lhs, rhs) {
            // Both are packages (type objects): check class hierarchy
            (Value::Package(l_name), Value::Package(r_name)) => {
                if l_name == r_name {
                    return true;
                }
                // Built-in type hierarchy relationships (e.g. Int <: Cool <: Any)
                if Self::type_matches(r_name, l_name) {
                    return true;
                }
                // Check if l_name is a subclass of r_name
                if let Some(class_def) = self.classes.get(l_name.as_str()) {
                    if class_def.parents.iter().any(|p| p == r_name) {
                        return true;
                    }
                    // Transitive: check if any parent subtypes r_name
                    for parent in &class_def.parents {
                        if self.parametric_arg_subtypes(&Value::Package(parent.clone()), rhs) {
                            return true;
                        }
                    }
                }
                false
            }
            // Both are parametric roles: recursively check
            (
                Value::ParametricRole {
                    base_name: lb,
                    type_args: la,
                },
                Value::ParametricRole {
                    base_name: rb,
                    type_args: ra,
                },
            ) => {
                if lb != rb || la.len() != ra.len() {
                    return false;
                }
                la.iter()
                    .zip(ra.iter())
                    .all(|(l, r)| self.parametric_arg_subtypes(l, r))
            }
            _ => lhs == rhs,
        }
    }

    /// Get raw bounds of a range as f64 (NOT adjusted for exclusivity).
    fn range_raw_bounds_f64(v: &Value) -> (f64, f64) {
        match v {
            Value::Range(a, b) => (*a as f64, *b as f64),
            Value::RangeExcl(a, b) => (*a as f64, *b as f64),
            Value::RangeExclStart(a, b) => (*a as f64, *b as f64),
            Value::RangeExclBoth(a, b) => (*a as f64, *b as f64),
            Value::GenericRange { start, end, .. } => (start.to_f64(), end.to_f64()),
            _ => (0.0, 0.0),
        }
    }

    /// Get exclusivity flags for a range: (start_val, end_val, excl_start, excl_end).
    fn range_exclusivity(v: &Value) -> (f64, f64, bool, bool) {
        match v {
            Value::Range(a, b) => (*a as f64, *b as f64, false, false),
            Value::RangeExcl(a, b) => (*a as f64, *b as f64, false, true),
            Value::RangeExclStart(a, b) => (*a as f64, *b as f64, true, false),
            Value::RangeExclBoth(a, b) => (*a as f64, *b as f64, true, true),
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => (start.to_f64(), end.to_f64(), *excl_start, *excl_end),
            _ => (0.0, 0.0, false, false),
        }
    }

    /// Check if a range has string endpoints.
    fn range_has_string_endpoints(v: &Value) -> bool {
        match v {
            Value::GenericRange { start, end, .. } => {
                matches!(**start, Value::Str(_)) || matches!(**end, Value::Str(_))
            }
            _ => false,
        }
    }

    /// Get raw string bounds of a range.
    fn range_raw_string_bounds(v: &Value) -> (String, String) {
        match v {
            Value::GenericRange { start, end, .. } => {
                (start.to_string_value(), end.to_string_value())
            }
            Value::Range(a, b) => (a.to_string(), b.to_string()),
            Value::RangeExcl(a, b) => (a.to_string(), b.to_string()),
            Value::RangeExclStart(a, b) => (a.to_string(), b.to_string()),
            Value::RangeExclBoth(a, b) => (a.to_string(), b.to_string()),
            _ => (String::new(), String::new()),
        }
    }

    /// Compute element count of a range as f64.
    fn range_elems_f64(v: &Value) -> f64 {
        match v {
            Value::Range(a, b) => {
                if *b == i64::MAX || *a == i64::MIN {
                    f64::INFINITY
                } else {
                    (*b - *a + 1) as f64
                }
            }
            Value::RangeExcl(a, b) => {
                if *b == i64::MAX || *a == i64::MIN {
                    f64::INFINITY
                } else {
                    (*b - *a) as f64
                }
            }
            Value::RangeExclStart(a, b) => {
                if *b == i64::MAX || *a == i64::MIN {
                    f64::INFINITY
                } else {
                    (*b - *a) as f64
                }
            }
            Value::RangeExclBoth(a, b) => {
                if *b == i64::MAX || *a == i64::MIN {
                    f64::INFINITY
                } else {
                    (*b - *a - 1) as f64
                }
            }
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                let s = start.to_f64();
                let e = end.to_f64();
                let count = e - s + 1.0;
                let adj = if *excl_start { 1.0 } else { 0.0 } + if *excl_end { 1.0 } else { 0.0 };
                count - adj
            }
            _ => 0.0,
        }
    }

    /// Check if a value is contained within a range.
    fn value_in_range(val: &Value, range: &Value) -> bool {
        let (r_min, r_max) = Self::range_raw_bounds_f64(range);
        let (_, _, r_es, r_ee) = Self::range_exclusivity(range);

        // For string ranges, compare strings
        if Self::range_has_string_endpoints(range) {
            let v_str = val.to_string_value();
            let (r_min_s, r_max_s) = Self::range_raw_string_bounds(range);
            let min_ok = if r_es {
                v_str > r_min_s
            } else {
                v_str >= r_min_s
            };
            let max_ok = if r_ee {
                v_str < r_max_s
            } else {
                v_str <= r_max_s
            };
            return min_ok && max_ok;
        }

        let v = val.to_f64();
        let min_ok = if r_es { v > r_min } else { v >= r_min };
        let max_ok = if r_ee { v < r_max } else { v <= r_max };
        min_ok && max_ok
    }

    pub(super) fn seq_value_to_f64(v: &Value) -> Option<f64> {
        match v {
            Value::Int(i) => Some(*i as f64),
            Value::Num(f) => Some(*f),
            Value::Rat(n, d) => {
                if *d != 0 {
                    Some(*n as f64 / *d as f64)
                } else {
                    None
                }
            }
            Value::Bool(b) => Some(if *b { 1.0 } else { 0.0 }),
            _ => None,
        }
    }

    /// Check if a value matches a type name for sequence type endpoints.
    /// Handles the Num→Rat mapping since mutsu uses Num for decimal values.
    pub(super) fn seq_type_matches(val: &Value, type_name: &str) -> bool {
        let actual = super::value_type_name(val);
        if actual == type_name {
            return true;
        }
        // Special case: Num values with non-integer values match "Rat"
        if type_name == "Rat"
            && let Value::Num(f) = val
        {
            return *f != f.floor(); // non-integer Num matches Rat
        }
        false
    }

    pub(super) fn seq_values_equal(a: &Value, b: &Value) -> bool {
        // Junction endpoint: check if a matches any junction value
        if let Value::Junction { values, .. } = b {
            return values.iter().any(|v| Self::seq_values_equal(a, v));
        }
        if let Value::Junction { values, .. } = a {
            return values.iter().any(|v| Self::seq_values_equal(v, b));
        }
        match (a, b) {
            (Value::Int(x), Value::Int(y)) => x == y,
            (Value::Str(x), Value::Str(y)) => x == y,
            (Value::Bool(x), Value::Bool(y)) => x == y,
            _ => {
                if let (Some(fa), Some(fb)) = (Self::seq_value_to_f64(a), Self::seq_value_to_f64(b))
                {
                    (fa - fb).abs() < 1e-12
                } else {
                    false
                }
            }
        }
    }

    // Compute the successor of a string (increment the last character, carrying over)
    pub(super) fn string_succ(s: &str) -> String {
        if s.is_empty() {
            return String::new();
        }
        let mut chars: Vec<char> = s.chars().collect();
        // Carry-over increment from the last character
        let mut carry = true;
        for ch in chars.iter_mut().rev() {
            if !carry {
                break;
            }
            if ch.is_ascii_lowercase() {
                if *ch == 'z' {
                    *ch = 'a';
                } else {
                    *ch = (*ch as u8 + 1) as char;
                    carry = false;
                }
            } else if ch.is_ascii_uppercase() {
                if *ch == 'Z' {
                    *ch = 'A';
                } else {
                    *ch = (*ch as u8 + 1) as char;
                    carry = false;
                }
            } else if ch.is_ascii_digit() {
                if *ch == '9' {
                    *ch = '0';
                } else {
                    *ch = (*ch as u8 + 1) as char;
                    carry = false;
                }
            } else {
                *ch = char::from_u32(*ch as u32 + 1).unwrap_or(*ch);
                carry = false;
            }
        }
        if carry {
            // All characters carried over, prepend appropriate char
            let first = chars[0];
            let prefix = if first.is_ascii_lowercase() {
                'a'
            } else if first.is_ascii_uppercase() {
                'A'
            } else if first.is_ascii_digit() {
                '1'
            } else {
                first
            };
            chars.insert(0, prefix);
        }
        chars.into_iter().collect()
    }

    pub(super) fn digit_string_succ_radix(s: &str, radix: u32) -> String {
        if s.is_empty() || !(2..=10).contains(&radix) {
            return s.to_string();
        }
        let mut chars: Vec<char> = s.chars().collect();
        let mut carry = true;
        for ch in chars.iter_mut().rev() {
            if !carry {
                break;
            }
            if !ch.is_ascii_digit() {
                return Self::string_succ(s);
            }
            let mut digit = (*ch as u8 - b'0') as u32;
            if digit + 1 >= radix {
                digit = 0;
                *ch = (b'0' + digit as u8) as char;
            } else {
                digit += 1;
                *ch = (b'0' + digit as u8) as char;
                carry = false;
            }
        }
        if carry {
            chars.insert(0, '1');
        }
        chars.into_iter().collect()
    }

    pub(super) fn digit_string_pred_radix(s: &str, radix: u32) -> Result<String, RuntimeError> {
        if s.is_empty() || !(2..=10).contains(&radix) {
            return Self::string_pred(s);
        }
        let mut chars: Vec<char> = s.chars().collect();
        let mut borrow = true;
        for ch in chars.iter_mut().rev() {
            if !borrow {
                break;
            }
            if !ch.is_ascii_digit() {
                return Self::string_pred(s);
            }
            let mut digit = (*ch as u8 - b'0') as u32;
            if digit == 0 {
                digit = radix - 1;
                *ch = (b'0' + digit as u8) as char;
            } else {
                digit -= 1;
                *ch = (b'0' + digit as u8) as char;
                borrow = false;
            }
        }
        if borrow {
            return Err(RuntimeError::new("Decrement out of range"));
        }
        Ok(chars.into_iter().collect())
    }

    // Compute the predecessor of a string (decrement the last character).
    // For multi-char alphabetic/digit strings, the leftmost character is never
    // removed; underflow (e.g. "AA".pred) is an error.
    pub(super) fn string_pred(s: &str) -> Result<String, RuntimeError> {
        if s.is_empty() {
            return Ok(String::new());
        }
        let mut chars: Vec<char> = s.chars().collect();
        // For single-char strings, just decrement the codepoint
        if chars.len() == 1 {
            let ch = chars[0];
            if let Some(prev) = char::from_u32(ch as u32 - 1) {
                return Ok(prev.to_string());
            }
            return Ok(s.to_string());
        }
        // Multi-char: decrement from end with borrow
        let mut borrow = true;
        for ch in chars.iter_mut().rev() {
            if !borrow {
                break;
            }
            if ch.is_ascii_lowercase() {
                if *ch == 'a' {
                    *ch = 'z';
                } else {
                    *ch = (*ch as u8 - 1) as char;
                    borrow = false;
                }
            } else if ch.is_ascii_uppercase() {
                if *ch == 'A' {
                    *ch = 'Z';
                } else {
                    *ch = (*ch as u8 - 1) as char;
                    borrow = false;
                }
            } else if ch.is_ascii_digit() {
                if *ch == '0' {
                    *ch = '9';
                } else {
                    *ch = (*ch as u8 - 1) as char;
                    borrow = false;
                }
            } else {
                if let Some(prev) = char::from_u32(*ch as u32 - 1) {
                    *ch = prev;
                }
                borrow = false;
            }
        }
        if borrow && chars.len() > 1 {
            return Err(RuntimeError::new("Decrement out of range"));
        }
        Ok(chars.into_iter().collect())
    }

    // Add step to a sequence value, preserving type where possible
    pub(super) fn seq_add(val: &Value, step: f64) -> Value {
        match val {
            Value::Int(i) => {
                if step == step.floor() && step.abs() < i64::MAX as f64 {
                    Value::Int(*i + step as i64)
                } else {
                    Value::Num(*i as f64 + step)
                }
            }
            Value::Num(f) => Value::Num(*f + step),
            Value::Rat(n, d) => {
                if *d != 0 && step == step.floor() && step.abs() < i64::MAX as f64 {
                    make_rat(*n + step as i64 * *d, *d)
                } else {
                    Value::Num(*n as f64 / *d as f64 + step)
                }
            }
            _ => Value::Num(Self::seq_value_to_f64(val).unwrap_or(0.0) + step),
        }
    }

    // Multiply a sequence value by a rational ratio (num/den), preserving Rat type
    pub(super) fn seq_mul_rat(val: &Value, num: i64, den: i64) -> Value {
        match val {
            Value::Int(i) => {
                // In geometric sequences with rational ratio, always produce Rat
                if let Some(product) = i.checked_mul(num) {
                    make_rat(product, den)
                } else {
                    Value::Num(*i as f64 * num as f64 / den as f64)
                }
            }
            Value::Num(f) => {
                let result = *f * num as f64 / den as f64;
                Value::Num(result)
            }
            Value::Rat(n, d) => {
                if let (Some(new_num), Some(new_den)) = (n.checked_mul(num), d.checked_mul(den)) {
                    make_rat(new_num, new_den)
                } else {
                    Value::Num(*n as f64 / *d as f64 * num as f64 / den as f64)
                }
            }
            _ => Self::seq_mul(val, num as f64 / den as f64),
        }
    }

    // Multiply a sequence value by ratio, preserving type where possible
    pub(super) fn seq_mul(val: &Value, ratio: f64) -> Value {
        match val {
            Value::Int(i) => {
                let result = *i as f64 * ratio;
                if ratio == ratio.floor() && result.abs() < i64::MAX as f64 {
                    Value::Int(result as i64)
                } else {
                    Value::Num(result)
                }
            }
            Value::Num(f) => Value::Num(*f * ratio),
            Value::Rat(n, d) => {
                if *d != 0 && ratio == ratio.floor() && ratio.abs() < i64::MAX as f64 {
                    make_rat(*n * ratio as i64, *d)
                } else {
                    Value::Num(*n as f64 / *d as f64 * ratio)
                }
            }
            _ => Value::Num(Self::seq_value_to_f64(val).unwrap_or(0.0) * ratio),
        }
    }

    /// Check if a value is list-like (Array, Seq, Slip).
    fn is_list_like(v: &Value) -> bool {
        matches!(
            v,
            Value::Array(..) | Value::Seq(_) | Value::Slip(_) | Value::LazyList(_)
        )
    }

    /// Check if a value is iterable (can be treated as a list in smartmatch).
    fn is_iterable(v: &Value) -> bool {
        matches!(
            v,
            Value::Array(..) | Value::Seq(_) | Value::Slip(_) | Value::LazyList(_)
        ) || v.is_range()
    }

    /// Check if a value is lazy.
    fn is_lazy(v: &Value) -> bool {
        matches!(v, Value::LazyList(_))
    }

    /// Check if two values are the same object (pointer equality).
    fn same_object(a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Array(a, _), Value::Array(b, _)) => Arc::ptr_eq(a, b),
            (Value::Seq(a), Value::Seq(b)) => Arc::ptr_eq(a, b),
            (Value::Slip(a), Value::Slip(b)) => Arc::ptr_eq(a, b),
            (Value::LazyList(a), Value::LazyList(b)) => Arc::ptr_eq(a, b),
            _ => false,
        }
    }

    /// Extract list items from a value, expanding ranges.
    fn extract_list_items(v: &Value) -> Vec<Value> {
        match v {
            Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => {
                items.as_ref().clone()
            }
            r if r.is_range() => Self::value_to_list(r),
            _ => vec![v.clone()],
        }
    }

    /// Perform list smartmatch with ** (HyperWhatever) support.
    /// Each RHS element is smartmatched against the corresponding LHS element.
    /// ** matches 0 or more elements. Consecutive **s are collapsed.
    fn list_smartmatch(&mut self, lhs: &[Value], rhs: &[Value]) -> bool {
        // Collapse consecutive HyperWhatevers in rhs
        let rhs_collapsed: Vec<&Value> = {
            let mut result = Vec::new();
            let mut prev_was_hw = false;
            for v in rhs {
                if matches!(v, Value::HyperWhatever) {
                    if !prev_was_hw {
                        result.push(v);
                    }
                    prev_was_hw = true;
                } else {
                    prev_was_hw = false;
                    result.push(v);
                }
            }
            result
        };
        self.list_smartmatch_recursive(lhs, 0, &rhs_collapsed, 0)
    }

    fn list_smartmatch_recursive(
        &mut self,
        lhs: &[Value],
        li: usize,
        rhs: &[&Value],
        ri: usize,
    ) -> bool {
        // Both exhausted — match
        if li == lhs.len() && ri == rhs.len() {
            return true;
        }
        // RHS exhausted but LHS has more — no match
        if ri == rhs.len() {
            return false;
        }
        // RHS has ** — try matching 0..n elements from LHS
        if matches!(rhs[ri], Value::HyperWhatever) {
            // Try consuming 0, 1, 2, ... elements from LHS
            for skip in 0..=(lhs.len() - li) {
                if self.list_smartmatch_recursive(lhs, li + skip, rhs, ri + 1) {
                    return true;
                }
            }
            return false;
        }
        // LHS exhausted but RHS has more non-** elements — no match
        if li == lhs.len() {
            return false;
        }
        // Match current element using smartmatch
        if self.element_smartmatch(&lhs[li], rhs[ri]) {
            self.list_smartmatch_recursive(lhs, li + 1, rhs, ri + 1)
        } else {
            false
        }
    }

    /// Smartmatch a single element: delegate to full smartmatch.
    fn element_smartmatch(&mut self, lhs_elem: &Value, rhs_elem: &Value) -> bool {
        self.smart_match(lhs_elem, rhs_elem)
    }
}
