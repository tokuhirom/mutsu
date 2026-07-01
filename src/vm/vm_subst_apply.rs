use super::vm_string_regex_ops::*;
use super::*;

impl Interpreter {
    /// Create a Match object for a substitution match.
    pub(super) fn make_subst_match(text: &str, start: usize, end: usize) -> Value {
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
            &[],
            Some(text),
        )
    }

    pub(super) fn select_substitution_ranges(
        all_matches: &[(usize, usize)],
        nth_spec: Option<&str>,
        x_spec: Option<&str>,
    ) -> Result<Vec<(usize, usize)>, RuntimeError> {
        // `:x(N)` requires exactly N matches; `:x(lo..hi)` requires at least
        // `lo` and keeps at most `hi`. Returns `(lo, hi)`.
        let x_bounds = x_spec.map(Self::parse_subst_x_spec);
        if let Some(raw) = nth_spec {
            // :nth may carry a comma-separated list of 1-based indices, e.g.
            // `:nth(1,3)`. Indices must be >= 1 and monotonically increasing.
            let nth_list = Self::parse_subst_nth_spec(raw)?;
            let mut selected: Vec<(usize, usize)> = Vec::new();
            for &n in &nth_list {
                if n <= all_matches.len() {
                    let range = all_matches[n - 1];
                    if !selected.contains(&range) {
                        selected.push(range);
                    }
                }
            }
            // When combined with :x(lo..hi), require at least `lo` and keep up
            // to `hi` of the selected matches.
            if let Some((lo, hi)) = x_bounds {
                if selected.len() < lo {
                    return Ok(Vec::new());
                }
                selected.truncate(hi);
            }
            return Ok(selected);
        }
        if let Some((lo, hi)) = x_bounds {
            if hi == 0 || all_matches.len() < lo {
                return Ok(Vec::new());
            }
            return Ok(all_matches.iter().copied().take(hi).collect());
        }
        Ok(all_matches.first().copied().into_iter().collect())
    }

    /// Parse a `:x` adverb spec into `(lo, hi)` match-count bounds. A bare count
    /// `"3"` is `(3, 3)`; a range `"1..3"` / `"1..^3"` maps to its endpoints;
    /// `"*"`/`"Inf"` is unbounded `(0, usize::MAX)`.
    fn parse_subst_x_spec(raw: &str) -> (usize, usize) {
        let token = raw.trim();
        if token == "*" || token.eq_ignore_ascii_case("Inf") {
            return (0, usize::MAX);
        }
        let parse_bound = |s: &str| s.trim().parse::<i64>().ok();
        if let Some(idx) = token.find("..") {
            let lo_s = &token[..idx];
            let mut rest = &token[idx + 2..];
            let excl = rest.starts_with('^');
            if excl {
                rest = &rest[1..];
            }
            let lo = parse_bound(lo_s).unwrap_or(0).max(0) as usize;
            let hi = if rest.trim() == "*" || rest.trim().eq_ignore_ascii_case("Inf") {
                usize::MAX
            } else {
                match parse_bound(rest) {
                    Some(h) if h >= 0 => {
                        let h = h as usize;
                        if excl { h.saturating_sub(1) } else { h }
                    }
                    _ => usize::MAX,
                }
            };
            return (lo, hi);
        }
        match parse_bound(token) {
            Some(n) if n >= 0 => (n as usize, n as usize),
            _ => (0, usize::MAX),
        }
    }

    /// Parse an `:nth` spec into a list of 1-based indices. Accepts a single
    /// integer or a comma-separated list (e.g. `1,3`). Validates that every
    /// index is >= 1 and that the list is monotonically increasing.
    fn parse_subst_nth_spec(raw: &str) -> Result<Vec<usize>, RuntimeError> {
        let token = raw.trim();
        if token.eq_ignore_ascii_case("-Inf") {
            return Err(RuntimeError::new("Invalid :nth index (-Inf)"));
        }
        let mut out: Vec<usize> = Vec::new();
        let mut prev: i64 = 0;
        for part in token.split(',') {
            let part = part.trim();
            if part.is_empty() {
                continue;
            }
            let n = part
                .parse::<i64>()
                .map_err(|_| RuntimeError::new(format!("Invalid :nth index ({part})")))?;
            if n < 1 {
                return Err(RuntimeError::new(format!(
                    "Attempt to retrieve before :1st match -- :nth({n})"
                )));
            }
            if n < prev {
                return Err(RuntimeError::new(format!(
                    "Attempt to fetch match #{n} after #{prev}"
                )));
            }
            prev = n;
            out.push(n as usize);
        }
        if out.is_empty() {
            return Err(RuntimeError::new(format!("Invalid :nth index ({token})")));
        }
        Ok(out)
    }

    pub(super) fn apply_substitutions(
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
            let repl = apply_subst_case_transforms(
                replacement,
                matched_text,
                samecase,
                samemark,
                sigspace,
                samespace,
            );
            out.push_str(&repl);
            prev_end_b = end_b;
        }
        out.push_str(&text[prev_end_b..]);
        out
    }

    /// Like `apply_substitutions` but expands `$0`, `$1`, etc. per match
    /// using the corresponding capture groups.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn apply_substitutions_with_captures(
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
            let repl = apply_subst_case_transforms(
                &expanded,
                matched_text,
                samecase,
                samemark,
                sigspace,
                samespace,
            );
            out.push_str(&repl);
            prev_end_b = end_b;
        }
        out.push_str(&text[prev_end_b..]);
        out
    }

    /// Build a substitution output when the replacement contains `{...}` code
    /// blocks. The replacement is interpolated *per match*, with `$/`, `$0`,
    /// `$1`, ... bound to the current match so closures like `{ $0 x 2 }` or
    /// `{ uc($/) }` see the correct match.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn apply_substitutions_dynamic(
        &mut self,
        text: &str,
        ranges: &[(usize, usize)],
        raw_replacement: &str,
        per_match_captures: &[Vec<String>],
        samecase: bool,
        sigspace: bool,
        samemark: bool,
        samespace: bool,
    ) -> String {
        // Snapshot the env entries we overwrite so we can restore them after.
        let saved_slash = self.env().get("/").cloned();
        let saved_caps: Vec<Option<Value>> = (0..10)
            .map(|n| self.env().get(&n.to_string()).cloned())
            .collect();

        let mut out = String::new();
        let mut prev_end_b = 0usize;
        for (i, (start, end)) in ranges.iter().enumerate() {
            let start_b = runtime::char_idx_to_byte(text, *start);
            let end_b = runtime::char_idx_to_byte(text, *end);
            out.push_str(&text[prev_end_b..start_b]);
            let matched_text = &text[start_b..end_b];

            let caps: &[String] = per_match_captures
                .get(i)
                .map(|c| c.as_slice())
                .unwrap_or(&[]);
            // Bind `$/` to a Match object for this match, and `$0`, `$1`, ...
            // to the positional captures.
            let match_obj = Value::make_match_object_full(
                matched_text.to_string(),
                *start as i64,
                *end as i64,
                caps,
                &std::collections::HashMap::new(),
                &std::collections::HashMap::new(),
                &[],
                &[],
                &[],
                Some(text),
            );
            self.env_mut().insert("/".to_string(), match_obj);
            for n in 0..10 {
                if let Some(cap) = caps.get(n) {
                    self.env_mut()
                        .insert(n.to_string(), Value::str(cap.clone()));
                } else {
                    self.env_mut().remove(&n.to_string());
                }
            }

            let interpolated = self.interpolate_subst_replacement_with_closures(raw_replacement);
            let expanded = if caps.is_empty() {
                interpolated
            } else {
                expand_capture_refs(&interpolated, caps)
            };
            let repl = apply_subst_case_transforms(
                &expanded,
                matched_text,
                samecase,
                samemark,
                sigspace,
                samespace,
            );
            out.push_str(&repl);
            prev_end_b = end_b;
        }
        out.push_str(&text[prev_end_b..]);

        // Restore overwritten env entries (`$/` is reset to the match list/object
        // by the caller after this returns).
        match saved_slash {
            Some(v) => {
                self.env_mut().insert("/".to_string(), v);
            }
            None => {
                self.env_mut().remove("/");
            }
        }
        for (n, saved) in saved_caps.into_iter().enumerate() {
            match saved {
                Some(v) => {
                    self.env_mut().insert(n.to_string(), v);
                }
                None => {
                    self.env_mut().remove(&n.to_string());
                }
            }
        }
        out
    }

    /// Interpolate a substitution replacement string, evaluating `{...}` blocks
    /// as closures (Raku double-quoted string semantics).
    pub(super) fn interpolate_subst_replacement_with_closures(&mut self, template: &str) -> String {
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
                        let saved_topic = self.env().get("_").cloned();
                        let val = loan_env!(self, eval_block_value(&stmts)).unwrap_or(Value::Nil);
                        // Restore $_ so the substitution can find the target
                        if let Some(topic) = saved_topic {
                            self.env_mut().insert("_".to_string(), topic);
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
                    let value = self.env().get(name).cloned().unwrap_or(Value::Nil);
                    out.push_str(&value.to_string_value());
                    i += 2 + close + 1;
                    continue;
                }
            }
            // Handle $var.method() interpolation (Raku: `$x.meth()` interpolates
            // the method call; a bare `$x.meth` leaves `.meth` literal). This also
            // covers `$/.chars()` and capture-var method calls. Evaluate the whole
            // term as an expression, like a `{...}` code block, so `$/`, `$0`, ...
            // (bound per match by the dynamic path) resolve correctly.
            if (bytes[i] == b'$' || bytes[i] == b'@')
                && i + 1 < bytes.len()
                && let Some(term_len) = subst_method_call_term(&template[i + 1..])
            {
                let expr_src = &template[i..i + 1 + term_len];
                let parsed = crate::parse_dispatch::parse_source(expr_src);
                if let Ok((stmts, _)) = parsed {
                    let saved_topic = self.env().get("_").cloned();
                    let val = loan_env!(self, eval_block_value(&stmts)).unwrap_or(Value::Nil);
                    if let Some(topic) = saved_topic {
                        self.env_mut().insert("_".to_string(), topic);
                    }
                    out.push_str(&val.to_string_value());
                    i += 1 + term_len;
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
                        let value = self.env().get(name).cloned().unwrap_or(Value::Nil);
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
                    let value = self.env().get(name).cloned().unwrap_or(Value::Nil);
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
                        let value = self.env().get(&arr_name).cloned().unwrap_or(Value::Nil);
                        out.push_str(&value.to_string_value());
                        i += 1 + name_len + close + 1;
                        continue;
                    }
                    let arr_name = format!("@{}", name);
                    let value = self.env().get(&arr_name).cloned().unwrap_or(Value::Nil);
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
