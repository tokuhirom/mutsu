use super::super::*;

impl Interpreter {
    /// Heuristic: returns true if a regex subrule arg expression syntactically
    /// looks like a fat-arrow Pair (`a => 1`), a colonpair (`:b(2)`), or a
    /// `|`-flattening prefix (`|...`). For these forms we cannot easily
    /// round-trip the value through a regex source string, so we keep the
    /// original expression and let the match-time evaluator handle them.
    pub(in crate::runtime) fn regex_arg_is_complex(arg: &str) -> bool {
        let trimmed = arg.trim_start();
        if trimmed.starts_with(':') || trimmed.starts_with('|') {
            return true;
        }
        // Detect top-level `=>` (fat-arrow Pair).
        let bytes: Vec<char> = arg.chars().collect();
        let mut paren = 0i32;
        let mut bracket = 0i32;
        let mut brace = 0i32;
        let mut quote: Option<char> = None;
        let mut escaped = false;
        let mut i = 0;
        while i < bytes.len() {
            let ch = bytes[i];
            if let Some(q) = quote {
                if escaped {
                    escaped = false;
                } else if ch == '\\' {
                    escaped = true;
                } else if ch == q {
                    quote = None;
                }
                i += 1;
                continue;
            }
            match ch {
                '\'' | '"' => quote = Some(ch),
                '(' => paren += 1,
                ')' => paren -= 1,
                '[' => bracket += 1,
                ']' => bracket -= 1,
                '{' => brace += 1,
                '}' => brace -= 1,
                '=' if paren == 0
                    && bracket == 0
                    && brace == 0
                    && i + 1 < bytes.len()
                    && bytes[i + 1] == '>' =>
                {
                    return true;
                }
                _ => {}
            }
            i += 1;
        }
        false
    }

    /// Returns true if a value can be safely formatted into a regex argument
    /// list and re-evaluated to the same value. Conservative: only simple
    /// scalar literals.
    pub(in crate::runtime) fn value_is_round_trippable(value: &Value) -> bool {
        matches!(
            value,
            Value::Int(_) | Value::Num(_) | Value::Str(_) | Value::Bool(_) | Value::Nil
        )
    }

    /// Walk a regex pattern source and, inside each top-level `{ ... }` code
    /// block, replace bare `$name` references for `param_names` with a
    /// parenthesised literal of the value bound in `self.env`. This lets
    /// regex code blocks see token parameters that would otherwise be lost
    /// when the pattern is matched in the caller's environment.
    pub(in crate::runtime) fn bake_bound_params_into_regex_code_blocks(
        &self,
        pattern: &str,
        param_names: &[String],
    ) -> String {
        if param_names.is_empty() {
            return pattern.to_string();
        }
        let chars: Vec<char> = pattern.chars().collect();
        let mut out = String::new();
        let mut i = 0usize;
        while i < chars.len() {
            let ch = chars[i];
            // Skip <...> assertions / subrule calls; we only rewrite code blocks.
            if ch == '<' {
                let mut depth = 1usize;
                out.push(ch);
                i += 1;
                let mut paren = 0usize;
                let mut bracket = 0usize;
                let mut brace = 0usize;
                let mut quote: Option<char> = None;
                let mut esc = false;
                while i < chars.len() && depth > 0 {
                    let c = chars[i];
                    if let Some(q) = quote {
                        if esc {
                            esc = false;
                        } else if c == '\\' {
                            esc = true;
                        } else if c == q {
                            quote = None;
                        }
                    } else {
                        match c {
                            '\'' | '"' => quote = Some(c),
                            '(' => paren += 1,
                            ')' => paren = paren.saturating_sub(1),
                            '[' => bracket += 1,
                            ']' => bracket = bracket.saturating_sub(1),
                            '{' => brace += 1,
                            '}' => brace = brace.saturating_sub(1),
                            '<' if paren == 0 && bracket == 0 && brace == 0 => depth += 1,
                            '>' if paren == 0 && bracket == 0 && brace == 0 => depth -= 1,
                            _ => {}
                        }
                    }
                    out.push(c);
                    i += 1;
                }
                continue;
            }
            if ch == '\\' {
                out.push(ch);
                i += 1;
                if i < chars.len() {
                    out.push(chars[i]);
                    i += 1;
                }
                continue;
            }
            if ch == '{' {
                // Capture body of code block.
                let mut depth = 1usize;
                let body_start = i + 1;
                let mut j = i + 1;
                while j < chars.len() && depth > 0 {
                    let c = chars[j];
                    if c == '{' {
                        depth += 1;
                    } else if c == '}' {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                    j += 1;
                }
                let body: String = chars[body_start..j].iter().collect();
                let baked = self.bake_params_in_code_text(&body, param_names);
                out.push('{');
                out.push_str(&baked);
                if j < chars.len() {
                    out.push('}');
                    i = j + 1;
                } else {
                    i = j;
                }
                continue;
            }
            out.push(ch);
            i += 1;
        }
        out
    }

    fn bake_params_in_code_text(&self, code: &str, param_names: &[String]) -> String {
        let chars: Vec<char> = code.chars().collect();
        let mut out = String::new();
        let mut i = 0usize;
        // Track quote state but allow $-substitution inside double-quoted
        // strings (Raku interpolates them too).
        let mut in_squote = false;
        let mut escaped = false;
        while i < chars.len() {
            let ch = chars[i];
            if in_squote {
                out.push(ch);
                if escaped {
                    escaped = false;
                } else if ch == '\\' {
                    escaped = true;
                } else if ch == '\'' {
                    in_squote = false;
                }
                i += 1;
                continue;
            }
            if ch == '\'' {
                in_squote = true;
                out.push(ch);
                i += 1;
                continue;
            }
            // Skip backslash escapes outside of single-quoted strings.
            if ch == '\\' && i + 1 < chars.len() {
                out.push(ch);
                out.push(chars[i + 1]);
                i += 2;
                continue;
            }
            if ch == '$' && i + 1 < chars.len() {
                let next = chars[i + 1];
                if next.is_alphabetic() || next == '_' {
                    let mut j = i + 1;
                    while j < chars.len() && (chars[j].is_alphanumeric() || chars[j] == '_') {
                        j += 1;
                    }
                    let name: String = chars[i + 1..j].iter().collect();
                    if param_names.iter().any(|p| p == &name)
                        && let Some(val) = self.env.get(&name).cloned()
                        && let Some(literal) = Self::value_to_raku_literal(&val)
                    {
                        out.push_str(&literal);
                        i = j;
                        continue;
                    }
                }
            }
            out.push(ch);
            i += 1;
        }
        out
    }

    fn value_to_raku_literal(value: &Value) -> Option<String> {
        match value {
            Value::Int(n) => Some(n.to_string()),
            Value::Num(n) => Some(format!("{n}e0")),
            Value::Bool(true) => Some("True".to_string()),
            Value::Bool(false) => Some("False".to_string()),
            Value::Nil => Some("Nil".to_string()),
            Value::Str(s) => {
                let escaped = s.replace('\\', "\\\\").replace('\'', "\\'");
                Some(format!("'{escaped}'"))
            }
            _ => None,
        }
    }

    pub(in crate::runtime) fn interpolate_bound_regex_scalars(&self, pattern: &str) -> String {
        let chars: Vec<char> = pattern.chars().collect();
        let mut out = String::new();
        let mut i = 0usize;
        while i < chars.len() {
            let ch = chars[i];
            if ch == '{' {
                let mut depth = 1usize;
                out.push(ch);
                i += 1;
                while i < chars.len() && depth > 0 {
                    let c = chars[i];
                    if c == '{' {
                        depth += 1;
                    } else if c == '}' {
                        depth -= 1;
                    }
                    out.push(c);
                    i += 1;
                }
                continue;
            }
            if ch == '<' {
                let mut depth = 1usize;
                out.push(ch);
                i += 1;
                let mut paren = 0usize;
                let mut bracket = 0usize;
                let mut brace = 0usize;
                let mut quote: Option<char> = None;
                let mut esc = false;
                while i < chars.len() && depth > 0 {
                    let c = chars[i];
                    if let Some(q) = quote {
                        if esc {
                            esc = false;
                        } else if c == '\\' {
                            esc = true;
                        } else if c == q {
                            quote = None;
                        }
                    } else {
                        match c {
                            '\'' | '"' => quote = Some(c),
                            '(' => paren += 1,
                            ')' => paren = paren.saturating_sub(1),
                            '[' => bracket += 1,
                            ']' => bracket = bracket.saturating_sub(1),
                            '{' => brace += 1,
                            '}' => brace = brace.saturating_sub(1),
                            '<' if paren == 0 && bracket == 0 && brace == 0 => depth += 1,
                            '>' if paren == 0 && bracket == 0 && brace == 0 => depth -= 1,
                            _ => {}
                        }
                    }
                    out.push(c);
                    i += 1;
                }
                continue;
            }
            if ch == '\\' {
                out.push(ch);
                i += 1;
                if i < chars.len() {
                    out.push(chars[i]);
                    i += 1;
                }
                continue;
            }
            if ch == '$' {
                let start = i;
                let mut j = i + 1;
                let parsed = if j < chars.len() && chars[j] == '{' {
                    j += 1;
                    let name_start = j;
                    while j < chars.len() && chars[j] != '}' {
                        j += 1;
                    }
                    if j < chars.len() && j > name_start {
                        let name: String = chars[name_start..j].iter().collect();
                        j += 1;
                        Some((name, j))
                    } else {
                        None
                    }
                } else if j < chars.len() && (chars[j].is_alphabetic() || chars[j] == '_') {
                    let name_start = j;
                    while j < chars.len()
                        && (chars[j].is_alphanumeric() || chars[j] == '_' || chars[j] == '-')
                    {
                        j += 1;
                    }
                    Some((chars[name_start..j].iter().collect::<String>(), j))
                } else {
                    None
                };
                if let Some((name, end)) = parsed {
                    if let Some(value) = self
                        .env
                        .get(&name)
                        .cloned()
                        .or_else(|| self.env.get(&format!("${name}")).cloned())
                    {
                        match value {
                            Value::Regex(pat) => out.push_str(&pat),
                            other => {
                                out.push_str(&Self::regex_escape_literal(&other.to_string_value()))
                            }
                        }
                    } else {
                        out.extend(chars[start..end].iter());
                    }
                    i = end;
                    continue;
                }
            }
            out.push(ch);
            i += 1;
        }
        out
    }

    pub(in crate::runtime) fn instantiate_named_regex_arg_calls(
        &self,
        pattern: &str,
    ) -> Result<String, RuntimeError> {
        let chars: Vec<char> = pattern.chars().collect();
        let mut out = String::new();
        let default_caps = RegexCaptures::default();
        let mut i = 0usize;
        while i < chars.len() {
            if chars[i] != '<' {
                out.push(chars[i]);
                i += 1;
                continue;
            }

            let mut depth = 1usize;
            let start = i + 1;
            i += 1;
            // Track nested parens/brackets/braces and quotes so that a
            // closing `>` inside a subrule arg list (e.g. `<.foo(:b(2))>`)
            // doesn't terminate the assertion prematurely.
            let mut paren = 0usize;
            let mut bracket = 0usize;
            let mut brace = 0usize;
            let mut quote: Option<char> = None;
            let mut esc = false;
            while i < chars.len() && depth > 0 {
                let c = chars[i];
                if let Some(q) = quote {
                    if esc {
                        esc = false;
                    } else if c == '\\' {
                        esc = true;
                    } else if c == q {
                        quote = None;
                    }
                } else {
                    match c {
                        '\'' | '"' => quote = Some(c),
                        '(' => paren += 1,
                        ')' => paren = paren.saturating_sub(1),
                        '[' => bracket += 1,
                        ']' => bracket = bracket.saturating_sub(1),
                        '{' => brace += 1,
                        '}' => brace = brace.saturating_sub(1),
                        '<' if paren == 0 && bracket == 0 && brace == 0 => depth += 1,
                        '>' if paren == 0 && bracket == 0 && brace == 0 => depth -= 1,
                        _ => {}
                    }
                }
                i += 1;
            }
            if depth != 0 {
                out.push('<');
                out.extend(chars[start..].iter());
                break;
            }

            let inner_end = i - 1;
            let inner: String = chars[start..inner_end].iter().collect();
            let spec = Self::parse_named_regex_lookup_spec(&inner);
            if spec.arg_exprs.is_empty() {
                out.push('<');
                out.push_str(&inner);
                out.push('>');
                continue;
            }

            let Some(open_idx) = inner.find('(') else {
                out.push('<');
                out.push_str(&inner);
                out.push('>');
                continue;
            };
            let Some(close_idx) = inner.rfind(')') else {
                out.push('<');
                out.push_str(&inner);
                out.push('>');
                continue;
            };

            let mut rendered_args = Vec::new();
            for arg in &spec.arg_exprs {
                // For complex / non-round-trippable values (Pair, Slip, Array, Hash, ...),
                // keep the original argument expression so the match-time evaluator can
                // re-evaluate and route them as named arguments or flatten Slips.
                if Self::regex_arg_is_complex(arg) {
                    rendered_args.push(arg.clone());
                    continue;
                }
                let Some(value) = self.eval_regex_expr_value(arg, &default_caps) else {
                    return Err(RuntimeError::new(format!(
                        "Failed to evaluate regex argument expression: {arg}"
                    )));
                };
                if Self::value_is_round_trippable(&value) {
                    rendered_args.push(Self::format_named_regex_arg_value(&value));
                } else {
                    // Fallback: keep the raw expression so it is re-evaluated later.
                    rendered_args.push(arg.clone());
                }
            }

            out.push('<');
            out.push_str(&inner[..open_idx + 1]);
            out.push_str(&rendered_args.join(", "));
            out.push_str(&inner[close_idx..]);
            out.push('>');
        }
        Ok(out)
    }

    pub(super) fn restore_env_entries(&mut self, restore: HashMap<String, Option<Value>>) {
        for (key, value) in restore {
            match value {
                Some(v) => {
                    self.env.insert(key, v);
                }
                None => {
                    self.env.remove(&key);
                }
            }
        }
    }

    pub(super) fn find_top_level_semicolon(text: &str) -> Option<usize> {
        let mut paren = 0usize;
        let mut bracket = 0usize;
        let mut brace = 0usize;
        let mut quote: Option<char> = None;
        let mut escaped = false;
        for (idx, ch) in text.char_indices() {
            if let Some(q) = quote {
                if escaped {
                    escaped = false;
                    continue;
                }
                if ch == '\\' {
                    escaped = true;
                    continue;
                }
                if ch == q {
                    quote = None;
                }
                continue;
            }
            match ch {
                '\'' | '"' => quote = Some(ch),
                '(' => paren += 1,
                ')' => paren = paren.saturating_sub(1),
                '[' => bracket += 1,
                ']' => bracket = bracket.saturating_sub(1),
                '{' => brace += 1,
                '}' => brace = brace.saturating_sub(1),
                ';' if paren == 0 && bracket == 0 && brace == 0 => return Some(idx),
                _ => {}
            }
        }
        None
    }

    pub(super) fn find_matching_brace_end(text: &str, open_idx: usize) -> Option<usize> {
        let mut depth = 0usize;
        let mut quote: Option<char> = None;
        let mut escaped = false;
        for (idx, ch) in text.char_indices().skip_while(|(i, _)| *i < open_idx) {
            if let Some(q) = quote {
                if escaped {
                    escaped = false;
                    continue;
                }
                if ch == '\\' {
                    escaped = true;
                    continue;
                }
                if ch == q {
                    quote = None;
                }
                continue;
            }
            match ch {
                '\'' | '"' => quote = Some(ch),
                '{' => depth += 1,
                '}' => {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        return Some(idx);
                    }
                }
                _ => {}
            }
        }
        None
    }
}
