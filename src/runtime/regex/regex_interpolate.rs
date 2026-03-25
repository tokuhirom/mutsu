use super::super::*;

impl Interpreter {
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
                while i < chars.len() && depth > 0 {
                    let c = chars[i];
                    if c == '<' {
                        depth += 1;
                    } else if c == '>' {
                        depth -= 1;
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
            while i < chars.len() && depth > 0 {
                match chars[i] {
                    '<' => depth += 1,
                    '>' => depth -= 1,
                    _ => {}
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
                let Some(value) = self.eval_regex_expr_value(arg, &default_caps) else {
                    return Err(RuntimeError::new(format!(
                        "Failed to evaluate regex argument expression: {arg}"
                    )));
                };
                rendered_args.push(Self::format_named_regex_arg_value(&value));
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
