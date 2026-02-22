use super::*;

/// A single translation rule used by `.trans`.
enum TransRule {
    /// Character-by-character mapping (from tr///‐style string pairs).
    CharMap {
        from_chars: Vec<char>,
        to_chars: Vec<char>,
    },
    /// Multi-character token mapping (from array pairs).
    TokenMap {
        from_tokens: Vec<String>,
        to_tokens: Vec<String>,
    },
    /// Regex-based replacement.
    Regex {
        pattern: String,
        replacement: String,
    },
}

/// Expand a tr-style spec string: `a..z` becomes all chars from 'a' to 'z'.
fn expand_trans_spec(spec: &str) -> Vec<char> {
    let chars: Vec<char> = spec.chars().collect();
    let mut result = Vec::new();
    let mut i = 0;
    while i < chars.len() {
        if i + 3 < chars.len() && chars[i + 1] == '.' && chars[i + 2] == '.' {
            let start = chars[i] as u32;
            let end = chars[i + 3] as u32;
            if start <= end {
                for c in start..=end {
                    if let Some(ch) = char::from_u32(c) {
                        result.push(ch);
                    }
                }
            }
            i += 4;
            continue;
        }
        result.push(chars[i]);
        i += 1;
    }
    result
}

/// Convert a Value to a list of strings (for array-based trans pairs).
fn value_to_string_list(v: &Value) -> Vec<String> {
    match v {
        Value::Array(items, ..) => items.iter().map(|i| i.to_string_value()).collect(),
        Value::Str(s) => {
            let expanded = expand_trans_spec(s);
            expanded.into_iter().map(|c| c.to_string()).collect()
        }
        other => {
            let expanded = expand_trans_spec(&other.to_string_value());
            expanded.into_iter().map(|c| c.to_string()).collect()
        }
    }
}

/// Check if a Pair key is a stringified regex pattern (e.g. `/ \s+ /`).
/// Returns the inner pattern if so.
fn extract_regex_pattern(key: &str) -> Option<&str> {
    if key.starts_with('/') && key.ends_with('/') && key.len() >= 3 {
        Some(&key[1..key.len() - 1])
    } else {
        None
    }
}

impl Interpreter {
    pub(super) fn dispatch_trans(
        &self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let text = target.to_string_value();

        let mut rules: Vec<TransRule> = Vec::new();
        let mut squash = false;
        let mut delete = false;
        let mut complement = false;

        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "s" | "squash" => {
                        squash = value.truthy();
                        continue;
                    }
                    "d" | "delete" => {
                        delete = value.truthy();
                        continue;
                    }
                    "c" | "complement" => {
                        complement = value.truthy();
                        continue;
                    }
                    _ => {}
                }
                rules.push(self.parse_trans_pair(key, value));
            }
        }

        if rules.is_empty() {
            return Ok(Value::Str(text));
        }

        let result = self.apply_trans_rules(&text, &rules, squash, delete, complement);
        Ok(Value::Str(result))
    }

    fn parse_trans_pair(&self, key: &str, value: &Value) -> TransRule {
        // Detect regex keys: when a Regex value is used as a Pair key,
        // it gets stringified to `/pattern/` by MakePair.
        if let Some(pattern) = extract_regex_pattern(key) {
            return TransRule::Regex {
                pattern: pattern.to_string(),
                replacement: value.to_string_value(),
            };
        }

        let to_list = value_to_string_list(value);
        let from_list = value_to_string_list(&Value::Str(key.to_string()));

        let has_multichar = from_list.iter().any(|s| s.chars().count() > 1);

        if has_multichar {
            TransRule::TokenMap {
                from_tokens: from_list,
                to_tokens: to_list,
            }
        } else {
            let from_chars: Vec<char> = from_list.iter().filter_map(|s| s.chars().next()).collect();
            let to_chars: Vec<char> = to_list.iter().filter_map(|s| s.chars().next()).collect();
            TransRule::CharMap {
                from_chars,
                to_chars,
            }
        }
    }

    fn apply_trans_rules(
        &self,
        text: &str,
        rules: &[TransRule],
        squash: bool,
        _delete: bool,
        complement: bool,
    ) -> String {
        if complement {
            return self.apply_trans_complement(text, rules, squash, _delete);
        }

        let chars: Vec<char> = text.chars().collect();
        let mut result = String::new();
        let mut i = 0;
        let mut last_replacement: Option<String> = None;

        while i < chars.len() {
            let mut best_len = 0;
            let mut best_replacement = String::new();
            let mut found = false;

            for rule in rules {
                match rule {
                    TransRule::CharMap {
                        from_chars,
                        to_chars,
                    } => {
                        if let Some(pos) = from_chars.iter().position(|&fc| fc == chars[i])
                            && 1 >= best_len
                        {
                            best_len = 1;
                            best_replacement = if pos < to_chars.len() {
                                to_chars[pos].to_string()
                            } else if !to_chars.is_empty() {
                                to_chars.last().unwrap().to_string()
                            } else {
                                String::new()
                            };
                            found = true;
                        }
                    }
                    TransRule::TokenMap {
                        from_tokens,
                        to_tokens,
                    } => {
                        let remaining: String = chars[i..].iter().collect();
                        for (ti, token) in from_tokens.iter().enumerate() {
                            let token_char_len = token.chars().count();
                            if remaining.starts_with(token.as_str()) && token_char_len > best_len {
                                best_len = token_char_len;
                                best_replacement = if ti < to_tokens.len() {
                                    to_tokens[ti].clone()
                                } else if !to_tokens.is_empty() {
                                    to_tokens.last().unwrap().clone()
                                } else {
                                    String::new()
                                };
                                found = true;
                            }
                        }
                    }
                    TransRule::Regex {
                        pattern,
                        replacement,
                    } => {
                        let remaining: String = chars[i..].iter().collect();
                        if let Some((start, end)) = self.regex_find_first(pattern, &remaining)
                            && start == 0
                            && end > 0
                            && end > best_len
                        {
                            best_len = end;
                            best_replacement = replacement.clone();
                            found = true;
                        }
                    }
                }
            }

            if found && best_len > 0 {
                if squash {
                    if last_replacement.as_deref() != Some(&best_replacement) {
                        result.push_str(&best_replacement);
                        last_replacement = Some(best_replacement);
                    }
                } else {
                    result.push_str(&best_replacement);
                    last_replacement = Some(best_replacement);
                }
                i += best_len;
            } else {
                result.push(chars[i]);
                last_replacement = None;
                i += 1;
            }
        }

        result
    }

    fn apply_trans_complement(
        &self,
        text: &str,
        rules: &[TransRule],
        squash: bool,
        delete: bool,
    ) -> String {
        let chars: Vec<char> = text.chars().collect();
        let mut result = String::new();
        let mut last_was_complement = false;

        let complement_replacement: String = rules
            .first()
            .map(|rule| match rule {
                TransRule::CharMap { to_chars, .. } => {
                    to_chars.first().map(|c| c.to_string()).unwrap_or_default()
                }
                TransRule::TokenMap { to_tokens, .. } => {
                    to_tokens.first().cloned().unwrap_or_default()
                }
                TransRule::Regex { replacement, .. } => replacement.clone(),
            })
            .unwrap_or_default();

        let mut i = 0;
        while i < chars.len() {
            let mut in_rule = false;
            let mut best_len = 0;
            let mut best_original = String::new();

            for rule in rules {
                match rule {
                    TransRule::CharMap { from_chars, .. } => {
                        if from_chars.contains(&chars[i]) {
                            in_rule = true;
                            best_len = 1;
                            best_original = chars[i].to_string();
                        }
                    }
                    TransRule::TokenMap { from_tokens, .. } => {
                        let remaining: String = chars[i..].iter().collect();
                        for token in from_tokens {
                            let tlen = token.chars().count();
                            if remaining.starts_with(token.as_str()) && tlen > best_len {
                                in_rule = true;
                                best_len = tlen;
                                best_original = token.clone();
                            }
                        }
                    }
                    TransRule::Regex { pattern, .. } => {
                        let remaining: String = chars[i..].iter().collect();
                        if let Some((start, end)) = self.regex_find_first(pattern, &remaining)
                            && start == 0
                            && end > 0
                        {
                            in_rule = true;
                            best_len = end;
                            best_original = remaining.chars().take(end).collect::<String>();
                        }
                    }
                }
            }

            if in_rule {
                result.push_str(&best_original);
                last_was_complement = false;
                i += if best_len > 0 { best_len } else { 1 };
            } else {
                if delete {
                    // Skip character.
                } else if squash {
                    if !last_was_complement {
                        result.push_str(&complement_replacement);
                    }
                } else {
                    result.push_str(&complement_replacement);
                }
                last_was_complement = true;
                i += 1;
            }
        }

        result
    }
}
