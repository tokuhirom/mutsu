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
    /// Regex-based replacement (static string).
    Regex {
        pattern: String,
        replacement: String,
    },
    /// Character-based mapping with closure replacement.
    CharClosure {
        from_chars: Vec<char>,
        closure: Value,
    },
    /// Regex-based replacement with closure.
    RegexClosure { pattern: String, closure: Value },
}

/// Expand a tr-style spec string: `a..z` becomes all chars from 'a' to 'z'.
/// Handles ambiguous ranges like `A..H..Z` (= `A..Z`) and leading/trailing `..`.
fn expand_trans_spec(spec: &str) -> Vec<char> {
    let chars: Vec<char> = spec.chars().collect();
    let mut result = Vec::new();
    let mut i = 0;
    while i < chars.len() {
        // Check for `X..Y` range pattern
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
            // Handle continuation ranges: `A..H..Z` means A..H then H..Z
            while i + 1 < chars.len() && chars[i] == '.' && chars[i + 1] == '.' {
                if i + 2 < chars.len() {
                    let prev_end = result.last().copied().unwrap_or('\0') as u32;
                    let new_end = chars[i + 2] as u32;
                    // Skip the range start since it was already added
                    if prev_end < new_end {
                        for c in (prev_end + 1)..=new_end {
                            if let Some(ch) = char::from_u32(c) {
                                result.push(ch);
                            }
                        }
                    }
                    i += 3;
                } else {
                    // Trailing `..` — add as literal dots
                    result.push('.');
                    result.push('.');
                    i += 2;
                }
            }
            continue;
        }
        result.push(chars[i]);
        i += 1;
    }
    result
}

/// Convert a Value to a list of strings (for array-based trans pairs).
/// Arrays are flattened, Ranges are iterated, strings are expanded via `expand_trans_spec`.
fn value_to_string_list(v: &Value) -> Vec<String> {
    match v {
        Value::Array(items, ..) => {
            let mut result = Vec::new();
            for item in items.iter() {
                match item {
                    Value::Array(..)
                    | Value::Range(..)
                    | Value::RangeExcl(..)
                    | Value::RangeExclStart(..)
                    | Value::RangeExclBoth(..)
                    | Value::GenericRange { .. } => {
                        let expanded = crate::runtime::utils::value_to_list(item);
                        for i in expanded.iter() {
                            result.push(i.to_string_value());
                        }
                    }
                    _ => result.push(item.to_string_value()),
                }
            }
            result
        }
        Value::Str(s) => {
            let expanded = expand_trans_spec(s);
            expanded.into_iter().map(|c| c.to_string()).collect()
        }
        // Handle Range types by iterating their elements
        Value::Range(..)
        | Value::RangeExcl(..)
        | Value::RangeExclStart(..)
        | Value::RangeExclBoth(..)
        | Value::GenericRange { .. } => {
            let items = crate::runtime::utils::value_to_list(v);
            items.iter().map(|i| i.to_string_value()).collect()
        }
        other => {
            let s = other.to_string_value();
            let expanded = expand_trans_spec(&s);
            expanded.into_iter().map(|c| c.to_string()).collect()
        }
    }
}

/// Check if a value is a callable closure (Sub/Block).
fn is_closure(v: &Value) -> bool {
    matches!(v, Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. })
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
    pub(crate) fn dispatch_trans(
        &mut self,
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
                // Check if the value is a closure
                if is_closure(value) {
                    if let Some(pattern) = extract_regex_pattern(key) {
                        rules.push(TransRule::RegexClosure {
                            pattern: pattern.to_string(),
                            closure: value.as_ref().clone(),
                        });
                    } else {
                        let from_chars = expand_trans_spec(key);
                        rules.push(TransRule::CharClosure {
                            from_chars,
                            closure: value.as_ref().clone(),
                        });
                    }
                } else {
                    rules.push(self.parse_trans_pair(key, value));
                }
            } else if let Value::ValuePair(key, value) = arg {
                // For ValuePair, the key preserves its original type
                if let Value::Regex(pattern) = key.as_ref() {
                    if is_closure(value) {
                        rules.push(TransRule::RegexClosure {
                            pattern: pattern.to_string(),
                            closure: value.as_ref().clone(),
                        });
                    } else {
                        rules.push(TransRule::Regex {
                            pattern: pattern.to_string(),
                            replacement: value.to_string_value(),
                        });
                    }
                } else if matches!(
                    key.as_ref(),
                    Value::Array(..)
                        | Value::Range(..)
                        | Value::RangeExcl(..)
                        | Value::RangeExclStart(..)
                        | Value::RangeExclBoth(..)
                        | Value::GenericRange { .. }
                ) {
                    let from_list = value_to_string_list(key);
                    let to_list = value_to_string_list(value);
                    // Decide CharMap vs TokenMap based on element lengths
                    let has_multichar = from_list.iter().any(|s| s.chars().count() > 1)
                        || to_list.iter().any(|s| s.chars().count() > 1);
                    if has_multichar {
                        rules.push(TransRule::TokenMap {
                            from_tokens: from_list,
                            to_tokens: to_list,
                        });
                    } else {
                        let from_chars: Vec<char> =
                            from_list.iter().filter_map(|s| s.chars().next()).collect();
                        let to_chars: Vec<char> =
                            to_list.iter().filter_map(|s| s.chars().next()).collect();
                        rules.push(TransRule::CharMap {
                            from_chars,
                            to_chars,
                        });
                    }
                } else if is_closure(value) {
                    // String key + closure value
                    let key_str = key.to_string_value();
                    if let Some(pattern) = extract_regex_pattern(&key_str) {
                        rules.push(TransRule::RegexClosure {
                            pattern: pattern.to_string(),
                            closure: value.as_ref().clone(),
                        });
                    } else {
                        let from_chars = expand_trans_spec(&key_str);
                        rules.push(TransRule::CharClosure {
                            from_chars,
                            closure: value.as_ref().clone(),
                        });
                    }
                } else {
                    let key_str = key.to_string_value();
                    rules.push(self.parse_trans_pair(&key_str, value));
                }
            }
        }

        if rules.is_empty() {
            return Ok(Value::str(text));
        }

        let result = self.apply_trans_rules(&text, &rules, squash, delete, complement)?;
        Ok(Value::str(result))
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
        let from_list = value_to_string_list(&Value::str(key.to_string()));

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
        &mut self,
        text: &str,
        rules: &[TransRule],
        squash: bool,
        delete: bool,
        complement: bool,
    ) -> Result<String, RuntimeError> {
        if complement {
            return Ok(self.apply_trans_complement(text, rules, squash, delete));
        }

        let chars: Vec<char> = text.chars().collect();
        let mut result = String::new();
        let mut i = 0;
        let mut last_replacement: Option<String> = None;

        while i < chars.len() {
            let mut best_len = 0;
            let mut best_replacement = String::new();
            let mut best_closure: Option<(Value, String)> = None;
            let mut best_captures: Option<Vec<String>> = None;
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
                            } else if delete {
                                String::new()
                            } else if !to_chars.is_empty() {
                                to_chars.last().unwrap().to_string()
                            } else {
                                String::new()
                            };
                            best_closure = None;
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
                                } else if delete {
                                    String::new()
                                } else if !to_tokens.is_empty() {
                                    to_tokens.last().unwrap().clone()
                                } else {
                                    String::new()
                                };
                                best_closure = None;
                                found = true;
                            }
                        }
                    }
                    TransRule::Regex {
                        pattern,
                        replacement,
                    } => {
                        if let Some(caps) = self.regex_match_with_captures_at(pattern, text, i)
                            && caps.from == i
                            && caps.to > i
                            && (caps.to - i) > best_len
                        {
                            best_len = caps.to - i;
                            best_replacement = replacement.clone();
                            best_closure = None;
                            found = true;
                        }
                    }
                    TransRule::CharClosure {
                        from_chars,
                        closure,
                    } => {
                        if let Some(_pos) = from_chars.iter().position(|&fc| fc == chars[i])
                            && 1 >= best_len
                        {
                            best_len = 1;
                            let matched = chars[i].to_string();
                            best_closure = Some((closure.clone(), matched));
                            found = true;
                        }
                    }
                    TransRule::RegexClosure { pattern, closure } => {
                        if let Some(caps) = self.regex_match_with_captures_at(pattern, text, i)
                            && caps.from == i
                            && caps.to > i
                            && (caps.to - i) > best_len
                        {
                            best_len = caps.to - i;
                            let matched: String = chars[i..caps.to].iter().collect();
                            best_captures = Some(caps.positional.clone());
                            best_closure = Some((closure.clone(), matched));
                            found = true;
                        }
                    }
                }
            }

            if found && best_len > 0 {
                let replacement = if let Some((closure, _matched)) = best_closure {
                    self.call_closure_for_trans(&closure, &_matched, best_captures.as_deref())?
                } else {
                    best_replacement
                };
                if squash {
                    if last_replacement.as_deref() != Some(&replacement) {
                        result.push_str(&replacement);
                        last_replacement = Some(replacement);
                    }
                } else {
                    result.push_str(&replacement);
                    last_replacement = Some(replacement);
                }
                i += best_len;
            } else {
                result.push(chars[i]);
                last_replacement = None;
                i += 1;
            }
        }

        Ok(result)
    }

    /// Call a closure for trans replacement, setting up $0, $1, etc.
    fn call_closure_for_trans(
        &mut self,
        closure: &Value,
        _matched: &str,
        captures: Option<&[String]>,
    ) -> Result<String, RuntimeError> {
        // Set positional captures ($0, $1, ...) if provided
        if let Some(caps) = captures {
            for (idx, cap) in caps.iter().enumerate() {
                self.env_mut()
                    .insert(idx.to_string(), Value::Str(cap.clone().into()));
            }
        }
        let result = self.call_sub_value(closure.clone(), vec![], true)?;
        Ok(result.to_string_value())
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
                TransRule::CharClosure { .. } | TransRule::RegexClosure { .. } => String::new(),
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
                    TransRule::Regex { pattern, .. } | TransRule::RegexClosure { pattern, .. } => {
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
                    TransRule::CharClosure { from_chars, .. } => {
                        if from_chars.contains(&chars[i]) {
                            in_rule = true;
                            best_len = 1;
                            best_original = chars[i].to_string();
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
