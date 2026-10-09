use super::*;

impl Interpreter {
    // Cost: O(n * (k + t + g*m)), n = chars of the text, k = from-chars of the
    // CharMap/CharClosure rules (linear `position` scan per char), t = total chars of
    // the token keys (each compared in place against `chars[i..]`), g = Regex rules
    // and m = the cost of one anchored regex attempt (all attempts share one
    // `MatchTarget` built once per call).
    pub(super) fn apply_trans_rules(
        &mut self,
        text: &str,
        rules: &[TransRule],
        squash: bool,
        delete: bool,
        complement: bool,
    ) -> Result<String, RuntimeError> {
        if complement {
            return self.apply_trans_complement(text, rules, squash, delete);
        }

        let chars: Vec<char> = text.chars().collect();
        let token_chars = trans_token_chars(rules);
        let target = trans_regex_target(rules, &chars);
        let mut result = String::new();
        let mut i = 0;
        let mut byte_pos = 0;
        let mut last_replacement: Option<String> = None;

        while i < chars.len() {
            let (grapheme_end_byte, grapheme_len) = grapheme_at(text, byte_pos);
            let mut best_len = 0;
            let mut best_end_byte = byte_pos;
            let mut best_replacement = String::new();
            let mut best_closure: Option<(Value, String)> = None;
            let mut best_captures: Option<Vec<String>> = None;
            let mut found = false;

            for (rule, rule_tokens) in rules.iter().zip(&token_chars) {
                match rule {
                    TransRule::CharMap {
                        from_chars,
                        to_chars,
                        cycle,
                    } => {
                        if grapheme_len == 1
                            && let Some(pos) = from_chars.iter().position(|&fc| fc == chars[i])
                            && 1 >= best_len
                        {
                            best_len = 1;
                            best_end_byte = grapheme_end_byte;
                            best_replacement = if pos < to_chars.len() {
                                to_chars[pos].to_string()
                            } else if delete || to_chars.is_empty() {
                                String::new()
                            } else if *cycle && !squash {
                                // Str=>Str form: cycle the replacement to the key
                                // length. Under `:squash` the form falls back to
                                // repeat-last so the squash collapse below reduces
                                // the run (`'123' => 'þð', :squash` => `þð`),
                                // matching `:delete`.
                                to_chars[pos % to_chars.len()].to_string()
                            } else {
                                // List form (or squashed Str=>Str): repeat the
                                // last replacement char.
                                to_chars.last().unwrap().to_string()
                            };
                            best_closure = None;
                            found = true;
                        }
                    }
                    TransRule::TokenMap { to_tokens, .. } => {
                        for (ti, token) in rule_tokens.iter().enumerate() {
                            let token_char_len = token.len();
                            if token_char_len > best_len
                                && chars[i..].starts_with(token)
                                && let Some(end_byte) =
                                    grapheme_end_after_chars(text, byte_pos, token_char_len)
                            {
                                best_len = token_char_len;
                                best_end_byte = end_byte;
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
                        if let Some(target) = &target
                            && let Some(caps) =
                                self.regex_match_with_captures_at_target(pattern, target, i)
                            && caps.from == i
                            && caps.to > i
                            && let Some(end_byte) =
                                grapheme_end_after_chars(text, byte_pos, caps.to - i)
                            && (caps.to - i) > best_len
                        {
                            best_len = caps.to - i;
                            best_end_byte = end_byte;
                            best_replacement = replacement.clone();
                            best_closure = None;
                            found = true;
                        }
                    }
                    TransRule::CharClosure {
                        from_chars,
                        closure,
                    } => {
                        if grapheme_len == 1
                            && from_chars.iter().any(|&fc| fc == chars[i])
                            && 1 >= best_len
                        {
                            best_len = 1;
                            best_end_byte = grapheme_end_byte;
                            let matched = chars[i].to_string();
                            best_closure = Some((closure.clone(), matched));
                            found = true;
                        }
                    }
                    TransRule::RegexClosure { pattern, closure } => {
                        if let Some(target) = &target
                            && let Some(caps) =
                                self.regex_match_with_captures_at_target(pattern, target, i)
                            && caps.from == i
                            && caps.to > i
                            && let Some(end_byte) =
                                grapheme_end_after_chars(text, byte_pos, caps.to - i)
                            && (caps.to - i) > best_len
                        {
                            best_len = caps.to - i;
                            best_end_byte = end_byte;
                            let matched: String = chars[i..caps.to].iter().collect();
                            best_captures = Some(
                                caps.positional
                                    .iter()
                                    .map(|slot| caps.slot_text(slot))
                                    .collect::<Vec<String>>(),
                            );
                            best_closure = Some((closure.clone(), matched));
                            found = true;
                        }
                    }
                    TransRule::TokenClosureMap { to_values, .. } => {
                        for (ti, token) in rule_tokens.iter().enumerate() {
                            let token_char_len = token.len();
                            if token_char_len > best_len
                                && chars[i..].starts_with(token)
                                && let Some(end_byte) =
                                    grapheme_end_after_chars(text, byte_pos, token_char_len)
                            {
                                best_len = token_char_len;
                                best_end_byte = end_byte;
                                let matched: String = token.iter().collect();
                                match to_values.get(ti) {
                                    Some(TokenReplacement::Closure(closure)) => {
                                        best_closure = Some((closure.clone(), matched));
                                        found = true;
                                    }
                                    Some(TokenReplacement::Static(s)) => {
                                        best_replacement = s.clone();
                                        best_closure = None;
                                        found = true;
                                    }
                                    None => {
                                        best_replacement = String::new();
                                        best_closure = None;
                                        found = true;
                                    }
                                }
                            }
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
                byte_pos = best_end_byte;
            } else {
                result.push_str(&text[byte_pos..grapheme_end_byte]);
                last_replacement = None;
                i += grapheme_len;
                byte_pos = grapheme_end_byte;
            }
        }

        Ok(result)
    }

    /// Call a closure for trans replacement, setting up $_ and $0, $1, etc.
    fn call_closure_for_trans(
        &mut self,
        closure: &Value,
        matched: &str,
        captures: Option<&[String]>,
    ) -> Result<String, RuntimeError> {
        // Build the topic ($_) value: if we have captures, create a Match object
        // so that $_[0] etc. work; otherwise just use the matched string.
        let topic = if let Some(caps) = captures {
            // Set positional captures ($0, $1, ...) as well
            for (idx, cap) in caps.iter().enumerate() {
                self.env_mut()
                    .insert(idx.to_string(), Value::str(cap.clone()));
            }
            Value::make_match_object_with_captures(
                0,
                matched.len() as i64,
                caps,
                &std::collections::HashMap::new(),
                crate::runtime::MatchTarget::new(matched),
            )
        } else {
            Value::str(matched.to_string())
        };

        // Set $_ to the matched text (or Match object) before calling the closure
        let old_topic = self.env().get("_").cloned();
        // A regex rule's closure also sees the match as `$/`.
        let old_match = if captures.is_some() {
            let old = self.env().get("/").cloned();
            self.env_mut().insert("/".to_string(), topic.clone());
            Some(old)
        } else {
            None
        };
        self.env_mut().insert("_".to_string(), topic);

        let result = self.call_sub_value(closure.clone(), vec![], true);
        if let Some(old) = old_match {
            match old {
                Some(old) => self.env_mut().insert("/".to_string(), old),
                None => self.env_mut().remove("/"),
            };
        }
        let result = result?;

        // Restore previous $_
        if let Some(old) = old_topic {
            self.env_mut().insert("_".to_string(), old);
        } else {
            self.env_mut().remove("_");
        }

        Ok(result.to_string_value())
    }

    // Cost: O(n * (k + t + g*m)), same shape as `apply_trans_rules`.
    fn apply_trans_complement(
        &mut self,
        text: &str,
        rules: &[TransRule],
        squash: bool,
        delete: bool,
    ) -> Result<String, RuntimeError> {
        let chars: Vec<char> = text.chars().collect();
        let token_chars = trans_token_chars(rules);
        let target = trans_regex_target(rules, &chars);
        let mut result = String::new();
        let mut last_was_complement = false;

        // Extract closure for complement replacement if the first rule has one.
        let complement_closure: Option<Value> = rules.first().and_then(|rule| match rule {
            TransRule::CharClosure { closure, .. } | TransRule::RegexClosure { closure, .. } => {
                Some(closure.clone())
            }
            _ => None,
        });

        let complement_replacement: String = if complement_closure.is_some() {
            String::new() // Will be computed dynamically
        } else {
            rules
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
                    TransRule::TokenClosureMap { .. } => String::new(),
                })
                .unwrap_or_default()
        };

        let mut i = 0;
        let mut byte_pos = 0;
        while i < chars.len() {
            let (grapheme_end_byte, grapheme_len) = grapheme_at(text, byte_pos);
            let mut in_rule = false;
            let mut best_len = 0;
            let mut best_end_byte = byte_pos;
            let mut best_original = String::new();

            for (rule, rule_tokens) in rules.iter().zip(&token_chars) {
                match rule {
                    TransRule::CharMap { from_chars, .. } => {
                        if grapheme_len == 1 && from_chars.contains(&chars[i])
                        {
                            in_rule = true;
                            best_len = 1;
                            best_end_byte = grapheme_end_byte;
                            best_original = chars[i].to_string();
                        }
                    }
                    TransRule::TokenMap { .. } | TransRule::TokenClosureMap { .. } => {
                        for token in rule_tokens {
                            let tlen = token.len();
                            if tlen > best_len
                                && chars[i..].starts_with(token)
                                && let Some(end_byte) =
                                    grapheme_end_after_chars(text, byte_pos, tlen)
                            {
                                in_rule = true;
                                best_len = tlen;
                                best_end_byte = end_byte;
                                best_original = token.iter().collect();
                            }
                        }
                    }
                    TransRule::Regex { pattern, .. } | TransRule::RegexClosure { pattern, .. } => {
                        if let Some(target) = &target
                            && let Some(caps) =
                                self.regex_match_with_captures_at_target(pattern, target, i)
                            && caps.from == i
                            && caps.to > i
                            && let Some(end_byte) =
                                grapheme_end_after_chars(text, byte_pos, caps.to - i)
                        {
                            in_rule = true;
                            best_len = caps.to - i;
                            best_end_byte = end_byte;
                            best_original = chars[i..caps.to].iter().collect();
                        }
                    }
                    TransRule::CharClosure { from_chars, .. } => {
                        if grapheme_len == 1 && from_chars.contains(&chars[i]) {
                            in_rule = true;
                            best_len = 1;
                            best_end_byte = grapheme_end_byte;
                            best_original = chars[i].to_string();
                        }
                    }
                }
            }

            if in_rule {
                result.push_str(&best_original);
                last_was_complement = false;
                i += best_len.max(1);
                byte_pos = best_end_byte;
            } else {
                let unmatched = &text[byte_pos..grapheme_end_byte];
                if delete {
                    // Skip grapheme.
                } else if squash {
                    if !last_was_complement {
                        if let Some(ref closure) = complement_closure {
                            let replacement =
                            self.call_closure_for_trans(closure, unmatched, None)?;
                            result.push_str(&replacement);
                        } else {
                            result.push_str(&complement_replacement);
                        }
                    }
                } else if let Some(ref closure) = complement_closure {
                    let replacement =
                        self.call_closure_for_trans(closure, unmatched, None)?;
                    result.push_str(&replacement);
                } else {
                    result.push_str(&complement_replacement);
                }
                last_was_complement = true;
                i += grapheme_len;
                byte_pos = grapheme_end_byte;
            }
        }

        Ok(result)
    }
}

/// The byte and char length of the grapheme beginning at a known boundary.
// Cost: O(g), g = chars in the grapheme.
fn grapheme_at(text: &str, from_byte: usize) -> (usize, usize) {
    let (start, grapheme) = crate::builtins::grapheme_index::Units::from(text, from_byte)
        .next()
        .expect("a loop position is before the end of the subject");
    (start + grapheme.len(), grapheme.chars().count())
}

/// The byte offset after `char_count` codepoints, if that offset is a
/// grapheme boundary.
// Cost: O(g), g = chars traversed to reach the requested boundary.
fn grapheme_end_after_chars(text: &str, from_byte: usize, char_count: usize) -> Option<usize> {
    if char_count == 0 {
        return Some(from_byte);
    }
    let mut consumed = 0;
    for (start, grapheme) in crate::builtins::grapheme_index::Units::from(text, from_byte) {
        consumed += grapheme.chars().count();
        if consumed == char_count {
            return Some(start + grapheme.len());
        }
        if consumed > char_count {
            return None;
        }
    }
    None
}

/// The token keys of every rule as char slices, one entry per rule (empty for a
/// rule without tokens), so each position compares them in place against
/// `chars[i..]` instead of copying the rest of the subject into a `String`.
// Cost: O(t), t = total chars of the token keys.
fn trans_token_chars(rules: &[TransRule]) -> Vec<Vec<Vec<char>>> {
    rules
        .iter()
        .map(|rule| match rule {
            TransRule::TokenMap { from_tokens, .. }
            | TransRule::TokenClosureMap { from_tokens, .. } => from_tokens
                .iter()
                .map(|token| token.chars().collect())
                .collect(),
            _ => Vec::new(),
        })
        .collect()
}

/// One `MatchTarget` for the whole call when any rule is a Regex key, shared by
/// the anchored attempt at every position (building it per position made a
/// Regex key O(n^2), #9142).
// Cost: O(n) when a Regex rule is present, O(r) otherwise, r = rules.
fn trans_regex_target(rules: &[TransRule], chars: &[char]) -> Option<crate::runtime::MatchTarget> {
    rules
        .iter()
        .any(|rule| {
            matches!(
                rule,
                TransRule::Regex { .. } | TransRule::RegexClosure { .. }
            )
        })
        .then(|| crate::runtime::MatchTarget::from_chars(chars))
}
