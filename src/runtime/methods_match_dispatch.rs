use super::*;

impl Interpreter {
    /// Dispatch .match method
    pub(super) fn dispatch_match_method(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Ok(Value::Nil);
        }
        let text = target.to_string_value();
        let mut overlap = false;
        let mut global = false;
        let mut anchored_pos: Option<usize> = None;
        let mut repeat_bounds: Option<(usize, Option<usize>)> = None;
        let mut pattern_arg: Option<&Value> = None;
        for arg in args {
            if let Value::Pair(key, value) = arg {
                if (key == "ov" || key == "overlap") && value.truthy() {
                    overlap = true;
                } else if key == "p" || key == "pos" {
                    anchored_pos = Some(value.to_f64() as usize);
                } else if (key == "g" || key == "global") && value.truthy() {
                    global = true;
                } else if key == "x" {
                    repeat_bounds = Self::parse_match_repeat_bounds(value);
                }
                continue;
            }
            if pattern_arg.is_none() {
                pattern_arg = Some(arg);
            }
        }
        let Some(pattern) = pattern_arg else {
            return Ok(Value::Nil);
        };
        let pat: String = match &pattern {
            Value::Regex(p) => p.to_string(),
            // Str patterns are treated as literal strings in Raku regex,
            // so wrap them in quotes to prevent regex metachar interpretation
            // (e.g., spaces are insignificant in Raku regex but significant in literals)
            Value::Str(p) => {
                // Escape any single quotes in the pattern, then wrap in quotes
                let escaped = p.replace('\'', "\\'");
                format!("'{}'", escaped)
            }
            _ => return Ok(Value::Nil),
        };
        if global || overlap || repeat_bounds.is_some() {
            let all = self.regex_match_all_with_captures(&pat, &text);
            let mut selected = if overlap {
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
            } else {
                self.select_non_overlapping_matches(all)
            };
            if let Some((min_required, max_to_return)) = repeat_bounds {
                let Some(restricted) =
                    Self::select_matches_by_repeat_bounds(selected, min_required, max_to_return)
                else {
                    self.env.insert("/".to_string(), Value::Nil);
                    return Ok(Value::array(Vec::new()));
                };
                selected = restricted;
            }
            if selected.is_empty() {
                self.env.insert("/".to_string(), Value::Nil);
                return Ok(Value::array(Vec::new()));
            }
            let matches: Vec<Value> = selected
                .iter()
                .map(|c| {
                    Value::make_match_object_full(
                        c.matched.clone(),
                        c.from as i64,
                        c.to as i64,
                        &c.positional,
                        &c.named,
                        &c.named_subcaps,
                        &c.positional_subcaps,
                        &c.positional_quantified,
                        Some(&text),
                    )
                })
                .collect();
            let result = Value::array(matches);
            self.env.insert("/".to_string(), result.clone());
            return Ok(result);
        }
        // Use anchored match if :p(N) or :pos(N) is specified
        let captures = if let Some(pos) = anchored_pos {
            self.regex_match_with_captures_at(&pat, &text, pos)
        } else {
            self.regex_match_with_captures(&pat, &text)
        };
        if let Some(captures) = captures {
            let matched = captures.matched.clone();
            let from = captures.from as i64;
            let to = captures.to as i64;
            // Execute code blocks from regex for side effects
            self.execute_regex_code_blocks(&captures.code_blocks);
            let match_obj = Value::make_match_object_full(
                matched,
                from,
                to,
                &captures.positional,
                &captures.named,
                &captures.named_subcaps,
                &captures.positional_subcaps,
                &captures.positional_quantified,
                Some(&text),
            );
            // Set positional capture env vars ($0, $1, ...) from match object
            if let Value::Instance { ref attributes, .. } = match_obj
                && let Some(Value::Array(list, _)) = attributes.get("list")
            {
                for (i, v) in list.iter().enumerate() {
                    self.env.insert(i.to_string(), v.clone());
                }
            }
            // Set named capture env vars from match object
            if let Value::Instance { ref attributes, .. } = match_obj
                && let Some(Value::Hash(named_hash)) = attributes.get("named")
            {
                for (k, v) in named_hash.iter() {
                    self.env.insert(format!("<{}>", k), v.clone());
                }
            }
            self.env.insert("/".to_string(), match_obj.clone());
            Ok(match_obj)
        } else {
            Ok(Value::Nil)
        }
    }
}
