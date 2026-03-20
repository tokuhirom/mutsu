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
        let mut continue_pos: Option<usize> = None;
        let mut repeat_bounds: Option<(usize, Option<usize>)> = None;
        let mut nth_arg: Option<Value> = None;
        let mut pattern_arg: Option<&Value> = None;
        for arg in args {
            if let Value::Pair(key, value) = arg {
                if (key == "ov" || key == "overlap") && value.truthy() {
                    overlap = true;
                } else if key == "p" || key == "pos" {
                    anchored_pos = Some(value.to_f64() as usize);
                } else if key == "c" || key == "continue" {
                    continue_pos = Some(value.to_f64() as usize);
                } else if (key == "g" || key == "global") && value.truthy() {
                    global = true;
                } else if key == "x" {
                    repeat_bounds = Self::parse_match_repeat_bounds(value);
                } else if key == "nth" {
                    nth_arg = Some(*value.clone());
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
        if global || overlap || repeat_bounds.is_some() || nth_arg.is_some() {
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
            // Apply :nth filtering before :x bounds
            if let Some(ref nth_val) = nth_arg {
                let indices = self.resolve_nth_value_indices(nth_val, selected.len())?;
                selected = indices
                    .into_iter()
                    .filter_map(|i| {
                        if i >= 1 && i <= selected.len() {
                            Some(selected[i - 1].clone())
                        } else {
                            None
                        }
                    })
                    .collect();
            }
            if let Some((min_required, max_to_return)) = repeat_bounds {
                let Some(restricted) =
                    Self::select_matches_by_repeat_bounds(selected, min_required, max_to_return)
                else {
                    self.env.insert("/".to_string(), Value::Nil);
                    return Ok(Value::Nil);
                };
                selected = restricted;
            }
            if selected.is_empty() {
                self.env.insert("/".to_string(), Value::Nil);
                // :nth with a single value returns Nil, not empty array
                if nth_arg.is_some() && repeat_bounds.is_none() && !global && !overlap {
                    return Ok(Value::Nil);
                }
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
            // :nth with a single Int returns a single Match, not an array
            if let Some(ref nth_val) = nth_arg
                && Self::is_single_nth(nth_val)
                && repeat_bounds.is_none()
            {
                if matches.len() == 1 {
                    let result = matches.into_iter().next().unwrap();
                    self.env.insert("/".to_string(), result.clone());
                    return Ok(result);
                } else {
                    self.env.insert("/".to_string(), Value::Nil);
                    return Ok(Value::Nil);
                }
            }
            let result = Value::array(matches);
            self.env.insert("/".to_string(), result.clone());
            return Ok(result);
        }
        // Use anchored match if :p(N) or :pos(N) is specified,
        // or search-from if :c(N) or :continue(N) is specified
        let captures = if let Some(pos) = anchored_pos {
            self.regex_match_with_captures_at(&pat, &text, pos)
        } else if let Some(pos) = continue_pos {
            self.regex_match_with_captures_from(&pat, &text, pos)
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

    /// Check if an :nth argument is a single integer (not a list or range)
    fn is_single_nth(val: &Value) -> bool {
        matches!(val, Value::Int(_) | Value::Num(_))
    }

    /// Resolve :nth argument (Value) into a list of 1-based indices.
    /// Validates that values are positive and monotonically increasing.
    fn resolve_nth_value_indices(
        &mut self,
        val: &Value,
        total_matches: usize,
    ) -> Result<Vec<usize>, RuntimeError> {
        let mut indices = Vec::new();
        match val {
            Value::Int(n) => {
                Self::validate_nth_value(*n)?;
                indices.push(*n as usize);
            }
            Value::Num(n) => {
                let i = *n as i64;
                Self::validate_nth_value(i)?;
                indices.push(i as usize);
            }
            Value::Array(items, _) | Value::Seq(items) | Value::Slip(items) => {
                // Check if any item is a LazyList (indicates partially-evaluated sequence)
                let has_lazy = items.iter().any(|v| matches!(v, Value::LazyList(_)));
                if has_lazy {
                    // Array contains lazy list items (e.g., from sequence operator
                    // `2, 4 ... *` stored as `[Int(2), LazyList(...)]`).
                    // Collect all non-lazy prefix items and peek into lazy list
                    // to detect the arithmetic step, then generate indices.
                    let mut seed_values: Vec<i64> = Vec::new();
                    let mut lazy_ref: Option<&crate::value::LazyList> = None;
                    for item in items.iter() {
                        if let Value::LazyList(ll) = item {
                            lazy_ref = Some(ll.as_ref());
                            // Peek at the first cached element to get the next seed
                            if let Some(cached) = ll.cache.lock().unwrap().as_ref() {
                                for cv in cached.iter() {
                                    if let Ok(v) = Self::value_to_nth_int(cv) {
                                        seed_values.push(v);
                                        break;
                                    }
                                }
                            }
                            break;
                        }
                        seed_values.push(Self::value_to_nth_int(item)?);
                    }
                    if seed_values.len() >= 2 {
                        // Arithmetic sequence detected
                        let step = seed_values[1] - seed_values[0];
                        let mut last: i64 = 0;
                        let mut current = seed_values[0];
                        loop {
                            if current <= 0 {
                                return Err(RuntimeError::new(format!(
                                    "Unexpected value ({}) for :nth; must be a positive integer",
                                    current
                                )));
                            }
                            if current <= last {
                                return Err(RuntimeError::new(format!(
                                    "Unexpected value ({}) for :nth; must be greater than {}",
                                    current, last
                                )));
                            }
                            if (current as usize) > total_matches {
                                break;
                            }
                            last = current;
                            indices.push(current as usize);
                            current += step;
                        }
                    } else if !seed_values.is_empty() {
                        // Only one seed value — force the lazy list
                        Self::validate_nth_value(seed_values[0])?;
                        indices.push(seed_values[0] as usize);
                        if let Some(ll) = lazy_ref {
                            let forced = self.force_lazy_list(ll)?;
                            let filtered: Vec<Value> = forced
                                .into_iter()
                                .filter(|v| !matches!(v, Value::LazyList(_)))
                                .collect();
                            Self::collect_nth_list_indices(&filtered, total_matches, &mut indices)?;
                        }
                    }
                } else {
                    Self::collect_nth_list_indices(items, total_matches, &mut indices)?;
                }
            }
            Value::Range(start, end) => {
                Self::validate_nth_value(*start)?;
                let effective_end = (*end as usize).min(total_matches) as i64;
                for i in *start..=effective_end {
                    if i > 0 {
                        indices.push(i as usize);
                    }
                }
            }
            Value::RangeExcl(start, end) => {
                Self::validate_nth_value(*start)?;
                let effective_end = (*end as usize).min(total_matches + 1) as i64;
                for i in *start..effective_end {
                    if i > 0 && (i as usize) <= total_matches {
                        indices.push(i as usize);
                    }
                }
            }
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                let s = Self::value_to_nth_int(start)?;
                let actual_start = if *excl_start { s + 1 } else { s };
                Self::validate_nth_value(actual_start)?;
                // Check for infinite end
                if matches!(end.as_ref(), Value::Num(n) if n.is_infinite() && *n > 0.0)
                    || matches!(end.as_ref(), Value::Whatever)
                {
                    for i in actual_start..=(total_matches as i64) {
                        if i > 0 {
                            indices.push(i as usize);
                        }
                    }
                } else {
                    let e = Self::value_to_nth_int(end)?;
                    let actual_end = if *excl_end { e - 1 } else { e };
                    for i in actual_start..=actual_end {
                        if i > 0 && (i as usize) <= total_matches {
                            indices.push(i as usize);
                        }
                    }
                }
            }
            Value::LazyList(ll) => {
                let forced = self.force_lazy_list_bridge(ll)?;
                Self::collect_nth_list_indices(&forced, total_matches, &mut indices)?;
            }
            _ => {
                let i = Self::value_to_nth_int(val)?;
                Self::validate_nth_value(i)?;
                indices.push(i as usize);
            }
        }
        Ok(indices)
    }

    /// Collect nth indices from a list of values, validating monotonicity.
    fn collect_nth_list_indices(
        items: &[Value],
        total_matches: usize,
        indices: &mut Vec<usize>,
    ) -> Result<(), RuntimeError> {
        let mut last: i64 = 0;
        for item in items.iter() {
            let i = Self::value_to_nth_int(item)?;
            if i <= 0 {
                return Err(RuntimeError::new(format!(
                    "Unexpected value ({}) for :nth; must be a positive integer",
                    i
                )));
            }
            if i <= last {
                return Err(RuntimeError::new(format!(
                    "Unexpected value ({}) for :nth; must be greater than {}",
                    i, last
                )));
            }
            // Stop if we've gone past total matches
            if (i as usize) > total_matches {
                break;
            }
            last = i;
            indices.push(i as usize);
        }
        Ok(())
    }

    fn validate_nth_value(n: i64) -> Result<(), RuntimeError> {
        if n <= 0 {
            return Err(RuntimeError::new(format!(
                "Unexpected value ({}) for :nth; must be a positive integer",
                n
            )));
        }
        Ok(())
    }

    fn value_to_nth_int(val: &Value) -> Result<i64, RuntimeError> {
        match val {
            Value::Int(n) => Ok(*n),
            Value::Num(n) => Ok(*n as i64),
            _ => {
                let s = val.to_string_value();
                s.parse::<i64>().map_err(|_| {
                    RuntimeError::new(format!("Cannot convert '{}' to integer for :nth", s))
                })
            }
        }
    }
}
