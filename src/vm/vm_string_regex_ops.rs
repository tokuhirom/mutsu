use super::*;
use crate::symbol::Symbol;

/// Apply samemark on a per-word basis: split both source and target by whitespace,
/// apply samemark to each word pair, then reassemble with the replacement's whitespace.
fn samemark_per_word(target: &str, source: &str) -> String {
    let src_words: Vec<&str> = source.split_whitespace().collect();
    if src_words.is_empty() {
        return target.to_string();
    }

    // Split target into words and whitespace segments
    let mut result = String::new();
    let mut word_idx = 0;
    let mut chars = target.chars().peekable();
    while chars.peek().is_some() {
        // Collect leading whitespace
        let mut ws = String::new();
        while let Some(&ch) = chars.peek() {
            if ch.is_whitespace() {
                ws.push(ch);
                chars.next();
            } else {
                break;
            }
        }
        result.push_str(&ws);
        // Collect word
        let mut word = String::new();
        while let Some(&ch) = chars.peek() {
            if ch.is_whitespace() {
                break;
            }
            word.push(ch);
            chars.next();
        }
        if !word.is_empty() {
            let src_word = if word_idx < src_words.len() {
                src_words[word_idx]
            } else {
                src_words.last().unwrap()
            };
            result.push_str(&crate::builtins::samemark_string(&word, src_word));
            word_idx += 1;
        }
    }
    result
}

fn normalize_subst_replacement(template: &str) -> String {
    let mut out = String::new();
    let mut chars = template.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch != '\\' {
            out.push(ch);
            continue;
        }
        let Some(next) = chars.peek().copied() else {
            out.push('\\');
            continue;
        };
        match next {
            '\\' => {
                out.push('\\');
                chars.next();
            }
            '&' => {
                out.push('&');
                chars.next();
            }
            _ => out.push('\\'),
        }
    }
    out
}

impl VM {
    fn canonical_infix_lookup_name(name: &str) -> std::borrow::Cow<'_, str> {
        if name == "(+)" {
            return std::borrow::Cow::Borrowed("+");
        }
        std::borrow::Cow::Borrowed(name)
    }

    fn should_retry_with_canonical_infix_name(name: &str) -> bool {
        matches!(
            name,
            "(<=)" | "⊆" | "(>=)" | "⊇" | "(<)" | "⊂" | "(>)" | "⊃" | "⊈" | "⊉" | "⊄" | "⊅"
        )
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_subst_op(
        &mut self,
        code: &CompiledCode,
        pattern_idx: u32,
        replacement_idx: u32,
        samemark: bool,
        global: bool,
        nth_idx: Option<u32>,
        x_count: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let pattern = Self::const_str(code, pattern_idx).to_string();
        let replacement = normalize_subst_replacement(Self::const_str(code, replacement_idx));
        let nth_spec = nth_idx.map(|idx| Self::const_str(code, idx).to_string());
        let x_count = x_count.map(|n| n as usize);
        let target = self
            .interpreter
            .env()
            .get("_")
            .cloned()
            .unwrap_or(Value::Nil);
        let text = target.to_string_value();

        if nth_spec.is_none() && x_count.is_none() && !global {
            if let Some((start, end)) = self.interpreter.regex_find_first(&pattern, &text) {
                let out = Self::apply_substitutions(&text, &[(start, end)], &replacement, samemark);
                let result = Value::str(out);
                self.interpreter
                    .env_mut()
                    .insert("_".to_string(), result.clone());
                self.interpreter.env_mut().insert("$_".to_string(), result);
                self.stack.push(Value::Bool(true));
            } else {
                self.stack.push(Value::Nil);
            }
            return Ok(());
        }

        let all_matches = self.interpreter.regex_find_all(&pattern, &text);
        let ranges = if global && nth_spec.is_none() && x_count.is_none() {
            all_matches
        } else {
            Self::select_substitution_ranges(&all_matches, nth_spec.as_deref(), x_count)?
        };
        if ranges.is_empty() {
            self.stack.push(Value::Nil);
            return Ok(());
        }

        let out = Self::apply_substitutions(&text, &ranges, &replacement, samemark);
        let result = Value::str(out);
        self.interpreter
            .env_mut()
            .insert("_".to_string(), result.clone());
        self.interpreter.env_mut().insert("$_".to_string(), result);
        // Push Bool::True so `$x ~~ s///` returns True on match
        self.stack.push(Value::Bool(true));
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_non_destructive_subst_op(
        &mut self,
        code: &CompiledCode,
        pattern_idx: u32,
        replacement_idx: u32,
        samemark: bool,
        global: bool,
        nth_idx: Option<u32>,
        x_count: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let pattern = Self::const_str(code, pattern_idx).to_string();
        let replacement = normalize_subst_replacement(Self::const_str(code, replacement_idx));
        let nth_spec = nth_idx.map(|idx| Self::const_str(code, idx).to_string());
        let x_count = x_count.map(|n| n as usize);
        let target = self
            .interpreter
            .env()
            .get("_")
            .cloned()
            .unwrap_or(Value::Nil);
        let text = target.to_string_value();

        if nth_spec.is_none() && x_count.is_none() && !global {
            if let Some((start, end)) = self.interpreter.regex_find_first(&pattern, &text) {
                let out = Self::apply_substitutions(&text, &[(start, end)], &replacement, samemark);
                self.stack.push(Value::str(out));
            } else {
                self.stack.push(Value::str(text));
            }
            return Ok(());
        }

        let all_matches = self.interpreter.regex_find_all(&pattern, &text);
        let ranges = if global && nth_spec.is_none() && x_count.is_none() {
            all_matches
        } else {
            Self::select_substitution_ranges(&all_matches, nth_spec.as_deref(), x_count)?
        };
        if ranges.is_empty() {
            self.stack.push(Value::str(text));
            return Ok(());
        }
        let out = Self::apply_substitutions(&text, &ranges, &replacement, samemark);
        self.stack.push(Value::str(out));
        Ok(())
    }

    fn select_substitution_ranges(
        all_matches: &[(usize, usize)],
        nth_spec: Option<&str>,
        x_count: Option<usize>,
    ) -> Result<Vec<(usize, usize)>, RuntimeError> {
        if let Some(raw) = nth_spec {
            let n = Self::parse_subst_nth_spec(raw)?;
            if n == 0 {
                return Err(RuntimeError::new("Invalid :nth index (must be >= 1)"));
            }
            if n > all_matches.len() {
                return Ok(Vec::new());
            }
            return Ok(vec![all_matches[n - 1]]);
        }
        if let Some(n) = x_count {
            if n == 0 {
                return Ok(Vec::new());
            }
            if all_matches.len() < n {
                return Ok(Vec::new());
            }
            return Ok(all_matches.iter().copied().take(n).collect());
        }
        Ok(all_matches.first().copied().into_iter().collect())
    }

    fn parse_subst_nth_spec(raw: &str) -> Result<usize, RuntimeError> {
        let token = raw.trim();
        if token.eq_ignore_ascii_case("-Inf") {
            return Err(RuntimeError::new("Invalid :nth index (-Inf)"));
        }
        let n = token
            .parse::<i64>()
            .map_err(|_| RuntimeError::new(format!("Invalid :nth index ({token})")))?;
        if n <= 0 {
            return Err(RuntimeError::new(format!("Invalid :nth index ({token})")));
        }
        Ok(n as usize)
    }

    fn apply_substitutions(
        text: &str,
        ranges: &[(usize, usize)],
        replacement: &str,
        samemark: bool,
    ) -> String {
        let mut out = String::new();
        let mut prev_end_b = 0usize;
        for (start, end) in ranges {
            let start_b = runtime::char_idx_to_byte(text, *start);
            let end_b = runtime::char_idx_to_byte(text, *end);
            out.push_str(&text[prev_end_b..start_b]);
            let matched_text = &text[start_b..end_b];
            let repl = if samemark {
                if matched_text.contains(char::is_whitespace)
                    && replacement.contains(char::is_whitespace)
                {
                    samemark_per_word(replacement, matched_text)
                } else {
                    crate::builtins::samemark_string(replacement, matched_text)
                }
            } else {
                replacement.to_string()
            };
            out.push_str(&repl);
            prev_end_b = end_b;
        }
        out.push_str(&text[prev_end_b..]);
        out
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_transliterate_op(
        &mut self,
        code: &CompiledCode,
        from_idx: u32,
        to_idx: u32,
        delete: bool,
        complement: bool,
        squash: bool,
        non_destructive: bool,
    ) -> Result<(), RuntimeError> {
        let from = Self::const_str(code, from_idx);
        let to = Self::const_str(code, to_idx);
        let target = self
            .interpreter
            .env()
            .get("_")
            .cloned()
            .unwrap_or(Value::Nil);
        let text = target.to_string_value();

        let translated = crate::builtins::transliterate::transliterate(
            &text, from, to, delete, squash, complement,
        );
        let result = Value::str(translated);

        // tr/// (lowercase) always modifies $_; TR/// (uppercase) only modifies
        // $_ in smartmatch context (so that $var ~~ TR/// writes back to $var).
        if !non_destructive || self.in_smartmatch_rhs {
            self.interpreter
                .env_mut()
                .insert("_".to_string(), result.clone());
        }
        // Signal to the smartmatch handler that this is a transliterate result
        // so it returns the result directly (as StrDistance) instead of comparing.
        if self.in_smartmatch_rhs {
            self.transliterate_in_smartmatch = true;
        }
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_hyper_op(
        &mut self,
        code: &CompiledCode,
        op_idx: u32,
        dwim_left: bool,
        dwim_right: bool,
    ) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap_or(Value::Nil);
        let left = self.stack.pop().unwrap_or(Value::Nil);
        let op = Self::const_str(code, op_idx).to_string();
        let left_list = Interpreter::value_to_list(&left);
        let right_list = Interpreter::value_to_list(&right);
        let left_len = left_list.len();
        let right_len = right_list.len();
        if left_len == 0 && right_len == 0 {
            self.stack.push(Value::array(Vec::new()));
            return Ok(());
        }
        let result_len = if !dwim_left && !dwim_right {
            if left_len != right_len {
                return Err(RuntimeError::new(format!(
                    "Non-dwimmy hyper operator: left has {} elements, right has {}",
                    left_len, right_len
                )));
            }
            left_len
        } else if dwim_left && dwim_right {
            std::cmp::max(left_len, right_len)
        } else if dwim_right {
            left_len
        } else {
            right_len
        };
        let is_smartmatch = op == "~~";
        let mut results = Vec::with_capacity(result_len);
        for i in 0..result_len {
            let l = if left_len == 0 {
                &Value::Int(0)
            } else {
                &left_list[i % left_len]
            };
            let r = if right_len == 0 {
                &Value::Int(0)
            } else {
                &right_list[i % right_len]
            };
            if is_smartmatch {
                results.push(Value::Bool(self.interpreter.smart_match_values(l, r)));
            } else {
                results.push(self.eval_reduction_operator_values(&op, l, r)?);
            }
        }
        let result = Value::array(results);
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_meta_op(
        &mut self,
        code: &CompiledCode,
        meta_idx: u32,
        op_idx: u32,
    ) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap_or(Value::Nil);
        let left = self.stack.pop().unwrap_or(Value::Nil);
        let meta = Self::const_str(code, meta_idx).to_string();
        let op = Self::const_str(code, op_idx).to_string();
        let result = match meta.as_str() {
            "R" => {
                if op == "..." || op == "...^" {
                    let exclude_end = op == "...^";
                    self.interpreter
                        .eval_sequence_values(right, left, exclude_end)?
                } else if op == "~~" {
                    Value::Bool(self.interpreter.smart_match_values(&right, &left))
                } else {
                    self.eval_reduction_operator_values(&op, &right, &left)?
                }
            }
            "X" => {
                let value_is_lazy = |v: &Value| match v {
                    Value::LazyList(_) => true,
                    Value::Range(_, end)
                    | Value::RangeExcl(_, end)
                    | Value::RangeExclStart(_, end)
                    | Value::RangeExclBoth(_, end) => *end == i64::MAX,
                    Value::GenericRange { end, .. } => {
                        let end_f = end.to_f64();
                        end_f.is_infinite() && end_f.is_sign_positive()
                    }
                    _ => false,
                };
                let lazy_inputs = value_is_lazy(&left) || value_is_lazy(&right);
                let lazy_limit = 256usize;
                let materialize_side = |v: &Value| -> Vec<Value> {
                    if value_is_lazy(v) {
                        let iter = ZipIter::from_value(v);
                        let len = iter.len().min(lazy_limit);
                        (0..len).map(|i| iter.nth(i)).collect()
                    } else {
                        runtime::value_to_list(v)
                    }
                };
                let left_list = materialize_side(&left);
                let right_list = materialize_side(&right);
                let mut results = Vec::new();
                if op.is_empty() || op == "," {
                    for l in &left_list {
                        for r in &right_list {
                            results.push(Value::array(vec![l.clone(), r.clone()]));
                        }
                    }
                } else if op == "~~" {
                    for l in &left_list {
                        for r in &right_list {
                            results.push(Value::Bool(self.interpreter.smart_match_values(l, r)));
                        }
                    }
                } else {
                    for l in &left_list {
                        for r in &right_list {
                            results.push(self.eval_reduction_operator_values(&op, l, r)?);
                        }
                    }
                }
                if lazy_inputs {
                    Value::LazyList(std::sync::Arc::new(crate::value::LazyList {
                        body: Vec::new(),
                        env: crate::env::Env::new(),
                        cache: std::sync::Mutex::new(Some(results)),
                    }))
                } else if results.is_empty() {
                    Value::Seq(std::sync::Arc::new(Vec::new()))
                } else {
                    Value::array(results)
                }
            }
            "Z" => {
                // Use lazy index-based iteration for ranges to avoid
                // materializing huge/infinite lists like 1..*.
                let left_iter = ZipIter::from_value(&left);
                let right_iter = ZipIter::from_value(&right);
                let len = left_iter.len().min(right_iter.len()).min(MAX_ZIP_EXPAND);
                let mut results = Vec::new();
                if op.is_empty() || op == "," {
                    for i in 0..len {
                        results.push(Value::array(vec![left_iter.nth(i), right_iter.nth(i)]));
                    }
                } else if op == "=>" {
                    for i in 0..len {
                        let key = left_iter.nth(i).to_string_value();
                        results.push(Value::Pair(key, Box::new(right_iter.nth(i))));
                    }
                } else {
                    // Check for 3-way zip reduction case ([Z+] a, b, c)
                    // where left has exactly 2 elements and the second is a list.
                    let nested_left = if left_iter.len() == 2 {
                        let second = left_iter.nth(1);
                        match &second {
                            Value::Array(..) | Value::Seq(_) | Value::Slip(_) => {
                                Some((left_iter.nth(0), runtime::value_to_list(&second)))
                            }
                            _ => None,
                        }
                    } else {
                        None
                    };
                    for i in 0..len {
                        if let Some((ref first, ref extra)) = nested_left {
                            let mut v = self.eval_reduction_operator_values(
                                &op,
                                first,
                                &right_iter.nth(i),
                            )?;
                            if let Some(extra_i) = extra.get(i) {
                                v = self.eval_reduction_operator_values(&op, &v, extra_i)?;
                            }
                            results.push(v);
                        } else {
                            results.push(self.eval_reduction_operator_values(
                                &op,
                                &left_iter.nth(i),
                                &right_iter.nth(i),
                            )?);
                        }
                    }
                }
                Value::array(results)
            }
            "!" => {
                let inner = self.eval_reduction_operator_values(&op, &left, &right)?;
                Value::Bool(!inner.truthy())
            }
            _ => {
                return Err(RuntimeError::new(format!(
                    "Unknown meta operator: {}",
                    meta
                )));
            }
        };
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_infix_func_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        right_arity: u32,
        modifier_idx: &Option<u32>,
    ) -> Result<(), RuntimeError> {
        let arity = right_arity as usize;
        let mut right_vals: Vec<Value> = Vec::with_capacity(arity);
        for _ in 0..arity {
            right_vals.push(self.stack.pop().unwrap_or(Value::Nil));
        }
        right_vals.reverse();
        let left_val = self.stack.pop().unwrap_or(Value::Nil);
        let name = Self::const_str(code, name_idx).to_string();
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx).to_string());
        let result = if name == "atan2" {
            let mut x = right_vals
                .first()
                .and_then(runtime::to_float_value)
                .unwrap_or(0.0);
            let mut y = runtime::to_float_value(&left_val).unwrap_or(0.0);
            if modifier.as_deref() == Some("R") {
                std::mem::swap(&mut x, &mut y);
            }
            Value::Num(y.atan2(x))
        } else if name == "sprintf" {
            let fmt = match &left_val {
                Value::Str(s) => s.to_string(),
                _ => String::new(),
            };
            if modifier.as_deref() == Some("X") {
                let mut parts = Vec::new();
                for val in &right_vals {
                    parts.push(runtime::format_sprintf(&fmt, Some(val)));
                }
                Value::str(parts.join(" "))
            } else {
                let arg = right_vals.first();
                let rendered = runtime::format_sprintf(&fmt, arg);
                Value::str(rendered)
            }
        } else {
            let mut call_args = vec![left_val.clone()];
            call_args.extend(right_vals.clone());
            if modifier.as_deref() == Some("R") && call_args.len() == 2 {
                call_args.swap(0, 1);
            }
            let lookup_name = Self::canonical_infix_lookup_name(&name);
            let infix_name = format!("infix:<{}>", lookup_name.as_ref());
            let assoc = self
                .interpreter
                .infix_associativity(&infix_name)
                .unwrap_or_else(|| "left".to_string());
            if assoc == "chain" && call_args.len() > 2 {
                let mut all_true = true;
                for pair in call_args.windows(2) {
                    let left = pair[0].clone();
                    let right = pair[1].clone();
                    let pair_result =
                        if let Some(v) = self.try_user_infix(&infix_name, &left, &right)? {
                            v
                        } else {
                            self.call_infix_fallback(
                                lookup_name.as_ref(),
                                Some(&infix_name),
                                vec![left, right],
                            )?
                        };
                    if !pair_result.truthy() {
                        all_true = false;
                        break;
                    }
                }
                Value::Bool(all_true)
            } else if call_args.len() == 2 {
                let right_val = right_vals.first().cloned().unwrap_or(Value::Nil);
                if let Some(result) = self.try_user_infix(&infix_name, &left_val, &right_val)? {
                    result
                } else {
                    self.call_infix_fallback(lookup_name.as_ref(), Some(&infix_name), call_args)?
                }
            } else {
                self.call_infix_fallback(lookup_name.as_ref(), Some(&infix_name), call_args)?
            }
        };
        self.stack.push(result);
        Ok(())
    }

    fn flip_flop_scope_key(&self) -> String {
        if let Some(Value::Int(id)) = self.interpreter.env().get("__mutsu_callable_id") {
            return format!("callable:{id}");
        }
        if let Some((package, name)) = self.interpreter.routine_stack_top() {
            return format!("routine:{package}::{name}");
        }
        "top".to_string()
    }

    fn flip_flop_operand_truthy(&mut self, value: &Value, is_rhs: bool) -> bool {
        match value {
            Value::Whatever => !is_rhs,
            Value::Regex(_)
            | Value::RegexWithAdverbs { .. }
            | Value::Routine { is_regex: true, .. } => {
                let topic = self
                    .interpreter
                    .env()
                    .get("_")
                    .cloned()
                    .unwrap_or(Value::Nil);
                self.interpreter.smart_match_values(&topic, value)
            }
            _ => value.truthy(),
        }
    }

    fn eval_expr_range(
        &mut self,
        code: &CompiledCode,
        start: usize,
        end: usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        let saved_depth = self.stack.len();
        self.run_range(code, start, end, compiled_fns)?;
        let value = self.stack.pop().unwrap_or(Value::Nil);
        self.stack.truncate(saved_depth);
        Ok(value)
    }

    fn flip_flop_step(
        &mut self,
        key: &str,
        lhs: bool,
        rhs: bool,
        exclude_start: bool,
        exclude_end: bool,
        is_fff: bool,
    ) -> Value {
        let seq = self
            .interpreter
            .get_state_var(key)
            .and_then(|v| match v {
                Value::Int(i) if *i > 0 => Some(*i),
                _ => None,
            })
            .unwrap_or(0);

        if seq > 0 {
            let current = seq;
            if rhs {
                self.interpreter
                    .set_state_var(key.to_string(), Value::Int(0));
                if exclude_end {
                    Value::Nil
                } else {
                    Value::Int(current)
                }
            } else {
                self.interpreter
                    .set_state_var(key.to_string(), Value::Int(current + 1));
                Value::Int(current)
            }
        } else if lhs {
            if !is_fff && rhs {
                self.interpreter
                    .set_state_var(key.to_string(), Value::Int(0));
                if exclude_start || exclude_end {
                    Value::Nil
                } else {
                    Value::Int(1)
                }
            } else {
                self.interpreter
                    .set_state_var(key.to_string(), Value::Int(2));
                if exclude_start {
                    Value::Nil
                } else {
                    Value::Int(1)
                }
            }
        } else {
            Value::Nil
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_flip_flop_expr_op(
        &mut self,
        code: &CompiledCode,
        ip: &mut usize,
        lhs_end: u32,
        rhs_end: u32,
        site_id: u64,
        exclude_start: bool,
        exclude_end: bool,
        is_fff: bool,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let lhs_start = *ip + 1;
        let lhs_end = lhs_end as usize;
        let rhs_start = lhs_end;
        let rhs_end = rhs_end as usize;

        if self.in_smartmatch_rhs {
            let lhs_pattern = self.eval_expr_range(code, lhs_start, lhs_end, compiled_fns)?;
            let rhs_pattern = self.eval_expr_range(code, rhs_start, rhs_end, compiled_fns)?;
            let scope = self.flip_flop_scope_key();
            let matcher_key = format!("__mutsu_ff_state::{scope}::{site_id}");
            let mut map = std::collections::HashMap::new();
            map.insert("__mutsu_ff_matcher".to_string(), Value::Bool(true));
            map.insert("key".to_string(), Value::str(matcher_key));
            map.insert("lhs".to_string(), lhs_pattern);
            map.insert("rhs".to_string(), rhs_pattern);
            map.insert("exclude_start".to_string(), Value::Bool(exclude_start));
            map.insert("exclude_end".to_string(), Value::Bool(exclude_end));
            map.insert("is_fff".to_string(), Value::Bool(is_fff));
            self.stack.push(Value::hash(map));
            *ip = rhs_end;
            return Ok(());
        }

        let scope = self.flip_flop_scope_key();
        let state_key = format!("__mutsu_ff_state::{scope}::{site_id}");
        let seq = self
            .interpreter
            .get_state_var(&state_key)
            .and_then(|v| match v {
                Value::Int(i) if *i > 0 => Some(*i),
                _ => None,
            })
            .unwrap_or(0);

        let (lhs, rhs) = if seq > 0 {
            if !is_fff {
                let _ = self.eval_expr_range(code, lhs_start, lhs_end, compiled_fns)?;
            }
            let rhs_value = self.eval_expr_range(code, rhs_start, rhs_end, compiled_fns)?;
            (false, self.flip_flop_operand_truthy(&rhs_value, true))
        } else {
            let lhs_value = self.eval_expr_range(code, lhs_start, lhs_end, compiled_fns)?;
            if !self.flip_flop_operand_truthy(&lhs_value, false) {
                (false, false)
            } else if is_fff {
                (true, false)
            } else {
                let rhs_value = self.eval_expr_range(code, rhs_start, rhs_end, compiled_fns)?;
                (true, self.flip_flop_operand_truthy(&rhs_value, true))
            }
        };

        let value = self.flip_flop_step(&state_key, lhs, rhs, exclude_start, exclude_end, is_fff);
        self.stack.push(value);
        *ip = rhs_end;
        Ok(())
    }

    fn call_infix_fallback(
        &mut self,
        name: &str,
        infix_name: Option<&str>,
        call_args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // When an infix operator is called with a single Iterable argument,
        // flatten it into elements (like a +@foo slurpy) and reduce over them.
        let call_args = if call_args.len() == 1 {
            match &call_args[0] {
                Value::Hash(map) => {
                    // Break Hash into Pairs
                    map.iter()
                        .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                        .collect::<Vec<_>>()
                }
                Value::Array(items, ..) => items.iter().cloned().collect(),
                _ => call_args,
            }
        } else {
            call_args
        };
        if call_args.len() >= 2 {
            let mut acc = call_args[0].clone();
            let mut reduced = true;
            for rhs in &call_args[1..] {
                match crate::runtime::Interpreter::apply_reduction_op(name, &acc, rhs) {
                    Ok(value) => acc = value,
                    Err(_) => {
                        reduced = false;
                        break;
                    }
                }
            }
            if reduced {
                return Ok(acc);
            }
        }
        if let Some(op_name) = infix_name
            && let Ok(v) = self
                .interpreter
                .call_user_routine_direct(op_name, call_args.clone())
        {
            return Ok(v);
        }
        if Self::should_retry_with_canonical_infix_name(name)
            && let Some(op_name) = infix_name
            && let Ok(v) = self.interpreter.call_function(op_name, call_args.clone())
        {
            return Ok(v);
        }
        match self.interpreter.call_function(name, call_args.clone()) {
            Ok(v) => Ok(v),
            Err(err) => {
                // `for foo-bar() -> ...` currently produces an infix AST fallback call.
                // If `foo-bar` has explicit empty signature `:()`, retry zero-arg dispatch.
                let is_empty_sig_rejection = err
                    .message
                    .starts_with("Too many positionals passed; expected 0 arguments but got more")
                    || err.message.starts_with("Unexpected named argument '");
                if is_empty_sig_rejection {
                    if let Ok(v) = self.interpreter.call_function(name, Vec::new()) {
                        return Ok(v);
                    }
                    if Self::should_retry_with_canonical_infix_name(name)
                        && let Some(op_name) = infix_name
                        && let Ok(v) = self.interpreter.call_function(op_name, Vec::new())
                    {
                        return Ok(v);
                    }
                    if let Some(op_name) = infix_name {
                        let op_env_name = format!("&{}", op_name);
                        if let Some(code_val) = self.interpreter.env().get(&op_env_name).cloned() {
                            return self.vm_call_on_value(code_val, Vec::new(), None);
                        }
                    }
                    let bare_env_name = format!("&{}", name);
                    if let Some(code_val) = self.interpreter.env().get(&bare_env_name).cloned() {
                        return self.vm_call_on_value(code_val, Vec::new(), None);
                    }
                    let method_name = name
                        .strip_prefix("infix:<")
                        .and_then(|s| s.strip_suffix('>'))
                        .unwrap_or(name);
                    if !method_name.is_empty()
                        && !call_args.is_empty()
                        && call_args[0].to_string_value() == "method"
                    {
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("name".to_string(), Value::str(method_name.to_string()));
                        attrs.insert("is_dispatcher".to_string(), Value::Bool(false));
                        let mut sig_attrs = std::collections::HashMap::new();
                        sig_attrs.insert("params".to_string(), Value::array(Vec::new()));
                        attrs.insert(
                            "signature".to_string(),
                            Value::make_instance(Symbol::intern("Signature"), sig_attrs),
                        );
                        attrs.insert("returns".to_string(), Value::Package(Symbol::intern("Mu")));
                        attrs.insert("of".to_string(), Value::Package(Symbol::intern("Mu")));
                        return Ok(Value::make_instance(Symbol::intern("Method"), attrs));
                    }
                    Err(RuntimeError::new(format!(
                        "Confused: two terms in a row (unknown infix function: {})",
                        name
                    )))
                } else {
                    if let Some(op_name) = infix_name {
                        let op_env_name = format!("&{}", op_name);
                        if let Some(code_val) = self.interpreter.env().get(&op_env_name).cloned() {
                            return self.vm_call_on_value(code_val, call_args, None);
                        }
                    }
                    let bare_env_name = format!("&{}", name);
                    if let Some(code_val) = self.interpreter.env().get(&bare_env_name).cloned() {
                        self.vm_call_on_value(code_val, call_args, None)
                    } else {
                        let method_name = name
                            .strip_prefix("infix:<")
                            .and_then(|s| s.strip_suffix('>'))
                            .unwrap_or(name);
                        if !method_name.is_empty()
                            && !call_args.is_empty()
                            && call_args[0].to_string_value() == "method"
                        {
                            let mut attrs = std::collections::HashMap::new();
                            attrs.insert("name".to_string(), Value::str(method_name.to_string()));
                            attrs.insert("is_dispatcher".to_string(), Value::Bool(false));
                            let mut sig_attrs = std::collections::HashMap::new();
                            sig_attrs.insert("params".to_string(), Value::array(Vec::new()));
                            attrs.insert(
                                "signature".to_string(),
                                Value::make_instance(Symbol::intern("Signature"), sig_attrs),
                            );
                            attrs.insert(
                                "returns".to_string(),
                                Value::Package(Symbol::intern("Mu")),
                            );
                            attrs.insert("of".to_string(), Value::Package(Symbol::intern("Mu")));
                            return Ok(Value::make_instance(Symbol::intern("Method"), attrs));
                        }
                        Err(RuntimeError::new(format!(
                            "Confused: two terms in a row (unknown infix function: {})",
                            name
                        )))
                    }
                }
            }
        }
    }
}

/// Maximum elements to produce from Z (zip) when iterating over ranges.
/// This caps the output for infinite ranges (e.g., `1..* Z** 1..*`).
/// Kept small because the meta-operator (e.g. `**`) may be expensive
/// for large values. The caller (e.g., `.[^5]`) will further limit.
// TODO: Ideally Z should return a lazy Seq and only compute elements on demand.
const MAX_ZIP_EXPAND: usize = 1_000;

/// Helper for lazy index-based iteration over values in Z (zip) operations.
/// Avoids materializing huge ranges like `1..*` into million-element Vecs.
enum ZipIter {
    /// Inclusive integer range: elements are start, start+1, ..., end
    IntRange { start: i64, count: usize },
    /// Exclusive-end integer range: elements are start, start+1, ..., end-1
    IntRangeExcl { start: i64, count: usize },
    /// Already-materialized list
    List(Vec<Value>),
    /// A list that ends with `*` (Whatever): the last real element is repeated
    /// to extend the list to any requested length.
    ExtendedList {
        items: Vec<Value>,
        /// The last real element (before `*`), used for extension
        fill: Value,
    },
}

impl ZipIter {
    fn from_value(val: &Value) -> Self {
        match val {
            Value::Range(a, b) => {
                let count = if *b >= *a {
                    ((*b - *a + 1) as usize).min(MAX_ZIP_EXPAND)
                } else {
                    0
                };
                ZipIter::IntRange { start: *a, count }
            }
            Value::RangeExcl(a, b) => {
                let count = if *b > *a {
                    ((*b - *a) as usize).min(MAX_ZIP_EXPAND)
                } else {
                    0
                };
                ZipIter::IntRangeExcl { start: *a, count }
            }
            Value::RangeExclStart(a, b) => {
                let start = *a + 1;
                let count = if *b >= start {
                    ((*b - start + 1) as usize).min(MAX_ZIP_EXPAND)
                } else {
                    0
                };
                ZipIter::IntRange { start, count }
            }
            Value::RangeExclBoth(a, b) => {
                let start = *a + 1;
                let count = if *b > start {
                    ((*b - start) as usize).min(MAX_ZIP_EXPAND)
                } else {
                    0
                };
                ZipIter::IntRangeExcl { start, count }
            }
            // Nil in zip context is a 1-element list (not empty), matching Raku behavior
            // where `Nil Z+ 2` yields `(2)` (Nil coerces to 0).
            Value::Nil => ZipIter::List(vec![Value::Nil]),
            _ => {
                let list = runtime::value_to_list(val);
                // Check for trailing Whatever (*) — extends the list by
                // repeating the last real element.
                if list.len() >= 2 && matches!(list.last(), Some(Value::Whatever)) {
                    let items: Vec<Value> = list[..list.len() - 1].to_vec();
                    let fill = items.last().cloned().unwrap_or(Value::Nil);
                    ZipIter::ExtendedList { items, fill }
                } else {
                    ZipIter::List(list)
                }
            }
        }
    }

    fn len(&self) -> usize {
        match self {
            ZipIter::IntRange { count, .. } | ZipIter::IntRangeExcl { count, .. } => *count,
            ZipIter::List(v) => v.len(),
            // Extended lists can match any length from the other side
            ZipIter::ExtendedList { .. } => usize::MAX,
        }
    }

    fn nth(&self, i: usize) -> Value {
        match self {
            ZipIter::IntRange { start, .. } | ZipIter::IntRangeExcl { start, .. } => {
                Value::Int(*start + i as i64)
            }
            ZipIter::List(v) => v[i].clone(),
            ZipIter::ExtendedList { items, fill } => {
                if i < items.len() {
                    items[i].clone()
                } else {
                    fill.clone()
                }
            }
        }
    }
}
