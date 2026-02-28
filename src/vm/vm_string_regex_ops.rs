use super::*;

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

impl VM {
    pub(super) fn exec_subst_op(
        &mut self,
        code: &CompiledCode,
        pattern_idx: u32,
        replacement_idx: u32,
        samemark: bool,
    ) -> Result<(), RuntimeError> {
        let pattern = Self::const_str(code, pattern_idx).to_string();
        let replacement = Self::const_str(code, replacement_idx).to_string();
        let target = self
            .interpreter
            .env()
            .get("_")
            .cloned()
            .unwrap_or(Value::Nil);
        let text = target.to_string_value();
        if let Some((start, end)) = self.interpreter.regex_find_first_bridge(&pattern, &text) {
            let start_b = runtime::char_idx_to_byte(&text, start);
            let end_b = runtime::char_idx_to_byte(&text, end);
            let matched_text = &text[start_b..end_b];
            let replacement = if samemark {
                // Use per-word samemark when both source and replacement contain whitespace
                if matched_text.contains(char::is_whitespace)
                    && replacement.contains(char::is_whitespace)
                {
                    samemark_per_word(&replacement, matched_text)
                } else {
                    crate::builtins::samemark_string(&replacement, matched_text)
                }
            } else {
                replacement
            };
            let mut out = String::new();
            out.push_str(&text[..start_b]);
            out.push_str(&replacement);
            out.push_str(&text[end_b..]);
            let result = Value::Str(out);
            self.interpreter.env_mut().insert("_".to_string(), result);
            // Push Bool::True so `$x ~~ s///` returns True on match
            self.stack.push(Value::Bool(true));
        } else {
            self.stack.push(Value::Nil);
        }
        Ok(())
    }

    pub(super) fn exec_non_destructive_subst_op(
        &mut self,
        code: &CompiledCode,
        pattern_idx: u32,
        replacement_idx: u32,
        samemark: bool,
    ) -> Result<(), RuntimeError> {
        let pattern = Self::const_str(code, pattern_idx).to_string();
        let replacement = Self::const_str(code, replacement_idx).to_string();
        let target = self
            .interpreter
            .env()
            .get("_")
            .cloned()
            .unwrap_or(Value::Nil);
        let text = target.to_string_value();
        if let Some((start, end)) = self.interpreter.regex_find_first_bridge(&pattern, &text) {
            let start_b = runtime::char_idx_to_byte(&text, start);
            let end_b = runtime::char_idx_to_byte(&text, end);
            let matched_text = &text[start_b..end_b];
            let replacement = if samemark {
                crate::builtins::samemark_string(&replacement, matched_text)
            } else {
                replacement
            };
            let mut out = String::new();
            out.push_str(&text[..start_b]);
            out.push_str(&replacement);
            out.push_str(&text[end_b..]);
            self.stack.push(Value::Str(out));
        } else {
            self.stack.push(Value::Str(text));
        }
        Ok(())
    }

    pub(super) fn exec_transliterate_op(
        &mut self,
        code: &CompiledCode,
        from_idx: u32,
        to_idx: u32,
        delete: bool,
        complement: bool,
        squash: bool,
    ) -> Result<(), RuntimeError> {
        let from = Self::const_str(code, from_idx).to_string();
        let to = Self::const_str(code, to_idx).to_string();
        let target = self
            .interpreter
            .env()
            .get("_")
            .cloned()
            .unwrap_or(Value::Nil);

        let mut args = vec![Value::Pair(from, Box::new(Value::Str(to)))];
        if delete {
            args.push(Value::Pair("d".to_string(), Box::new(Value::Bool(true))));
        }
        if complement {
            args.push(Value::Pair("c".to_string(), Box::new(Value::Bool(true))));
        }
        if squash {
            args.push(Value::Pair("s".to_string(), Box::new(Value::Bool(true))));
        }

        let result = self.interpreter.dispatch_trans(target, &args)?;
        if self.in_smartmatch_rhs {
            self.interpreter
                .env_mut()
                .insert("_".to_string(), result.clone());
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
            results.push(self.eval_reduction_operator_values(&op, l, r)?);
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
                let left_list = runtime::value_to_list(&left);
                let right_list = runtime::value_to_list(&right);
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
                Value::array(results)
            }
            "Z" => {
                let left_list = runtime::value_to_list(&left);
                let right_list = runtime::value_to_list(&right);
                let len = left_list.len().min(right_list.len());
                let mut results = Vec::new();
                if op.is_empty() || op == "," {
                    for i in 0..len {
                        results.push(Value::array(vec![
                            left_list[i].clone(),
                            right_list[i].clone(),
                        ]));
                    }
                } else if op == "=>" {
                    for i in 0..len {
                        let key = left_list[i].to_string_value();
                        results.push(Value::Pair(key, Box::new(right_list[i].clone())));
                    }
                } else {
                    let nested_left = if left_list.len() == 2 {
                        match &left_list[1] {
                            Value::Array(..) | Value::Seq(_) | Value::Slip(_) => {
                                Some(runtime::value_to_list(&left_list[1]))
                            }
                            _ => None,
                        }
                    } else {
                        None
                    };
                    for i in 0..len {
                        if let Some(extra) = &nested_left {
                            let mut v = self.eval_reduction_operator_values(
                                &op,
                                &left_list[0],
                                &right_list[i],
                            )?;
                            if let Some(extra_i) = extra.get(i) {
                                v = self.eval_reduction_operator_values(&op, &v, extra_i)?;
                            }
                            results.push(v);
                        } else {
                            results.push(self.eval_reduction_operator_values(
                                &op,
                                &left_list[i],
                                &right_list[i],
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
            let fmt = match left_val {
                Value::Str(s) => s,
                _ => String::new(),
            };
            if modifier.as_deref() == Some("X") {
                let mut parts = Vec::new();
                for val in &right_vals {
                    parts.push(runtime::format_sprintf(&fmt, Some(val)));
                }
                Value::Str(parts.join(" "))
            } else {
                let arg = right_vals.first();
                let rendered = runtime::format_sprintf(&fmt, arg);
                Value::Str(rendered)
            }
        } else {
            let mut call_args = vec![left_val.clone()];
            call_args.extend(right_vals.clone());
            if modifier.as_deref() == Some("R") && call_args.len() == 2 {
                call_args.swap(0, 1);
            }
            let infix_name = format!("infix:<{}>", name);
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
                            self.call_infix_fallback(&name, Some(&infix_name), vec![left, right])?
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
                    self.call_infix_fallback(&name, Some(&infix_name), call_args)?
                }
            } else {
                self.call_infix_fallback(&name, Some(&infix_name), call_args)?
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
            map.insert("key".to_string(), Value::Str(matcher_key));
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
                    if let Some(op_name) = infix_name {
                        let op_env_name = format!("&{}", op_name);
                        if let Some(code_val) = self.interpreter.env().get(&op_env_name).cloned() {
                            return self.interpreter.eval_call_on_value(code_val, Vec::new());
                        }
                    }
                    let bare_env_name = format!("&{}", name);
                    if let Some(code_val) = self.interpreter.env().get(&bare_env_name).cloned() {
                        return self.interpreter.eval_call_on_value(code_val, Vec::new());
                    }
                    Err(RuntimeError::new(format!(
                        "Unknown infix function: {}",
                        name
                    )))
                } else {
                    if let Some(op_name) = infix_name {
                        let op_env_name = format!("&{}", op_name);
                        if let Some(code_val) = self.interpreter.env().get(&op_env_name).cloned() {
                            return self.interpreter.eval_call_on_value(code_val, call_args);
                        }
                    }
                    let bare_env_name = format!("&{}", name);
                    if let Some(code_val) = self.interpreter.env().get(&bare_env_name).cloned() {
                        self.interpreter.eval_call_on_value(code_val, call_args)
                    } else {
                        Err(RuntimeError::new(format!(
                            "Unknown infix function: {}",
                            name
                        )))
                    }
                }
            }
        }
    }
}
