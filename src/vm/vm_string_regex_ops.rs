use super::*;

/// Expand a tr/// character spec, supporting `a..z` ranges.
fn expand_tr_spec(spec: &str) -> Vec<char> {
    let chars: Vec<char> = spec.chars().collect();
    let mut result = Vec::new();
    let mut i = 0;
    while i < chars.len() {
        if i + 2 < chars.len() && chars[i + 1] == '.' && i + 3 <= chars.len() {
            // Check for `a..z` pattern (two dots)
            if i + 3 < chars.len() && chars[i + 2] == '.' {
                // `a..z` range
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
        }
        result.push(chars[i]);
        i += 1;
    }
    result
}

/// Perform transliteration: replace each char in `from` with corresponding char in `to`.
fn transliterate_str(text: &str, from_spec: &str, to_spec: &str) -> String {
    let from_chars = expand_tr_spec(from_spec);
    let to_chars = expand_tr_spec(to_spec);
    text.chars()
        .map(|c| {
            if let Some(pos) = from_chars.iter().position(|&fc| fc == c) {
                if pos < to_chars.len() {
                    to_chars[pos]
                } else if !to_chars.is_empty() {
                    *to_chars.last().unwrap()
                } else {
                    c
                }
            } else {
                c
            }
        })
        .collect()
}

impl VM {
    pub(super) fn exec_subst_op(
        &mut self,
        code: &CompiledCode,
        pattern_idx: u32,
        replacement_idx: u32,
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
            let mut out = String::new();
            out.push_str(&text[..start_b]);
            out.push_str(&replacement);
            out.push_str(&text[end_b..]);
            let result = Value::Str(out);
            self.interpreter
                .env_mut()
                .insert("_".to_string(), result.clone());
            self.stack.push(result);
        } else {
            self.stack.push(Value::Str(text));
        }
        Ok(())
    }

    pub(super) fn exec_non_destructive_subst_op(
        &mut self,
        code: &CompiledCode,
        pattern_idx: u32,
        replacement_idx: u32,
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
    ) {
        let from = Self::const_str(code, from_idx).to_string();
        let to = Self::const_str(code, to_idx).to_string();
        let target = self
            .interpreter
            .env()
            .get("_")
            .cloned()
            .unwrap_or(Value::Nil);
        let text = target.to_string_value();
        let result = transliterate_str(&text, &from, &to);
        self.interpreter
            .env_mut()
            .insert("_".to_string(), Value::Str(result.clone()));
        self.stack.push(Value::Str(result));
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
        let op = Self::const_str(code, op_idx);
        let result = Interpreter::eval_hyper_op(op, &left, &right, dwim_left, dwim_right)?;
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
            "R" => Interpreter::apply_reduction_op(&op, &right, &left)?,
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
                } else {
                    for l in &left_list {
                        for r in &right_list {
                            results.push(Interpreter::apply_reduction_op(&op, l, r)?);
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
                    for i in 0..len {
                        results.push(Interpreter::apply_reduction_op(
                            &op,
                            &left_list[i],
                            &right_list[i],
                        )?);
                    }
                }
                Value::array(results)
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
            Value::Nil
        };
        self.stack.push(result);
        Ok(())
    }
}
