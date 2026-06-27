use super::*;

impl Interpreter {
    pub(super) fn exec_infix_func_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        right_arity: u32,
        modifier_idx: &Option<u32>,
        compiled_fns: &HashMap<String, CompiledFunction>,
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
            // A lexical `&infix:<op>` binding (e.g. a `&infix:<@@>` parameter)
            // shadows the package-level operator of the same name. Guarded by the
            // shadow-name set so the common case (no operator param) is free.
            if !self.amp_param_shadowed_names.is_empty()
                && self
                    .amp_param_shadowed_names
                    .contains(&Symbol::intern(&infix_name))
                && let Some(callable) = self.lexical_infix_override(code, &infix_name)
            {
                let result = self.call_sub_value(callable, call_args, false)?;
                self.stack.push(result);
                return Ok(());
            }
            let assoc = self
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
                                compiled_fns,
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
                    self.call_infix_fallback(
                        lookup_name.as_ref(),
                        Some(&infix_name),
                        call_args,
                        compiled_fns,
                    )?
                }
            } else {
                // For multi-arg calls (list-associative flattened chains),
                // try the user-defined function first before falling back
                // to built-in reduction.
                if let Some(def) =
                    loan_env!(self, resolve_function_with_types(&infix_name, &call_args))
                {
                    self.compile_and_call_function_def(&def, call_args.clone(), compiled_fns)?
                } else {
                    self.call_infix_fallback(
                        lookup_name.as_ref(),
                        Some(&infix_name),
                        call_args,
                        compiled_fns,
                    )?
                }
            }
        };
        self.stack.push(result);
        Ok(())
    }

    fn flip_flop_scope_key(&self) -> String {
        if let Some(Value::Int(id)) = self.env().get("__mutsu_callable_id") {
            return format!("callable:{id}");
        }
        if let Some(frame) = self.routine_stack_top() {
            return format!("routine:{}::{}", frame.package, frame.name);
        }
        "top".to_string()
    }

    fn flip_flop_operand_truthy(&mut self, value: &Value, is_rhs: bool) -> bool {
        match value {
            // `*` means "always" on the LHS and "never" on the RHS.
            Value::Whatever => !is_rhs,
            // A boolean operand is used directly (the common
            // `$n == 3 ff $n == 5` comparison form).
            Value::Bool(b) => *b,
            // Every other operand (a regex, or a constant such as `2 ff 4`)
            // is smart-matched against the topic `$_` — the sed-style line
            // matching where `2 ff 4` means "from when `$_ ~~ 2` until
            // `$_ ~~ 4`".
            _ => {
                let topic = self.env().get("_").cloned().unwrap_or(Value::Nil);
                self.vm_smart_match(&topic, value)
            }
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
            .get_state_var(key)
            .and_then(|v| match v {
                Value::Int(i) if *i > 0 => Some(*i),
                _ => None,
            })
            .unwrap_or(0);

        if seq > 0 {
            let current = seq;
            if rhs {
                self.set_state_var(key.to_string(), Value::Int(0));
                if exclude_end {
                    Value::Nil
                } else {
                    Value::Int(current)
                }
            } else {
                self.set_state_var(key.to_string(), Value::Int(current + 1));
                Value::Int(current)
            }
        } else if lhs {
            if !is_fff && rhs {
                self.set_state_var(key.to_string(), Value::Int(0));
                if exclude_start || exclude_end {
                    Value::Nil
                } else {
                    Value::Int(1)
                }
            } else {
                self.set_state_var(key.to_string(), Value::Int(2));
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
        compiled_fns: &HashMap<String, CompiledFunction>,
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
            && let Ok(v) = loan_env!(self, call_user_routine_direct(op_name, call_args.clone()))
        {
            return Ok(v);
        }
        if Self::should_retry_with_canonical_infix_name(name)
            && let Some(op_name) = infix_name
            && let Ok(v) =
                self.call_function_compiled_first(op_name, call_args.clone(), compiled_fns)
        {
            return Ok(v);
        }
        match self.call_function_compiled_first(name, call_args.clone(), compiled_fns) {
            Ok(v) => Ok(v),
            Err(err) => {
                // `for foo-bar() -> ...` currently produces an infix AST fallback call.
                // If `foo-bar` has explicit empty signature `:()`, retry zero-arg dispatch.
                let is_empty_sig_rejection = err
                    .message
                    .starts_with("Too many positionals passed; expected 0 arguments but got more")
                    || err.message.starts_with("Unexpected named argument '");
                if is_empty_sig_rejection {
                    if let Ok(v) = self.call_function_compiled_first(name, Vec::new(), compiled_fns)
                    {
                        return Ok(v);
                    }
                    if Self::should_retry_with_canonical_infix_name(name)
                        && let Some(op_name) = infix_name
                        && let Ok(v) =
                            self.call_function_compiled_first(op_name, Vec::new(), compiled_fns)
                    {
                        return Ok(v);
                    }
                    if let Some(op_name) = infix_name {
                        let op_env_name = format!("&{}", op_name);
                        if let Some(code_val) = self.env().get(&op_env_name).cloned() {
                            return self.vm_call_on_value(code_val, Vec::new(), None);
                        }
                    }
                    let bare_env_name = format!("&{}", name);
                    if let Some(code_val) = self.env().get(&bare_env_name).cloned() {
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
                    Err(RuntimeError::syntax_confused_with_reason(
                        "Two terms in a row",
                    ))
                } else {
                    if let Some(op_name) = infix_name {
                        let op_env_name = format!("&{}", op_name);
                        if let Some(code_val) = self.env().get(&op_env_name).cloned() {
                            return self.vm_call_on_value(code_val, call_args, None);
                        }
                    }
                    let bare_env_name = format!("&{}", name);
                    if let Some(code_val) = self.env().get(&bare_env_name).cloned() {
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
                        Err(RuntimeError::syntax_confused_with_reason(
                            "Two terms in a row",
                        ))
                    }
                }
            }
        }
    }
}
