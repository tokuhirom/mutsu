use super::*;
use crate::meta_ns::MetaNs;
use crate::value::ValueMap;

impl Interpreter {
    pub(super) fn exec_infix_func_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        right_arity: u32,
        modifier_idx: &Option<u32>,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let arity = right_arity as usize;
        let mut right_vals: Vec<Value> = Vec::with_capacity(arity);
        for _ in 0..arity {
            right_vals.push(self.stack.pop().unwrap_or(Value::NIL));
        }
        right_vals.reverse();
        let left_val = self.stack.pop().unwrap_or(Value::NIL);
        let name = Self::const_str(code, name_idx).to_string();
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx).to_string());
        let result = {
            let mut call_args = vec![left_val.clone()];
            call_args.extend(right_vals.clone());
            if modifier.as_deref() == Some("R") && call_args.len() == 2 {
                call_args.swap(0, 1);
            }
            let lookup_name = name.as_str();
            let infix_name = format!("infix:<{lookup_name}>");
            // A lexical `&infix:<op>` binding (e.g. a `&infix:<@@>` parameter)
            // shadows the package-level operator of the same name. Guarded by the
            // shadow-name set so the common case (no operator param) is free.
            if !self.lexicals.amp_param_shadowed_names.is_empty()
                && self
                    .lexicals
                    .amp_param_shadowed_names
                    .contains(&Symbol::intern(&infix_name))
                && let Some(callable) = self.lexical_infix_override(code, &infix_name)
            {
                // Only user code can declare an `is rw` parameter that needs the
                // `WrapVarRef` tag of a variable operand; a builtin operator bound
                // to the parameter (`f(&[<], ...)`) takes plain values.
                let call_args = if matches!(callable.view(), ValueView::Routine { .. }) {
                    call_args
                        .into_iter()
                        .map(Self::unwrap_var_ref_value)
                        .collect()
                } else {
                    call_args
                };
                let result = self.call_sub_value(callable, call_args)?;
                self.apply_pending_rw_writeback(code);
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
                                lookup_name,
                                Some(&infix_name),
                                vec![left, right],
                                code,
                                compiled_fns,
                            )?
                        };
                    if !pair_result.truthy() {
                        all_true = false;
                        break;
                    }
                }
                Value::truth(all_true)
            } else if call_args.len() == 2 {
                let right_val = right_vals.first().cloned().unwrap_or(Value::NIL);
                let (left_plain, right_plain) = (
                    Self::unwrap_var_ref_value(left_val.clone()).into_descalarized(),
                    Self::unwrap_var_ref_value(right_val.clone()).into_descalarized(),
                );
                // A user `infix:<op>` that names no Junction parameter still
                // autothreads a Junction operand, one leaf at a time, so
                // `'a' eq any(...)` with a user `multi infix:<eq>(C:D, C:D)` in
                // scope stays a Junction rather than binding it as a whole.
                if self.user_infix_override(&infix_name)
                    && (matches!(left_plain.view(), ValueView::Junction { .. })
                        || matches!(right_plain.view(), ValueView::Junction { .. }))
                    && !self.user_infix_accepts_junction(&infix_name, &left_plain, &right_plain)
                {
                    let threaded = self.thread_user_infix_junction(
                        &name,
                        modifier.as_deref(),
                        left_plain,
                        right_plain,
                        code,
                        compiled_fns,
                    )?;
                    self.apply_pending_rw_writeback(code);
                    self.stack.push(threaded);
                    return Ok(());
                }
                if let Some(result) = self.try_user_infix(&infix_name, &left_val, &right_val)? {
                    result
                } else {
                    // `core_unicode_arith_alias_infix` is always native, so it
                    // needs plain values -- unwrap ONLY for this call, into
                    // throwaway copies. `left_val`/`right_val` themselves stay
                    // wrapped: `call_infix_fallback` below may still resolve a
                    // PURELY custom `infix:<op>` sub by name (one
                    // `try_user_infix` never sees, because that helper only
                    // competes against a CORE-recognized operator) whose
                    // parameter is declared `is rw` (French's
                    // `&infix:<plus_égal>`), and that still needs the
                    // `WrapVarRef` tag to bind the caller's container.
                    let left_plain = Self::unwrap_var_ref_value(left_val.clone());
                    let right_plain = Self::unwrap_var_ref_value(right_val.clone());
                    if let Some(result) =
                        self.core_unicode_arith_alias_infix(&name, &left_plain, &right_plain)?
                    {
                        result
                    } else {
                        self.call_infix_fallback(
                            lookup_name,
                            Some(&infix_name),
                            call_args,
                            code,
                            compiled_fns,
                        )?
                    }
                }
            } else {
                // For multi-arg calls (list-associative flattened chains),
                // run a proto body before resolving a multi candidate, just
                // as the binary path does.
                if let Some(result) = self.vm_try_run_nontrivial_proto_body(
                    &infix_name,
                    call_args.clone(),
                    compiled_fns,
                ) {
                    result?
                } else if let Some(def) =
                    loan_env!(self, resolve_function_with_types(&infix_name, &call_args))
                        .ok()
                        .flatten()
                {
                    self.compile_and_call_function_def(&def, call_args.clone(), compiled_fns)?
                } else {
                    self.call_infix_fallback(
                        lookup_name,
                        Some(&infix_name),
                        call_args,
                        code,
                        compiled_fns,
                    )?
                }
            }
        };
        self.apply_pending_rw_writeback(code);
        self.stack.push(result);
        Ok(())
    }

    /// Whether the user candidate `infix:<op>` resolves for `left`/`right`
    /// with a parameter typed `Junction`/`Mu` on a Junction operand's side,
    /// i.e. it takes the Junction as a whole instead of autothreading it.
    // Cost: O(c), c = candidates of the operator.
    fn user_infix_accepts_junction(
        &mut self,
        infix_name: &str,
        left: &Value,
        right: &Value,
    ) -> bool {
        let args = [left.clone(), right.clone()];
        let Some(def) = loan_env!(self, resolve_function_with_types(infix_name, &args))
            .ok()
            .flatten()
        else {
            return false;
        };
        let takes = |idx: usize| {
            def.param_defs
                .iter()
                .filter(|p| !p.named)
                .nth(idx)
                .and_then(|p| p.type_constraint.as_deref())
                .is_some_and(|t| matches!(t.split(':').next().unwrap_or(t), "Junction" | "Mu"))
        };
        (matches!(left.view(), ValueView::Junction { .. }) && takes(0))
            || (matches!(right.view(), ValueView::Junction { .. }) && takes(1))
    }

    /// Autothread a Junction operand through a user `infix:<op>`, one eigenstate
    /// at a time, re-entering the ordinary two-operand path for each leaf.
    // Cost: O(k * c), k = eigenstates, c = cost of one leaf call.
    fn thread_user_infix_junction(
        &mut self,
        name: &str,
        modifier: Option<&str>,
        left: Value,
        right: Value,
        code: &CompiledCode,
        compiled_fns: &CompiledFns,
    ) -> Result<Value, RuntimeError> {
        if let ValueView::Junction { kind, values } = left.view() {
            let mut out = Vec::with_capacity(values.len());
            for v in values.iter() {
                out.push(self.thread_user_infix_junction(
                    name,
                    modifier,
                    v.clone(),
                    right.clone(),
                    code,
                    compiled_fns,
                )?);
            }
            return Ok(Value::junction(kind, out));
        }
        if let ValueView::Junction { kind, values } = right.view() {
            let mut out = Vec::with_capacity(values.len());
            for v in values.iter() {
                out.push(self.thread_user_infix_junction(
                    name,
                    modifier,
                    left.clone(),
                    v.clone(),
                    code,
                    compiled_fns,
                )?);
            }
            return Ok(Value::junction(kind, out));
        }
        let lookup_name = name;
        let infix_name = format!("infix:<{lookup_name}>");
        let mut call_args = vec![left.clone(), right.clone()];
        if modifier == Some("R") {
            call_args.swap(0, 1);
        }
        if let Some(v) = self.try_user_infix(&infix_name, &call_args[0], &call_args[1])? {
            return Ok(v);
        }
        self.call_infix_fallback(
            lookup_name,
            Some(&infix_name),
            call_args,
            code,
            compiled_fns,
        )
    }

    fn flip_flop_scope_key(&self) -> String {
        if let Some(id) = self.env().callable_id() {
            return format!("callable:{id}");
        }
        if let Some(frame) = self.routine_stack_top() {
            return format!("routine:{}::{}", frame.package, frame.name);
        }
        "top".to_string()
    }

    fn flip_flop_operand_truthy(&mut self, value: &Value, is_rhs: bool) -> bool {
        match value.view() {
            // `*` means "always" on the LHS and "never" on the RHS.
            ValueView::Whatever => !is_rhs,
            // A boolean operand is used directly (the common
            // `$n == 3 ff $n == 5` comparison form).
            ValueView::Bool(b) => b,
            // Every other operand (a regex, or a constant such as `2 ff 4`)
            // is smart-matched against the topic `$_` — the sed-style line
            // matching where `2 ff 4` means "from when `$_ ~~ 2` until
            // `$_ ~~ 4`".
            _ => {
                let topic = self.env().get("_").cloned().unwrap_or(Value::NIL);
                self.vm_smart_match(&topic, value)
            }
        }
    }

    fn eval_expr_range(
        &mut self,
        code: &CompiledCode,
        start: usize,
        end: usize,
        compiled_fns: &CompiledFns,
    ) -> Result<Value, RuntimeError> {
        let saved_depth = self.stack.len();
        self.run_range(code, start, end, compiled_fns)?;
        let value = self.stack.pop().unwrap_or(Value::NIL);
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
        let key = (crate::symbol::Symbol::intern(key), None);
        let seq = self
            .get_state_var(key)
            .and_then(|v| match v.view() {
                ValueView::Int(i) if i > 0 => Some(i),
                _ => None,
            })
            .unwrap_or(0);

        if seq > 0 {
            let current = seq;
            if rhs {
                self.set_state_var(key, Value::int(0));
                if exclude_end {
                    Value::NIL
                } else {
                    Value::int(current)
                }
            } else {
                self.set_state_var(key, Value::int(current + 1));
                Value::int(current)
            }
        } else if lhs {
            if !is_fff && rhs {
                self.set_state_var(key, Value::int(0));
                if exclude_start || exclude_end {
                    Value::NIL
                } else {
                    Value::int(1)
                }
            } else {
                self.set_state_var(key, Value::int(2));
                if exclude_start {
                    Value::NIL
                } else {
                    Value::int(1)
                }
            }
        } else {
            Value::NIL
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
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let lhs_start = *ip + 1;
        let lhs_end = lhs_end as usize;
        let rhs_start = lhs_end;
        let rhs_end = rhs_end as usize;

        if self.topic_state.in_smartmatch_rhs {
            let lhs_pattern = self.eval_expr_range(code, lhs_start, lhs_end, compiled_fns)?;
            let rhs_pattern = self.eval_expr_range(code, rhs_start, rhs_end, compiled_fns)?;
            let scope = self.flip_flop_scope_key();
            let matcher_key = MetaNs::FfState.owned_key_from_parts(&[&scope, &site_id.to_string()]);
            let mut map = ValueMap::default();
            map.insert("__mutsu_ff_matcher".to_string(), Value::TRUE);
            map.insert("key".to_string(), Value::str(matcher_key));
            map.insert("lhs".to_string(), lhs_pattern);
            map.insert("rhs".to_string(), rhs_pattern);
            map.insert("exclude_start".to_string(), Value::truth(exclude_start));
            map.insert("exclude_end".to_string(), Value::truth(exclude_end));
            map.insert("is_fff".to_string(), Value::truth(is_fff));
            self.stack.push(Value::hash(map));
            *ip = rhs_end;
            return Ok(());
        }

        let scope = self.flip_flop_scope_key();
        let state_key = MetaNs::FfState.owned_key_from_parts(&[&scope, &site_id.to_string()]);
        let seq = self
            .get_state_var((crate::symbol::Symbol::intern(&state_key), None))
            .and_then(|v| match v.view() {
                ValueView::Int(i) if i > 0 => Some(i),
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

    /// The CORE candidate of `×` (U+00D7) / `÷` (U+00F7): the very arithmetic
    /// `infix:<*>` / `infix:</>` performs, strict numeric coercion included.
    ///
    /// rakudo makes the Unicode spellings aliases of the ASCII routines
    /// (`&infix:<×> === &infix:<*>`), so a user `multi infix:<×>` adds a
    /// candidate in front of that shared core set rather than replacing it:
    /// when the user candidate declines (a `where` clause that does not hold,
    /// as in Math::Vector's 3-dimensions-only `×`), the core candidate runs and
    /// an operand with no numeric coercion throws `Cannot resolve caller
    /// Numeric(...)`. mutsu diverts `×` off `OpCode::Mul` as soon as any user
    /// `infix:<×>` is declared (`parse_multiplicative_op`), so the generic
    /// `call_infix_fallback` reduction answered for it instead — and that runs
    /// the *lenient* `builtins::arith_*`, which read an object as 0 and turned
    /// a `dies-ok` into a silent `Int`.
    ///
    /// `Some` means this was one of those two spellings and the core candidate
    /// has answered (or thrown); `None` leaves every other operator to the
    /// generic fallback.
    fn core_unicode_arith_alias_infix(
        &mut self,
        name: &str,
        left: &Value,
        right: &Value,
    ) -> Result<Option<Value>, RuntimeError> {
        let multiply = match name {
            "\u{00D7}" => true,
            "\u{00F7}" => false,
            _ => return Ok(None),
        };
        let (l, r) = match self.coerce_numeric_bridge_pair_strict(left.clone(), right.clone())? {
            Ok(pair) => pair,
            Err(failure) => return Ok(Some(failure)),
        };
        let result = if multiply {
            crate::builtins::arith_mul(l, r)
        } else {
            crate::builtins::arith_div(l, r)?
        };
        Ok(Some(result))
    }
}
