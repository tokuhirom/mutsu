use super::*;

impl Interpreter {
    /// Whether a user-declared `sub infix:<op>` is in scope for `canon`
    /// (e.g. `"infix:<+>"`). The set is empty in the overwhelming common
    /// case, keeping tight numeric loops free of any registry lookup.
    #[inline]
    fn user_infix_override(&self, canon: &str) -> bool {
        !self.user_declared_infix_ops.is_empty() && self.user_declared_infix_ops.contains(canon)
    }

    pub(super) fn exec_add_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // A user-declared `infix:<+>` sub can override even native Int/Num
        // addition (roast S06-operator-overloading/infix.t), so the native
        // fast paths below must be skipped whenever one is in scope.
        let has_override = self.user_infix_override("infix:<+>");
        // Fast path: Int + Int (most common case in numeric loops)
        if !has_override
            && let Some(a) = left.as_int()
            && let Some(b) = right.as_int()
            && let Some(result) = a.checked_add(b)
        {
            self.stack.push(Value::int(result));
            return Ok(());
        }
        // Fast path: Num + Num
        if !has_override
            && let Some(a) = left.as_num()
            && let Some(b) = right.as_num()
        {
            self.stack.push(Value::num(a + b));
            return Ok(());
        }
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            if let Some(result) = vm.try_user_infix("infix:<+>", &l, &r)? {
                return Ok(result);
            }
            if crate::builtins::arith::is_temporal_operand(&l)
                || crate::builtins::arith::is_temporal_operand(&r)
            {
                return crate::builtins::arith_add(l, r);
            }
            let (l, r) = vm.coerce_numeric_bridge_pair_strict(l, r)?;
            crate::builtins::arith_add(l, r)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_sub_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // A user `infix:<->` overrides the native fast paths, same as `+`.
        let has_override = self.user_infix_override("infix:<->");
        // Fast path: Int - Int
        if !has_override
            && let Some(a) = left.as_int()
            && let Some(b) = right.as_int()
            && let Some(result) = a.checked_sub(b)
        {
            self.stack.push(Value::int(result));
            return Ok(());
        }
        // Fast path: Num - Num
        if !has_override
            && let Some(a) = left.as_num()
            && let Some(b) = right.as_num()
        {
            self.stack.push(Value::num(a - b));
            return Ok(());
        }
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            if let Some(result) = vm.try_user_infix("infix:<->", &l, &r)? {
                return Ok(result);
            }
            if crate::builtins::arith::is_temporal_operand(&l)
                || crate::builtins::arith::is_temporal_operand(&r)
            {
                return Ok(crate::builtins::arith_sub(l, r));
            }
            let (l, r) = vm.coerce_numeric_bridge_pair_strict(l, r)?;
            Ok(crate::builtins::arith_sub(l, r))
        })?;
        self.stack.push(result);
        Ok(())
    }

    /// When the user has overloaded the list-associative comma operator
    /// (`sub infix:<,> {...}`), a bare value-list expression (`5, 5`) dispatches
    /// to that sub with ALL comma operands at once (raku's `infix:<,>` is
    /// list-associative), instead of building a `List`. Returns `true` when the
    /// call was made (result pushed); `false` when no override applies, in which
    /// case the operands are left on the stack for normal list construction.
    ///
    /// Guarded on the (normally empty) `user_declared_infix_ops` set so ordinary
    /// list construction pays only a set-emptiness check. Argument-list commas
    /// (`f(a, b)`) are compiled directly, not through `MakeArray`, so they are
    /// unaffected — matching raku, where `infix:<,>` overloads value lists only.
    pub(super) fn try_comma_overload(&mut self, n: u32) -> Result<bool, RuntimeError> {
        if n < 2
            || self.user_declared_infix_ops.is_empty()
            || !self.user_declared_infix_ops.contains("infix:<,>")
        {
            return Ok(false);
        }
        let n = n as usize;
        let start = self.stack.len() - n;
        let args: Vec<Value> = self.stack.drain(start..).collect();
        if let Some(def) = loan_env!(self, resolve_function_with_types("infix:<,>", &args)) {
            let empty_fns = CompiledFns::default();
            let result = self.compile_and_call_function_def(&def, args, &empty_fns)?;
            self.stack.push(result);
            Ok(true)
        } else {
            // No candidate matched the operand count: restore the stack and let
            // normal list construction proceed.
            for a in args {
                self.stack.push(a);
            }
            Ok(false)
        }
    }

    /// Try to dispatch a binary operation to a user-defined infix operator.
    /// Returns Some(result) if a user-defined candidate matched, None otherwise.
    pub(super) fn try_user_infix(
        &mut self,
        op_name: &str,
        left: &Value,
        right: &Value,
    ) -> Result<Option<Value>, RuntimeError> {
        let args = vec![left.clone(), right.clone()];
        if let Some(def) = loan_env!(self, resolve_function_with_types(op_name, &args)) {
            let empty_fns = CompiledFns::default();
            let result = self.compile_and_call_function_def(&def, args, &empty_fns)?;
            return Ok(Some(result));
        }
        Ok(None)
    }

    pub(super) fn exec_mul_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // A user `infix:<*>` overrides the native fast paths, same as `+`.
        let has_override = self.user_infix_override("infix:<*>");
        // Fast path: Int * Int
        if !has_override
            && let Some(a) = left.as_int()
            && let Some(b) = right.as_int()
            && let Some(result) = a.checked_mul(b)
        {
            self.stack.push(Value::int(result));
            return Ok(());
        }
        // Fast path: Num * Num
        if !has_override
            && let Some(a) = left.as_num()
            && let Some(b) = right.as_num()
        {
            self.stack.push(Value::num(a * b));
            return Ok(());
        }
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            if let Some(result) = vm.try_user_infix("infix:<*>", &l, &r)? {
                return Ok(result);
            }
            let (l, r) = vm.coerce_numeric_bridge_pair_strict(l, r)?;
            Ok(crate::builtins::arith_mul(l, r))
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_div_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            if let Some(result) = vm.try_user_infix("infix:</>", &l, &r)? {
                return Ok(result);
            }
            let (l, r) = vm.coerce_numeric_bridge_pair_strict(l, r)?;
            crate::builtins::arith_div(l, r)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_mod_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            // Duration % Real => Duration. Handle before numeric coercion strips
            // the Duration wrapper down to a bare number.
            if crate::builtins::arith::is_temporal_operand(&l) {
                return crate::builtins::arith_mod(l, r);
            }
            let (l, r) = vm.coerce_numeric_bridge_pair_strict(l, r)?;
            crate::builtins::arith_mod(l, r)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_pow_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // A user `infix:<**>` overrides the native fast path, same as `+`.
        let has_override = self.user_infix_override("infix:<**>");
        // Fast path: Int ** small non-negative Int
        if !has_override
            && let Some(base) = left.as_int()
            && let Some(exp) = right.as_int()
            && (0..=30).contains(&exp)
        {
            let mut result: i64 = 1;
            let mut overflow = false;
            for _ in 0..exp {
                if let Some(r) = result.checked_mul(base) {
                    result = r;
                } else {
                    overflow = true;
                    break;
                }
            }
            if !overflow {
                self.stack.push(Value::int(result));
                return Ok(());
            }
        }
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            if let Some(result) = vm.try_user_infix("infix:<**>", &l, &r)? {
                return Ok(result);
            }
            let (l, r) = vm.coerce_numeric_bridge_pair_strict(l, r)?;
            Ok(crate::builtins::arith_pow(l, r))
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_negate_op(&mut self) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap();
        // Type objects (Mu, Any, etc.) cannot be negated
        if let ValueView::Package(name) = val.view()
            && matches!(name.resolve().as_str(), "Mu" | "Any")
        {
            return Err(RuntimeError::new(format!(
                "Cannot resolve caller prefix:<->({}:U); none of these signatures matches:\n    (\\a)",
                name.resolve()
            )));
        }
        // For strings, first coerce to numeric (preserving Rat/Complex types and
        // producing X::Str::Numeric for invalid strings), then negate the result.
        if let Some(s) = val.as_str() {
            let trimmed = s.trim();
            if trimmed.is_empty() {
                self.stack.push(Value::int(0));
                return Ok(());
            }
            if let Some(numeric) = crate::runtime::str_numeric::parse_raku_str_to_numeric(trimmed) {
                self.stack.push(crate::builtins::arith_negate(numeric)?);
            } else {
                return Err(RuntimeError::typed_msg(
                    "X::Str::Numeric",
                    format!(
                        "Cannot convert string to number: base-10 number must begin with valid digits or '.' in '{}'",
                        s
                    ),
                ));
            }
            return Ok(());
        }
        let val = self.coerce_numeric_bridge_value(val)?;
        self.stack.push(crate::builtins::arith_negate(val)?);
        Ok(())
    }

    pub(super) fn exec_int_bit_neg_op(&mut self) {
        let val = self.stack.pop().unwrap();
        if let Some(n) = val.as_int() {
            self.stack.push(Value::int(!n));
        } else {
            let n = val.to_bigint();
            self.stack.push(Value::from_bigint(!n));
        }
    }

    pub(super) fn exec_bool_bit_neg_op(&mut self) {
        let val = self.stack.pop().unwrap();
        self.stack.push(Value::truth(!val.truthy()));
    }

    pub(super) fn exec_str_bit_neg_op(&mut self) {
        let val = self.stack.pop().unwrap();
        if Self::is_buf_value(&val) {
            let bytes = Self::extract_buf_bytes(&val);
            let negated: Vec<Value> = bytes.iter().map(|b| Value::int((!b) as i64)).collect();
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("bytes".to_string(), Value::array(negated));
            // Determine result type: if input is utf8, result is utf8; otherwise Buf
            let result_type = if let ValueView::Instance { class_name, .. } = val.view() {
                class_name.resolve().to_string()
            } else {
                "Buf".to_string()
            };
            self.stack
                .push(Value::make_instance(Symbol::intern(&result_type), attrs));
        } else {
            let s = crate::runtime::utils::coerce_to_str(&val);
            let negated: Vec<u8> = s.as_bytes().iter().map(|b| !b).collect();
            let result = String::from_utf8_lossy(&negated).into_owned();
            self.stack.push(Value::str(result));
        }
    }
}
