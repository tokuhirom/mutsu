use super::*;

impl Interpreter {
    pub(super) fn exec_add_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // Fast path: Int + Int (most common case in numeric loops)
        if let Value::Int(a) = &left
            && let Value::Int(b) = &right
            && let Some(result) = a.checked_add(*b)
        {
            self.stack.push(Value::Int(result));
            return Ok(());
        }
        // Fast path: Num + Num
        if let Value::Num(a) = &left
            && let Value::Num(b) = &right
        {
            self.stack.push(Value::Num(a + b));
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
        // Fast path: Int - Int
        if let Value::Int(a) = &left
            && let Value::Int(b) = &right
            && let Some(result) = a.checked_sub(*b)
        {
            self.stack.push(Value::Int(result));
            return Ok(());
        }
        // Fast path: Num - Num
        if let Value::Num(a) = &left
            && let Value::Num(b) = &right
        {
            self.stack.push(Value::Num(a - b));
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
            let empty_fns = HashMap::new();
            let result = self.compile_and_call_function_def(&def, args, &empty_fns)?;
            return Ok(Some(result));
        }
        Ok(None)
    }

    pub(super) fn exec_mul_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // Fast path: Int * Int
        if let Value::Int(a) = &left
            && let Value::Int(b) = &right
            && let Some(result) = a.checked_mul(*b)
        {
            self.stack.push(Value::Int(result));
            return Ok(());
        }
        // Fast path: Num * Num
        if let Value::Num(a) = &left
            && let Value::Num(b) = &right
        {
            self.stack.push(Value::Num(a * b));
            return Ok(());
        }
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
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
        // Fast path: Int ** small non-negative Int
        if let Value::Int(base) = &left
            && let Value::Int(exp) = &right
            && *exp >= 0
            && *exp <= 30
        {
            let mut result: i64 = 1;
            let mut overflow = false;
            for _ in 0..*exp {
                if let Some(r) = result.checked_mul(*base) {
                    result = r;
                } else {
                    overflow = true;
                    break;
                }
            }
            if !overflow {
                self.stack.push(Value::Int(result));
                return Ok(());
            }
        }
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            let (l, r) = vm.coerce_numeric_bridge_pair_strict(l, r)?;
            Ok(crate::builtins::arith_pow(l, r))
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_negate_op(&mut self) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap();
        // Type objects (Mu, Any, etc.) cannot be negated
        if let Value::Package(name) = &val
            && matches!(name.resolve().as_str(), "Mu" | "Any")
        {
            return Err(RuntimeError::new(format!(
                "Cannot resolve caller prefix:<->({}:U); none of these signatures matches:\n    (\\a)",
                name.resolve()
            )));
        }
        // For strings, first coerce to numeric (preserving Rat/Complex types and
        // producing X::Str::Numeric for invalid strings), then negate the result.
        if let Value::Str(ref s) = val {
            let trimmed = s.trim();
            if trimmed.is_empty() {
                self.stack.push(Value::Int(0));
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
        match val {
            Value::Int(n) => self.stack.push(Value::Int(!n)),
            other => {
                let n = other.to_bigint();
                self.stack.push(Value::from_bigint(!n));
            }
        }
    }

    pub(super) fn exec_bool_bit_neg_op(&mut self) {
        let val = self.stack.pop().unwrap();
        self.stack.push(Value::Bool(!val.truthy()));
    }

    pub(super) fn exec_str_bit_neg_op(&mut self) {
        let val = self.stack.pop().unwrap();
        if Self::is_buf_value(&val) {
            let bytes = Self::extract_buf_bytes(&val);
            let negated: Vec<Value> = bytes.iter().map(|b| Value::Int((!b) as i64)).collect();
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("bytes".to_string(), Value::array(negated));
            // Determine result type: if input is utf8, result is utf8; otherwise Buf
            let result_type = if let Value::Instance { class_name, .. } = &val {
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
