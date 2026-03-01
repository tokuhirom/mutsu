use super::*;
use std::sync::Arc;

impl VM {
    fn is_xx_reeval_thunk(data: &crate::value::SubData) -> bool {
        if !data.params.is_empty()
            || !data.param_defs.is_empty()
            || !data.assumed_positional.is_empty()
            || !data.assumed_named.is_empty()
            || data.body.len() != 1
        {
            return false;
        }
        match &data.body[0] {
            crate::ast::Stmt::Expr(expr) => matches!(
                expr,
                crate::ast::Expr::Call { .. }
                    | crate::ast::Expr::MethodCall { .. }
                    | crate::ast::Expr::DynamicMethodCall { .. }
                    | crate::ast::Expr::HyperMethodCall { .. }
                    | crate::ast::Expr::HyperMethodCallDynamic { .. }
                    | crate::ast::Expr::CallOn { .. }
                    | crate::ast::Expr::Block(_)
                    | crate::ast::Expr::DoBlock { .. }
                    | crate::ast::Expr::AnonSub { .. }
                    | crate::ast::Expr::AnonSubParams { .. }
                    | crate::ast::Expr::Lambda { .. }
            ),
            _ => false,
        }
    }

    pub(super) fn exec_add_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            if let Some(result) = vm.try_user_infix("infix:<+>", &l, &r)? {
                Ok(result)
            } else {
                crate::builtins::arith_add(l, r)
            }
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_sub_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            if let Some(result) = vm.try_user_infix("infix:<->", &l, &r)? {
                Ok(result)
            } else {
                Ok(crate::builtins::arith_sub(l, r))
            }
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
        if let Some(def) = self.interpreter.resolve_function_with_types(op_name, &args) {
            let result = self.interpreter.call_function_def(&def, &args)?;
            return Ok(Some(result));
        }
        Ok(None)
    }

    pub(super) fn exec_mul_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            Ok(crate::builtins::arith_mul(l, r))
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_div_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            crate::builtins::arith_div(l, r)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_mod_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            crate::builtins::arith_mod(l, r)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_pow_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            Ok(crate::builtins::arith_pow(l, r))
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_negate_op(&mut self) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap();
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

    pub(super) fn exec_decont_op(&mut self) {
        let val = self.stack.pop().unwrap();
        let new_val = match val {
            Value::Array(items, _) => Value::Array(items, false),
            other => other,
        };
        self.stack.push(new_val);
    }

    pub(super) fn exec_make_slip_op(&mut self) {
        let val = self.stack.pop().unwrap();
        let items = match &val {
            Value::Array(items, ..) => (*items).to_vec(),
            Value::Slip(items) => (*items).to_vec(),
            Value::Seq(items) => (*items).to_vec(),
            Value::Capture { positional, named } => {
                let mut items = positional.clone();
                for (k, v) in named {
                    items.push(Value::Pair(k.clone(), Box::new(v.clone())));
                }
                items
            }
            Value::Hash(map) => map
                .iter()
                .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                .collect(),
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => crate::runtime::utils::value_to_list(&val),
            _ => vec![val],
        };
        self.stack.push(Value::slip(items));
    }

    pub(super) fn exec_not_op(&mut self) {
        let val = self.stack.pop().unwrap();
        self.stack.push(Value::Bool(!val.truthy()));
    }

    pub(super) fn exec_bool_coerce_op(&mut self) {
        let val = self.stack.pop().unwrap();
        let out = match &val {
            Value::Regex(_)
            | Value::RegexWithAdverbs { .. }
            | Value::Routine { is_regex: true, .. } => {
                let topic = self
                    .interpreter
                    .env()
                    .get("_")
                    .cloned()
                    .unwrap_or(Value::Nil);
                Value::Bool(self.interpreter.smart_match_values(&topic, &val))
            }
            _ => Value::Bool(val.truthy()),
        };
        self.stack.push(out);
    }

    pub(super) fn exec_concat_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack.push(Value::Str(format!(
            "{}{}",
            crate::runtime::utils::coerce_to_str(&left),
            crate::runtime::utils::coerce_to_str(&right)
        )));
    }

    pub(super) fn exec_int_div_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            let val = match (&l, &r) {
                (Value::Int(a), Value::Int(b)) if *b != 0 => Value::Int(a.div_euclid(*b)),
                (Value::Int(_), Value::Int(_)) => {
                    return Err(RuntimeError::numeric_divide_by_zero());
                }
                (Value::BigInt(a), Value::BigInt(b)) if *b != num_bigint::BigInt::from(0i64) => {
                    Value::from_bigint(num_integer::Integer::div_floor(a, b))
                }
                (Value::BigInt(a), Value::Int(b)) if *b != 0 => {
                    let bb = num_bigint::BigInt::from(*b);
                    Value::from_bigint(num_integer::Integer::div_floor(a, &bb))
                }
                (Value::Int(a), Value::BigInt(b)) if *b != num_bigint::BigInt::from(0i64) => {
                    let aa = num_bigint::BigInt::from(*a);
                    Value::from_bigint(num_integer::Integer::div_floor(&aa, b))
                }
                _ => {
                    let a = runtime::to_int(&l);
                    let b = runtime::to_int(&r);
                    if b == 0 {
                        return Err(RuntimeError::numeric_divide_by_zero());
                    }
                    Value::Int(a.div_euclid(b))
                }
            };
            Ok(val)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_int_mod_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            let val = match (&l, &r) {
                (Value::Int(a), Value::Int(b)) if *b != 0 => Value::Int(a.rem_euclid(*b)),
                (Value::Int(_), Value::Int(_)) => {
                    return Err(RuntimeError::new("Modulo by zero"));
                }
                _ => {
                    let a = runtime::to_int(&l);
                    let b = runtime::to_int(&r);
                    if b == 0 {
                        return Err(RuntimeError::new("Modulo by zero"));
                    }
                    Value::Int(a.rem_euclid(b))
                }
            };
            Ok(val)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_gcd_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let a = left.to_bigint().abs();
        let b = right.to_bigint().abs();
        let g = num_integer::Integer::gcd(&a, &b);
        self.stack.push(Value::from_bigint(g));
    }

    pub(super) fn exec_lcm_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let a = left.to_bigint().abs();
        let b = right.to_bigint().abs();
        let result = if a.is_zero() && b.is_zero() {
            Value::Int(0)
        } else {
            let g = num_integer::Integer::gcd(&a, &b);
            Value::from_bigint(&a / &g * &b)
        };
        self.stack.push(result);
    }

    pub(super) fn exec_infix_min_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        if matches!(&left, Value::Package(name) if name == "Any") {
            self.stack.push(right);
            return;
        }
        if matches!(&right, Value::Package(name) if name == "Any") {
            self.stack.push(left);
            return;
        }
        let left_is_failure =
            matches!(&left, Value::Instance { class_name, .. } if class_name == "Failure");
        let right_is_failure =
            matches!(&right, Value::Instance { class_name, .. } if class_name == "Failure");
        if left_is_failure {
            self.stack.push(left);
            return;
        }
        if right_is_failure {
            self.stack.push(right);
            return;
        }
        let ord = cmp_values(&left, &right);
        self.stack.push(if ord.is_le() { left } else { right });
    }

    pub(super) fn exec_infix_max_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        if matches!(&left, Value::Package(name) if name == "Any") {
            self.stack.push(right);
            return;
        }
        if matches!(&right, Value::Package(name) if name == "Any") {
            self.stack.push(left);
            return;
        }
        let left_is_failure =
            matches!(&left, Value::Instance { class_name, .. } if class_name == "Failure");
        let right_is_failure =
            matches!(&right, Value::Instance { class_name, .. } if class_name == "Failure");
        if left_is_failure {
            self.stack.push(left);
            return;
        }
        if right_is_failure {
            self.stack.push(right);
            return;
        }
        let ord = cmp_values(&left, &right);
        self.stack.push(if ord.is_ge() { left } else { right });
    }

    pub(super) fn exec_string_repeat_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self
            .interpreter
            .call_function("infix:<x>", vec![left, right])?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_list_repeat_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let thunk = match &left {
            Value::Sub(data) => Some(data.clone()),
            Value::WeakSub(weak) => weak.upgrade(),
            _ => None,
        };
        if let Some(data) = thunk.filter(|data| Self::is_xx_reeval_thunk(data.as_ref()))
            && let Value::Int(n) = right
        {
            let n = n.max(0) as usize;
            let items = self.interpreter.eval_xx_repeat_thunk(data.as_ref(), n)?;
            self.stack.push(Value::Seq(Arc::new(items)));
            return Ok(());
        }
        let result = self
            .interpreter
            .call_function("infix:<xx>", vec![left, right])?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_function_compose_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let composed = self.interpreter.compose_callables(left, right);
        self.stack.push(composed);
    }

    pub(super) fn exec_but_mixin_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let role_composed = match &right {
            Value::Pair(name, boxed)
                if self.interpreter.has_role(name)
                    && matches!(boxed.as_ref(), Value::Array(..)) =>
            {
                Some(
                    self.interpreter
                        .eval_does_values(left.clone(), right.clone()),
                )
            }
            Value::Package(name) if self.interpreter.has_role(&name.resolve()) => Some(
                self.interpreter
                    .eval_does_values(left.clone(), right.clone()),
            ),
            Value::Str(name) if self.interpreter.has_role(name) => Some(
                self.interpreter
                    .eval_does_values(left.clone(), right.clone()),
            ),
            _ => None,
        };
        if let Some(composed) = role_composed {
            self.stack.push(composed?);
            return Ok(());
        }
        // Determine the mixin type from the right-hand value
        let mixin_type = match &right {
            Value::Bool(_) => "Bool".to_string(),
            Value::Int(_) | Value::BigInt(_) => "Int".to_string(),
            Value::Num(_) => "Num".to_string(),
            Value::Str(_) => "Str".to_string(),
            Value::Package(name) => name.resolve(),
            Value::Enum { enum_type, .. } => enum_type.resolve(),
            _ => "Any".to_string(),
        };
        // If left is already a Mixin, add to existing mixins
        let result = match left {
            Value::Mixin(inner, mut mixins) => {
                mixins.insert(mixin_type.clone(), right);
                Value::Mixin(inner, mixins)
            }
            other => {
                let mut mixins = std::collections::HashMap::new();
                mixins.insert(mixin_type, right);
                Value::Mixin(Box::new(other), mixins)
            }
        };
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_isa_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let type_name = right.to_string_value();
        let result = left.isa_check(&type_name);
        self.stack.push(Value::Bool(result));
    }

    pub(super) fn exec_does_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.interpreter.eval_does_values(left, right)?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_does_var_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let updated = self.interpreter.eval_does_values(left, right)?;
        let name = Self::const_str(code, name_idx).to_string();
        self.interpreter
            .env_mut()
            .insert(name.clone(), updated.clone());
        self.update_local_if_exists(code, &name, &updated);
        self.stack.push(updated);
        Ok(())
    }

    pub(super) fn exec_make_pair_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // Preserve the original key type for `.key` to return the correct type.
        // Use ValuePair for non-string keys, Pair for string keys.
        match &left {
            Value::Str(_) => {
                let key = left.to_string_value();
                self.stack.push(Value::Pair(key, Box::new(right)));
            }
            _ => {
                self.stack
                    .push(Value::ValuePair(Box::new(left), Box::new(right)));
            }
        }
    }

    pub(super) fn exec_bit_and_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (l, r) = runtime::coerce_numeric(left, right);
        let result = match (l, r) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a & b),
            (Value::BigInt(a), Value::BigInt(b)) => Value::from_bigint(a & b),
            (Value::BigInt(a), Value::Int(b)) => {
                Value::from_bigint(a & num_bigint::BigInt::from(b))
            }
            (Value::Int(a), Value::BigInt(b)) => {
                Value::from_bigint(num_bigint::BigInt::from(a) & b)
            }
            _ => Value::Int(0),
        };
        self.stack.push(result);
    }

    pub(super) fn exec_bit_or_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (l, r) = runtime::coerce_numeric(left, right);
        let result = match (l, r) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a | b),
            (Value::BigInt(a), Value::BigInt(b)) => Value::from_bigint(a | b),
            (Value::BigInt(a), Value::Int(b)) => {
                Value::from_bigint(a | num_bigint::BigInt::from(b))
            }
            (Value::Int(a), Value::BigInt(b)) => {
                Value::from_bigint(num_bigint::BigInt::from(a) | b)
            }
            _ => Value::Int(0),
        };
        self.stack.push(result);
    }

    pub(super) fn exec_bit_xor_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (l, r) = runtime::coerce_numeric(left, right);
        let result = match (l, r) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a ^ b),
            (Value::BigInt(a), Value::BigInt(b)) => Value::from_bigint(a ^ b),
            (Value::BigInt(a), Value::Int(b)) => {
                Value::from_bigint(a ^ num_bigint::BigInt::from(b))
            }
            (Value::Int(a), Value::BigInt(b)) => {
                Value::from_bigint(num_bigint::BigInt::from(a) ^ b)
            }
            _ => Value::Int(0),
        };
        self.stack.push(result);
    }

    pub(super) fn exec_bit_shift_left_op(&mut self) {
        fn shift_left_i64(a: i64, b: i64) -> Value {
            if b < 0 {
                let shift = b.unsigned_abs();
                let shifted = if shift >= i64::BITS as u64 {
                    if a < 0 { -1 } else { 0 }
                } else {
                    a >> (shift as u32)
                };
                return Value::Int(shifted);
            }
            let shift = b as u64;
            if shift >= i64::BITS as u64 {
                return Value::from_bigint(num_bigint::BigInt::from(a) << (shift as usize));
            }
            // Use BigInt for the shift to avoid i64 overflow (Raku integers are arbitrary precision)
            Value::from_bigint(num_bigint::BigInt::from(a) << (shift as usize))
        }

        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (l, r) = runtime::coerce_numeric(left, right);
        let result = match (l, r) {
            (Value::Int(a), Value::Int(b)) => shift_left_i64(a, b),
            _ => Value::Int(0),
        };
        self.stack.push(result);
    }

    pub(super) fn exec_bit_shift_right_op(&mut self) {
        fn shift_right_i64(a: i64, b: i64) -> Value {
            if b < 0 {
                let shift = b.unsigned_abs();
                if shift >= i64::BITS as u64 {
                    return Value::from_bigint(num_bigint::BigInt::from(a) << (shift as usize));
                }
                if let Some(v) = a.checked_shl(shift as u32) {
                    Value::Int(v)
                } else {
                    Value::from_bigint(num_bigint::BigInt::from(a) << (shift as usize))
                }
            } else {
                let shift = b as u64;
                let shifted = if shift >= i64::BITS as u64 {
                    if a < 0 { -1 } else { 0 }
                } else {
                    a >> (shift as u32)
                };
                Value::Int(shifted)
            }
        }

        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (l, r) = runtime::coerce_numeric(left, right);
        let result = match (l, r) {
            (Value::Int(a), Value::Int(b)) => shift_right_i64(a, b),
            _ => Value::Int(0),
        };
        self.stack.push(result);
    }

    pub(super) fn exec_bool_bit_or_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack.push(Value::Bool(left.truthy() | right.truthy()));
    }

    pub(super) fn exec_bool_bit_and_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack.push(Value::Bool(left.truthy() & right.truthy()));
    }

    pub(super) fn exec_bool_bit_xor_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack.push(Value::Bool(left.truthy() ^ right.truthy()));
    }
}
