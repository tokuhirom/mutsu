use super::*;
use std::sync::Arc;

impl VM {
    pub(super) fn exec_add_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
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
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            Ok(crate::builtins::arith_mul(l, r))
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_div_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self
            .eval_binary_with_junctions(left, right, |_, l, r| crate::builtins::arith_div(l, r))?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_mod_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self
            .eval_binary_with_junctions(left, right, |_, l, r| crate::builtins::arith_mod(l, r))?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_pow_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            Ok(crate::builtins::arith_pow(l, r))
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_negate_op(&mut self) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap();
        self.stack.push(crate::builtins::arith_negate(val)?);
        Ok(())
    }

    pub(super) fn exec_int_bit_neg_op(&mut self) {
        let val = self.stack.pop().unwrap();
        match &val {
            Value::Int(n) => self.stack.push(Value::Int(!n)),
            _ => {
                use num_traits::ToPrimitive;
                let n = val.to_bigint();
                let i = n.to_i64().unwrap_or(0);
                self.stack.push(Value::Int(!i));
            }
        }
    }

    pub(super) fn exec_bool_bit_neg_op(&mut self) {
        let val = self.stack.pop().unwrap();
        self.stack.push(Value::Bool(!val.truthy()));
    }

    pub(super) fn exec_make_slip_op(&mut self) {
        let val = self.stack.pop().unwrap();
        let items = match val {
            Value::Array(items, ..) => (*items).clone(),
            Value::Slip(items) => (*items).clone(),
            Value::Seq(items) => (*items).clone(),
            Value::Capture { positional, named } => {
                let mut items = positional;
                for (k, v) in named {
                    items.push(Value::Pair(k, Box::new(v)));
                }
                items
            }
            Value::Hash(map) => map
                .iter()
                .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                .collect(),
            other => vec![other],
        };
        self.stack.push(Value::slip(items));
    }

    pub(super) fn exec_not_op(&mut self) {
        let val = self.stack.pop().unwrap();
        self.stack.push(Value::Bool(!val.truthy()));
    }

    pub(super) fn exec_bool_coerce_op(&mut self) {
        let val = self.stack.pop().unwrap();
        self.stack.push(Value::Bool(val.truthy()));
    }

    pub(super) fn exec_concat_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack.push(Value::Str(format!(
            "{}{}",
            left.to_string_value(),
            right.to_string_value()
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
        let ord = cmp_values(&left, &right);
        self.stack.push(if ord.is_le() { left } else { right });
    }

    pub(super) fn exec_infix_max_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let ord = cmp_values(&left, &right);
        self.stack.push(if ord.is_ge() { left } else { right });
    }

    pub(super) fn exec_string_repeat_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let s = left.to_string_value();
        let n = match right {
            Value::Int(n) => n.max(0) as usize,
            _ => 0,
        };
        self.stack.push(Value::Str(s.repeat(n)));
    }

    pub(super) fn exec_list_repeat_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let n = match right {
            Value::Int(n) => n.max(0) as usize,
            _ => 0,
        };
        // When repeating a Slip, flatten its items into the result
        if let Value::Slip(ref slip_items) = left {
            let mut items = Vec::with_capacity(slip_items.len() * n);
            for _ in 0..n {
                items.extend(slip_items.iter().cloned());
            }
            self.stack.push(Value::Seq(Arc::new(items)));
        } else {
            let items: Vec<Value> = std::iter::repeat_n(left, n).collect();
            self.stack.push(Value::Seq(Arc::new(items)));
        }
    }

    pub(super) fn exec_function_compose_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        use crate::ast::{Expr, Stmt};
        static COMPOSE_ID: std::sync::atomic::AtomicU64 =
            std::sync::atomic::AtomicU64::new(1_000_000);
        let id = COMPOSE_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let left_name = format!("__compose_left_{}__", id);
        let right_name = format!("__compose_right_{}__", id);
        let env = {
            let mut env = std::collections::HashMap::new();
            env.insert(left_name.clone(), left);
            env.insert(right_name.clone(), right);
            env
        };
        let composed = Value::make_sub_with_id(
            String::new(),
            "<composed>".to_string(),
            vec!["x".to_string()],
            vec![Stmt::Expr(Expr::Call {
                name: left_name,
                args: vec![Expr::Call {
                    name: right_name,
                    args: vec![Expr::Var("x".to_string())],
                }],
            })],
            env,
            id,
        );
        self.stack.push(composed);
    }

    pub(super) fn exec_but_mixin_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // Determine the mixin type from the right-hand value
        let mixin_type = match &right {
            Value::Bool(_) => "Bool".to_string(),
            Value::Int(_) | Value::BigInt(_) => "Int".to_string(),
            Value::Num(_) => "Num".to_string(),
            Value::Str(_) => "Str".to_string(),
            Value::Enum { enum_type, .. } => enum_type.clone(),
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
            _ => Value::Int(0),
        };
        self.stack.push(result);
    }

    pub(super) fn exec_bit_shift_left_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (l, r) = runtime::coerce_numeric(left, right);
        let result = match (l, r) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a << b),
            _ => Value::Int(0),
        };
        self.stack.push(result);
    }

    pub(super) fn exec_bit_shift_right_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (l, r) = runtime::coerce_numeric(left, right);
        let result = match (l, r) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a >> b),
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
