use super::*;

pub(super) fn value_to_f64(v: &Value) -> f64 {
    match v {
        Value::Int(n) => *n as f64,
        Value::BigInt(n) => num_traits::ToPrimitive::to_f64(n).unwrap_or(0.0),
        Value::Num(n) => *n,
        Value::Rat(n, d) => {
            if *d != 0 {
                *n as f64 / *d as f64
            } else {
                0.0
            }
        }
        Value::BigRat(n, d) => {
            if d != &num_bigint::BigInt::from(0) {
                num_traits::ToPrimitive::to_f64(n).unwrap_or(0.0)
                    / num_traits::ToPrimitive::to_f64(d).unwrap_or(1.0)
            } else {
                0.0
            }
        }
        Value::Str(s) => s.parse::<f64>().unwrap_or(0.0),
        _ => 0.0,
    }
}

impl VM {
    fn parse_numeric_string_for_spaceship(s: &str) -> Result<f64, RuntimeError> {
        s.trim().parse::<f64>().map_err(|_| {
            RuntimeError::new(format!(
                "X::Str::Numeric: Cannot convert string '{}' to a number",
                s
            ))
        })
    }

    fn numeric_spaceship_ordering(
        left: &Value,
        right: &Value,
    ) -> Result<std::cmp::Ordering, RuntimeError> {
        match (left, right) {
            (Value::Str(a), Value::Str(b)) => {
                let a = Self::parse_numeric_string_for_spaceship(a)?;
                let b = Self::parse_numeric_string_for_spaceship(b)?;
                Ok(a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal))
            }
            (Value::Str(a), _) => {
                let a = Self::parse_numeric_string_for_spaceship(a)?;
                let b = runtime::to_float_value(right).unwrap_or(0.0);
                Ok(a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal))
            }
            (_, Value::Str(b)) => {
                let a = runtime::to_float_value(left).unwrap_or(0.0);
                let b = Self::parse_numeric_string_for_spaceship(b)?;
                Ok(a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal))
            }
            _ => Ok(Self::spaceship_ordering(left, right)),
        }
    }

    pub(super) fn exec_num_eq_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            let needs_float = !std::mem::discriminant(&l).eq(&std::mem::discriminant(&r))
                || matches!(l, Value::Nil)
                || matches!(l, Value::Rat(_, _));
            if needs_float {
                Ok(Value::Bool(
                    runtime::to_float_value(&l) == runtime::to_float_value(&r),
                ))
            } else {
                Ok(Value::Bool(l == r))
            }
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_num_ne_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            if matches!(l, Value::Nil) || matches!(r, Value::Nil) {
                Ok(Value::Bool(
                    runtime::to_float_value(&l) != runtime::to_float_value(&r),
                ))
            } else {
                Ok(Value::Bool(l != r))
            }
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_num_lt_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            Interpreter::compare(l, r, |o| o < 0)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_num_le_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            Interpreter::compare(l, r, |o| o <= 0)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_num_gt_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            Interpreter::compare(l, r, |o| o > 0)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_num_ge_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            Interpreter::compare(l, r, |o| o >= 0)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_approx_eq_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let tolerance = self
            .interpreter
            .env()
            .get("*TOLERANCE")
            .and_then(|v| match v {
                Value::Num(n) => Some(*n),
                _ => None,
            })
            .unwrap_or(1e-15);
        let a = value_to_f64(&left);
        let b = value_to_f64(&right);
        let max_abs = a.abs().max(b.abs());
        let result = if max_abs == 0.0 {
            true
        } else {
            (a - b).abs() / max_abs <= tolerance
        };
        self.stack.push(Value::Bool(result));
        Ok(())
    }

    pub(super) fn exec_container_eq_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack.push(Value::Bool(left == right));
    }

    pub(super) fn exec_str_eq_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            Ok(Value::Bool(l.to_string_value() == r.to_string_value()))
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_str_ne_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            Ok(Value::Bool(l.to_string_value() != r.to_string_value()))
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_str_lt_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            Ok(Value::Bool(l.to_string_value() < r.to_string_value()))
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_str_gt_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            Ok(Value::Bool(l.to_string_value() > r.to_string_value()))
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_str_le_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            Ok(Value::Bool(l.to_string_value() <= r.to_string_value()))
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_str_ge_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            Ok(Value::Bool(l.to_string_value() >= r.to_string_value()))
        })?;
        self.stack.push(result);
        Ok(())
    }

    fn spaceship_ordering(left: &Value, right: &Value) -> std::cmp::Ordering {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => a.cmp(b),
            (Value::Rat(_, _), _)
            | (_, Value::Rat(_, _))
            | (Value::FatRat(_, _), _)
            | (_, Value::FatRat(_, _)) => {
                if let (Some((an, ad)), Some((bn, bd))) =
                    (runtime::to_rat_parts(left), runtime::to_rat_parts(right))
                {
                    runtime::compare_rat_parts((an, ad), (bn, bd))
                } else {
                    left.to_string_value().cmp(&right.to_string_value())
                }
            }
            (Value::Num(a), Value::Num(b)) => a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal),
            (Value::Int(a), Value::Num(b)) => (*a as f64)
                .partial_cmp(b)
                .unwrap_or(std::cmp::Ordering::Equal),
            (Value::Num(a), Value::Int(b)) => a
                .partial_cmp(&(*b as f64))
                .unwrap_or(std::cmp::Ordering::Equal),
            (Value::Version { parts: ap, .. }, Value::Version { parts: bp, .. }) => {
                runtime::version_cmp_parts(ap, bp)
            }
            _ => left.to_string_value().cmp(&right.to_string_value()),
        }
    }

    pub(super) fn exec_spaceship_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let ord = Self::numeric_spaceship_ordering(&left, &right)?;
        self.stack.push(runtime::make_order(ord));
        Ok(())
    }

    pub(super) fn exec_before_after_op(&mut self, is_before: bool) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let ord = Self::spaceship_ordering(&left, &right);
        let result = if is_before {
            ord == std::cmp::Ordering::Less
        } else {
            ord == std::cmp::Ordering::Greater
        };
        self.stack.push(Value::Bool(result));
    }

    pub(super) fn exec_cmp_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let ord = Self::spaceship_ordering(&left, &right);
        self.stack.push(runtime::make_order(ord));
    }

    pub(super) fn exec_leg_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let ord = left.to_string_value().cmp(&right.to_string_value());
        self.stack.push(runtime::make_order(ord));
    }

    pub(super) fn exec_strict_eq_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack.push(Value::Bool(left == right));
    }

    pub(super) fn exec_strict_ne_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack.push(Value::Bool(left != right));
    }

    pub(super) fn exec_eqv_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result =
            self.eval_binary_with_junctions(left, right, |_, l, r| Ok(Value::Bool(l.eqv(&r))))?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_smart_match_expr_op(
        &mut self,
        code: &CompiledCode,
        ip: &mut usize,
        rhs_end: u32,
        negate: bool,
        lhs_var: &Option<String>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let left = self.stack.pop().unwrap();
        let rhs_start = *ip + 1;
        let rhs_end = rhs_end as usize;
        let saved_topic = self.interpreter.env().get("_").cloned();
        self.interpreter
            .env_mut()
            .insert("_".to_string(), left.clone());
        let saved_in_smartmatch_rhs = self.in_smartmatch_rhs;
        self.in_smartmatch_rhs = true;
        let rhs_run = self.run_range(code, rhs_start, rhs_end, compiled_fns);
        self.in_smartmatch_rhs = saved_in_smartmatch_rhs;
        rhs_run?;
        let right = self.stack.pop().unwrap_or(Value::Nil);
        if let Some(var_name) = lhs_var {
            let modified_topic = self
                .interpreter
                .env()
                .get("_")
                .cloned()
                .unwrap_or(Value::Nil);
            self.interpreter
                .env_mut()
                .insert(var_name.clone(), modified_topic);
        }
        if let Some(v) = saved_topic {
            self.interpreter.env_mut().insert("_".to_string(), v);
        } else {
            self.interpreter.env_mut().remove("_");
        }
        let out = if negate {
            self.eval_binary_with_junctions(left, right, Self::not_smart_match_op)?
        } else {
            self.eval_binary_with_junctions(left, right, Self::smart_match_op)?
        };
        self.stack.push(out);
        self.sync_locals_from_env(code);
        *ip = rhs_end;
        Ok(())
    }

    pub(super) fn exec_divisible_by_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (l, r) = runtime::coerce_numeric(left, right);
        let result = match (l, r) {
            (Value::Int(_), Value::Int(0)) => {
                return Err(RuntimeError::new("Divisibility by zero"));
            }
            (Value::Int(a), Value::Int(b)) => Value::Bool(a % b == 0),
            _ => Value::Bool(false),
        };
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_not_divisible_by_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (l, r) = runtime::coerce_numeric(left, right);
        let result = match (l, r) {
            (Value::Int(_), Value::Int(0)) => {
                return Err(RuntimeError::new("Divisibility by zero"));
            }
            (Value::Int(a), Value::Int(b)) => Value::Bool(a % b != 0),
            _ => Value::Bool(true),
        };
        self.stack.push(result);
        Ok(())
    }
}
