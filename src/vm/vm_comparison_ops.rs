use super::*;

/// Extract (real, imaginary) parts from a value, treating non-Complex as having im=0.
fn complex_parts(v: &Value) -> (f64, f64) {
    match v {
        Value::Complex(re, im) => (*re, *im),
        Value::Mixin(inner, _) => complex_parts(inner),
        other => (value_to_f64(other), 0.0),
    }
}

pub(super) fn value_to_f64(v: &Value) -> f64 {
    runtime::to_float_value(v).unwrap_or(0.0)
}

fn is_rationalish(v: &Value) -> bool {
    matches!(
        v,
        Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _)
    )
}

/// Check if a value is NaN (for numeric comparison semantics).
fn is_nan_value(v: &Value) -> bool {
    matches!(v, Value::Num(n) if n.is_nan())
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
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            // NaN is unordered: NaN == anything is always False
            if is_nan_value(&l) || is_nan_value(&r) {
                return Ok(Value::Bool(false));
            }
            if let (Some(a), Some(b)) =
                (runtime::to_big_rat_parts(&l), runtime::to_big_rat_parts(&r))
                && (is_rationalish(&l) || is_rationalish(&r))
            {
                Ok(Value::Bool(runtime::big_rat_parts_equal(a, b)))
            } else {
                let needs_float = !std::mem::discriminant(&l).eq(&std::mem::discriminant(&r))
                    || matches!(l, Value::Nil);
                if needs_float {
                    Ok(Value::Bool(
                        runtime::to_float_value(&l) == runtime::to_float_value(&r),
                    ))
                } else {
                    Ok(Value::Bool(l == r))
                }
            }
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_num_ne_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // != is a negation meta-operator shortcut for !==.
        // It first evaluates == (which autothreads through junctions),
        // then negates the boolean-collapsed result, always returning Bool.
        let eq_result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            // NaN is unordered: NaN == anything is always False
            if is_nan_value(&l) || is_nan_value(&r) {
                return Ok(Value::Bool(false));
            }
            if let (Some(a), Some(b)) =
                (runtime::to_big_rat_parts(&l), runtime::to_big_rat_parts(&r))
                && (is_rationalish(&l) || is_rationalish(&r))
            {
                Ok(Value::Bool(runtime::big_rat_parts_equal(a, b)))
            } else if matches!(l, Value::Nil) || matches!(r, Value::Nil) {
                Ok(Value::Bool(
                    runtime::to_float_value(&l) == runtime::to_float_value(&r),
                ))
            } else {
                Ok(Value::Bool(l == r))
            }
        })?;
        self.stack.push(Value::Bool(!eq_result.truthy()));
        Ok(())
    }

    pub(super) fn exec_num_lt_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            Interpreter::compare(l, r, |o| o < 0)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_num_le_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            Interpreter::compare(l, r, |o| o <= 0)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_num_gt_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            Interpreter::compare(l, r, |o| o > 0)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_num_ge_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            Interpreter::compare(l, r, |o| o >= 0)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_approx_eq_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (left, right) = self.coerce_numeric_bridge_pair(left, right)?;
        let tolerance = self
            .interpreter
            .get_dynamic_var("*TOLERANCE")
            .ok()
            .and_then(|v| match v {
                Value::Num(n) => Some(n),
                Value::Rat(n, d) => Some(n as f64 / d as f64),
                Value::Int(n) => Some(n as f64),
                _ => None,
            })
            .unwrap_or(1e-15);
        // Extract Complex components, treating Real as Complex with im=0
        let (lr, li) = complex_parts(&left);
        let (rr, ri) = complex_parts(&right);
        let approx_f64 = |a: f64, b: f64| -> bool {
            // Equal values (including Inf == Inf, -Inf == -Inf) are always approximately equal
            if a == b {
                return true;
            }
            // NaN is never approximately equal to anything
            if a.is_nan() || b.is_nan() {
                return false;
            }
            let diff = (a - b).abs();
            // Per Raku spec: if either side is zero, use absolute tolerance;
            // otherwise use relative percentage difference.
            if a == 0.0 || b == 0.0 {
                return diff < tolerance;
            }
            let max_abs = a.abs().max(b.abs());
            diff / max_abs <= tolerance
        };
        let result = approx_f64(lr, rr) && approx_f64(li, ri);
        self.stack.push(Value::Bool(result));
        Ok(())
    }

    pub(super) fn exec_container_eq_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(Value::Bool(crate::runtime::values_identical(&left, &right)));
    }

    pub(super) fn exec_str_eq_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            if Self::is_buf_value(&l) && Self::is_buf_value(&r) {
                Ok(Value::Bool(
                    Self::buf_cmp_bytes(&l, &r) == std::cmp::Ordering::Equal,
                ))
            } else {
                Ok(Value::Bool(l.to_string_value() == r.to_string_value()))
            }
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_str_ne_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // ne is a negation meta-operator shortcut for !eq.
        // It first evaluates eq (which autothreads through junctions),
        // then negates the boolean-collapsed result, always returning Bool.
        let eq_result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            if Self::is_buf_value(&l) && Self::is_buf_value(&r) {
                Ok(Value::Bool(
                    Self::buf_cmp_bytes(&l, &r) == std::cmp::Ordering::Equal,
                ))
            } else {
                Ok(Value::Bool(l.to_string_value() == r.to_string_value()))
            }
        })?;
        self.stack.push(Value::Bool(!eq_result.truthy()));
        Ok(())
    }

    fn buf_cmp_bytes(l: &Value, r: &Value) -> std::cmp::Ordering {
        let lb = Self::extract_buf_bytes(l);
        let rb = Self::extract_buf_bytes(r);
        lb.cmp(&rb)
    }

    pub(super) fn exec_str_lt_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            if Self::is_buf_value(&l) && Self::is_buf_value(&r) {
                Ok(Value::Bool(
                    Self::buf_cmp_bytes(&l, &r) == std::cmp::Ordering::Less,
                ))
            } else {
                Ok(Value::Bool(l.to_string_value() < r.to_string_value()))
            }
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_str_gt_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            if Self::is_buf_value(&l) && Self::is_buf_value(&r) {
                Ok(Value::Bool(
                    Self::buf_cmp_bytes(&l, &r) == std::cmp::Ordering::Greater,
                ))
            } else {
                Ok(Value::Bool(l.to_string_value() > r.to_string_value()))
            }
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_str_le_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            if Self::is_buf_value(&l) && Self::is_buf_value(&r) {
                Ok(Value::Bool(
                    Self::buf_cmp_bytes(&l, &r) != std::cmp::Ordering::Greater,
                ))
            } else {
                Ok(Value::Bool(l.to_string_value() <= r.to_string_value()))
            }
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_str_ge_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            if Self::is_buf_value(&l) && Self::is_buf_value(&r) {
                Ok(Value::Bool(
                    Self::buf_cmp_bytes(&l, &r) != std::cmp::Ordering::Less,
                ))
            } else {
                Ok(Value::Bool(l.to_string_value() >= r.to_string_value()))
            }
        })?;
        self.stack.push(result);
        Ok(())
    }

    fn spaceship_ordering(left: &Value, right: &Value) -> std::cmp::Ordering {
        // Unwrap Mixin for comparison
        let left = match left {
            Value::Mixin(inner, _) => inner.as_ref(),
            other => other,
        };
        let right = match right {
            Value::Mixin(inner, _) => inner.as_ref(),
            other => other,
        };
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => a.cmp(b),
            (l, r)
                if (is_rationalish(l) || is_rationalish(r))
                    && matches!(
                        l,
                        Value::Int(_)
                            | Value::BigInt(_)
                            | Value::Rat(_, _)
                            | Value::FatRat(_, _)
                            | Value::BigRat(_, _)
                    )
                    && matches!(
                        r,
                        Value::Int(_)
                            | Value::BigInt(_)
                            | Value::Rat(_, _)
                            | Value::FatRat(_, _)
                            | Value::BigRat(_, _)
                    ) =>
            {
                runtime::to_big_rat_parts(l)
                    .zip(runtime::to_big_rat_parts(r))
                    .and_then(|(a, b)| runtime::compare_big_rat_parts(a, b))
                    .unwrap_or(std::cmp::Ordering::Equal)
            }
            (Value::Num(a), Value::Num(b)) => a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal),
            (Value::Int(a), Value::Num(b)) => (*a as f64)
                .partial_cmp(b)
                .unwrap_or(std::cmp::Ordering::Equal),
            (Value::Num(a), Value::Int(b)) => a
                .partial_cmp(&(*b as f64))
                .unwrap_or(std::cmp::Ordering::Equal),
            (
                Value::Instance {
                    attributes: left_attrs,
                    ..
                },
                Value::Instance {
                    attributes: right_attrs,
                    ..
                },
            ) if left_attrs.contains_key("year")
                && left_attrs.contains_key("month")
                && left_attrs.contains_key("day")
                && left_attrs.contains_key("hour")
                && left_attrs.contains_key("minute")
                && left_attrs.contains_key("second")
                && left_attrs.contains_key("timezone")
                && right_attrs.contains_key("year")
                && right_attrs.contains_key("month")
                && right_attrs.contains_key("day")
                && right_attrs.contains_key("hour")
                && right_attrs.contains_key("minute")
                && right_attrs.contains_key("second")
                && right_attrs.contains_key("timezone") =>
            {
                let (ly, lm, ld, lh, lmin, ls, ltz) =
                    crate::builtins::methods_0arg::temporal::datetime_attrs(left_attrs);
                let (ry, rm, rd, rh, rmin, rs, rtz) =
                    crate::builtins::methods_0arg::temporal::datetime_attrs(right_attrs);
                let left_instant =
                    crate::builtins::methods_0arg::temporal::datetime_to_instant_leap_aware(
                        ly, lm, ld, lh, lmin, ls, ltz,
                    );
                let right_instant =
                    crate::builtins::methods_0arg::temporal::datetime_to_instant_leap_aware(
                        ry, rm, rd, rh, rmin, rs, rtz,
                    );
                left_instant
                    .partial_cmp(&right_instant)
                    .unwrap_or(std::cmp::Ordering::Equal)
            }
            (l, r)
                if matches!(
                    l,
                    Value::Int(_)
                        | Value::BigInt(_)
                        | Value::Num(_)
                        | Value::Rat(_, _)
                        | Value::FatRat(_, _)
                        | Value::BigRat(_, _)
                ) && matches!(
                    r,
                    Value::Int(_)
                        | Value::BigInt(_)
                        | Value::Num(_)
                        | Value::Rat(_, _)
                        | Value::FatRat(_, _)
                        | Value::BigRat(_, _)
                ) =>
            {
                value_to_f64(l)
                    .partial_cmp(&value_to_f64(r))
                    .unwrap_or(std::cmp::Ordering::Equal)
            }
            // Complex cmp: compare real parts first, then imaginary parts
            // NaN sorts as Greater (More) in Raku
            (Value::Complex(ar, ai), Value::Complex(br, bi)) => {
                if ar.is_nan() || ai.is_nan() {
                    return std::cmp::Ordering::Greater;
                }
                match ar.partial_cmp(br).unwrap_or(std::cmp::Ordering::Greater) {
                    std::cmp::Ordering::Equal => {
                        ai.partial_cmp(bi).unwrap_or(std::cmp::Ordering::Greater)
                    }
                    ord => ord,
                }
            }
            // Complex vs Real: treat Real as Complex with im=0
            (Value::Complex(ar, ai), _) => {
                if ar.is_nan() || ai.is_nan() {
                    return std::cmp::Ordering::Greater;
                }
                let br = value_to_f64(right);
                match ar.partial_cmp(&br).unwrap_or(std::cmp::Ordering::Greater) {
                    std::cmp::Ordering::Equal => {
                        ai.partial_cmp(&0.0).unwrap_or(std::cmp::Ordering::Greater)
                    }
                    ord => ord,
                }
            }
            (_, Value::Complex(br, bi)) => {
                if br.is_nan() || bi.is_nan() {
                    return std::cmp::Ordering::Greater;
                }
                let ar = value_to_f64(left);
                match ar.partial_cmp(br).unwrap_or(std::cmp::Ordering::Greater) {
                    std::cmp::Ordering::Equal => 0.0f64
                        .partial_cmp(bi)
                        .unwrap_or(std::cmp::Ordering::Greater),
                    ord => ord,
                }
            }
            (Value::Version { parts: ap, .. }, Value::Version { parts: bp, .. }) => {
                runtime::version_cmp_parts(ap, bp)
            }
            _ => left.to_string_value().cmp(&right.to_string_value()),
        }
    }

    pub(super) fn exec_spaceship_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (left, right) = self.coerce_numeric_bridge_pair(left, right)?;
        // NaN <=> anything produces Nil (unordered)
        if is_nan_value(&left) || is_nan_value(&right) {
            self.stack.push(Value::Nil);
            return Ok(());
        }
        // Complex <=> Real: check $*TOLERANCE for imaginary part
        let left = self.coerce_complex_to_real_if_tolerant(&left)?;
        let right = self.coerce_complex_to_real_if_tolerant(&right)?;
        let ord = Self::numeric_spaceship_ordering(&left, &right)?;
        self.stack.push(runtime::make_order(ord));
        Ok(())
    }

    /// If a Complex value has an imaginary part within `$*TOLERANCE` (relative),
    /// coerce it to its real part. Otherwise throw if it's Complex with a
    /// significant imaginary component.
    fn coerce_complex_to_real_if_tolerant(&self, val: &Value) -> Result<Value, RuntimeError> {
        if let Value::Complex(re, im) = val {
            if *im == 0.0 {
                return Ok(Value::Num(*re));
            }
            let tolerance = self
                .interpreter
                .get_dynamic_var("$*TOLERANCE")
                .ok()
                .and_then(|v| runtime::to_float_value(&v))
                .unwrap_or(1e-15);
            let re_abs = re.abs();
            if re_abs != 0.0 && im.abs() / re_abs <= tolerance {
                Ok(Value::Num(*re))
            } else {
                Err(RuntimeError::new(
                    "Cannot use <=> on Complex number with non-negligible imaginary part",
                ))
            }
        } else {
            Ok(val.clone())
        }
    }

    pub(super) fn exec_before_after_op(&mut self, is_before: bool) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (left, right) = self
            .coerce_numeric_bridge_pair(left.clone(), right.clone())
            .unwrap_or((left, right));
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
        if Self::is_buf_value(&left) && Self::is_buf_value(&right) {
            let ord = Self::buf_cmp_bytes(&left, &right);
            self.stack.push(runtime::make_order(ord));
            return;
        }
        // NaN in cmp context: compare as string "NaN"
        if is_nan_value(&left) || is_nan_value(&right) {
            let ord = left.to_string_value().cmp(&right.to_string_value());
            self.stack.push(runtime::make_order(ord));
            return;
        }
        let (left, right) = self
            .coerce_numeric_bridge_pair(left.clone(), right.clone())
            .unwrap_or((left, right));
        let ord = Self::spaceship_ordering(&left, &right);
        self.stack.push(runtime::make_order(ord));
    }

    pub(super) fn exec_leg_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let ord = left.to_string_value().cmp(&right.to_string_value());
        self.stack.push(runtime::make_order(ord));
    }

    pub(super) fn exec_strict_eq_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            Ok(Value::Bool(runtime::values_identical(&l, &r)))
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_strict_ne_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // !== is a negation meta-operator applied to ===.
        // It first evaluates === (which autothreads through junctions),
        // then negates the boolean-collapsed result, always returning Bool.
        let eq_result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            Ok(Value::Bool(runtime::values_identical(&l, &r)))
        })?;
        self.stack.push(Value::Bool(!eq_result.truthy()));
        Ok(())
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
        self.transliterate_in_smartmatch = false;
        let rhs_run = self.run_range(code, rhs_start, rhs_end, compiled_fns);
        self.in_smartmatch_rhs = saved_in_smartmatch_rhs;
        let was_transliterate = self.transliterate_in_smartmatch;
        self.transliterate_in_smartmatch = false;
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
        // When RHS was a transliterate (tr///), return the result directly.
        // In Raku, $x ~~ tr/a/b/ returns a StrDistance that stringifies to the after-string.
        let out = if was_transliterate {
            if negate {
                // !~~ tr/// — negate the truthiness of the result
                Value::Bool(!right.truthy())
            } else {
                right
            }
        } else if negate {
            // Smartmatch must NOT force lazy values (lazy ~~ anything → False)
            self.eval_smartmatch_with_junctions(left, right, true)?
        } else {
            self.eval_smartmatch_with_junctions(left, right, false)?
        };
        self.stack.push(out);
        self.env_dirty = true;
        *ip = rhs_end;
        Ok(())
    }

    pub(super) fn exec_divisible_by_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (l, r) = runtime::coerce_numeric(left.clone(), right);
        let result = match (l, r) {
            (Value::Int(_), Value::Int(0)) => {
                return Err(RuntimeError::numeric_divide_by_zero_full(
                    Some(left),
                    Some("infix:<%%>"),
                ));
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
        let (l, r) = runtime::coerce_numeric(left.clone(), right);
        let result = match (l, r) {
            (Value::Int(_), Value::Int(0)) => {
                return Err(RuntimeError::numeric_divide_by_zero_full(
                    Some(left),
                    Some("infix:<%%>"),
                ));
            }
            (Value::Int(a), Value::Int(b)) => Value::Bool(a % b != 0),
            _ => Value::Bool(true),
        };
        self.stack.push(result);
        Ok(())
    }
}
