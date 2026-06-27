use super::*;

/// Check if a value is a numeric type object (undefined) used in numeric context.
/// In Raku, numeric type objects (Int, Num, Rat, etc.) throw a hard error when
/// used in numeric comparison, even inside `quietly`. Non-numeric type objects
/// (Any, Str, etc.) only produce a suppressible warning and coerce to 0.
fn check_type_object_in_numeric_context(v: &Value) -> Result<(), RuntimeError> {
    if let Value::Package(name) = v {
        let type_name = name.resolve();
        let is_numeric_type = matches!(
            type_name.as_ref(),
            "Int" | "Num" | "Rat" | "FatRat" | "Complex" | "int" | "num"
        );
        if is_numeric_type {
            return Err(RuntimeError::new(format!(
                "Use of uninitialized value of type {} in numeric context",
                type_name
            )));
        }
    }
    Ok(())
}

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

pub(super) fn is_rationalish(v: &Value) -> bool {
    matches!(
        v,
        Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _)
    )
}

/// Check if a value is NaN (for numeric comparison semantics).
/// In Raku, Rat(0,0) and FatRat(0,0) behave like NaN.
pub(super) fn is_nan_value(v: &Value) -> bool {
    match v {
        Value::Num(n) => n.is_nan(),
        Value::Rat(0, 0) | Value::FatRat(0, 0) => true,
        Value::BigRat(n, d) => n.is_zero() && d.is_zero(),
        _ => false,
    }
}

/// Check if a value is Inf (positive infinity).
/// Includes Rat(n,0)/FatRat(n,0) where n > 0.
pub(super) fn is_pos_inf(v: &Value) -> bool {
    match v {
        Value::Num(n) => n.is_infinite() && n.is_sign_positive(),
        Value::Rat(n, 0) | Value::FatRat(n, 0) => *n > 0,
        Value::BigRat(n, d) => !n.is_zero() && d.is_zero() && n.is_positive(),
        _ => false,
    }
}

/// Check if a value is -Inf (negative infinity).
/// Includes Rat(n,0)/FatRat(n,0) where n < 0.
pub(super) fn is_neg_inf(v: &Value) -> bool {
    match v {
        Value::Num(n) => n.is_infinite() && n.is_sign_negative(),
        Value::Rat(n, 0) | Value::FatRat(n, 0) => *n < 0,
        Value::BigRat(n, d) => !n.is_zero() && d.is_zero() && n.is_negative(),
        _ => false,
    }
}

/// Extract range components (start, end, excl_start, excl_end) from a range value.
fn extract_range_parts(v: &Value) -> Option<(Value, Value, bool, bool)> {
    match v {
        Value::Range(a, b) => Some((Value::Int(*a), Value::Int(*b), false, false)),
        Value::RangeExcl(a, b) => Some((Value::Int(*a), Value::Int(*b), false, true)),
        Value::RangeExclStart(a, b) => Some((Value::Int(*a), Value::Int(*b), true, false)),
        Value::RangeExclBoth(a, b) => Some((Value::Int(*a), Value::Int(*b), true, true)),
        Value::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => Some((
            start.as_ref().clone(),
            end.as_ref().clone(),
            *excl_start,
            *excl_end,
        )),
        _ => None,
    }
}

/// Compare two ranges using Raku cmp semantics:
/// min first, then excludes-min (False < True), then max, then excludes-max (True < False).
pub(super) fn range_cmp(left: &Value, right: &Value) -> std::cmp::Ordering {
    let (l_start, l_end, l_excl_start, l_excl_end) = extract_range_parts(left).unwrap();
    let (r_start, r_end, r_excl_start, r_excl_end) = extract_range_parts(right).unwrap();

    // Compare min values using cmp semantics
    let start_cmp = cmp_values(&l_start, &r_start);
    if start_cmp != std::cmp::Ordering::Equal {
        return start_cmp;
    }
    // Compare excludes-min: False < True (excluding min means higher effective start)
    let excl_start_cmp = l_excl_start.cmp(&r_excl_start);
    if excl_start_cmp != std::cmp::Ordering::Equal {
        return excl_start_cmp;
    }
    // Compare max values
    let end_cmp = cmp_values(&l_end, &r_end);
    if end_cmp != std::cmp::Ordering::Equal {
        return end_cmp;
    }
    // Compare excludes-max: True < False (excluding max means lower effective end)
    // Note: we reverse the comparison here
    r_excl_end.cmp(&l_excl_end)
}

/// Recursively apply cmp semantics to two values (for use in list element comparison).
pub(super) fn cmp_values(left: &Value, right: &Value) -> std::cmp::Ordering {
    // Handle NaN
    if is_nan_value(left) || is_nan_value(right) {
        return left.to_string_value().cmp(&right.to_string_value());
    }

    // Unwrap Mixin
    let left = match left {
        Value::Mixin(inner, _) => inner.as_ref(),
        other => other,
    };
    let right = match right {
        Value::Mixin(inner, _) => inner.as_ref(),
        other => other,
    };

    // Inf/-Inf handling for non-numeric types
    if is_pos_inf(left) {
        return if is_pos_inf(right) {
            std::cmp::Ordering::Equal
        } else {
            std::cmp::Ordering::Greater
        };
    }
    if is_neg_inf(left) {
        return if is_neg_inf(right) {
            std::cmp::Ordering::Equal
        } else {
            std::cmp::Ordering::Less
        };
    }
    if is_pos_inf(right) {
        return std::cmp::Ordering::Less;
    }
    if is_neg_inf(right) {
        return std::cmp::Ordering::Greater;
    }

    Interpreter::spaceship_ordering(left, right)
}

/// Get list elements from a value (Array, Seq, List, Slip, etc.)
pub(super) fn get_list_elements(v: &Value) -> Option<&[Value]> {
    match v {
        Value::Array(elems, _) => Some(elems.as_slice()),
        Value::Seq(elems) | Value::HyperSeq(elems) | Value::RaceSeq(elems) => {
            Some(elems.as_slice())
        }
        Value::Slip(elems) => Some(elems.as_slice()),
        _ => None,
    }
}

/// Check if a value is any range variant.
pub(super) fn is_range_value(v: &Value) -> bool {
    extract_range_parts(v).is_some()
}

/// Expand a range value into a list of integer values (for list-based comparison).
pub(super) fn expand_range_to_list(v: &Value) -> Vec<Value> {
    match v {
        Value::Range(a, b) => (*a..=*b).map(Value::Int).collect(),
        Value::RangeExcl(a, b) => (*a..*b).map(Value::Int).collect(),
        Value::RangeExclStart(a, b) => ((*a + 1)..=*b).map(Value::Int).collect(),
        Value::RangeExclBoth(a, b) => ((*a + 1)..*b).map(Value::Int).collect(),
        Value::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
            // For GenericRange with integer endpoints, expand to list
            if let (Some(s), Some(e)) = (value_to_i64(start), value_to_i64(end)) {
                let s = if *excl_start { s + 1 } else { s };
                let e = if *excl_end { e } else { e + 1 };
                (s..e).map(Value::Int).collect()
            } else {
                // For non-integer ranges, return as-is (shouldn't happen in practice for cmp)
                vec![v.clone()]
            }
        }
        _ => vec![v.clone()],
    }
}

/// Try to extract an i64 from a Value.
fn value_to_i64(v: &Value) -> Option<i64> {
    match v {
        Value::Int(n) => Some(*n),
        Value::Num(n) if n.fract() == 0.0 && *n >= i64::MIN as f64 && *n <= i64::MAX as f64 => {
            Some(*n as i64)
        }
        Value::Rat(n, d) if *d != 0 && *n % *d == 0 => Some(*n / *d),
        _ => None,
    }
}

impl Interpreter {
    fn parse_numeric_string_for_spaceship(s: &str) -> Result<f64, RuntimeError> {
        s.trim().parse::<f64>().map_err(|_| {
            RuntimeError::new(format!(
                "X::Str::Numeric: Cannot convert string '{}' to a number",
                s
            ))
        })
    }

    pub(super) fn numeric_spaceship_ordering(
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
                // Handle Int/BigInt comparison with exact precision
                match (&l, &r) {
                    (Value::BigInt(a), Value::Int(b)) | (Value::Int(b), Value::BigInt(a)) => {
                        return Ok(Value::Bool(**a == num_bigint::BigInt::from(*b)));
                    }
                    (Value::BigInt(a), Value::BigInt(b)) => {
                        return Ok(Value::Bool(a == b));
                    }
                    _ => {}
                }
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

    /// Native-int-aware `!=` for cross-signed native integer comparisons.
    /// Replicates Rakudo/MoarVM behaviour: when comparing a native unsigned
    /// variable with a native signed variable and the signed operand holds a
    /// negative value, the result is `False` (the values are considered
    /// incomparable rather than unequal).
    pub(super) fn exec_num_ne_native_op(&mut self, flags: u8) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let left_unsigned = flags & 1 != 0;
        let right_unsigned = flags & 2 != 0;
        // Cross-signed check: if the signed operand is negative, return False.
        if left_unsigned != right_unsigned {
            let signed_val = if left_unsigned { &right } else { &left };
            let is_negative = match signed_val {
                Value::Int(n) => *n < 0,
                Value::BigInt(n) => n.sign() == num_bigint::Sign::Minus,
                _ => false,
            };
            if is_negative {
                self.stack.push(Value::Bool(false));
                return Ok(());
            }
        }
        // Fall through to standard != logic
        let eq_result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
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
        // Fast path: Int < Int
        if let Value::Int(a) = &left
            && let Value::Int(b) = &right
        {
            self.stack.push(Value::Bool(a < b));
            return Ok(());
        }
        // Fast path: Num < Num
        if let Value::Num(a) = &left
            && let Value::Num(b) = &right
        {
            self.stack.push(Value::Bool(a < b));
            return Ok(());
        }
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            check_type_object_in_numeric_context(&l)?;
            check_type_object_in_numeric_context(&r)?;
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
            check_type_object_in_numeric_context(&l)?;
            check_type_object_in_numeric_context(&r)?;
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
            check_type_object_in_numeric_context(&l)?;
            check_type_object_in_numeric_context(&r)?;
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
            check_type_object_in_numeric_context(&l)?;
            check_type_object_in_numeric_context(&r)?;
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
        let tolerance = loan_env!(self, get_dynamic_var("*TOLERANCE"))
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
}
