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

fn is_rationalish(v: &Value) -> bool {
    matches!(
        v,
        Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _)
    )
}

/// Check if a value is NaN (for numeric comparison semantics).
/// In Raku, Rat(0,0) and FatRat(0,0) behave like NaN.
fn is_nan_value(v: &Value) -> bool {
    match v {
        Value::Num(n) => n.is_nan(),
        Value::Rat(0, 0) | Value::FatRat(0, 0) => true,
        Value::BigRat(n, d) => n.is_zero() && d.is_zero(),
        _ => false,
    }
}

/// Check if a value is Inf (positive infinity).
/// Includes Rat(n,0)/FatRat(n,0) where n > 0.
fn is_pos_inf(v: &Value) -> bool {
    match v {
        Value::Num(n) => n.is_infinite() && n.is_sign_positive(),
        Value::Rat(n, 0) | Value::FatRat(n, 0) => *n > 0,
        Value::BigRat(n, d) => !n.is_zero() && d.is_zero() && n.is_positive(),
        _ => false,
    }
}

/// Check if a value is -Inf (negative infinity).
/// Includes Rat(n,0)/FatRat(n,0) where n < 0.
fn is_neg_inf(v: &Value) -> bool {
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
fn range_cmp(left: &Value, right: &Value) -> std::cmp::Ordering {
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
fn cmp_values(left: &Value, right: &Value) -> std::cmp::Ordering {
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

    VM::spaceship_ordering(left, right)
}

/// Get list elements from a value (Array, Seq, List, Slip, etc.)
fn get_list_elements(v: &Value) -> Option<&[Value]> {
    match v {
        Value::Array(elems, _) => Some(elems.as_slice()),
        Value::Seq(elems) => Some(elems.as_slice()),
        Value::Slip(elems) => Some(elems.as_slice()),
        _ => None,
    }
}

/// Check if a value is any range variant.
fn is_range_value(v: &Value) -> bool {
    extract_range_parts(v).is_some()
}

/// Expand a range value into a list of integer values (for list-based comparison).
fn expand_range_to_list(v: &Value) -> Vec<Value> {
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

    pub(super) fn exec_num_lt_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
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

    /// Container identity (`=:=`).
    ///
    /// `flags` encodes whether operands are provably fresh containers:
    ///   bit 0 = left operand is a fresh container (e.g. array index),
    ///   bit 1 = right operand is a fresh container.
    ///
    /// When at least one operand is a fresh container and both values
    /// are non-reference types (no `Arc` identity), the two stack
    /// values can never be the same container, so we return `False`.
    /// Reference types (Array, Hash, Sub, Instance, …) have `Arc`
    /// pointer identity which `values_identical` already checks.
    pub(super) fn exec_container_eq_op(&mut self, flags: u8) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let any_fresh = flags != 0;
        let result = if any_fresh && Self::is_value_non_reference(&left, &right) {
            // A fresh container (e.g. from `[$x][0]`) holding a non-reference
            // value can never be the same container as any other expression.
            false
        } else {
            crate::runtime::values_identical(&left, &right)
        };
        self.stack.push(Value::Bool(result));
    }

    /// Returns `true` when **both** values are simple, non-reference
    /// types where stack copies can never carry container identity
    /// (Int, Str, Bool, Nil, Rat, …).
    ///
    /// Note: `Package` is NOT included because type objects are singletons —
    /// two variables holding the same Package should be considered `=:=`.
    fn is_value_non_reference(left: &Value, right: &Value) -> bool {
        fn is_non_ref(v: &Value) -> bool {
            matches!(
                v,
                Value::Int(_)
                    | Value::BigInt(_)
                    | Value::Num(_)
                    | Value::Str(_)
                    | Value::Bool(_)
                    | Value::Nil
                    | Value::Rat(..)
                    | Value::FatRat(..)
                    | Value::BigRat(..)
                    | Value::Complex(..)
                    | Value::Whatever
                    | Value::HyperWhatever
                    | Value::Enum { .. }
                    | Value::Version { .. }
            )
        }
        is_non_ref(left) && is_non_ref(right)
    }

    /// Container identity (`=:=`) when both operands are named variables.
    /// Resolves alias chains to find the binding root of each name, then:
    /// - If both roots are the same name, they share a container → True.
    /// - For reference types (Array, Hash, Sub, Instance), use Arc pointer identity.
    /// - Otherwise → False (distinct scalar containers).
    pub(super) fn exec_container_eq_named_op(
        &mut self,
        code: &CompiledCode,
        left_name_idx: u32,
        right_name_idx: u32,
    ) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let left_name = Self::const_str(code, left_name_idx);
        let right_name = Self::const_str(code, right_name_idx);

        // Resolve alias chains to find the binding root of each variable.
        let left_root = self.resolve_alias_root(left_name);
        let right_root = self.resolve_alias_root(right_name);

        let result = if left_root == right_root {
            // Same binding root → same container.
            true
        } else if !Self::is_value_non_reference(&left, &right) {
            // Reference types: check Arc pointer identity.
            crate::runtime::values_identical(&left, &right)
        } else {
            // Distinct scalar containers → never identical.
            false
        };
        self.stack.push(Value::Bool(result));
    }

    /// Walk the `__mutsu_sigilless_alias::` chain to find the ultimate
    /// binding root for a variable name.
    fn resolve_alias_root(&self, name: &str) -> String {
        let mut current = name.to_string();
        let mut seen = std::collections::HashSet::new();
        while seen.insert(current.clone()) {
            let key = format!("__mutsu_sigilless_alias::{}", current);
            if let Some(Value::Str(next)) = self.interpreter.env().get(&key) {
                current = next.to_string();
            } else {
                break;
            }
        }
        current
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
            // Enum values: compare by their integer value
            (Value::Enum { value: av, .. }, Value::Enum { value: bv, .. }) => {
                av.as_i64().cmp(&bv.as_i64())
            }
            // Range cmp Range (both must be ranges; Range vs List handled below)
            (l, r) if is_range_value(l) && is_range_value(r) => range_cmp(l, r),
            // Real cmp Range (where Real is not a list/range)
            (_, r)
                if is_range_value(r)
                    && !is_range_value(left)
                    && get_list_elements(left).is_none() =>
            {
                let as_range = Value::GenericRange {
                    start: Arc::new(left.clone()),
                    end: Arc::new(left.clone()),
                    excl_start: false,
                    excl_end: false,
                };
                range_cmp(&as_range, r)
            }
            // Range cmp Real (where Real is not a list/range)
            (l, _)
                if is_range_value(l)
                    && !is_range_value(right)
                    && get_list_elements(right).is_none() =>
            {
                let as_range = Value::GenericRange {
                    start: Arc::new(right.clone()),
                    end: Arc::new(right.clone()),
                    excl_start: false,
                    excl_end: false,
                };
                range_cmp(l, &as_range)
            }
            // List/Array cmp (including Range vs List: expand range to list)
            (l, r)
                if get_list_elements(l).is_some()
                    || get_list_elements(r).is_some()
                    || is_range_value(l)
                    || is_range_value(r) =>
            {
                let l_vec;
                let l_elems: &[Value] = if let Some(elems) = get_list_elements(l) {
                    elems
                } else if is_range_value(l) {
                    l_vec = expand_range_to_list(l);
                    &l_vec
                } else {
                    l_vec = vec![l.clone()];
                    &l_vec
                };
                let r_vec;
                let r_elems: &[Value] = if let Some(elems) = get_list_elements(r) {
                    elems
                } else if is_range_value(r) {
                    r_vec = expand_range_to_list(r);
                    &r_vec
                } else {
                    r_vec = vec![r.clone()];
                    &r_vec
                };
                for (le, re) in l_elems.iter().zip(r_elems.iter()) {
                    let ord = cmp_values(le, re);
                    if ord != std::cmp::Ordering::Equal {
                        return ord;
                    }
                }
                l_elems.len().cmp(&r_elems.len())
            }
            _ => left.to_string_value().cmp(&right.to_string_value()),
        }
    }

    pub(super) fn exec_spaceship_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            // NaN <=> anything produces Nil (unordered)
            if is_nan_value(&l) || is_nan_value(&r) {
                return Ok(Value::Nil);
            }
            // Complex <=> Real: check $*TOLERANCE for imaginary part
            let l = vm.coerce_complex_to_real_if_tolerant(&l)?;
            let r = vm.coerce_complex_to_real_if_tolerant(&r)?;
            let ord = Self::numeric_spaceship_ordering(&l, &r)?;
            Ok(runtime::make_order(ord))
        })?;
        self.stack.push(result);
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
        // For lists, ranges, and mixed-type comparisons (e.g. Pair cmp Inf),
        // use cmp_values which handles these cases correctly.
        if get_list_elements(&left).is_some()
            || get_list_elements(&right).is_some()
            || is_range_value(&left)
            || is_range_value(&right)
            || is_pos_inf(&left)
            || is_neg_inf(&left)
            || is_pos_inf(&right)
            || is_neg_inf(&right)
        {
            let ord = cmp_values(&left, &right);
            self.stack.push(runtime::make_order(ord));
            return;
        }
        let (left, right) = self
            .coerce_numeric_bridge_pair(left.clone(), right.clone())
            .unwrap_or((left, right));
        let ord = Self::spaceship_ordering(&left, &right);
        self.stack.push(runtime::make_order(ord));
    }

    pub(super) fn exec_coll_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let left_s = left.to_string_value();
        let right_s = right.to_string_value();

        // Get $*COLLATION settings
        let settings = self.get_collation_settings();
        let result = crate::builtins::collation::coll_compare(&left_s, &right_s, &settings);
        self.stack.push(result);
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

    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_smart_match_expr_op(
        &mut self,
        code: &CompiledCode,
        ip: &mut usize,
        rhs_end: u32,
        negate: bool,
        lhs_var: &Option<String>,
        rhs_is_match_regex: bool,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let left = self.stack.pop().unwrap();
        let rhs_start = *ip + 1;
        let rhs_end = rhs_end as usize;
        let saved_topic = self.interpreter.env().get("_").cloned();
        self.interpreter
            .env_mut()
            .insert("_".to_string(), left.clone());
        // Sync env->locals first so that any values modified by interpreter
        // calls (e.g. EVAL modifying $GLOBAL:: variables) are picked up
        // before we overwrite env with local values for regex interpolation.
        if self.env_dirty {
            self.sync_locals_from_env(code);
            self.env_dirty = false;
        }
        self.sync_regex_interpolation_env_from_locals(code);
        let saved_in_smartmatch_rhs = self.in_smartmatch_rhs;
        self.in_smartmatch_rhs = true;
        self.transliterate_in_smartmatch = false;
        self.substitution_in_smartmatch = false;
        let rhs_run = self.run_range(code, rhs_start, rhs_end, compiled_fns);
        self.in_smartmatch_rhs = saved_in_smartmatch_rhs;
        let was_transliterate = self.transliterate_in_smartmatch;
        let was_substitution = self.substitution_in_smartmatch;
        self.transliterate_in_smartmatch = false;
        self.substitution_in_smartmatch = false;
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
        } else if was_substitution {
            // s/// returns Match on success, False on failure.
            // For !~~, negate the boolean result.
            if negate {
                Value::Bool(!right.truthy())
            } else {
                right
            }
        } else if negate {
            // Smartmatch must NOT force lazy values (lazy ~~ anything → False)
            self.eval_smartmatch_with_junctions_ex(left, right, true, rhs_is_match_regex)?
        } else {
            self.eval_smartmatch_with_junctions_ex(left, right, false, rhs_is_match_regex)?
        };
        self.stack.push(out);
        self.env_dirty = true;
        *ip = rhs_end;
        Ok(())
    }

    pub(super) fn exec_scalarize_regex_match_result_op(&mut self) -> Result<(), RuntimeError> {
        let value = self.stack.pop().unwrap_or(Value::Nil);
        let scalarized = match value {
            Value::Nil => Value::Int(0),
            Value::Array(items, _) | Value::Seq(items) | Value::Slip(items) => {
                Value::Int(items.len() as i64)
            }
            Value::Capture { positional, .. } => Value::Int(positional.len() as i64),
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Match" => {
                if let Some(Value::Array(items, _)) = attributes.get("list") {
                    Value::Int(items.len() as i64)
                } else {
                    Value::Int(1)
                }
            }
            other if other.truthy() => Value::Int(1),
            _ => Value::Int(0),
        };
        self.stack.push(scalarized);
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
