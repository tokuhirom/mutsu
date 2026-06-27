//! String comparison and ordering ops (`cmp`/`leg`/`<=>`/`eqv`/before/after).
use super::vm_comparison_ops::{
    cmp_values, expand_range_to_list, get_list_elements, is_nan_value, is_neg_inf, is_pos_inf,
    is_range_value, is_rationalish, range_cmp, value_to_f64,
};
use super::*;

impl Interpreter {
    /// Coerce both operands of a string comparator (`eq`/`ne`/`lt`/`gt`/`le`/
    /// `ge`) through a user-defined `Str`/`Stringy` method when present, so an
    /// instance compares by its string value (Raku's `infix:<eq>` is `.Str`
    /// coercion). Junctions and plain values pass through unchanged, preserving
    /// autothreading. This is an internal redispatch with no surrounding
    /// CallMethod op, so drain any captured-outer writeback into the caller.
    fn coerce_str_compare_operands(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<(Value, Value), RuntimeError> {
        let caller_code = self.current_code;
        let left = self.coerce_stringy_operand(left)?;
        let right = self.coerce_stringy_operand(right)?;
        self.reconcile_caller_after_internal_dispatch(caller_code);
        Ok((left, right))
    }

    pub(super) fn exec_str_eq_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (left, right) = self.coerce_str_compare_operands(left, right)?;
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            if Self::is_buf_value(&l) && Self::is_buf_value(&r) {
                Ok(Value::Bool(
                    Self::buf_cmp_bytes(&l, &r) == std::cmp::Ordering::Equal,
                ))
            } else {
                Ok(Value::Bool(l.to_str_context() == r.to_str_context()))
            }
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_str_ne_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (left, right) = self.coerce_str_compare_operands(left, right)?;
        // ne is a negation meta-operator shortcut for !eq.
        // It first evaluates eq (which autothreads through junctions),
        // then negates the boolean-collapsed result, always returning Bool.
        let eq_result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            if Self::is_buf_value(&l) && Self::is_buf_value(&r) {
                Ok(Value::Bool(
                    Self::buf_cmp_bytes(&l, &r) == std::cmp::Ordering::Equal,
                ))
            } else {
                Ok(Value::Bool(l.to_str_context() == r.to_str_context()))
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
        let (left, right) = self.coerce_str_compare_operands(left, right)?;
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            if Self::is_buf_value(&l) && Self::is_buf_value(&r) {
                Ok(Value::Bool(
                    Self::buf_cmp_bytes(&l, &r) == std::cmp::Ordering::Less,
                ))
            } else {
                Ok(Value::Bool(l.to_str_context() < r.to_str_context()))
            }
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_str_gt_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (left, right) = self.coerce_str_compare_operands(left, right)?;
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            if Self::is_buf_value(&l) && Self::is_buf_value(&r) {
                Ok(Value::Bool(
                    Self::buf_cmp_bytes(&l, &r) == std::cmp::Ordering::Greater,
                ))
            } else {
                Ok(Value::Bool(l.to_str_context() > r.to_str_context()))
            }
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_str_le_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (left, right) = self.coerce_str_compare_operands(left, right)?;
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            if Self::is_buf_value(&l) && Self::is_buf_value(&r) {
                Ok(Value::Bool(
                    Self::buf_cmp_bytes(&l, &r) != std::cmp::Ordering::Greater,
                ))
            } else {
                Ok(Value::Bool(l.to_str_context() <= r.to_str_context()))
            }
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_str_ge_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let (left, right) = self.coerce_str_compare_operands(left, right)?;
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            if Self::is_buf_value(&l) && Self::is_buf_value(&r) {
                Ok(Value::Bool(
                    Self::buf_cmp_bytes(&l, &r) != std::cmp::Ordering::Less,
                ))
            } else {
                Ok(Value::Bool(l.to_str_context() >= r.to_str_context()))
            }
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn spaceship_ordering(left: &Value, right: &Value) -> std::cmp::Ordering {
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
                    crate::builtins::methods_0arg::temporal::datetime_attrs(&(left_attrs).as_map());
                let (ry, rm, rd, rh, rmin, rs, rtz) =
                    crate::builtins::methods_0arg::temporal::datetime_attrs(
                        &(right_attrs).as_map(),
                    );
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
    fn coerce_complex_to_real_if_tolerant(&mut self, val: &Value) -> Result<Value, RuntimeError> {
        if let Value::Complex(re, im) = val {
            if *im == 0.0 {
                return Ok(Value::Num(*re));
            }
            let tolerance = loan_env!(self, get_dynamic_var("$*TOLERANCE"))
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

    pub(super) fn exec_unicmp_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let left_s = left.to_string_value();
        let right_s = right.to_string_value();

        // Unlike `coll`, `unicmp` always uses the default Unicode collation and
        // is not influenced by the `$*COLLATION` dynamic variable.
        let settings = crate::builtins::collation::CollationSettings::default();
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

    /// The `.WHAT`-flavoured type tag of a *genuinely lazy* iterable (one that
    /// `eqv` cannot compare without iterating forever), or `None` when the value
    /// is not lazy. The tag follows `.WHAT`: a bare lazy `Seq` is `Seq`, a
    /// `.List`-coerced one is `List`, a `.Array`/`@`-coerced one is `Array`.
    fn lazy_eqv_type(v: &Value) -> Option<&'static str> {
        match v {
            Value::LazyList(ll) if ll.is_genuinely_lazy() => Some(if ll.in_array_context() {
                "Array"
            } else if ll.in_list_context() {
                "List"
            } else {
                "Seq"
            }),
            Value::Array(_, kind) if kind.is_lazy() => Some("Array"),
            _ => None,
        }
    }

    pub(super) fn exec_eqv_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // `eqv` on two lazy iterables of the SAME type cannot be answered without
        // iterating them, so it throws X::Cannot::Lazy (action `eqv`). Two lazy
        // iterables of DIFFERENT type are trivially not-eqv (no iteration needed),
        // and a single lazy operand falls through to the normal element compare,
        // which short-circuits on the length/laziness mismatch.
        match (Self::lazy_eqv_type(&left), Self::lazy_eqv_type(&right)) {
            (Some(a), Some(b)) if a == b => return Err(RuntimeError::cannot_lazy("eqv")),
            (Some(_), Some(_)) => {
                self.stack.push(Value::Bool(false));
                return Ok(());
            }
            _ => {}
        }
        let result =
            self.eval_binary_with_junctions(left, right, |_, l, r| Ok(Value::Bool(l.eqv(&r))))?;
        self.stack.push(result);
        Ok(())
    }
}
