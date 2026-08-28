use super::*;

/// Check if a value is a concrete numeric type object (undefined) used as an
/// operand of an infix numeric operator. In Raku, the numeric infix multis
/// (`+ - * / % **`, `== != < > <= >= <=>`) have no candidate accepting an
/// undefined operand of a concrete numeric type, so they throw a fatal
/// `X::Numeric::Uninitialized` (not suppressed by `quietly`). Non-concrete-numeric
/// type objects (Any, Str, Cool, Numeric, Complex, ...) instead fall through to a
/// generic candidate that warns and coerces to 0, so they must NOT be caught here.
pub(crate) fn check_type_object_in_numeric_context(v: &Value) -> Result<(), RuntimeError> {
    if let ValueView::Package(name) = v.view() {
        let type_name = name.resolve();
        // Rakudo hard-errors for these concrete numeric type objects. `Complex`
        // is deliberately excluded — its infix candidates warn+coerce like the
        // generic path (verified against rakudo).
        let is_numeric_type = matches!(
            type_name.as_ref(),
            "Int" | "Num" | "Rat" | "FatRat" | "Real" | "Bool"
        );
        if is_numeric_type {
            return Err(RuntimeError::numeric_uninitialized(&type_name));
        }
    }
    Ok(())
}

/// Extract (real, imaginary) parts from a value, treating non-Complex as having im=0.
fn complex_parts(v: &Value) -> (f64, f64) {
    match v.view() {
        ValueView::Complex(re, im) => (re, im),
        ValueView::Mixin(inner, _) => complex_parts(inner),
        _ => (value_to_f64(v), 0.0),
    }
}

pub(super) fn value_to_f64(v: &Value) -> f64 {
    runtime::to_float_value(v).unwrap_or(0.0)
}

pub(super) fn is_rationalish(v: &Value) -> bool {
    matches!(
        v.view(),
        ValueView::Rat(_, _) | ValueView::FatRat(_, _) | ValueView::BigRat(_, _)
    )
}

/// Check if a value is NaN (for numeric comparison semantics).
/// In Raku, Rat(0,0) and FatRat(0,0) behave like NaN.
pub(super) fn is_nan_value(v: &Value) -> bool {
    match v.view() {
        ValueView::Num(n) => n.is_nan(),
        ValueView::Rat(0, 0) | ValueView::FatRat(0, 0) => true,
        ValueView::BigRat(n, d) => n.is_zero() && d.is_zero(),
        _ => false,
    }
}

/// Check if a value is Inf (positive infinity).
/// Includes Rat(n,0)/FatRat(n,0) where n > 0.
pub(super) fn is_pos_inf(v: &Value) -> bool {
    match v.view() {
        ValueView::Num(n) => n.is_infinite() && n.is_sign_positive(),
        ValueView::Rat(n, 0) | ValueView::FatRat(n, 0) => n > 0,
        ValueView::BigRat(n, d) => !n.is_zero() && d.is_zero() && n.is_positive(),
        _ => false,
    }
}

/// Check if a value is -Inf (negative infinity).
/// Includes Rat(n,0)/FatRat(n,0) where n < 0.
pub(super) fn is_neg_inf(v: &Value) -> bool {
    match v.view() {
        ValueView::Num(n) => n.is_infinite() && n.is_sign_negative(),
        ValueView::Rat(n, 0) | ValueView::FatRat(n, 0) => n < 0,
        ValueView::BigRat(n, d) => !n.is_zero() && d.is_zero() && n.is_negative(),
        _ => false,
    }
}

/// Extract range components (start, end, excl_start, excl_end) from a range value.
fn extract_range_parts(v: &Value) -> Option<(Value, Value, bool, bool)> {
    match v.view() {
        ValueView::Range(a, b) => Some((Value::int(a), Value::int(b), false, false)),
        ValueView::RangeExcl(a, b) => Some((Value::int(a), Value::int(b), false, true)),
        ValueView::RangeExclStart(a, b) => Some((Value::int(a), Value::int(b), true, false)),
        ValueView::RangeExclBoth(a, b) => Some((Value::int(a), Value::int(b), true, true)),
        ValueView::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => Some((
            start.as_ref().clone(),
            end.as_ref().clone(),
            excl_start,
            excl_end,
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

/// Unwrap an allomorph / runtime mixin (`<5.0>` RatStr, `42 but Role`) to its
/// underlying numeric base for `==`/`!=`. Without this, comparing two mixins of
/// the same variant falls to structural `l == r`, which also compares the string
/// halves — so `<5.0> == <5>` was `False` instead of `5.0 == 5` (`True`). The
/// other comparison ops already unwrap via `cmp_values`.
fn deref_allomorph_numeric(v: Value) -> Value {
    match v.view() {
        ValueView::Mixin(inner, _) => deref_allomorph_numeric(inner.as_ref().clone()),
        _ => v,
    }
}

/// Recursively apply cmp semantics to two values (for use in list element comparison).
pub(super) fn cmp_values(left: &Value, right: &Value) -> std::cmp::Ordering {
    // Handle NaN
    if is_nan_value(left) || is_nan_value(right) {
        return left.to_string_value().cmp(&right.to_string_value());
    }

    // Unwrap Mixin
    let left = match left.view() {
        ValueView::Mixin(inner, _) => inner.as_ref(),
        _ => left,
    };
    let right = match right.view() {
        ValueView::Mixin(inner, _) => inner.as_ref(),
        _ => right,
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
    // Delegates to the value-wall accessor: the slice borrows from `v`
    // directly, which a `view()` guard cannot provide.
    v.as_list_items_with_hyper()
}

/// Check if a value is any range variant.
pub(super) fn is_range_value(v: &Value) -> bool {
    extract_range_parts(v).is_some()
}

/// Expand a range value into a list of integer values (for list-based comparison).
pub(super) fn expand_range_to_list(v: &Value) -> Vec<Value> {
    match v.view() {
        ValueView::Range(a, b) => (a..=b).map(Value::int).collect(),
        ValueView::RangeExcl(a, b) => (a..b).map(Value::int).collect(),
        ValueView::RangeExclStart(a, b) => ((a + 1)..=b).map(Value::int).collect(),
        ValueView::RangeExclBoth(a, b) => ((a + 1)..b).map(Value::int).collect(),
        ValueView::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
            // For GenericRange with integer endpoints, expand to list
            if let (Some(s), Some(e)) = (value_to_i64(start), value_to_i64(end)) {
                let s = if excl_start { s + 1 } else { s };
                let e = if excl_end { e } else { e + 1 };
                (s..e).map(Value::int).collect()
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
    match v.view() {
        ValueView::Int(n) => Some(n),
        ValueView::Num(n) if n.fract() == 0.0 && n >= i64::MIN as f64 && n <= i64::MAX as f64 => {
            Some(n as i64)
        }
        ValueView::Rat(n, d) if d != 0 && n % d == 0 => Some(n / d),
        _ => None,
    }
}

impl Interpreter {
    fn parse_numeric_string_for_spaceship(s: &str) -> Result<f64, RuntimeError> {
        let t = s.trim();
        // Raku numifies an empty (or whitespace-only) string to 0, so `"" <=> 0`
        // is `Same` rather than an X::Str::Numeric.
        if t.is_empty() {
            return Ok(0.0);
        }
        t.parse::<f64>().map_err(|_| {
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
        match (left.view(), right.view()) {
            (ValueView::Str(a), ValueView::Str(b)) => {
                let a = Self::parse_numeric_string_for_spaceship(&a)?;
                let b = Self::parse_numeric_string_for_spaceship(&b)?;
                Ok(a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal))
            }
            (ValueView::Str(a), _) => {
                let a = Self::parse_numeric_string_for_spaceship(&a)?;
                let b = runtime::to_float_value(right).unwrap_or(0.0);
                Ok(a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal))
            }
            (_, ValueView::Str(b)) => {
                let a = runtime::to_float_value(left).unwrap_or(0.0);
                let b = Self::parse_numeric_string_for_spaceship(&b)?;
                Ok(a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal))
            }
            _ => Ok(Self::spaceship_ordering(left, right)),
        }
    }

    /// The full `infix:<==>` semantics, shared so that calling the operator as
    /// a routine — `&infix:<==>($a, $b)`, which is exactly what the real
    /// `Test.rakumod`'s `cmp-ok` does via `&CALLER::LEXICAL::("infix:<$op>")` —
    /// behaves identically to the `$a == $b` operator form. The routine path
    /// used to land on the pure `apply_reduction_op` fold (via a separate,
    /// incomplete numeric-coercion bridge) instead, so e.g. `&infix:<==>(Inf.Rat,
    /// Inf)` answered `False` where the operator answers `True`.
    pub(crate) fn num_eq_values(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        self.eval_binary_with_junctions(left, right, |vm, l, r| {
            check_type_object_in_numeric_context(&l)?;
            check_type_object_in_numeric_context(&r)?;
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            // rakudo's last-resort candidate is `multi infix:<==>(Any \a, Any
            // \b) { a.Numeric == b.Numeric }`, so two objects the bridge left
            // alone still compare by their `.Numeric`. The bridge only numifies
            // an object that does `Real`/`Numeric` or has a USER `Numeric`
            // method, which misses a native type that is neither -- `DateTime`
            // is neither in rakudo either, yet `$a == $b` compares fine there.
            // Structural equality answered False for two DateTimes naming the
            // same instant in different timezones (roast
            // S32-temporal/DateTime.t's leap-second subtest).
            //
            // Deliberately scoped to `==`/`!=` rather than the shared numeric
            // bridge: `-` and `<=>` have their own temporal candidates
            // (`DateTime - DateTime` is a `Duration`), and numifying there
            // destroys them.
            let (l, r) = match (l.view(), r.view()) {
                (ValueView::Instance { .. }, ValueView::Instance { .. }) => {
                    match (
                        vm.call_method_with_values(l.clone(), "Numeric", vec![]),
                        vm.call_method_with_values(r.clone(), "Numeric", vec![]),
                    ) {
                        // Only when BOTH numify to something that is no longer
                        // an object; otherwise keep the originals so an object
                        // with no `.Numeric` still reaches the structural path.
                        (Ok(ln), Ok(rn))
                            if !matches!(ln.view(), ValueView::Instance { .. })
                                && !matches!(rn.view(), ValueView::Instance { .. }) =>
                        {
                            (ln, rn)
                        }
                        _ => (l, r),
                    }
                }
                _ => (l, r),
            };
            let (l, r) = (deref_allomorph_numeric(l), deref_allomorph_numeric(r));
            // NaN is unordered: NaN == anything is always False
            if is_nan_value(&l) || is_nan_value(&r) {
                return Ok(Value::FALSE);
            }
            if let (Some(a), Some(b)) =
                (runtime::to_big_rat_parts(&l), runtime::to_big_rat_parts(&r))
                && (is_rationalish(&l) || is_rationalish(&r))
            {
                Ok(Value::truth(runtime::big_rat_parts_equal(a, b)))
            } else {
                // Handle Int/BigInt comparison with exact precision
                match (l.view(), r.view()) {
                    (ValueView::BigInt(a), ValueView::Int(b))
                    | (ValueView::Int(b), ValueView::BigInt(a)) => {
                        return Ok(Value::truth(**a == num_bigint::BigInt::from(b)));
                    }
                    (ValueView::BigInt(a), ValueView::BigInt(b)) => {
                        return Ok(Value::truth(*a == *b));
                    }
                    _ => {}
                }
                // Two numeric-looking Strs (`"1" == "1 "`, `"1.0" == "1"`) must
                // compare their NUMERIC value, not their literal bytes — Raku's
                // numeric-string coercion trims surrounding whitespace, so a
                // trailing/leading space made two otherwise-equal numbers
                // compare unequal under the `same_variant` raw-`l == r` shortcut
                // below. Two non-numeric Strs of the same shape (mutsu's
                // bare-string enum modeling, e.g. `$status == Broken`) still
                // fall through to that shortcut unchanged — `==` is
                // deliberately lenient about non-numeric strings (see
                // `infix_is_strictly_numeric`'s doc comment), so this only
                // widens the case where BOTH sides actually parse as numbers.
                let both_numeric_strs = matches!(l.view(), ValueView::Str(_))
                    && matches!(r.view(), ValueView::Str(_))
                    && runtime::to_float_value(&l).is_some()
                    && runtime::to_float_value(&r).is_some();
                let needs_float = !l.same_variant(&r) || l.is_nil() || both_numeric_strs;
                if needs_float {
                    Ok(Value::truth(
                        runtime::to_float_value(&l) == runtime::to_float_value(&r),
                    ))
                } else {
                    Ok(Value::truth(l == r))
                }
            }
        })
    }

    pub(super) fn exec_num_eq_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.num_eq_values(left, right)?;
        self.stack.push(result);
        Ok(())
    }

    /// `!=` is a negation meta-operator shortcut for `!==`: it evaluates `==`
    /// (autothreading through junctions via [`Self::num_eq_values`]) and then
    /// negates the boolean-collapsed result, always returning `Bool`. Sharing
    /// [`Self::num_eq_values`] is the same "one oracle" reasoning as that
    /// method's own doc comment — the routine form `&infix:<!=>($a, $b)` must
    /// answer identically to `$a != $b`.
    pub(crate) fn num_ne_values(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        let eq_result = self.num_eq_values(left, right)?;
        Ok(Value::truth(!eq_result.truthy()))
    }

    pub(super) fn exec_num_ne_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.num_ne_values(left, right)?;
        self.stack.push(result);
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
            let is_negative = match signed_val.view() {
                ValueView::Int(n) => n < 0,
                ValueView::BigInt(n) => n.sign() == num_bigint::Sign::Minus,
                _ => false,
            };
            if is_negative {
                self.stack.push(Value::FALSE);
                return Ok(());
            }
        }
        // Fall through to standard != logic
        let eq_result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            check_type_object_in_numeric_context(&l)?;
            check_type_object_in_numeric_context(&r)?;
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            if is_nan_value(&l) || is_nan_value(&r) {
                return Ok(Value::FALSE);
            }
            if let (Some(a), Some(b)) =
                (runtime::to_big_rat_parts(&l), runtime::to_big_rat_parts(&r))
                && (is_rationalish(&l) || is_rationalish(&r))
            {
                Ok(Value::truth(runtime::big_rat_parts_equal(a, b)))
            } else if !l.same_variant(&r) || l.is_nil() {
                // Mirror `==` exactly: operands of different shapes (a Str and
                // an Int, say) compare as floats. Structural equality here made
                // `"" != 0` answer True while `"" == 0` answered False.
                Ok(Value::truth(
                    runtime::to_float_value(&l) == runtime::to_float_value(&r),
                ))
            } else {
                Ok(Value::truth(l == r))
            }
        })?;
        self.stack.push(Value::truth(!eq_result.truthy()));
        Ok(())
    }

    /// Shared body for `<`, used by both the stack-based `exec_num_lt_op`
    /// opcode and the routine form (`&infix:<<>($a, $b)`), so the two call
    /// shapes cannot drift — see the doc comment on [`Self::num_eq_values`]
    /// for why that matters.
    pub(crate) fn num_lt_values(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        // Fast path: Int/Int or Num/Num, skipping the generic BigRat-capable
        // `compare` path (its conversions + allocation are wasted on the
        // overwhelmingly common case of two native-shaped operands).
        if let ValueView::Int(a) = left.view()
            && let ValueView::Int(b) = right.view()
        {
            return Ok(Value::truth(a < b));
        }
        if let ValueView::Num(a) = left.view()
            && let ValueView::Num(b) = right.view()
        {
            return Ok(Value::truth(a < b));
        }
        self.eval_binary_with_junctions(left, right, |vm, l, r| {
            check_type_object_in_numeric_context(&l)?;
            check_type_object_in_numeric_context(&r)?;
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            Interpreter::compare(l, r, |o| o < 0)
        })
    }

    /// Shared body for `<=` — see [`Self::num_lt_values`].
    pub(crate) fn num_le_values(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        if let ValueView::Int(a) = left.view()
            && let ValueView::Int(b) = right.view()
        {
            return Ok(Value::truth(a <= b));
        }
        if let ValueView::Num(a) = left.view()
            && let ValueView::Num(b) = right.view()
        {
            return Ok(Value::truth(a <= b));
        }
        self.eval_binary_with_junctions(left, right, |vm, l, r| {
            check_type_object_in_numeric_context(&l)?;
            check_type_object_in_numeric_context(&r)?;
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            Interpreter::compare(l, r, |o| o <= 0)
        })
    }

    /// Shared body for `>` — see [`Self::num_lt_values`].
    pub(crate) fn num_gt_values(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        if let ValueView::Int(a) = left.view()
            && let ValueView::Int(b) = right.view()
        {
            return Ok(Value::truth(a > b));
        }
        if let ValueView::Num(a) = left.view()
            && let ValueView::Num(b) = right.view()
        {
            return Ok(Value::truth(a > b));
        }
        self.eval_binary_with_junctions(left, right, |vm, l, r| {
            check_type_object_in_numeric_context(&l)?;
            check_type_object_in_numeric_context(&r)?;
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            Interpreter::compare(l, r, |o| o > 0)
        })
    }

    /// Shared body for `>=` — see [`Self::num_lt_values`].
    pub(crate) fn num_ge_values(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        if let ValueView::Int(a) = left.view()
            && let ValueView::Int(b) = right.view()
        {
            return Ok(Value::truth(a >= b));
        }
        if let ValueView::Num(a) = left.view()
            && let ValueView::Num(b) = right.view()
        {
            return Ok(Value::truth(a >= b));
        }
        self.eval_binary_with_junctions(left, right, |vm, l, r| {
            check_type_object_in_numeric_context(&l)?;
            check_type_object_in_numeric_context(&r)?;
            let (l, r) = vm.coerce_numeric_bridge_pair(l, r)?;
            Interpreter::compare(l, r, |o| o >= 0)
        })
    }

    pub(super) fn exec_num_lt_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.num_lt_values(left, right)?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_num_le_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.num_le_values(left, right)?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_num_gt_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.num_gt_values(left, right)?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_num_ge_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.num_ge_values(left, right)?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_approx_eq_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.approx_eq_values(left, right)?;
        self.stack.push(result);
        Ok(())
    }

    /// `=~=` on two values (shared by the ApproxEq opcode and the hyper/meta
    /// paths, e.g. `»=~=«`). Honors `$*TOLERANCE`.
    pub(crate) fn approx_eq_values(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        let (left, right) = self.coerce_numeric_bridge_pair(left, right)?;
        let tolerance = loan_env!(self, get_dynamic_var("*TOLERANCE"))
            .ok()
            .and_then(|v| match v.view() {
                ValueView::Num(n) => Some(n),
                ValueView::Rat(n, d) => Some(n as f64 / d as f64),
                ValueView::Int(n) => Some(n as f64),
                _ => None,
            })
            .unwrap_or(1e-15);
        // Extract Complex components, treating Real as Complex with im=0
        let (lr, li) = complex_parts(&left);
        let (rr, ri) = complex_parts(&right);
        let approx_f64 = |a: f64, b: f64| -> bool {
            // Two identical infinities (Inf == Inf, -Inf == -Inf) are approximately
            // equal regardless of tolerance — the relative-difference formula below
            // would compute Inf/Inf = NaN. Finite equal values are NOT short-circuited:
            // with `$*TOLERANCE = 0` even `1 ≅ 1` must be False (per the spec, a zero
            // tolerance makes every comparison fail), so they fall through to the
            // strict `<` test below.
            if a == b && a.is_infinite() {
                return true;
            }
            // NaN is never approximately equal to anything
            if a.is_nan() || b.is_nan() {
                return false;
            }
            let diff = (a - b).abs();
            // Per Raku spec: the difference must be strictly LESS than the tolerance
            // (`$*TOLERANCE = 0` fails all comparisons). If either side is zero, use
            // the absolute difference; otherwise the relative percentage difference.
            if a == 0.0 || b == 0.0 {
                return diff < tolerance;
            }
            let max_abs = a.abs().max(b.abs());
            diff < tolerance * max_abs
        };
        // For two pure reals the imaginary parts are both exactly 0 and must not be
        // subjected to the tolerance test (`$*TOLERANCE = 0` would otherwise make
        // `0 < 0` false and wrongly reject every real comparison); only compare the
        // imaginary parts when at least one operand is genuinely Complex.
        let im_ok = (li == 0.0 && ri == 0.0) || approx_f64(li, ri);
        let result = approx_f64(lr, rr) && im_ok;
        Ok(Value::truth(result))
    }
}
