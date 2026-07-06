//! Range-arithmetic helpers: offset, scale, divide, and mixin-range wrappers.

use crate::value::{RuntimeError, Value, ValueView, make_rat};

/// Extract `(start, end, excl_start, excl_end)` from any Range variant
/// (the int-endpoint variants or a `GenericRange`). Returns `None` for
/// non-Range values.
pub(crate) fn range_bounds(range_val: &Value) -> Option<(Value, Value, bool, bool)> {
    Some(match range_val.view() {
        ValueView::Range(a, b) => (Value::int(a), Value::int(b), false, false),
        ValueView::RangeExcl(a, b) => (Value::int(a), Value::int(b), false, true),
        ValueView::RangeExclStart(a, b) => (Value::int(a), Value::int(b), true, false),
        ValueView::RangeExclBoth(a, b) => (Value::int(a), Value::int(b), true, true),
        ValueView::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => (
            start.as_ref().clone(),
            end.as_ref().clone(),
            excl_start,
            excl_end,
        ),
        _ => return None,
    })
}

/// Offset both bounds of a Range by `delta` using `op` (add or subtract),
/// preserving exclusivity. Returns `Some(range)` when `range_val` is a Range and
/// `delta` is a real number (Int/Num/Rat/FatRat/BigInt). Collapses to a
/// canonical integer range when both endpoints remain `Int` (so `(2..4) + 1`
/// is `eqv` to the literal `3..5`); falls back to `GenericRange` otherwise.
pub(crate) fn range_offset(
    range_val: &Value,
    delta: &Value,
    op: impl Fn(Value, Value) -> Value,
) -> Option<Value> {
    if !matches!(
        delta.view(),
        ValueView::Int(_)
            | ValueView::Num(_)
            | ValueView::Rat(_, _)
            | ValueView::FatRat(_, _)
            | ValueView::BigInt(_)
    ) {
        return None;
    }
    let (start, end, excl_start, excl_end) = range_bounds(range_val)?;
    Some(canonical_int_range(
        op(start, delta.clone()),
        op(end, delta.clone()),
        excl_start,
        excl_end,
    ))
}

pub(crate) fn range_scale(range_val: &Value, factor: &Value) -> Option<Value> {
    let n = match factor.view() {
        ValueView::Int(i) => Some(i as f64),
        ValueView::Num(f) => Some(f),
        ValueView::Rat(n, d) if d != 0 => Some(n as f64 / d as f64),
        _ => None,
    }?;
    let (start, end, excl_start, excl_end) = range_bounds(range_val)?;
    let scale_val = |v: Value| -> Value {
        match v.view() {
            ValueView::Int(i) => {
                let result = i as f64 * n;
                if result == result.floor() && result.abs() < i64::MAX as f64 {
                    Value::int(result as i64)
                } else {
                    Value::num(result)
                }
            }
            ValueView::Num(f) => Value::num(f * n),
            ValueView::Rat(rn, rd) if rd != 0 => Value::num(rn as f64 / rd as f64 * n),
            _ => Value::num(v.to_f64() * n),
        }
    };
    let new_start = scale_val(start);
    let new_end = scale_val(end);
    Some(canonical_int_range(
        new_start, new_end, excl_start, excl_end,
    ))
}

/// Build a Range value, collapsing to the canonical integer-range variant
/// (`Range`/`RangeExcl`/`RangeExclStart`/`RangeExclBoth`) when both endpoints
/// are `Int`, so the result is `eqv` to a literal range (e.g. `(2..^5) * 5`
/// must equal `10..^25`). Falls back to `GenericRange` for non-Int endpoints.
pub(crate) fn canonical_int_range(
    start: Value,
    end: Value,
    excl_start: bool,
    excl_end: bool,
) -> Value {
    if let (Some(a), Some(b)) = (start.as_int(), end.as_int()) {
        return match (excl_start, excl_end) {
            (false, false) => Value::range(a, b),
            (false, true) => Value::range_excl(a, b),
            (true, false) => Value::range_excl_start(a, b),
            (true, true) => Value::range_excl_both(a, b),
        };
    }
    Value::generic_range(start, end, excl_start, excl_end)
}

/// Divide a Range by a numeric factor. Returns Some(range) if the left is a Range.
pub(crate) fn range_divide(range_val: &Value, divisor: &Value) -> Option<Value> {
    // Get divisor as rational (num, den) for exact division
    let (div_n, div_d): (i64, i64) = match divisor.view() {
        ValueView::Int(i) if i != 0 => (i, 1),
        ValueView::Rat(n, d) if d != 0 && n != 0 => (n, d),
        ValueView::Num(f) if f != 0.0 => return range_divide_float(range_val, f),
        _ => return None,
    };
    let (start, end, excl_start, excl_end) = match range_val.view() {
        ValueView::Range(a, b) => (Value::int(a), Value::int(b), false, false),
        ValueView::RangeExcl(a, b) => (Value::int(a), Value::int(b), false, true),
        ValueView::RangeExclStart(a, b) => (Value::int(a), Value::int(b), true, false),
        ValueView::RangeExclBoth(a, b) => (Value::int(a), Value::int(b), true, true),
        ValueView::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => (
            start.as_ref().clone(),
            end.as_ref().clone(),
            excl_start,
            excl_end,
        ),
        _ => return None,
    };
    // Dividing by (div_n/div_d) = multiplying by (div_d/div_n)
    // Use make_rat for proper normalization, but preserve Rat type (don't collapse to Int)
    let div_val = |v: Value| -> Value {
        let num = match v.view() {
            ValueView::Int(i) => i,
            ValueView::Rat(n, d) => {
                // (n/d) / (div_n/div_d) = (n*div_d) / (d*div_n)
                let result = make_rat(n * div_d, d * div_n);
                // Ensure result stays Rat even if whole number
                if let Some(i) = result.as_int() {
                    return Value::rat_raw(i, 1);
                }
                return result;
            }
            _ => return Value::num(v.to_f64() * div_d as f64 / div_n as f64),
        };
        let result = make_rat(num * div_d, div_n);
        // Ensure result stays Rat even if whole number
        if let Some(i) = result.as_int() {
            return Value::rat_raw(i, 1);
        }
        result
    };
    let new_start = div_val(start);
    let new_end = div_val(end);
    Some(Value::generic_range(
        new_start, new_end, excl_start, excl_end,
    ))
}

/// Divide a Range by a float factor.
pub(crate) fn range_divide_float(range_val: &Value, n: f64) -> Option<Value> {
    let (start, end, excl_start, excl_end) = match range_val.view() {
        ValueView::Range(a, b) => (Value::int(a), Value::int(b), false, false),
        ValueView::RangeExcl(a, b) => (Value::int(a), Value::int(b), false, true),
        ValueView::RangeExclStart(a, b) => (Value::int(a), Value::int(b), true, false),
        ValueView::RangeExclBoth(a, b) => (Value::int(a), Value::int(b), true, true),
        ValueView::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => (
            start.as_ref().clone(),
            end.as_ref().clone(),
            excl_start,
            excl_end,
        ),
        _ => return None,
    };
    let div_val = |v: Value| -> Value { Value::num(v.to_f64() / n) };
    let new_start = div_val(start);
    let new_end = div_val(end);
    Some(Value::generic_range(
        new_start, new_end, excl_start, excl_end,
    ))
}

// ── Mixin-Range arithmetic helper ────────────────────────────────────
/// If the left operand is a Mixin wrapping a Range (e.g. `(2..^5) but Meows`),
/// perform the arithmetic on the inner Range and re-wrap the result with the
/// same Mixin. Returns `None` if neither operand is a Mixin-wrapped Range.
pub(crate) fn mixin_range_arith<F>(
    left: Value,
    right: Value,
    op: F,
) -> Option<Result<Value, RuntimeError>>
where
    F: Fn(Value, Value) -> Result<Value, RuntimeError>,
{
    match left.view() {
        ValueView::Mixin(inner, mixins) if inner.is_range() => {
            let inner_val = (**inner).clone();
            let mix_clone = (**mixins).clone();
            Some(op(inner_val, right).map(|r| Value::mixin(r, mix_clone)))
        }
        _ => None,
    }
}

/// Same as `mixin_range_arith` but for non-Result arithmetic functions.
pub(crate) fn mixin_range_arith_val<F>(left: Value, right: Value, op: F) -> Option<Value>
where
    F: Fn(Value, Value) -> Value,
{
    match left.view() {
        ValueView::Mixin(inner, mixins) if inner.is_range() => {
            let result = op((**inner).clone(), right);
            Some(Value::mixin(result, (**mixins).clone()))
        }
        _ => None,
    }
}
