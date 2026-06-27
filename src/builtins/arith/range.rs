#![allow(clippy::result_large_err)]
//! Range-arithmetic helpers: offset, scale, divide, and mixin-range wrappers.

use crate::value::{RuntimeError, Value, make_rat};
use std::sync::Arc;

/// Extract `(start, end, excl_start, excl_end)` from any Range variant
/// (the int-endpoint variants or a `GenericRange`). Returns `None` for
/// non-Range values.
pub(crate) fn range_bounds(range_val: &Value) -> Option<(Value, Value, bool, bool)> {
    Some(match range_val {
        Value::Range(a, b) => (Value::Int(*a), Value::Int(*b), false, false),
        Value::RangeExcl(a, b) => (Value::Int(*a), Value::Int(*b), false, true),
        Value::RangeExclStart(a, b) => (Value::Int(*a), Value::Int(*b), true, false),
        Value::RangeExclBoth(a, b) => (Value::Int(*a), Value::Int(*b), true, true),
        Value::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => (
            start.as_ref().clone(),
            end.as_ref().clone(),
            *excl_start,
            *excl_end,
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
        delta,
        Value::Int(_) | Value::Num(_) | Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigInt(_)
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
    let n = match factor {
        Value::Int(i) => Some(*i as f64),
        Value::Num(f) => Some(*f),
        Value::Rat(n, d) if *d != 0 => Some(*n as f64 / *d as f64),
        _ => None,
    }?;
    let (start, end, excl_start, excl_end) = range_bounds(range_val)?;
    let scale_val = |v: Value| -> Value {
        match v {
            Value::Int(i) => {
                let result = i as f64 * n;
                if result == result.floor() && result.abs() < i64::MAX as f64 {
                    Value::Int(result as i64)
                } else {
                    Value::Num(result)
                }
            }
            Value::Num(f) => Value::Num(f * n),
            Value::Rat(rn, rd) if rd != 0 => Value::Num(rn as f64 / rd as f64 * n),
            other => Value::Num(other.to_f64() * n),
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
    if let (Value::Int(a), Value::Int(b)) = (&start, &end) {
        let (a, b) = (*a, *b);
        return match (excl_start, excl_end) {
            (false, false) => Value::Range(a, b),
            (false, true) => Value::RangeExcl(a, b),
            (true, false) => Value::RangeExclStart(a, b),
            (true, true) => Value::RangeExclBoth(a, b),
        };
    }
    Value::GenericRange {
        start: Arc::new(start),
        end: Arc::new(end),
        excl_start,
        excl_end,
    }
}

/// Divide a Range by a numeric factor. Returns Some(range) if the left is a Range.
pub(crate) fn range_divide(range_val: &Value, divisor: &Value) -> Option<Value> {
    // Get divisor as rational (num, den) for exact division
    let (div_n, div_d): (i64, i64) = match divisor {
        Value::Int(i) if *i != 0 => (*i, 1),
        Value::Rat(n, d) if *d != 0 && *n != 0 => (*n, *d),
        Value::Num(f) if *f != 0.0 => return range_divide_float(range_val, *f),
        _ => return None,
    };
    let (start, end, excl_start, excl_end) = match range_val {
        Value::Range(a, b) => (Value::Int(*a), Value::Int(*b), false, false),
        Value::RangeExcl(a, b) => (Value::Int(*a), Value::Int(*b), false, true),
        Value::RangeExclStart(a, b) => (Value::Int(*a), Value::Int(*b), true, false),
        Value::RangeExclBoth(a, b) => (Value::Int(*a), Value::Int(*b), true, true),
        Value::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => (
            start.as_ref().clone(),
            end.as_ref().clone(),
            *excl_start,
            *excl_end,
        ),
        _ => return None,
    };
    // Dividing by (div_n/div_d) = multiplying by (div_d/div_n)
    // Use make_rat for proper normalization, but preserve Rat type (don't collapse to Int)
    let div_val = |v: Value| -> Value {
        let num = match &v {
            Value::Int(i) => *i,
            Value::Rat(n, d) => {
                // (n/d) / (div_n/div_d) = (n*div_d) / (d*div_n)
                let result = make_rat(n * div_d, d * div_n);
                // Ensure result stays Rat even if whole number
                if let Value::Int(i) = result {
                    return Value::Rat(i, 1);
                }
                return result;
            }
            _ => return Value::Num(v.to_f64() * div_d as f64 / div_n as f64),
        };
        let result = make_rat(num * div_d, div_n);
        // Ensure result stays Rat even if whole number
        if let Value::Int(i) = result {
            return Value::Rat(i, 1);
        }
        result
    };
    let new_start = div_val(start);
    let new_end = div_val(end);
    Some(Value::GenericRange {
        start: Arc::new(new_start),
        end: Arc::new(new_end),
        excl_start,
        excl_end,
    })
}

/// Divide a Range by a float factor.
pub(crate) fn range_divide_float(range_val: &Value, n: f64) -> Option<Value> {
    let (start, end, excl_start, excl_end) = match range_val {
        Value::Range(a, b) => (Value::Int(*a), Value::Int(*b), false, false),
        Value::RangeExcl(a, b) => (Value::Int(*a), Value::Int(*b), false, true),
        Value::RangeExclStart(a, b) => (Value::Int(*a), Value::Int(*b), true, false),
        Value::RangeExclBoth(a, b) => (Value::Int(*a), Value::Int(*b), true, true),
        Value::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => (
            start.as_ref().clone(),
            end.as_ref().clone(),
            *excl_start,
            *excl_end,
        ),
        _ => return None,
    };
    let div_val = |v: Value| -> Value { Value::Num(v.to_f64() / n) };
    let new_start = div_val(start);
    let new_end = div_val(end);
    Some(Value::GenericRange {
        start: Arc::new(new_start),
        end: Arc::new(new_end),
        excl_start,
        excl_end,
    })
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
    match &left {
        Value::Mixin(inner, mixins) if inner.is_range() => {
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
    match &left {
        Value::Mixin(inner, mixins) if inner.is_range() => {
            let result = op((**inner).clone(), right);
            Some(Value::mixin(result, (**mixins).clone()))
        }
        _ => None,
    }
}
