//! Integer-bound analysis shared by `Range.int-bounds` and `Range.minmax`.
//!
//! Rakudo derives both from the same two facts about a Range's endpoints:
//! whether the range is `is-int` (both endpoints are genuine `Int`s, in which
//! case an excluded end is simply a +1/-1 adjustment), and — when it is not —
//! whether the endpoints still admit integer bounds at all.
//!
//! The integer-bound rule, verified against `raku` v2026.06:
//!
//! * both endpoints must be finite Reals (`1..Inf`, `-Inf..5`, `1..*`,
//!   `NaN..NaN` and `'a'..'z'` have none);
//! * the **lower** endpoint must already be integral — `(1.1..5.2).int-bounds`
//!   is `False`, it is NOT rounded outward;
//! * the lower bound is `min.floor + excludes-min`;
//! * the upper bound is `max.floor`, minus one more when the max is excluded
//!   *and* integral (`1..^5.0` is `(1, 4)` but `1..^5.5` is `(1, 5)`).

use crate::builtins::arith::range::range_bounds;
use crate::value::{Value, ValueView};
use num_traits::Zero;

/// The i64 Range variants use `i64::MIN`/`i64::MAX` as a sentinel for an open
/// (`-Inf`/`Inf`/`*`) end. The sentinel only means "open" when it appears
/// alone: `int64.Range` is the genuine full-i64 range and has concrete bounds.
fn open_i64_end(range_val: &Value) -> bool {
    match range_val.view() {
        ValueView::Range(s, e)
        | ValueView::RangeExcl(s, e)
        | ValueView::RangeExclStart(s, e)
        | ValueView::RangeExclBoth(s, e) => (s == i64::MIN) ^ (e == i64::MAX),
        _ => false,
    }
}

/// `Range.is-int` in Rakudo's sense: both endpoints are genuine integers.
/// A `Whatever`/infinite end is not one, so `1..*` is not `is-int` here even
/// though the i64 variants store it as `i64::MAX`.
fn endpoints_are_int(range_val: &Value, start: &Value, end: &Value) -> bool {
    if open_i64_end(range_val) {
        return false;
    }
    let is_int = |v: &Value| matches!(v.view(), ValueView::Int(_) | ValueView::BigInt(_));
    is_int(start) && is_int(end)
}

/// `floor` of a finite Real endpoint, as an `Int`/`BigInt`. `None` for
/// anything without a finite real value (`Inf`, `NaN`, `Whatever`, a `Str`
/// endpoint, a zero-denominator Rat, ...).
fn endpoint_floor(v: &Value) -> Option<Value> {
    match v.view() {
        ValueView::Int(_) | ValueView::BigInt(_) => Some(v.clone()),
        ValueView::Bool(b) => Some(Value::int(i64::from(b))),
        ValueView::Num(f) => {
            if f.is_finite() {
                Some(Value::int(f.floor() as i64))
            } else {
                None
            }
        }
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) if d != 0 => {
            let q = n / d;
            let r = n % d;
            if r != 0 && (n < 0) != (d < 0) {
                Some(Value::int(q - 1))
            } else {
                Some(Value::int(q))
            }
        }
        ValueView::BigRat(n, d) if !d.is_zero() => {
            use num_integer::Integer;
            use num_traits::Signed;
            let (q, r) = n.div_rem(d);
            if !r.is_zero() && n.is_negative() != d.is_negative() {
                Some(Value::bigint(q - 1))
            } else {
                Some(Value::bigint(q))
            }
        }
        _ => None,
    }
}

/// Is this endpoint exactly an integer (so an excluded end shifts the bound)?
fn endpoint_is_integral(v: &Value) -> bool {
    match v.view() {
        ValueView::Int(_) | ValueView::BigInt(_) | ValueView::Bool(_) => true,
        ValueView::Num(f) => f.is_finite() && f.floor() == f,
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) if d != 0 => n % d == 0,
        ValueView::BigRat(n, d) if !d.is_zero() => (n % d).is_zero(),
        _ => false,
    }
}

fn add_int(v: &Value, delta: i64) -> Value {
    match v.view() {
        ValueView::Int(i) => match i.checked_add(delta) {
            Some(n) => Value::int(n),
            None => Value::bigint(num_bigint::BigInt::from(i) + delta),
        },
        ValueView::BigInt(n) => Value::bigint(n.as_ref() + delta),
        _ => v.clone(),
    }
}

/// `(from, to)` integer bounds of a Range, or `None` when the range has none.
/// Returns `None` for a non-Range value too — callers gate on the type first.
pub(crate) fn range_int_bounds(range_val: &Value) -> Option<(Value, Value)> {
    let (start, end, excl_start, excl_end) = range_bounds(range_val)?;
    if open_i64_end(range_val) {
        return None;
    }
    // The lower endpoint is never rounded outward: a fractional min simply has
    // no integer bounds (raku: `(1.1..5.2).int-bounds` is False).
    if !endpoint_is_integral(&start) {
        return None;
    }
    let from = endpoint_floor(&start)?;
    let to = endpoint_floor(&end)?;
    let from = if excl_start { add_int(&from, 1) } else { from };
    let to = if excl_end && endpoint_is_integral(&end) {
        add_int(&to, -1)
    } else {
        to
    };
    Some((from, to))
}

/// `Range.minmax`: the `(min, max)` pair with excluded ends folded in.
///
/// `Err(())` marks the raku error case — a non-`is-int` Range with an excluded
/// end, where the excluded bound cannot be named as a concrete value. Callers
/// turn that into `X::AdHoc: Cannot return minmax on Range with excluded ends`.
pub(crate) fn range_minmax(range_val: &Value) -> Option<Result<(Value, Value), ()>> {
    let (start, end, excl_start, excl_end) = range_bounds(range_val)?;
    if endpoints_are_int(range_val, &start, &end) {
        let min = if excl_start {
            add_int(&start, 1)
        } else {
            start
        };
        let max = if excl_end { add_int(&end, -1) } else { end };
        return Some(Ok((min, max)));
    }
    if excl_start || excl_end {
        return Some(Err(()));
    }
    Some(Ok((
        open_end_value(range_val, &start, true),
        open_end_value(range_val, &end, false),
    )))
}

/// Render an endpoint for `minmax`, mapping the i64 open-end sentinel and a
/// `Whatever` end to the infinity they stand for (`(1..Inf).minmax` is
/// `(1, Inf)`, not the raw `i64::MAX`), exactly as `Range.bounds` does.
fn open_end_value(range_val: &Value, endpoint: &Value, is_start: bool) -> Value {
    if matches!(
        endpoint.view(),
        ValueView::Whatever | ValueView::HyperWhatever
    ) {
        return Value::num(if is_start {
            f64::NEG_INFINITY
        } else {
            f64::INFINITY
        });
    }
    if open_i64_end(range_val) {
        match endpoint.view() {
            ValueView::Int(i64::MIN) if is_start => return Value::num(f64::NEG_INFINITY),
            ValueView::Int(i64::MAX) if !is_start => return Value::num(f64::INFINITY),
            _ => {}
        }
    }
    endpoint.clone()
}
