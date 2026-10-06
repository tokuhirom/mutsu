//! `Range`'s rows (ADR-11276 §10, slice 3A proof rows for the `Range` shape;
//! slice 3C moves the rest of its methods).

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::rng::builtin_rand;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($name:literal, $handler:expr) => {
        MethodRow {
            owner: "Range",
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
    ($name:literal, $arity:literal, $handler:expr, $flags:expr) => {
        MethodRow {
            owner: "Range",
            name: $name,
            arity: $arity,
            handler: Handler::Narrow($handler),
            flags: $flags,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("excludes-min", excludes_min),
    row!("excludes-max", excludes_max),
    row!("bounds", bounds),
    row!("is-int", is_int),
    row!("infinite", infinite),
    row!("int-bounds", int_bounds),
    row!("rand", rand),
    // `in-range($got)` and `in-range($got, $what)`: the argument is any value
    // the range can be compared with, so any plain argument is admitted.
    row!("in-range", 1, in_range_value, RowFlags::ANY_ARGS),
    row!("in-range", 2, in_range_what, RowFlags::ANY_ARGS),
    row!("elems", elems),
    row!("min", min),
    row!("max", max),
    row!("minmax", minmax),
    row!("Numeric", numeric),
    row!("list", list),
    // `sum` and `reverse` are the one implementation every list-like shares.
    row!("sum", super::list_aggregate::sum),
    row!("reverse", super::list::reverse),
    // `contains` and `index` stringify the range, as every `Cool` does.
    MethodRow {
        owner: "Range",
        name: "contains",
        arity: 1,
        handler: Handler::Pure(crate::builtins::method_table::str_search::contains),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Range",
        name: "index",
        arity: 1,
        handler: Handler::Pure(crate::builtins::method_table::str_search::index),
        flags: RowFlags::NONE,
        named: &[],
    },
];

/// `Range.excludes-min`: whether the lower endpoint is excluded (`^..`).
// Cost: O(1).
pub(crate) fn excludes_min(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Range(..) | ValueView::RangeExcl(..) => Some(Ok(Value::FALSE)),
        ValueView::RangeExclStart(..) | ValueView::RangeExclBoth(..) => Some(Ok(Value::TRUE)),
        ValueView::GenericRange { excl_start, .. } => Some(Ok(Value::truth(excl_start))),
        _ => None,
    }
}

/// `Range.excludes-max`: whether the upper endpoint is excluded (`..^`).
// Cost: O(1).
pub(crate) fn excludes_max(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Range(..) | ValueView::RangeExclStart(..) => Some(Ok(Value::FALSE)),
        ValueView::RangeExcl(..) | ValueView::RangeExclBoth(..) => Some(Ok(Value::TRUE)),
        ValueView::GenericRange { excl_end, .. } => Some(Ok(Value::truth(excl_end))),
        _ => None,
    }
}

/// `Range.bounds`: the two endpoints as they were written, an open end
/// (`*`, `Inf`) answered as an infinity.
// Cost: O(1).
pub(crate) fn bounds(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Range(a, b)
        | ValueView::RangeExcl(a, b)
        | ValueView::RangeExclStart(a, b)
        | ValueView::RangeExclBoth(a, b) => Some(Ok(Value::array(vec![
            if a == i64::MIN {
                Value::num(f64::NEG_INFINITY)
            } else {
                Value::int(a)
            },
            if b == i64::MAX {
                Value::num(f64::INFINITY)
            } else {
                Value::int(b)
            },
        ]))),
        ValueView::GenericRange { start, end, .. } => {
            let s = match start.as_ref().view() {
                ValueView::Whatever | ValueView::HyperWhatever => Value::num(f64::NEG_INFINITY),
                _ => start.as_ref().clone(),
            };
            let e = match end.as_ref().view() {
                ValueView::Whatever | ValueView::HyperWhatever => Value::num(f64::INFINITY),
                _ => end.as_ref().clone(),
            };
            Some(Ok(Value::array(vec![s, e])))
        }
        _ => None,
    }
}

/// `Range.is-int`: whether both endpoints are genuine integers. An open end
/// (`*`, `Inf`) is not one; the rule is the one `int-bounds` and `minmax`
/// use.
// Cost: O(1).
pub(crate) fn is_int(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    crate::builtins::range_bounds_int::range_is_int(target).map(|is_int| Ok(Value::truth(is_int)))
}

/// `Range.infinite`: whether either end is open.
// Cost: O(1).
pub(crate) fn infinite(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Range(..)
        | ValueView::RangeExcl(..)
        | ValueView::RangeExclStart(..)
        | ValueView::RangeExclBoth(..)
        | ValueView::GenericRange { .. } => Some(Ok(Value::truth(
            crate::builtins::methods_0arg::is_infinite_range(target),
        ))),
        _ => None,
    }
}

/// `Range.int-bounds`, the zero-argument candidate: the `(from, to)` List, or
/// a failure when the range has no integer bounds (an infinite or `Whatever`
/// end, a `Str` range, a fractional lower bound). The two-argument
/// `int-bounds($from is rw, $to is rw --> Bool)` candidate needs the caller's
/// containers, so the VM serves it (`vm/vm_range_int_bounds.rs`), not a row.
// Cost: O(1) for an `Int` range, O(d) for big endpoints, d = digits.
pub(crate) fn int_bounds(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !target.is_range() {
        return None;
    }
    Some(
        match crate::builtins::range_bounds_int::range_int_bounds(target) {
            Some((from, to)) => Ok(Value::array(vec![from, to])),
            None => Err(RuntimeError::new("Cannot determine integer bounds")),
        },
    )
}

/// `Range.rand`: a `Num` in the range, and a failure for a range whose
/// endpoints are not ordered (`min >= max`, whatever the exclusions) or that has
/// a non-numeric end. An excluded end is never returned.
// Cost: O(1).
pub(crate) fn rand(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    // The next double above `from` / below `to`: the excluded ends.
    let above = |from: f64| f64::from_bits(from.to_bits().saturating_add(1));
    let below = |to: f64| f64::from_bits(to.to_bits().saturating_sub(1));
    let sample = |from: f64, to: f64| from + builtin_rand() * (to - from);
    let (from, to, excl_start, excl_end) = match target.view() {
        ValueView::Range(start, end) => (start, end, false, false),
        ValueView::RangeExcl(start, end) => (start, end, false, true),
        ValueView::RangeExclStart(start, end) => (start, end, true, false),
        ValueView::RangeExclBoth(start, end) => (start, end, true, true),
        ValueView::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
            let (Some(mut from), Some(mut to)) = (
                crate::runtime::to_float_value(start),
                crate::runtime::to_float_value(end),
            ) else {
                return Some(Ok(non_numeric_failure()));
            };
            if from >= to {
                return Some(Ok(invalid_endpoints_failure(start, end)));
            }
            if excl_start {
                from = above(from);
            }
            if excl_end {
                to = below(to);
            }
            if !from.is_finite() || !to.is_finite() || from > to {
                return Some(Ok(Value::NIL));
            }
            return Some(Ok(Value::num(sample(from, to))));
        }
        _ => return None,
    };
    if from >= to {
        return Some(Ok(invalid_endpoints_failure(
            &Value::int(from),
            &Value::int(to),
        )));
    }
    let (from, to) = (from as f64, to as f64);
    let mut v = sample(from, to);
    if excl_start && v <= from {
        v = above(from);
    }
    if excl_end && v >= to {
        v = below(to);
    }
    Some(Ok(Value::num(v)))
}

/// The `X::Range::Rand::InvalidEndpoints` failure `Range.rand` answers for a
/// range with `min >= max`.
// Cost: O(1).
fn invalid_endpoints_failure(min: &Value, max: &Value) -> Value {
    let (smin, smax) = (min.to_str_context(), max.to_str_context());
    let message = if crate::runtime::to_float_value(min) == crate::runtime::to_float_value(max) {
        "Impossible to generate random numbers for a range where endpoints are equal".to_string()
    } else {
        format!(
            "Impossible to get a random number from range containing no values.\n\
             The sequence (...) operator supports descension between {smin} and {smax},\n\
             but for a random number between {smin} and {smax}, ({smax}..{smin}).rand is\n\
             likely to be functionally equivalent to what was meant by ({smin}..{smax}).rand"
        )
    };
    let mut ex_attrs = std::collections::HashMap::new();
    ex_attrs.insert("min".to_string(), min.clone());
    ex_attrs.insert("max".to_string(), max.clone());
    ex_attrs.insert("message".to_string(), Value::str(message));
    let ex = Value::make_instance(Symbol::intern("X::Range::Rand::InvalidEndpoints"), ex_attrs);
    let mut failure_attrs = std::collections::HashMap::new();
    failure_attrs.insert("exception".to_string(), ex);
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}

/// The failure `Range.rand` answers for a non-numeric end.
// Cost: O(1).
fn non_numeric_failure() -> Value {
    let mut ex_attrs = std::collections::HashMap::new();
    ex_attrs.insert(
        "message".to_string(),
        Value::str("Cannot get a random value from a non-numeric Range".to_string()),
    );
    let ex = Value::make_instance(Symbol::intern("X::AdHoc"), ex_attrs);
    let mut failure_attrs = std::collections::HashMap::new();
    failure_attrs.insert("exception".to_string(), ex);
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}

/// `Range.in-range($got)`.
// Cost: O(1) for numeric ends; O(n) for a string range, n = chars of the
// endpoints and the value.
pub(crate) fn in_range_value(
    target: &Value,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    in_range(target, &args[0], "Value")
}

/// `Range.in-range($got, $what)`: `$what` names the checked thing in the
/// out-of-range error.
// Cost: O(1) for numeric ends; O(n) for a string range, n = chars of the
// endpoints, the value and the label.
pub(crate) fn in_range_what(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    in_range(target, &args[0], &args[1].to_string_value())
}

/// `value` is in `target`, or an `X::OutOfRange` that says `what` was not.
// Cost: see [`in_range_what`].
fn in_range(target: &Value, value: &Value, what: &str) -> Option<Result<Value, RuntimeError>> {
    crate::builtins::arith::range::range_bounds(target)?;
    if range_contains_value(target, value) {
        return Some(Ok(Value::TRUE));
    }
    use crate::builtins::methods_0arg::raku_repr::raku_value;
    let msg = format!(
        "{} out of range. Is: {}, should be in {}",
        what,
        raku_value(value),
        raku_value(target)
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert("got".to_string(), value.clone());
    let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    Some(Err(err))
}

/// Whether `val` lies within `range`, honoring the range's exclusivity and
/// Whatever endpoints. Mirrors `Interpreter::value_in_range` for the numeric
/// and string-endpoint cases used by `.in-range`.
// Cost: O(1) for numeric ends; O(n) for a string range, n = chars of the
// endpoints and the value.
fn range_contains_value(range: &Value, val: &Value) -> bool {
    let Some((start, end, excl_start, excl_end)) =
        crate::builtins::arith::range::range_bounds(range)
    else {
        return false;
    };
    let start_whatever = matches!(start.view(), ValueView::Whatever | ValueView::HyperWhatever);
    let end_whatever = matches!(end.view(), ValueView::Whatever | ValueView::HyperWhatever);
    let string_range =
        matches!(start.view(), ValueView::Str(_)) || matches!(end.view(), ValueView::Str(_));
    if string_range {
        let v = val.to_string_value();
        let smin = start.to_string_value();
        let smax = end.to_string_value();
        let min_ok = start_whatever || if excl_start { v > smin } else { v >= smin };
        let max_ok = end_whatever || if excl_end { v < smax } else { v <= smax };
        return min_ok && max_ok;
    }
    let v = val.to_f64();
    let min_ok = start_whatever || {
        let vmin = start.to_f64();
        if excl_start { v > vmin } else { v >= vmin }
    };
    let max_ok = end_whatever || {
        let vmax = end.to_f64();
        if excl_end { v < vmax } else { v <= vmax }
    };
    min_ok && max_ok
}

/// `Range.elems`: the number of elements; an unbounded range is lazy and
/// answers a `X::Cannot::Lazy` failure.
// Cost: O(1) for an Int range or one with Int/BigInt endpoints, O(e)
// otherwise, e = elements (a range of other endpoints is expanded to count).
pub(crate) fn elems(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let lazy = || Some(Ok(crate::runtime::utils::cannot_lazy_failure("elems")));
    let count = match target.view() {
        ValueView::Range(start, end) if start == i64::MIN || end == i64::MAX => return lazy(),
        ValueView::Range(start, end) => (end - start + 1).max(0),
        ValueView::RangeExcl(start, end) | ValueView::RangeExclStart(start, end)
            if start == i64::MIN || end == i64::MAX =>
        {
            return lazy();
        }
        ValueView::RangeExcl(start, end) | ValueView::RangeExclStart(start, end) => {
            (end - start).max(0)
        }
        ValueView::RangeExclBoth(start, end) if start == i64::MIN || end == i64::MAX => {
            return lazy();
        }
        ValueView::RangeExclBoth(start, end) => (end - start - 1).max(0),
        ValueView::GenericRange { .. }
            if crate::builtins::methods_0arg::is_infinite_range(target) =>
        {
            return lazy();
        }
        // An Int/BigInt-ended range counts from its endpoints (the same
        // exact count `.Numeric` uses); expanding it would stop at
        // `MAX_RANGE_EXPAND`.
        ValueView::GenericRange { start, end, .. }
            if matches!(start.view(), ValueView::Int(_) | ValueView::BigInt(_))
                && matches!(end.view(), ValueView::Int(_) | ValueView::BigInt(_)) =>
        {
            return Some(Ok(crate::value::radix_numeric::coerce_to_numeric(
                target.clone(),
            )));
        }
        ValueView::GenericRange { .. } => crate::runtime::utils::value_to_list(target).len() as i64,
        _ => return None,
    };
    Some(Ok(Value::int(count)))
}

/// `Range.min`: the lower endpoint as written (`-Inf` for an open start).
// Cost: O(1).
pub(crate) fn min(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Range(a, _)
        | ValueView::RangeExcl(a, _)
        | ValueView::RangeExclStart(a, _)
        | ValueView::RangeExclBoth(a, _) => Some(Ok(if a == i64::MIN {
            Value::num(f64::NEG_INFINITY)
        } else {
            Value::int(a)
        })),
        ValueView::GenericRange { start, .. } => {
            let s = start.as_ref();
            Some(Ok(match s.view() {
                ValueView::Whatever | ValueView::HyperWhatever => Value::num(f64::NEG_INFINITY),
                _ => s.clone(),
            }))
        }
        _ => None,
    }
}

/// `Range.max`: the upper endpoint as written (`Inf` for an open end).
// Cost: O(1).
pub(crate) fn max(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Range(_, b)
        | ValueView::RangeExcl(_, b)
        | ValueView::RangeExclStart(_, b)
        | ValueView::RangeExclBoth(_, b) => Some(Ok(if b == i64::MAX {
            Value::num(f64::INFINITY)
        } else {
            Value::int(b)
        })),
        ValueView::GenericRange { end, .. } => {
            let e = end.as_ref();
            Some(Ok(match e.view() {
                ValueView::Whatever | ValueView::HyperWhatever => Value::num(f64::INFINITY),
                _ => e.clone(),
            }))
        }
        _ => None,
    }
}

/// `Range.minmax` folds an excluded end into the returned bound, but only
/// when the range is `is-int` -- an excluded *non-integer* end (`1.1..^5.2`,
/// `'a'..^'z'`, `1..^Inf`) has no nameable concrete bound, and raku fails
/// with `X::AdHoc: Cannot return minmax on Range with excluded ends`.
// Cost: O(1).
pub(crate) fn minmax(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match crate::builtins::range_bounds_int::range_minmax(target)? {
        Ok((min_val, max_val)) => Some(Ok(Value::array(vec![min_val, max_val]))),
        Err(()) => Some(Err(RuntimeError::new(
            "Cannot return minmax on Range with excluded ends",
        ))),
    }
}

/// `Range.Numeric` (and `.Int`, `.Real`, `.Num` through the cascade): the
/// number of elements. An unbounded range yields `Inf` for the real-valued
/// coercions and fails for `.Int`. `target` is a Range.
// Cost: O(1) for an Int range or Int/BigInt endpoints, O(e) otherwise.
pub(crate) fn numeric_coercion(target: &Value, method: &str) -> Result<Value, RuntimeError> {
    if crate::builtins::methods_0arg::is_infinite_range(target) {
        return if method == "Int" {
            Err(RuntimeError::new("Cannot convert Inf to Int".to_string()))
        } else {
            Ok(Value::num(f64::INFINITY))
        };
    }
    // An Int/BigInt-ended range counts from its endpoints (no expansion cap).
    if let ValueView::GenericRange { start, end, .. } = target.view()
        && matches!(start.view(), ValueView::Int(_) | ValueView::BigInt(_))
        && matches!(end.view(), ValueView::Int(_) | ValueView::BigInt(_))
    {
        let exact = crate::value::radix_numeric::coerce_to_numeric(target.clone());
        return Ok(if method == "Num" {
            Value::num(exact.to_f64())
        } else {
            exact
        });
    }
    let count = match target.view() {
        ValueView::Range(s, e) => (e - s + 1).max(0),
        ValueView::RangeExcl(s, e) | ValueView::RangeExclStart(s, e) => (e - s).max(0),
        ValueView::RangeExclBoth(s, e) => (e - s - 1).max(0),
        // GenericRange (e.g. Rat endpoints `1.5..5.5`) has no closed-form count;
        // materialize it the same way `.elems` does.
        _ => crate::runtime::utils::value_to_list(target).len() as i64,
    };
    Ok(if method == "Num" {
        Value::num(count as f64)
    } else {
        Value::int(count)
    })
}

/// `Range.Numeric`.
// Cost: see [`numeric_coercion`].
pub(crate) fn numeric(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target
        .is_range()
        .then(|| numeric_coercion(target, "Numeric"))
}

/// `Range.list` (and `.Array`, through [`listify`]'s cascade caller):
/// the elements. An unbounded range stays lazy: a lazy List for `.list`, a
/// lazy Array for `.Array` (Rakudo: `(1..*).list.^name` is `List`,
/// `(1.5..*).Array.is-lazy`). `want_array` makes the result a real `@`-sigiled
/// Array whose aggregate elements are itemized.
// Cost: O(1) for an unbounded range; O(e) otherwise, e = elements.
pub(crate) fn listify(target: &Value, want_array: bool) -> Option<Result<Value, RuntimeError>> {
    // ADR-0040 slice 2: `.Array` builds a REAL Array, whose elements are
    // `Scalar` containers, so aggregates itemize on the way in. `.list` builds
    // a List, whose elements are not containers, so it must not.
    let wrap = |items: Vec<Value>| {
        if want_array {
            crate::runtime::utils::itemize_real_array_elements(Value::real_array(items))
        } else {
            Value::array(items)
        }
    };
    if let Some(ll) = crate::runtime::unbounded_range::lazy_list(target) {
        let ll = if want_array {
            ll.with_array_context()
        } else {
            ll.with_list_context()
        };
        return Some(Ok(Value::lazy_list(crate::gc::Gc::new(ll))));
    }
    // An open end of an Int range (`i64::MIN`/`MAX`) becomes a lazy array
    // (supports indexing; `.Capture` on it throws).
    let open = |a: i64, b: i64| b == i64::MAX || a == i64::MIN;
    match target.view() {
        ValueView::Range(a, b) if open(a, b) => {
            Some(Ok(crate::runtime::utils::coerce_to_array(target.clone())))
        }
        ValueView::Range(a, b) => Some(Ok(wrap((a..=b).map(Value::int).collect()))),
        ValueView::RangeExcl(a, b) if open(a, b) => {
            Some(Ok(crate::runtime::utils::coerce_to_array(target.clone())))
        }
        ValueView::RangeExcl(a, b) => Some(Ok(wrap((a..b).map(Value::int).collect()))),
        ValueView::RangeExclStart(a, b) if open(a, b) => {
            Some(Ok(crate::runtime::utils::coerce_to_array(target.clone())))
        }
        ValueView::RangeExclStart(a, b) => Some(Ok(wrap((a + 1..=b).map(Value::int).collect()))),
        ValueView::RangeExclBoth(a, b) if open(a, b) => {
            Some(Ok(crate::runtime::utils::coerce_to_array(target.clone())))
        }
        ValueView::RangeExclBoth(a, b) => Some(Ok(wrap((a + 1..b).map(Value::int).collect()))),
        ValueView::GenericRange { .. } => {
            Some(Ok(wrap(crate::runtime::utils::value_to_list(target))))
        }
        _ => None,
    }
}

/// `Range.list`.
// Cost: see [`listify`].
pub(crate) fn list(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    listify(target, false)
}
