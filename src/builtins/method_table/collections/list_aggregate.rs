//! Aggregate and combinator methods shared by the method table and cascade.

use super::{Handler, MethodRow, RowFlags};
use crate::runtime;
use crate::value::{RuntimeError, Value, ValueView};
use num_bigint::BigInt as NumBigInt;

macro_rules! rows {
    ($owner:literal: $($name:literal => $handler:ident),* $(,)?) => {
        &[$(MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }),*]
    };
}

pub(super) static ANY_ROWS: &[MethodRow] = rows!["Any":
    "min" => min,
    "max" => max,
    "minpairs" => minpairs,
    "maxpairs" => maxpairs,
    "minmax" => minmax,
    "sum" => sum,
];

pub(super) static LIST_ROWS: &[MethodRow] = rows!["List":
    "permutations" => permutations,
    "combinations" => combinations,
];

/// `combinations($of)`: the first row to take a `Range` argument, so it opts
/// in to every plain argument.
pub(super) static LIST_COMBINATIONS_OF: &[MethodRow] = &[MethodRow {
    owner: "List",
    name: "combinations",
    arity: 1,
    handler: Handler::Narrow(combinations_of),
    flags: RowFlags::ANY_ARGS,
    named: &[],
}];

fn plain_extrema_items(target: &Value) -> Option<Vec<Value>> {
    // A `Date`, `DateTime`, `Instant` or `Duration` is one item and has no
    // other to compare with.
    if super::any_collection::scalar_like(target) {
        return Some(vec![target.clone()]);
    }
    let items = match target.view() {
        ValueView::Array(items, _) => items.to_vec(),
        ValueView::Bool(_)
        | ValueView::Str(_)
        | ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Rat(..)
        | ValueView::FatRat(..)
        | ValueView::BigRat(..)
        | ValueView::Complex(..) => vec![target.clone()],
        _ => return None,
    };
    if items.iter().all(plain_extrema_value) {
        Some(items)
    } else {
        None
    }
}

fn plain_extrema_value(value: &Value) -> bool {
    matches!(
        value.view(),
        ValueView::Bool(_)
            | ValueView::Str(_)
            | ValueView::Int(_)
            | ValueView::BigInt(_)
            | ValueView::Num(_)
            | ValueView::Rat(..)
            | ValueView::FatRat(..)
            | ValueView::BigRat(..)
            | ValueView::Complex(..)
    )
}

fn extrema(target: &Value, args: &[Value], want_max: bool) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    if let ValueView::Hash(map) = target.view() {
        let mut best: Option<(Value, Value)> = None;
        for (key, value) in map.iter() {
            let typed_key = map.typed_key(key);
            if best.as_ref().is_none_or(|(current, _)| {
                let ordering = crate::runtime::compare_values(&typed_key, current);
                (want_max && ordering > 0) || (!want_max && ordering < 0)
            }) {
                best = Some((typed_key, map.typed_pair(key, value.clone())));
            }
        }
        return Some(Ok(best.map_or_else(
            || {
                if want_max {
                    Value::num(f64::NEG_INFINITY)
                } else {
                    Value::num(f64::INFINITY)
                }
            },
            |(_, pair)| pair,
        )));
    }
    let items = plain_extrema_items(target)?;
    let Some(mut best) = items.first().cloned() else {
        return Some(Ok(if want_max {
            Value::num(f64::NEG_INFINITY)
        } else {
            Value::num(f64::INFINITY)
        }));
    };
    for item in items.iter().skip(1) {
        let ordering = crate::runtime::compare_values(item, &best);
        if (want_max && ordering > 0) || (!want_max && ordering < 0) {
            best = item.clone();
        }
    }
    Some(Ok(best))
}

// Cost: O(e), e = plain positional elements compared with the running extremum.
pub(crate) fn min(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    extrema(target, args, false)
}

// Cost: O(e), e = plain positional elements compared with the running extremum.
pub(crate) fn max(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    extrema(target, args, true)
}

fn extrema_pairs(
    target: &Value,
    args: &[Value],
    want_max: bool,
) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    if let ValueView::Hash(map) = target.view() {
        let mut best: Option<Value> = None;
        let mut result = Vec::new();
        for (key, value) in map.iter() {
            let pair = map.typed_pair(key, value.clone());
            let ordering = best.as_ref().map_or(std::cmp::Ordering::Equal, |current| {
                crate::runtime::compare_values(value, current).cmp(&0)
            });
            let replaces = best.is_none()
                || (want_max && ordering == std::cmp::Ordering::Greater)
                || (!want_max && ordering == std::cmp::Ordering::Less);
            if replaces {
                best = Some(value.clone());
                result.clear();
                result.push(pair);
            } else if ordering == std::cmp::Ordering::Equal {
                result.push(pair);
            }
        }
        return Some(Ok(Value::seq(result)));
    }
    let items = plain_extrema_items(target)?;
    if items.is_empty() {
        return Some(Ok(Value::seq(Vec::new())));
    }
    let mut best = items[0].clone();
    let mut result = vec![Value::value_pair(Value::int(0), best.clone())];
    for (index, item) in items.iter().enumerate().skip(1) {
        let ordering = crate::runtime::compare_values(item, &best);
        let replaces = (want_max && ordering > 0) || (!want_max && ordering < 0);
        if replaces {
            best = item.clone();
            result.clear();
            result.push(Value::value_pair(Value::int(index as i64), item.clone()));
        } else if ordering == 0 {
            result.push(Value::value_pair(Value::int(index as i64), item.clone()));
        }
    }
    Some(Ok(Value::seq(result)))
}

// Cost: O(e), e = plain positional elements compared with the running extremum.
pub(crate) fn minpairs(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    extrema_pairs(target, args, false)
}

// Cost: O(e), e = plain positional elements compared with the running extremum.
pub(crate) fn maxpairs(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    extrema_pairs(target, args, true)
}

/// The inclusive range from the minimum to maximum values in an Any receiver's list.
// Cost: O(e), e = elements and their minimum/maximum candidates.
pub(crate) fn minmax(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    let list_target = crate::runtime::utils::hashlike_receiver_as_pairs_list(target, "minmax")
        .unwrap_or_else(|| target.clone());
    let items = match list_target.view() {
        ValueView::Array(items, ..) => items.to_vec(),
        _ => crate::runtime::utils::value_to_list_for_receiver(&list_target),
    };
    let mut candidates = Vec::new();
    for item in &items {
        crate::runtime::builtins_collection::collect_minmax_candidates_pub(item, &mut candidates);
    }
    if candidates.is_empty() {
        Some(Ok(Value::generic_range(
            Value::num(f64::INFINITY),
            Value::num(f64::NEG_INFINITY),
            false,
            false,
        )))
    } else {
        let mut min = &candidates[0];
        let mut max = &candidates[0];
        for item in &candidates[1..] {
            if runtime::compare_values(item, min) < 0 {
                min = item;
            }
            if runtime::compare_values(item, max) > 0 {
                max = item;
            }
        }
        Some(Ok(
            crate::runtime::builtins_collection::make_inclusive_range_pub(min.clone(), max.clone()),
        ))
    }
}

/// Sum values in an Any receiver's list, preserving numeric promotion and Junctions.
// Cost: O(e + t + b), e = elements, t = numeric string bytes parsed, b = arithmetic digit growth.
pub(crate) fn sum(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    match target.view() {
        ValueView::Array(items, ..) => {
            if items
                .iter()
                .any(|v| matches!(v.view(), ValueView::Junction { .. }))
            {
                let result = items.iter().cloned().try_fold(
                    Value::int(0),
                    |acc, item| -> Result<Value, RuntimeError> {
                        crate::builtins::methods_0arg::collection::add_with_junction_threading(
                            acc, item,
                        )
                    },
                );
                return Some(result);
            }
            if let Err(err) = validate_sum_items(&items) {
                return Some(Err(err));
            }
            Some(
                items
                    .iter()
                    .cloned()
                    .try_fold(Value::int(0), crate::builtins::arith_add),
            )
        }
        ValueView::Range(a, b) => {
            if a > b {
                Some(Ok(Value::int(0)))
            } else {
                let n = b - a + 1;
                let sum = if (a + b) % 2 == 0 {
                    ((a + b) / 2) * n
                } else {
                    (a + b) * (n / 2)
                };
                Some(Ok(Value::int(sum)))
            }
        }
        ValueView::RangeExcl(a, b) => {
            if a >= b {
                Some(Ok(Value::int(0)))
            } else {
                let end = b - 1;
                let n = end - a + 1;
                let sum = if (a + end) % 2 == 0 {
                    ((a + end) / 2) * n
                } else {
                    (a + end) * (n / 2)
                };
                Some(Ok(Value::int(sum)))
            }
        }
        ValueView::RangeExclStart(a, b) => {
            let start = a + 1;
            if start > b {
                Some(Ok(Value::int(0)))
            } else {
                let n = b - start + 1;
                let sum = if (start + b) % 2 == 0 {
                    ((start + b) / 2) * n
                } else {
                    (start + b) * (n / 2)
                };
                Some(Ok(Value::int(sum)))
            }
        }
        ValueView::RangeExclBoth(a, b) => {
            let start = a + 1;
            let end = b - 1;
            if start > end {
                Some(Ok(Value::int(0)))
            } else {
                let n = end - start + 1;
                let sum = if (start + end) % 2 == 0 {
                    ((start + end) / 2) * n
                } else {
                    (start + end) * (n / 2)
                };
                Some(Ok(Value::int(sum)))
            }
        }
        ValueView::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
            let start_bi = value_as_bigint(start);
            let end_bi = value_as_bigint(end);
            if let (Some(a), Some(b)) = (start_bi, end_bi) {
                let one = NumBigInt::from(1);
                let two = NumBigInt::from(2);
                let zero = NumBigInt::from(0);
                let effective_start = if excl_start { &a + &one } else { a };
                let effective_end = if excl_end { &b - &one } else { b };
                if effective_start > effective_end {
                    Some(Ok(Value::int(0)))
                } else {
                    let n = &effective_end - &effective_start + &one;
                    let s_plus = &effective_start + &effective_end;
                    let sum = if &s_plus % &two == zero {
                        (&s_plus / &two) * &n
                    } else {
                        &s_plus * (&n / &two)
                    };
                    if let Ok(val) = i64::try_from(&sum) {
                        Some(Ok(Value::int(val)))
                    } else {
                        Some(Ok(Value::bigint(sum)))
                    }
                }
            } else {
                let items = runtime::value_to_list(target);
                let has_rat = items
                    .iter()
                    .any(|v| matches!(v.view(), ValueView::Rat(_, _)));
                if has_rat {
                    let mut num: i64 = 0;
                    let mut den: i64 = 1;
                    for item in &items {
                        let (in_num, in_den) = match item.view() {
                            ValueView::Rat(n, d) => (n, d),
                            ValueView::Int(n) => (n, 1),
                            _ => (runtime::to_int(item), 1),
                        };
                        num = num * in_den + in_num * den;
                        den *= in_den;
                        let g = gcd_u64(num.unsigned_abs(), den.unsigned_abs()) as i64;
                        if g > 1 {
                            num /= g;
                            den /= g;
                        }
                    }
                    if den == 1 {
                        Some(Ok(Value::int(num)))
                    } else {
                        Some(Ok(Value::rat_raw(num, den)))
                    }
                } else {
                    let total: i64 = items.iter().map(runtime::to_int).sum();
                    Some(Ok(Value::int(total)))
                }
            }
        }
        _ => {
            let list_target = crate::runtime::utils::hashlike_receiver_as_pairs_list(target, "sum")
                .unwrap_or_else(|| target.clone());
            let items = crate::runtime::utils::value_to_list_for_receiver(&list_target);
            if items
                .iter()
                .any(|v| matches!(v.view(), ValueView::Junction { .. }))
            {
                return Some(items.into_iter().try_fold(
                    Value::int(0),
                    |acc, item| -> Result<Value, RuntimeError> {
                        crate::builtins::methods_0arg::collection::add_with_junction_threading(
                            acc, item,
                        )
                    },
                ));
            }
            if let Err(err) = validate_sum_items(&items) {
                return Some(Err(err));
            }
            Some(
                items
                    .into_iter()
                    .try_fold(Value::int(0), crate::builtins::arith_add),
            )
        }
    }
}

/// Validate elements before the arithmetic fold, whose primitive numeric
/// coercion is intentionally more permissive for some non-numeric values.
// Cost: O(e + t), e = elements, t = bytes in numeric strings parsed.
fn validate_sum_items(items: &[Value]) -> Result<(), RuntimeError> {
    for item in items {
        if matches!(item.view(), ValueView::Pair(..) | ValueView::ValuePair(..)) {
            return Err(crate::runtime::numeric_no_match_error("Pair"));
        }
        if let ValueView::Str(s) = item.view()
            && crate::runtime::str_numeric::parse_raku_str_to_numeric(&s).is_none()
        {
            let reason = "base-10 number must begin with valid digits or '.'".to_string();
            let msg = format!("Cannot convert string '{}' to number: {}", *s, reason);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("source".to_string(), Value::str(s.to_string()));
            attrs.insert("reason".to_string(), Value::str(reason));
            attrs.insert("pos".to_string(), Value::int(0));
            attrs.insert("target-name".to_string(), Value::str("Numeric".to_string()));
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex = Value::make_instance(crate::symbol::Symbol::intern("X::Str::Numeric"), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
    }
    Ok(())
}

/// Return the lazy sequence of permutations of a List or Array.
// Cost: O(e) to snapshot, plus bigint work for e! when e > 20; O(e) per permutation pulled.
pub(crate) fn permutations(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    let items = if crate::runtime::utils::is_shaped_array(target) {
        crate::runtime::utils::shaped_array_leaves(target)
    } else {
        target
            .as_list_items()
            .map(|items| items.to_vec())
            .unwrap_or_else(|| runtime::value_to_list(target))
    };
    if items.len() > 20 {
        let factorial = crate::builtins::functions::factorial_bigint(items.len() as u64);
        let ll = crate::value::LazyList {
            body: Vec::new(),
            env: crate::env::Env::new(),
            cache: std::sync::Mutex::new(None),
            generation_state: std::sync::Mutex::new(None),
            compiled_code: None,
            compiled_fns: None,
            elems_count: Some(Value::bigint_arc(factorial)),
            scan_spec: None,
            sequence_spec: None,
            coroutine: None,
            lazy_pipe: None,
            closure_seq: None,
            walk_pending: None,
            cat_pull: None,
            array_context: false,
            list_context: false,
            cached_no_sink: false,
            itemized: false,
        };
        return Some(Ok(Value::lazy_list(crate::gc::Gc::new(ll))));
    }
    Some(Ok(
        crate::builtins::methods_0arg::collection::permutations_seq(items),
    ))
}

/// Return every subset of a List or Array as a lazy sequence.
// Cost: O(e) per call to snapshot the receiver; O(k) per combination pulled.
pub(crate) fn combinations(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    let items = if crate::runtime::utils::is_shaped_array(target) {
        crate::runtime::utils::shaped_array_leaves(target)
    } else {
        target
            .as_list_items()
            .map(|items| items.to_vec())
            .unwrap_or_else(|| runtime::value_to_list(target))
    };
    let n = items.len() as i64;
    Some(Ok(
        crate::builtins::methods_0arg::collection::combinations_seq(items, 0, n),
    ))
}

/// `combinations($of)`: the subsets of a size, or of every size in a `Range`.
/// A negative size has no subsets. The row admits any plain argument
/// (`RowFlags::ANY_ARGS`), so a `Range` reaches it; this is also the native
/// cascade's one-argument `combinations`.
// Cost: O(e) per call to snapshot the receiver; O(k) per combination pulled.
pub(crate) fn combinations_of(
    target: &Value,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    use crate::builtins::methods_0arg::collection::combinations_seq;
    let [arg] = args else {
        return None;
    };
    let items = target
        .as_list_items()
        .map(|items| items.to_vec())
        .unwrap_or_else(|| runtime::value_to_list_for_receiver(target));
    match arg.view() {
        ValueView::Range(a, b) => Some(Ok(combinations_seq(items, a, b))),
        ValueView::RangeExcl(a, b) => Some(Ok(combinations_seq(items, a, b - 1))),
        ValueView::RangeExclStart(a, b) => Some(Ok(combinations_seq(items, a + 1, b))),
        ValueView::RangeExclBoth(a, b) => Some(Ok(combinations_seq(items, a + 1, b - 1))),
        ValueView::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
            let mut lo = runtime::to_int(start);
            let mut hi = runtime::to_int(end);
            if excl_start {
                lo += 1;
            }
            if excl_end {
                hi -= 1;
            }
            Some(Ok(combinations_seq(items, lo, hi)))
        }
        _ => {
            let k = runtime::to_int(arg);
            if k < 0 {
                Some(Ok(Value::seq(Vec::new())))
            } else {
                Some(Ok(combinations_seq(items, k, k)))
            }
        }
    }
}

/// If a value represents an integer, return it as BigInt.
// Cost: O(b + t), b = integer digits and t = input string bytes parsed.
fn value_as_bigint(v: &Value) -> Option<NumBigInt> {
    match v.view() {
        ValueView::Int(i) => Some(NumBigInt::from(i)),
        ValueView::BigInt(n) => Some((**n).clone()),
        ValueView::Num(f) => {
            if f.is_finite() && f == f.trunc() && f.abs() < i64::MAX as f64 {
                Some(NumBigInt::from(f as i64))
            } else {
                None
            }
        }
        ValueView::Rat(n, d) => {
            if d != 0 && n % d == 0 {
                Some(NumBigInt::from(n / d))
            } else {
                None
            }
        }
        ValueView::Str(s) => {
            let trimmed = s.trim();
            trimmed.parse::<i64>().ok().map(NumBigInt::from)
        }
        ValueView::Bool(b) => Some(NumBigInt::from(if b { 1 } else { 0 })),
        _ => None,
    }
}

/// Greatest common divisor of two unsigned integers.
// Cost: O(log(min(a, b))) Euclidean steps.
fn gcd_u64(mut a: u64, mut b: u64) -> u64 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}
