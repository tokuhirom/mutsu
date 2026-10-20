//! Aggregate and combinator methods shared by the method table and cascade.

use super::{Handler, MethodRow, RowFlags};
use crate::runtime;
use crate::value::{RuntimeError, Value, ValueView, const_str_eq};
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
/// in to every plain argument. Rakudo declares it on `List` and on `Any`
/// (`self.list.combinations(|c)`), which a `Seq`, a `Range` or a `Str`
/// reaches (ADR-11276 §9.100).
pub(super) static LIST_COMBINATIONS_OF: &[MethodRow] =
    &[combinations_of_row("List"), combinations_of_row("Any")];

const fn combinations_of_row(owner: &'static str) -> MethodRow {
    MethodRow {
        owner,
        name: "combinations",
        arity: 1,
        handler: Handler::Narrow(combinations_of),
        // `Any`'s candidate takes a type object too (`Str.combinations(1)`).
        flags: if const_str_eq(owner, "Any") {
            RowFlags::ANY_ARGS.or(RowFlags::TYPE_OBJECT_OK)
        } else {
            RowFlags::ANY_ARGS
        },
        named: &[],
    }
}

fn plain_extrema_items(target: &Value) -> Option<Vec<Value>> {
    // A `Date`, `DateTime`, `Instant`, `Duration` or `Pair` is one item and
    // has no other to compare with.
    if super::any_collection::scalar_like(target)
        || matches!(
            target.view(),
            ValueView::Pair(..) | ValueView::ValuePair(..)
        )
    {
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
    // A lazy list has no extremum to find (#12971); a `Range` answers its own
    // bounds through its rows.
    if !target.is_range() && crate::builtins::methods_0arg::is_value_lazy(target) {
        return Some(Err(RuntimeError::cannot_lazy(if want_max {
            "max"
        } else {
            "min"
        })));
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
    if matches!(target.view(), ValueView::Bool(_)) {
        let (key, value) = if want_max { ("True", 1) } else { ("False", 0) };
        return Some(Ok(Value::array(vec![Value::pair(
            key.to_string(),
            Value::int(value),
        )])));
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
        return Some(Ok(Value::array(result)));
    }
    let items = plain_extrema_items(target)?;
    if items.is_empty() {
        return Some(Ok(Value::array(Vec::new())));
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
    Some(Ok(Value::array(result)))
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
    if !target.is_range() && crate::builtins::methods_0arg::is_value_lazy(target) {
        return Some(Err(RuntimeError::cannot_lazy("minmax")));
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
        // The endpoints are `Real`s: a `Match` is compared as the `Cool` it is,
        // then numifies through its text (a non-numeric one is a `Failure`).
        let endpoint = |item: &Value| -> Result<Value, RuntimeError> {
            if item.match_str_value().is_none() {
                return Ok(item.clone());
            }
            match crate::runtime::utils::str_numeric_operand_failure(item) {
                Some(failure) => Ok(failure),
                None => crate::builtins::arith_add(Value::int(0), item.clone()),
            }
        };
        Some(endpoint(min).and_then(|lo| {
            endpoint(max)
                .map(|hi| crate::runtime::builtins_collection::make_inclusive_range_pub(lo, hi))
        }))
    }
}

/// The sum of the Int range `start..end` (both inclusive; `None` past the
/// `i64` limits, which only an exclusive end at `i64::MIN` or start at
/// `i64::MAX` reaches, an empty range). `raw_end` is the range's stored end:
/// `i64::MAX` is an unbounded one (`1..*`), whose sum is `Inf` as in Rakudo.
/// Computed in big integers, so a range whose sum exceeds `i64` is exact.
// Cost: O(1) (fixed-width big-integer arithmetic on two `i64` bounds).
fn int_range_sum(start: Option<i64>, end: Option<i64>, raw_end: i64) -> Value {
    let (Some(start), Some(end)) = (start, end) else {
        return Value::int(0);
    };
    if raw_end == i64::MAX {
        return Value::num(f64::INFINITY);
    }
    if start > end {
        return Value::int(0);
    }
    let (start, end) = (NumBigInt::from(start), NumBigInt::from(end));
    let n = &end - &start + 1;
    Value::from_bigint((start + end) * n / 2)
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
            // A plain `Match` numifies through its text (`Match.Numeric` is
            // `self.Str.Numeric`): a non-numeric one makes the sum a Failure.
            if let Some(failure) = items
                .iter()
                .filter(|item| item.match_str_value().is_some())
                .find_map(crate::runtime::utils::str_numeric_operand_failure)
            {
                return Some(Ok(failure));
            }
            Some(
                items
                    .iter()
                    .cloned()
                    .try_fold(Value::int(0), crate::builtins::arith_add),
            )
        }
        // An Int range's sum is closed-form; an end of `i64::MAX` is `*`/`Inf`.
        ValueView::Range(a, b) => Some(Ok(int_range_sum(Some(a), Some(b), b))),
        ValueView::RangeExcl(a, b) => Some(Ok(int_range_sum(Some(a), b.checked_sub(1), b))),
        ValueView::RangeExclStart(a, b) => Some(Ok(int_range_sum(a.checked_add(1), Some(b), b))),
        ValueView::RangeExclBoth(a, b) => {
            Some(Ok(int_range_sum(a.checked_add(1), b.checked_sub(1), b)))
        }
        ValueView::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
            // An unbounded range (`1..^Inf`, `^Inf`) sums to `Inf`, as `1..*`.
            if crate::builtins::is_infinite_range(target)
                && match start.view() {
                    ValueView::Int(_) | ValueView::BigInt(_) | ValueView::Rat(..) => true,
                    ValueView::Num(n) => n.is_finite(),
                    _ => false,
                }
            {
                return Some(Ok(Value::num(f64::INFINITY)));
            }
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
    // A lazy or infinite receiver has no end to enumerate (`X::Cannot::Lazy`).
    if let Some(err) = crate::runtime::Interpreter::lazy_guard_error("permutations", target) {
        return Some(Err(err));
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
            deleted_indices: Default::default(),
            descriptor: None,
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
    // A lazy or infinite receiver has no end to enumerate (`X::Cannot::Lazy`).
    if let Some(err) = crate::runtime::Interpreter::lazy_guard_error("combinations", target) {
        return Some(Err(err));
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
    // A `$k` read out of a variable is an itemized Range or Int.
    let arg = arg.descalarize();
    // `self.list.combinations` counts the list first: a lazy one throws
    // (`(1..*).combinations(2)` is `X::Cannot::Lazy`), never hangs.
    if crate::builtins::methods_0arg::is_lazy_count_source(target)
        || crate::builtins::methods_0arg::is_infinite_range(target)
    {
        return Some(Ok(crate::runtime::utils::cannot_lazy_failure("elems")));
    }
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
