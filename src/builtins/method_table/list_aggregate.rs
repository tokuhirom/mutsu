//! Aggregate and combinator methods shared by the method table and cascade.

use super::{Handler, MethodRow};
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
        }),*]
    };
}

pub(super) static ANY_ROWS: &[MethodRow] = rows!["Any":
    "minmax" => minmax,
    "sum" => sum,
];

pub(super) static LIST_ROWS: &[MethodRow] = rows!["List":
    "permutations" => permutations,
    "combinations" => combinations,
];

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
