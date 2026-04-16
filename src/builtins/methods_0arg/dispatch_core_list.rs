/// List/sequence operations: end, flat, sort, reverse, unique, repeated, floor,
/// ceiling, round, truncate, narrow, sqrt
use crate::value::{RuntimeError, Value};
use num_traits::{Signed, Zero};
use std::sync::Arc;

use super::{flatten_deep_value, is_infinite_range, raku_round, raku_round_to_value};

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    match method {
        "end" => {
            if let Some(items) = target.as_list_items() {
                return Some(Some(Ok(Value::Int(items.len() as i64 - 1))));
            }
            Some(match target {
                Value::Hash(items) => Some(Ok(Value::Int(items.len() as i64 - 1))),
                Value::Set(items, _) => Some(Ok(Value::Int(items.len() as i64 - 1))),
                Value::Bag(items, _) => Some(Ok(Value::Int(items.len() as i64 - 1))),
                Value::Mix(items, _) => Some(Ok(Value::Int(items.len() as i64 - 1))),
                Value::Junction { values, .. } => Some(Ok(Value::Int(values.len() as i64 - 1))),
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
                    if let Some(Value::Array(bytes, ..)) = attributes.get("bytes") {
                        Some(Ok(Value::Int(bytes.len() as i64 - 1)))
                    } else {
                        Some(Ok(Value::Int(-1)))
                    }
                }
                Value::LazyList(_) => None,
                _ => Some(Ok(Value::Int(0))),
            })
        }
        "flat" => Some(match target {
            Value::Array(items, kind) => {
                if *kind == crate::value::ArrayKind::Shaped {
                    let leaves = crate::runtime::utils::shaped_array_leaves(target);
                    return Some(Some(Ok(Value::Seq(Arc::new(leaves)))));
                }
                let mut result = Vec::new();
                for item in items.iter() {
                    flatten_deep_value(item, &mut result, false);
                }
                Some(Ok(Value::Seq(Arc::new(result))))
            }
            Value::Seq(items) | Value::Slip(items) => {
                let mut result = Vec::new();
                for item in items.iter() {
                    flatten_deep_value(item, &mut result, false);
                }
                Some(Ok(Value::Seq(Arc::new(result))))
            }
            other if is_infinite_range(other) => Some(Ok(other.clone())),
            Value::LazyList(_) => None, // fall through to runtime to force
            _ => {
                let mut result = Vec::new();
                flatten_deep_value(target, &mut result, false);
                Some(Ok(Value::Seq(Arc::new(result))))
            }
        }),
        "sort" => Some(match target {
            Value::Array(items, kind) => {
                let mut sorted = if *kind == crate::value::ArrayKind::Shaped
                    && items.iter().any(|v| matches!(v, Value::Array(..)))
                {
                    crate::runtime::utils::shaped_array_leaves(target)
                } else {
                    (**items).clone()
                };
                sorted.sort_by(|a, b| crate::runtime::compare_values(a, b).cmp(&0));
                Some(Ok(Value::array(sorted)))
            }
            _ => None,
        }),
        "reverse" => Some(match target {
            Value::Array(items, kind) => {
                // Multi-dim shaped arrays cannot be reversed
                if *kind == crate::value::ArrayKind::Shaped
                    && let Some(shape) = crate::runtime::utils::shaped_array_shape(target)
                    && shape.len() > 1
                {
                    return Some(Some(Err(
                        crate::value::RuntimeError::illegal_on_fixed_dimension_array("reverse"),
                    )));
                }
                let mut reversed = (**items).clone();
                reversed.reverse();
                // .reverse returns a List (Seq in Raku), not an Array
                Some(Ok(Value::Array(
                    std::sync::Arc::new(reversed),
                    crate::value::ArrayKind::List,
                )))
            }
            Value::Range(a, b)
            | Value::RangeExcl(a, b)
            | Value::RangeExclStart(a, b)
            | Value::RangeExclBoth(a, b) => {
                if *b == i64::MAX || *a == i64::MIN {
                    None
                } else {
                    let mut reversed = crate::runtime::utils::value_to_list(target);
                    reversed.reverse();
                    Some(Ok(Value::Array(
                        std::sync::Arc::new(reversed),
                        crate::value::ArrayKind::List,
                    )))
                }
            }
            Value::GenericRange { end, .. } => {
                // Check if the range is empty or infinite from the end side.
                // For e.g. `1 .. -Inf`, the range is empty, reverse returns empty list.
                let end_is_neg_inf = matches!(end.as_ref(), Value::Num(n) if n.is_infinite() && n.is_sign_negative());
                if end_is_neg_inf {
                    // Empty range -- reverse is empty
                    return Some(Some(Ok(Value::Array(
                        std::sync::Arc::new(Vec::new()),
                        crate::value::ArrayKind::List,
                    ))));
                }
                // For finite generic ranges, expand and reverse
                let items = crate::runtime::utils::value_to_list(target);
                // If value_to_list returned just the range itself, fall through
                if items.len() == 1 && matches!(items.first(), Some(Value::GenericRange { .. })) {
                    None
                } else {
                    let mut reversed = items;
                    reversed.reverse();
                    Some(Ok(Value::Array(
                        std::sync::Arc::new(reversed),
                        crate::value::ArrayKind::List,
                    )))
                }
            }
            Value::Str(s) => Some(Ok(Value::str(s.chars().rev().collect()))),
            Value::Instance {
                class_name,
                attributes,
                ..
            } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
                let mut bytes = if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                    items.to_vec()
                } else {
                    Vec::new()
                };
                bytes.reverse();
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("bytes".to_string(), Value::array(bytes));
                Some(Ok(Value::make_instance(*class_name, attrs)))
            }
            _ => None,
        }),
        "unique" => Some(match target {
            Value::Array(items, ..) => {
                let mut seen: Vec<Value> = Vec::new();
                let mut result = Vec::new();
                for item in items.iter() {
                    if !seen
                        .iter()
                        .any(|existing| crate::runtime::values_identical(existing, item))
                    {
                        seen.push(item.clone());
                        result.push(item.clone());
                    }
                }
                Some(Ok(Value::array(result)))
            }
            Value::Seq(items) | Value::Slip(items) => {
                let mut seen: Vec<Value> = Vec::new();
                let mut result = Vec::new();
                for item in items.iter() {
                    if !seen
                        .iter()
                        .any(|existing| crate::runtime::values_identical(existing, item))
                    {
                        seen.push(item.clone());
                        result.push(item.clone());
                    }
                }
                Some(Ok(Value::array(result)))
            }
            Value::LazyList(_) => None,
            // Supply.unique is handled by native_supply
            Value::Instance { class_name, .. } if class_name == "Supply" => None,
            _ => Some(Ok(target.clone())),
        }),
        "repeated" => Some(match target {
            Value::Array(items, ..) => {
                let mut seen: Vec<Value> = Vec::new();
                let mut result = Vec::new();
                for item in items.iter() {
                    if seen
                        .iter()
                        .any(|existing| crate::runtime::values_identical(existing, item))
                    {
                        result.push(item.clone());
                    } else {
                        seen.push(item.clone());
                    }
                }
                Some(Ok(Value::array(result)))
            }
            Value::Seq(items) | Value::Slip(items) => {
                let mut seen: Vec<Value> = Vec::new();
                let mut result = Vec::new();
                for item in items.iter() {
                    if seen
                        .iter()
                        .any(|existing| crate::runtime::values_identical(existing, item))
                    {
                        result.push(item.clone());
                    } else {
                        seen.push(item.clone());
                    }
                }
                Some(Ok(Value::array(result)))
            }
            Value::LazyList(_) => None,
            _ => Some(Ok(Value::array(Vec::new()))),
        }),
        "floor" => Some(match target {
            Value::Num(f) if f.is_nan() || f.is_infinite() => Some(Ok(Value::Num(*f))),
            Value::Num(f) => Some(Ok(Value::Int(f.floor() as i64))),
            Value::Int(i) => Some(Ok(Value::Int(*i))),
            Value::BigInt(_) => Some(Ok(target.clone())),
            Value::Rat(n, d) if *d != 0 => {
                let q = *n / *d;
                let r = *n % *d;
                if r != 0 && (*n < 0) != (*d < 0) {
                    Some(Ok(Value::Int(q - 1)))
                } else {
                    Some(Ok(Value::Int(q)))
                }
            }
            Value::BigRat(n, d) if !d.is_zero() => {
                use num_integer::Integer;
                let (q, r) = n.div_rem(d);
                if !r.is_zero() && n.is_negative() != d.is_negative() {
                    Some(Ok(Value::bigint(q - 1)))
                } else {
                    Some(Ok(Value::bigint(q)))
                }
            }
            Value::FatRat(n, d) if *d != 0 => {
                let q = *n / *d;
                let r = *n % *d;
                if r != 0 && (*n < 0) != (*d < 0) {
                    Some(Ok(Value::Int(q - 1)))
                } else {
                    Some(Ok(Value::Int(q)))
                }
            }
            Value::Complex(re, im) => Some(Ok(Value::Complex(re.floor(), im.floor()))),
            Value::Rat(_, d) if *d == 0 => Some(Ok(
                RuntimeError::divide_by_zero_failure_for_method("floor", "Rational"),
            )),
            Value::FatRat(_, d) if *d == 0 => Some(Ok(
                RuntimeError::divide_by_zero_failure_for_method("floor", "Rational"),
            )),
            Value::BigRat(_, d) if d.is_zero() => Some(Ok(
                RuntimeError::divide_by_zero_failure_for_method("floor", "Rational"),
            )),
            _ => None,
        }),
        "ceiling" | "ceil" => Some(match target {
            Value::Num(f) if f.is_nan() || f.is_infinite() => Some(Ok(Value::Num(*f))),
            Value::Num(f) => Some(Ok(Value::Int(f.ceil() as i64))),
            Value::Int(i) => Some(Ok(Value::Int(*i))),
            Value::BigInt(_) => Some(Ok(target.clone())),
            Value::Rat(n, d) if *d != 0 => {
                let q = *n / *d;
                let r = *n % *d;
                if r != 0 && (*n < 0) == (*d < 0) {
                    Some(Ok(Value::Int(q + 1)))
                } else {
                    Some(Ok(Value::Int(q)))
                }
            }
            Value::BigRat(n, d) if !d.is_zero() => {
                use num_integer::Integer;
                let (q, r) = n.div_rem(d);
                if !r.is_zero() && n.is_negative() == d.is_negative() {
                    Some(Ok(Value::bigint(q + 1)))
                } else {
                    Some(Ok(Value::bigint(q)))
                }
            }
            Value::FatRat(n, d) if *d != 0 => {
                let q = *n / *d;
                let r = *n % *d;
                if r != 0 && (*n < 0) == (*d < 0) {
                    Some(Ok(Value::Int(q + 1)))
                } else {
                    Some(Ok(Value::Int(q)))
                }
            }
            Value::Complex(re, im) => Some(Ok(Value::Complex(re.ceil(), im.ceil()))),
            Value::Rat(_, d) if *d == 0 => Some(Ok(
                RuntimeError::divide_by_zero_failure_for_method("ceiling", "Rational"),
            )),
            Value::FatRat(_, d) if *d == 0 => Some(Ok(
                RuntimeError::divide_by_zero_failure_for_method("ceiling", "Rational"),
            )),
            Value::BigRat(_, d) if d.is_zero() => Some(Ok(
                RuntimeError::divide_by_zero_failure_for_method("ceiling", "Rational"),
            )),
            _ => None,
        }),
        "round" => Some(match target {
            Value::Num(f) if f.is_nan() || f.is_infinite() => Some(Ok(Value::Num(*f))),
            Value::Num(f) => Some(Ok(raku_round_to_value(*f))),
            Value::Int(i) => Some(Ok(Value::Int(*i))),
            Value::BigInt(_) => Some(Ok(target.clone())),
            Value::Rat(n, d) if *d != 0 => {
                let f = *n as f64 / *d as f64;
                Some(Ok(raku_round_to_value(f)))
            }
            Value::BigRat(n, d) if !d.is_zero() => {
                use num_bigint::BigInt;
                use num_integer::Integer;
                // round = floor(x + 0.5) for Raku semantics
                // For BigRat: floor((2n + d) / 2d)
                let two_n: BigInt = n * 2;
                let two_d: BigInt = d * 2;
                let sum: BigInt = &two_n + d;
                let (q, r) = sum.div_rem(&two_d);
                if !r.is_zero() && sum.is_negative() != two_d.is_negative() {
                    Some(Ok(Value::bigint(q - 1)))
                } else {
                    Some(Ok(Value::bigint(q)))
                }
            }
            Value::FatRat(n, d) if *d != 0 => {
                let f = *n as f64 / *d as f64;
                Some(Ok(raku_round_to_value(f)))
            }
            Value::Complex(re, im) => Some(Ok(Value::Complex(raku_round(*re), raku_round(*im)))),
            Value::Rat(_, d) if *d == 0 => Some(Ok(
                RuntimeError::divide_by_zero_failure_for_method("round", "Rational"),
            )),
            Value::FatRat(_, d) if *d == 0 => Some(Ok(
                RuntimeError::divide_by_zero_failure_for_method("round", "Rational"),
            )),
            Value::BigRat(_, d) if d.is_zero() => Some(Ok(
                RuntimeError::divide_by_zero_failure_for_method("round", "Rational"),
            )),
            _ => None,
        }),
        "truncate" => Some(match target {
            Value::Num(f) if f.is_nan() || f.is_infinite() => Some(Ok(Value::Num(*f))),
            Value::Num(f) => Some(Ok(Value::Int(f.trunc() as i64))),
            Value::Int(i) => Some(Ok(Value::Int(*i))),
            Value::BigInt(_) => Some(Ok(target.clone())),
            Value::Rat(n, d) if *d != 0 => Some(Ok(Value::Int(*n / *d))),
            Value::BigRat(n, d) if !d.is_zero() => Some(Ok(Value::bigint(n / d))),
            Value::FatRat(n, d) if *d != 0 => Some(Ok(Value::Int(*n / *d))),
            Value::Complex(re, im) => Some(Ok(Value::Complex(re.trunc(), im.trunc()))),
            Value::Rat(_, d) if *d == 0 => Some(Ok(
                RuntimeError::divide_by_zero_failure_for_method("truncate", "Rational"),
            )),
            Value::FatRat(_, d) if *d == 0 => Some(Ok(
                RuntimeError::divide_by_zero_failure_for_method("truncate", "Rational"),
            )),
            Value::BigRat(_, d) if d.is_zero() => Some(Ok(
                RuntimeError::divide_by_zero_failure_for_method("truncate", "Rational"),
            )),
            _ => None,
        }),
        "narrow" => Some(match target {
            Value::Int(i) => Some(Ok(Value::Int(*i))),
            Value::Rat(n, d) if *d != 0 && *n % *d == 0 => Some(Ok(Value::Int(*n / *d))),
            Value::Rat(n, d) => Some(Ok(Value::Rat(*n, *d))),
            Value::Num(f) if f.is_finite() => {
                // Use tolerance (1e-15) to check if approximately an integer
                let rounded = f.round();
                let tol = 1e-15;
                let diff = (*f - rounded).abs();
                let max = f.abs().max(rounded.abs());
                let approx_int = if max == 0.0 { true } else { diff / max <= tol };
                if approx_int {
                    Some(Ok(Value::Int(rounded as i64)))
                } else {
                    Some(Ok(Value::Num(*f)))
                }
            }
            Value::Num(f) => Some(Ok(Value::Num(*f))),
            Value::Complex(re, im) => {
                // Check if imaginary part is approximately zero
                let tol = 1e-15;
                let im_approx_zero = if *im == 0.0 {
                    true
                } else {
                    let max_mag = re.abs().max(im.abs());
                    if max_mag == 0.0 {
                        true
                    } else {
                        im.abs() / max_mag <= tol
                    }
                };
                if im_approx_zero {
                    // Narrow to real part, then try narrowing that to Int
                    let rounded = re.round();
                    let re_approx_int = if re.is_finite() {
                        let diff = (*re - rounded).abs();
                        let max = re.abs().max(rounded.abs());
                        if max == 0.0 { true } else { diff / max <= tol }
                    } else {
                        false
                    };
                    if re_approx_int {
                        Some(Ok(Value::Int(rounded as i64)))
                    } else {
                        Some(Ok(Value::Num(*re)))
                    }
                } else {
                    // Check if real part is approximately zero
                    let re_approx_zero = if *re == 0.0 {
                        true
                    } else {
                        let max_mag = re.abs().max(im.abs());
                        if max_mag == 0.0 {
                            true
                        } else {
                            re.abs() / max_mag <= tol
                        }
                    };
                    let new_re = if re_approx_zero { 0.0 } else { *re };
                    let new_im = *im;
                    Some(Ok(Value::Complex(new_re, new_im)))
                }
            }
            _ => Some(Ok(target.clone())),
        }),
        "sqrt" => Some(match target {
            Value::Int(i) => Some(Ok(Value::Num((*i as f64).sqrt()))),
            Value::Num(f) => Some(Ok(Value::Num(f.sqrt()))),
            Value::Rat(n, d) if *d != 0 => Some(Ok(Value::Num((*n as f64 / *d as f64).sqrt()))),
            Value::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt();
                let re = ((mag + r) / 2.0).sqrt();
                let im = i.signum() * ((mag - r) / 2.0).sqrt();
                Some(Ok(Value::Complex(re, im)))
            }
            _ => None,
        }),
        _ => None,
    }
}
