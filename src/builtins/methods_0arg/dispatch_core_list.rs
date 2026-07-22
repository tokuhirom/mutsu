/// List/sequence operations: end, flat, sort, reverse, unique, repeated, floor,
/// ceiling, round, truncate, narrow, sqrt
use crate::value::{RuntimeError, Value, ValueView};
use num_traits::{Signed, Zero};

use super::{is_infinite_range, raku_round, raku_round_to_value};

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    match method {
        "end" => {
            // A lazy (infinite-backed) array/list has no last index; raku throws
            // `X::Cannot::Lazy` (`Cannot .elems a lazy list`) rather than
            // returning the capped backing's last index.
            if super::is_lazy_count_source(target) {
                return Some(super::range_elems_lazy_failure("elems"));
            }
            if let Some(items) = target.as_list_items() {
                return Some(Some(Ok(Value::int(items.len() as i64 - 1))));
            }
            Some(match target.view() {
                ValueView::Hash(items) => Some(Ok(Value::int(items.len() as i64 - 1))),
                ValueView::Set(items, _) => Some(Ok(Value::int(items.len() as i64 - 1))),
                ValueView::Bag(items, _) => Some(Ok(Value::int(items.len() as i64 - 1))),
                ValueView::Mix(items, _) => Some(Ok(Value::int(items.len() as i64 - 1))),
                ValueView::Junction { values, .. } => Some(Ok(Value::int(values.len() as i64 - 1))),
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
                    if let Some(ValueView::Array(bytes, ..)) =
                        attributes.as_map().get("bytes").map(Value::view)
                    {
                        Some(Ok(Value::int(bytes.len() as i64 - 1)))
                    } else {
                        Some(Ok(Value::int(-1)))
                    }
                }
                ValueView::LazyList(_) => None,
                _ => Some(Ok(Value::int(0))),
            })
        }
        "flat" => Some(match target.view() {
            ValueView::Array(_, crate::value::ArrayKind::Shaped) => {
                let leaves = crate::runtime::utils::shaped_array_leaves(target);
                Some(Ok(Value::seq(leaves)))
            }
            _ if is_infinite_range(target) => Some(Ok(target.clone())),
            ValueView::LazyList(_) => Some(Ok(target.clone())), // flat of a lazy list is still lazy
            _ => {
                // Single source of truth: delegate to `flat_val` (also used by
                // the `flat()` function) with List context (flatten_arrays =
                // true). A Seq/List of nested arrays then descends one level --
                // e.g. `(@a xx 4).flat` flattens its element arrays to match
                // raku -- while a top-level real Array still itemizes its `[..]`
                // children. The old per-method `flatten_deep_value` passed
                // `false` for Seq children and so left them un-flattened.
                // De-itemize the top-level receiver first: `$(1,2,3).flat`
                // un-itemizes to `(1,2,3)` and then flattens (Raku semantics);
                // nested itemized items stay single (handled by `flat_val`).
                let mut result = Vec::new();
                crate::builtins::flat_val(
                    &crate::builtins::deitemize_flat_operand(target),
                    &mut result,
                    true,
                );
                Some(Ok(Value::seq(result)))
            }
        }),
        "sort" => Some(match target.view() {
            ValueView::Array(items, kind) => {
                let mut sorted = if kind == crate::value::ArrayKind::Shaped
                    && items
                        .iter()
                        .any(|v| matches!(v.view(), ValueView::Array(..)))
                {
                    crate::runtime::utils::shaped_array_leaves(target)
                } else {
                    (**items).clone().items
                };
                sorted.sort_by(|a, b| crate::runtime::compare_values(a, b).cmp(&0));
                Some(Ok(Value::seq(sorted)))
            }
            _ => None,
        }),
        "reverse" => Some(match target.view() {
            ValueView::Array(items, kind) => {
                // Multi-dim shaped arrays cannot be reversed
                if kind == crate::value::ArrayKind::Shaped
                    && let Some(shape) = crate::runtime::utils::shaped_array_shape(target)
                    && shape.len() > 1
                {
                    return Some(Some(Err(
                        crate::value::RuntimeError::illegal_on_fixed_dimension_array("reverse"),
                    )));
                }
                let mut reversed = (**items).clone();
                reversed.reverse();
                // .reverse returns a Seq in Raku, not an Array
                Some(Ok(Value::seq(reversed.items)))
            }
            ValueView::Range(a, b)
            | ValueView::RangeExcl(a, b)
            | ValueView::RangeExclStart(a, b)
            | ValueView::RangeExclBoth(a, b) => {
                if b == i64::MAX || a == i64::MIN {
                    None
                } else {
                    let mut reversed = crate::runtime::utils::value_to_list(target);
                    reversed.reverse();
                    Some(Ok(Value::seq(reversed)))
                }
            }
            ValueView::GenericRange { end, .. } => {
                // Check if the range is empty or infinite from the end side.
                // For e.g. `1 .. -Inf`, the range is empty, reverse returns empty list.
                let end_is_neg_inf = matches!(end.as_ref().view(), ValueView::Num(n) if n.is_infinite() && n.is_sign_negative());
                if end_is_neg_inf {
                    // Empty range -- reverse is empty
                    return Some(Some(Ok(Value::seq(Vec::new()))));
                }
                // For finite generic ranges, expand and reverse
                let items = crate::runtime::utils::value_to_list(target);
                // If value_to_list returned just the range itself, fall through
                if items.len() == 1
                    && matches!(
                        items.first().map(Value::view),
                        Some(ValueView::GenericRange { .. })
                    )
                {
                    None
                } else {
                    let mut reversed = items;
                    reversed.reverse();
                    Some(Ok(Value::seq(reversed)))
                }
            }
            // `Any.reverse` is `self.list.reverse`, so a non-Iterable is a
            // one-element list and reverses to itself: `"abc".reverse` is
            // `("abc",).Seq`, NOT `"cba"` (that is `.flip`). The `reverse(...)`
            // *function* form already did this; the method form did not.
            ValueView::Str(_)
            | ValueView::Int(_)
            | ValueView::BigInt(_)
            | ValueView::Num(_)
            | ValueView::Bool(_)
            | ValueView::Rat(..)
            | ValueView::FatRat(..)
            | ValueView::BigRat(..)
            | ValueView::Complex(..)
            | ValueView::Pair(..)
            | ValueView::ValuePair(..) => Some(Ok(Value::seq(vec![target.clone()]))),
            ValueView::Seq(items) | ValueView::Slip(items) => {
                // A non-lazy Seq/Slip reverses its materialized elements. (Lazy
                // Seqs are deferred-materialized by the slow path before reaching
                // here, so `items` already holds the pulled values.)
                let mut reversed = items.to_vec();
                reversed.reverse();
                Some(Ok(Value::seq(reversed)))
            }
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
                let mut bytes = if let Some(ValueView::Array(items, ..)) =
                    attributes.as_map().get("bytes").map(Value::view)
                {
                    items.to_vec()
                } else {
                    Vec::new()
                };
                bytes.reverse();
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("bytes".to_string(), Value::array(bytes));
                Some(Ok(Value::make_instance(class_name, attrs)))
            }
            _ => None,
        }),
        "unique" => Some(match target.view() {
            ValueView::Array(items, ..) => {
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
                Some(Ok(Value::seq(result)))
            }
            ValueView::Seq(items) | ValueView::Slip(items) => {
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
                Some(Ok(Value::seq(result)))
            }
            ValueView::LazyList(_) => None,
            // Supply.unique is handled by native_supply
            ValueView::Instance { class_name, .. } if class_name == "Supply" => None,
            _ => Some(Ok(target.clone())),
        }),
        "repeated" => Some(match target.view() {
            ValueView::Array(items, ..) => {
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
                Some(Ok(Value::seq(result)))
            }
            ValueView::Seq(items) | ValueView::Slip(items) => {
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
                Some(Ok(Value::seq(result)))
            }
            ValueView::LazyList(_) => None,
            _ => Some(Ok(Value::seq(Vec::new()))),
        }),
        "floor" => Some(match target.view() {
            ValueView::Num(f) if f.is_nan() || f.is_infinite() => Some(Ok(Value::num(f))),
            ValueView::Num(f) => Some(Ok(Value::int(f.floor() as i64))),
            ValueView::Int(i) => Some(Ok(Value::int(i))),
            ValueView::BigInt(_) => Some(Ok(target.clone())),
            ValueView::Rat(n, d) if d != 0 => {
                let q = n / d;
                let r = n % d;
                if r != 0 && (n < 0) != (d < 0) {
                    Some(Ok(Value::int(q - 1)))
                } else {
                    Some(Ok(Value::int(q)))
                }
            }
            ValueView::BigRat(n, d) if !d.is_zero() => {
                use num_integer::Integer;
                let (q, r) = n.div_rem(d);
                if !r.is_zero() && n.is_negative() != d.is_negative() {
                    Some(Ok(Value::bigint(q - 1)))
                } else {
                    Some(Ok(Value::bigint(q)))
                }
            }
            ValueView::FatRat(n, d) if d != 0 => {
                let q = n / d;
                let r = n % d;
                if r != 0 && (n < 0) != (d < 0) {
                    Some(Ok(Value::int(q - 1)))
                } else {
                    Some(Ok(Value::int(q)))
                }
            }
            ValueView::Complex(re, im) => Some(Ok(Value::complex(re.floor(), im.floor()))),
            ValueView::Rat(_, 0) => Some(Ok(RuntimeError::divide_by_zero_failure_for_method(
                "floor", "Rational",
            ))),
            ValueView::FatRat(_, 0) => Some(Ok(RuntimeError::divide_by_zero_failure_for_method(
                "floor", "Rational",
            ))),
            ValueView::BigRat(_, d) if d.is_zero() => Some(Ok(
                RuntimeError::divide_by_zero_failure_for_method("floor", "Rational"),
            )),
            _ => None,
        }),
        "ceiling" | "ceil" => Some(match target.view() {
            ValueView::Num(f) if f.is_nan() || f.is_infinite() => Some(Ok(Value::num(f))),
            ValueView::Num(f) => Some(Ok(Value::int(f.ceil() as i64))),
            ValueView::Int(i) => Some(Ok(Value::int(i))),
            ValueView::BigInt(_) => Some(Ok(target.clone())),
            ValueView::Rat(n, d) if d != 0 => {
                let q = n / d;
                let r = n % d;
                if r != 0 && (n < 0) == (d < 0) {
                    Some(Ok(Value::int(q + 1)))
                } else {
                    Some(Ok(Value::int(q)))
                }
            }
            ValueView::BigRat(n, d) if !d.is_zero() => {
                use num_integer::Integer;
                let (q, r) = n.div_rem(d);
                if !r.is_zero() && n.is_negative() == d.is_negative() {
                    Some(Ok(Value::bigint(q + 1)))
                } else {
                    Some(Ok(Value::bigint(q)))
                }
            }
            ValueView::FatRat(n, d) if d != 0 => {
                let q = n / d;
                let r = n % d;
                if r != 0 && (n < 0) == (d < 0) {
                    Some(Ok(Value::int(q + 1)))
                } else {
                    Some(Ok(Value::int(q)))
                }
            }
            ValueView::Complex(re, im) => Some(Ok(Value::complex(re.ceil(), im.ceil()))),
            ValueView::Rat(_, 0) => Some(Ok(RuntimeError::divide_by_zero_failure_for_method(
                "ceiling", "Rational",
            ))),
            ValueView::FatRat(_, 0) => Some(Ok(RuntimeError::divide_by_zero_failure_for_method(
                "ceiling", "Rational",
            ))),
            ValueView::BigRat(_, d) if d.is_zero() => Some(Ok(
                RuntimeError::divide_by_zero_failure_for_method("ceiling", "Rational"),
            )),
            _ => None,
        }),
        "round" => Some(match target.view() {
            ValueView::Num(f) if f.is_nan() || f.is_infinite() => Some(Ok(Value::num(f))),
            ValueView::Num(f) => Some(Ok(raku_round_to_value(f))),
            ValueView::Int(i) => Some(Ok(Value::int(i))),
            ValueView::BigInt(_) => Some(Ok(target.clone())),
            ValueView::Rat(n, d) if d != 0 => {
                let f = n as f64 / d as f64;
                Some(Ok(raku_round_to_value(f)))
            }
            ValueView::BigRat(n, d) if !d.is_zero() => {
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
            ValueView::FatRat(n, d) if d != 0 => {
                let f = n as f64 / d as f64;
                Some(Ok(raku_round_to_value(f)))
            }
            ValueView::Complex(re, im) => Some(Ok(Value::complex(raku_round(re), raku_round(im)))),
            ValueView::Rat(_, 0) => Some(Ok(RuntimeError::divide_by_zero_failure_for_method(
                "round", "Rational",
            ))),
            ValueView::FatRat(_, 0) => Some(Ok(RuntimeError::divide_by_zero_failure_for_method(
                "round", "Rational",
            ))),
            ValueView::BigRat(_, d) if d.is_zero() => Some(Ok(
                RuntimeError::divide_by_zero_failure_for_method("round", "Rational"),
            )),
            _ => None,
        }),
        "truncate" => Some(match target.view() {
            ValueView::Num(f) if f.is_nan() || f.is_infinite() => Some(Ok(Value::num(f))),
            ValueView::Num(f) => Some(Ok(Value::int(f.trunc() as i64))),
            ValueView::Int(i) => Some(Ok(Value::int(i))),
            ValueView::BigInt(_) => Some(Ok(target.clone())),
            ValueView::Rat(n, d) if d != 0 => Some(Ok(Value::int(n / d))),
            ValueView::BigRat(n, d) if !d.is_zero() => Some(Ok(Value::bigint(n / d))),
            ValueView::FatRat(n, d) if d != 0 => Some(Ok(Value::int(n / d))),
            ValueView::Complex(re, im) => Some(Ok(Value::complex(re.trunc(), im.trunc()))),
            ValueView::Rat(_, 0) => Some(Ok(RuntimeError::divide_by_zero_failure_for_method(
                "truncate", "Rational",
            ))),
            ValueView::FatRat(_, 0) => Some(Ok(RuntimeError::divide_by_zero_failure_for_method(
                "truncate", "Rational",
            ))),
            ValueView::BigRat(_, d) if d.is_zero() => Some(Ok(
                RuntimeError::divide_by_zero_failure_for_method("truncate", "Rational"),
            )),
            _ => None,
        }),
        "narrow" => Some(match target.view() {
            ValueView::Int(i) => Some(Ok(Value::int(i))),
            ValueView::Rat(n, d) if d != 0 && n % d == 0 => Some(Ok(Value::int(n / d))),
            ValueView::Rat(n, d) => Some(Ok(Value::rat_raw(n, d))),
            ValueView::Num(f) if f.is_finite() => {
                // Use tolerance (1e-15) to check if approximately an integer
                let rounded = f.round();
                let tol = 1e-15;
                let diff = (f - rounded).abs();
                let max = f.abs().max(rounded.abs());
                let approx_int = if max == 0.0 { true } else { diff / max <= tol };
                if approx_int {
                    Some(Ok(Value::int(rounded as i64)))
                } else {
                    Some(Ok(Value::num(f)))
                }
            }
            ValueView::Num(f) => Some(Ok(Value::num(f))),
            ValueView::Complex(re, im) => {
                // Check if imaginary part is approximately zero
                let tol = 1e-15;
                let im_approx_zero = if im == 0.0 {
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
                        let diff = (re - rounded).abs();
                        let max = re.abs().max(rounded.abs());
                        if max == 0.0 { true } else { diff / max <= tol }
                    } else {
                        false
                    };
                    if re_approx_int {
                        Some(Ok(Value::int(rounded as i64)))
                    } else {
                        Some(Ok(Value::num(re)))
                    }
                } else {
                    // Check if real part is approximately zero
                    let re_approx_zero = if re == 0.0 {
                        true
                    } else {
                        let max_mag = re.abs().max(im.abs());
                        if max_mag == 0.0 {
                            true
                        } else {
                            re.abs() / max_mag <= tol
                        }
                    };
                    let new_re = if re_approx_zero { 0.0 } else { re };
                    let new_im = im;
                    Some(Ok(Value::complex(new_re, new_im)))
                }
            }
            _ => Some(Ok(target.clone())),
        }),
        "sqrt" => Some(match target.view() {
            ValueView::Int(i) => Some(Ok(Value::num((i as f64).sqrt()))),
            ValueView::Num(f) => Some(Ok(Value::num(f.sqrt()))),
            ValueView::Rat(n, d) if d != 0 => Some(Ok(Value::num((n as f64 / d as f64).sqrt()))),
            ValueView::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt();
                let re = ((mag + r) / 2.0).sqrt();
                let im = i.signum() * ((mag - r) / 2.0).sqrt();
                Some(Ok(Value::complex(re, im)))
            }
            _ => None,
        }),
        _ => None,
    }
}
