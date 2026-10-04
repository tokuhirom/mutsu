/// List/sequence operations: end, flat, sort, reverse, unique, repeated, floor,
/// ceiling, round, truncate, narrow, sqrt
use crate::value::value_buf::{buf_elems_or_empty, buf_len_or_zero, make_buf};
use crate::value::{RuntimeError, Value, ValueView};

use super::is_infinite_range;

/// `unique` and `repeated` share one pass over the input, keeping the values
/// already seen in a [`crate::runtime::IdentityIndex`] so the duplicate test does not rescan
/// them all: both were O(n^2) here, which is why `(^160_000).unique` never
/// finished. The three container shapes (Array, Seq, Slip) differ only in how
/// the items are reached, so they iterate through one helper rather than three
/// copies of the loop.
fn unique_seq<'a>(items: impl Iterator<Item = &'a Value>) -> Value {
    let mut seen = crate::runtime::IdentityIndex::new();
    let mut result = Vec::new();
    for item in items {
        if !seen.contains(item) {
            seen.insert(item.clone());
            result.push(item.clone());
        }
    }
    Value::seq(result)
}

fn repeated_seq<'a>(items: impl Iterator<Item = &'a Value>) -> Value {
    let mut seen = crate::runtime::IdentityIndex::new();
    let mut result = Vec::new();
    for item in items {
        if seen.contains(item) {
            result.push(item.clone());
        } else {
            seen.insert(item.clone());
        }
    }
    Value::seq(result)
}

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    match method {
        // Cost: O(1) on a reified list/array, hash, set/bag/mix or buf (a length read).
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
                } if crate::runtime::utils::is_native_elems_class(&class_name.resolve()) => {
                    let len = buf_len_or_zero(&attributes);
                    Some(Ok(Value::int(len as i64 - 1)))
                }
                ValueView::LazyList(_) => None,
                // A buffer-backed instance of any class (upstream NativeCall's
                // `CArray[T]`, a mixin over `CArray`) counts its elements.
                _ if let Some((_, attributes)) = crate::value::value_buf::buf_target(target) => {
                    let len = buf_len_or_zero(&attributes);
                    Some(Ok(Value::int(len as i64 - 1)))
                }
                _ => Some(Ok(Value::int(0))),
            })
        }
        // Cost: O(1) per call on an Array or List (a lazy Seq, `ListGen::Flat`,
        // that flattens one element per pull), a LazyList or an infinite Range;
        // otherwise O(t), t = leaves reached through flattenable nesting.
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
                let operand = crate::builtins::deitemize_flat_operand(target);
                // An Array or List flattens lazily, one element per pull, through
                // the same `flat_val`: a real Array's itemized elements stay
                // single, a List's flatten in turn.
                if let ValueView::Array(
                    _,
                    kind @ (crate::value::ArrayKind::Array | crate::value::ArrayKind::List),
                ) = operand.view()
                {
                    let flatten_children = kind == crate::value::ArrayKind::List;
                    return Some(Some(Ok(Value::seq_list_gen(
                        crate::value::ListGen::flat(operand, flatten_children),
                        false,
                    ))));
                }
                let mut result = Vec::new();
                crate::builtins::flat_val(&operand, &mut result, true);
                Some(Ok(Value::seq(result)))
            }
        }),
        // Cost: O(e log e) comparisons, e = elements of the invocant (copied, then
        // sorted with `compare_values`).
        "sort" => Some(match target.view() {
            // An object element may stringify through a user `Str`, which
            // only the interpreter's dispatched `cmp` can call.
            ValueView::Array(items, _)
                if crate::runtime::utils::sort_needs_dispatched_cmp(items.iter()) =>
            {
                None
            }
            ValueView::Array(items, kind) => {
                let mut sorted = if kind == crate::value::ArrayKind::Shaped
                    && items
                        .iter()
                        .any(|v| matches!(v.view(), ValueView::Array(..)))
                {
                    crate::runtime::utils::shaped_array_leaves(target)
                } else {
                    (**items).clone().into_items()
                };
                sorted.sort_by(|a, b| crate::runtime::compare_values(a, b).cmp(&0));
                Some(Ok(Value::seq(sorted)))
            }
            _ => None,
        }),
        // Cost: O(e), e = elements of the invocant (copied in reverse; a finite Range
        // is expanded first).
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
                Some(Ok(Value::seq(reversed.into_items())))
            }
            ValueView::Range(a, b)
            | ValueView::RangeExcl(a, b)
            | ValueView::RangeExclStart(a, b)
            | ValueView::RangeExclBoth(a, b) => {
                if b == i64::MAX || a == i64::MIN {
                    Some(Err(RuntimeError::cannot_lazy("reverse")))
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
                if super::is_infinite_range(target) {
                    return Some(Some(Err(RuntimeError::cannot_lazy("reverse"))));
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
            ValueView::Seq(items) => {
                // A non-lazy Seq/Slip reverses its materialized elements. (Lazy
                // Seqs are deferred-materialized by the slow path before reaching
                // here, so `items` already holds the pulled values.)
                let mut reversed = items.to_vec();
                reversed.reverse();
                Some(Ok(Value::seq(reversed)))
            }
            ValueView::Slip(items) => {
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
                let mut bytes = buf_elems_or_empty(&attributes);
                bytes.reverse();
                Some(Ok(make_buf(class_name, bytes)))
            }
            _ => None,
        }),
        // Cost: O(e) average when every element is an Int/BigInt/Str/Bool/Num (hash
        // buckets in `IdentityIndex`); O(e * u) otherwise, e = elements, u = distinct
        // elements of any other kind (Rat, Pair, object, list, ...), which the index
        // cannot bucket and so compares against every candidate. Rakudo: O(e) (keyed
        // on `.WHICH`) -- see #9161.
        "unique" => Some(match target.view() {
            ValueView::Array(items, ..) => Some(Ok(unique_seq(items.iter()))),
            ValueView::Seq(items) => Some(Ok(unique_seq(items.iter()))),
            ValueView::Slip(items) => Some(Ok(unique_seq(items.iter()))),
            ValueView::LazyList(_) => None,
            // Supply.unique is handled by native_supply
            ValueView::Instance { class_name, .. } if class_name == "Supply" => None,
            _ => Some(Ok(target.clone())),
        }),
        // Cost: same as `unique`: O(e) average for Int/BigInt/Str/Bool/Num elements,
        // O(e * u) for any other kind. Rakudo: O(e) -- see #9161.
        "repeated" => Some(match target.view() {
            ValueView::Array(items, ..) => Some(Ok(repeated_seq(items.iter()))),
            ValueView::Seq(items) => Some(Ok(repeated_seq(items.iter()))),
            ValueView::Slip(items) => Some(Ok(repeated_seq(items.iter()))),
            ValueView::LazyList(_) => None,
            _ => Some(Ok(Value::seq(Vec::new()))),
        }),
        // The numeric types' rows' implementations (ADR-11276,
        // `method_table::real`); the cascade still reaches them for receivers
        // the table has no shape for.
        // Cost: O(1) for word-sized values; O(b) for big ones, b = size in bits.
        "floor" => Some(crate::builtins::method_table::real::floor_of(target)),
        // Cost: as `floor`.
        "ceiling" | "ceil" => Some(crate::builtins::method_table::real::ceiling_of(target)),
        // Cost: as `floor`.
        "round" => Some(crate::builtins::method_table::real::round_of(target)),
        // Cost: as `floor`.
        "truncate" => Some(crate::builtins::method_table::real::truncate_of(target)),
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
        // Cost: O(n) for list-like inputs, n = elements; O(1) for fixed-width
        // numeric inputs; O(d) for BigRat, d = numerator and denominator limbs.
        "sqrt" => {
            // List-like values numify to their element count before applying
            // the numeric square-root method (`(0, 1, 2, 3).sqrt == 2`).
            if let Some(items) = target.as_list_items() {
                return Some(Some(Ok(Value::num((items.len() as f64).sqrt()))));
            }
            Some(crate::builtins::arith::sqrt_numeric(target).map(Ok))
        }
        _ => None,
    }
}
