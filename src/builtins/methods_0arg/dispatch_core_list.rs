/// List/sequence operations: end, flat, sort, reverse, unique, repeated, floor,
/// ceiling, round, truncate, narrow, sqrt
use crate::value::value_buf::buf_len_or_zero;
use crate::value::{RuntimeError, Value, ValueView};

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    match method {
        // Cost: O(1) on a reified list/array, hash, set/bag/mix or buf (a length read).
        "end" => {
            if crate::builtins::method_table::any_collection::scalar_like(target) {
                return Some(Some(crate::builtins::method_table::any_collection::end(
                    target,
                    &[],
                )));
            }
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
        // The collection transformations' shared handlers are also the
        // method-table rows (ADR-11276).
        // Cost: O(1) per call on supported reified inputs; otherwise O(t),
        // t = leaves reached through flattenable nesting.
        "flat" => Some(crate::builtins::method_table::list_transform::flat(
            target,
            &[],
        )),
        // Cost: O(e log e) comparisons and O(e) copied values; e = elements.
        "sort" => Some(crate::builtins::method_table::list_transform::sort(
            target,
            &[],
        )),
        // Cost: O(e), e = elements passed to the shared List row handler.
        "reverse" => {
            if crate::builtins::method_table::any_collection::scalar_like(target) {
                Some(crate::builtins::method_table::any_collection::reverse(
                    target,
                    &[],
                ))
            } else {
                Some(crate::builtins::method_table::list::reverse(target, &[]))
            }
        }
        // Cost: O(e) average for bucketed values, O(e * u) otherwise;
        // e = elements, u = distinct values of kinds that require equality scans.
        "unique" => Some(crate::builtins::method_table::list_transform::unique(
            target,
            &[],
        )),
        // Cost: same as unique.
        "repeated" => Some(crate::builtins::method_table::list_transform::repeated(
            target,
            &[],
        )),
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
