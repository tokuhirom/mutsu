//! Driving a `LazyList` from the builtin layer: forcing a scan reduction
//! (`[\+] 1..*`) with the builtin arithmetic, and the zero-argument
//! `.pairs`/`.antipairs`/`.kv` index-pipe stage over a lazy invocant. Both
//! need the operator and range primitives above `Value`, so they live here
//! rather than as `LazyList` methods (issue #10779).

use crate::value::{IndexTransform, LazyList, Value, ValueView};

/// A zero-argument `.pairs`/`.antipairs`/`.kv` over a lazy invocant, as a
/// lazy index-pipe stage instead of a forced (possibly infinite) source;
/// `None` for any other method or a non-lazy invocant. The invocant is a
/// genuinely-lazy `LazyList` (also `needs_vm_lazy_dispatch` when
/// `vm_dispatch`), or an unbounded range of any element type (`1..*`,
/// `^Inf`, `1.5..*`, `"a"..*`), which Rakudo also reports `.is-lazy`
/// through these methods.
///
/// Cost: O(1) — builds the stage only; elements are pulled on demand.
pub(crate) fn index_pipe_method(target: &Value, method: &str, vm_dispatch: bool) -> Option<Value> {
    let transform = match method {
        "pairs" => IndexTransform::Pairs,
        "antipairs" => IndexTransform::AntiPairs,
        "kv" => IndexTransform::Kv,
        _ => return None,
    };
    let source = match target.view() {
        ValueView::LazyList(ll)
            if ll.is_genuinely_lazy() && (!vm_dispatch || ll.needs_vm_lazy_dispatch()) =>
        {
            target.clone()
        }
        _ if crate::runtime::unbounded_range::first(target).is_some() => target.clone(),
        _ => return None,
    };
    Some(Value::lazy_list(crate::gc::Gc::new(
        LazyList::new_index_pipe(source, transform),
    )))
}

/// Force a scan-based lazy list to compute up to `needed` elements.
/// Uses builtin arithmetic for common operators. Returns the cached elements.
/// This can be called from contexts without VM access (builtins, interpreter).
// Cost: O(k), k = elements still to compute (`needed` minus those cached).
pub(crate) fn force_scan_to(ll: &LazyList, needed: usize) -> Vec<Value> {
    let scan_mutex = match &ll.scan_spec {
        Some(s) => s,
        None => return ll.cache.lock().unwrap().clone().unwrap_or_default(),
    };

    let mut spec = scan_mutex.lock().unwrap();
    let mut cache_guard = ll.cache.lock().unwrap();
    let out = cache_guard.get_or_insert_with(Vec::new);

    if out.len() >= needed {
        return out[..needed].to_vec();
    }

    let remaining = needed - out.len();
    let already = spec.computed_count;
    let source = spec.source.clone();
    let base_op = spec.op.clone();
    let negate = spec.negate;

    // Generate source values
    let new_values: Vec<Value> = match source.view() {
        ValueView::Range(a, b) => {
            let start = a + already as i64;
            let end = if b == i64::MAX { a + needed as i64 } else { b };
            (start..=end).take(remaining).map(Value::int).collect()
        }
        ValueView::RangeExcl(a, b) => {
            let start = a + already as i64;
            let end = if b == i64::MAX { a + needed as i64 } else { b };
            (start..end).take(remaining).map(Value::int).collect()
        }
        _ => {
            let items = crate::runtime::utils::value_to_list(&source);
            items.into_iter().skip(already).take(remaining).collect()
        }
    };

    let mut acc = spec.accumulator.clone();
    for val in new_values {
        acc = Some(match acc.take() {
            None => {
                out.push(val.clone());
                val
            }
            Some(prev) => {
                let v = scan_binary_op(&base_op, prev, val);
                let v = if negate {
                    if v.truthy() {
                        Value::FALSE
                    } else {
                        Value::TRUE
                    }
                } else {
                    v
                };
                out.push(v.clone());
                v
            }
        });
        spec.computed_count += 1;
    }
    spec.accumulator = acc;
    out.clone()
}

/// Apply a binary operator for scan reduction. Supports common builtin ops.
fn scan_binary_op(op: &str, left: Value, right: Value) -> Value {
    match op {
        "+" => crate::builtins::arith::arith_add(left, right).unwrap_or(Value::NIL),
        "-" => crate::builtins::arith::arith_sub(left, right),
        "*" => crate::builtins::arith::arith_mul(left, right),
        "/" => crate::builtins::arith::arith_div(left, right).unwrap_or(Value::NIL),
        "%" | "mod" => crate::builtins::arith::arith_mod(left, right).unwrap_or(Value::NIL),
        "**" => crate::builtins::arith::arith_pow(left, right),
        "~" => Value::str(format!(
            "{}{}",
            left.to_string_value(),
            right.to_string_value()
        )),
        "max" => {
            if left.to_f64() >= right.to_f64() {
                left
            } else {
                right
            }
        }
        "min" => {
            if left.to_f64() <= right.to_f64() {
                left
            } else {
                right
            }
        }
        _ => Value::NIL, // Unsupported op — VM path handles these
    }
}
