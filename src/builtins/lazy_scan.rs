//! Driving a `LazyList` from the builtin layer: forcing a scan reduction
//! (`[\+] 1..*`) with the builtin arithmetic. It needs the operator and range
//! primitives above `Value`, so it lives here rather than as a `LazyList`
//! method (issue #10779).

use crate::value::{LazyList, Value, ValueView};

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
        "%" => crate::builtins::arith::arith_mod(left, right).unwrap_or(Value::NIL),
        "mod" => crate::builtins::arith::arith_mod_op(left, right).unwrap_or(Value::NIL),
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
