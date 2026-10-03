use crate::runtime;
use crate::value::{RuntimeError, Value, ValueView};
use num_traits::ToPrimitive;

/// The counted forms of Array/List head and tail, preserving mutable Array
/// element cells while leaving immutable List values unchanged.
// Cost: O(k) on Array/List, k = selected elements; O(e) otherwise,
// e = receiver elements materialized before selecting a window.
pub(super) fn dispatch(
    target: &Value,
    method: &str,
    arg: &Value,
) -> Option<Result<Value, RuntimeError>> {
    match method {
        // ADR-0040 slices 1-2: n-argument collection methods decompose their
        // own RECEIVER into ITS elements --
        // `.head`/`.tail`/`.combinations`/`.batch`/`.fmt`/`.pick`/`.roll`. That
        // is a question the itemization the receiver carries as an element of
        // some OTHER container has no say in (`[[2,3],[4,[5,6]]]».pick(*)` must
        // shuffle each inner array's own elements). Slice 1 drew the same
        // distinction for the 0-arg forms in `dispatch_core_range.rs`; this is
        // the n-arg half.
        // Cost: O(k), k = elements requested, on an Array, a List, a reified Seq or
        // an integer Range; O(e) on any other list-like, e = elements of the invocant
        // (decomposed into a Vec first).
        "head" => {
            let n: i64 = match arg.view() {
                ValueView::Int(i) => i,
                ValueView::Rat(num, den) => {
                    if den == 0 {
                        0
                    } else {
                        num / den
                    }
                }
                ValueView::Num(f) => f as i64,
                ValueView::BigInt(bi) => {
                    // For very large BigInts that don't fit in i64:
                    // negative => treat as negative (returns empty), positive => clamp to MAX
                    bi.to_i64()
                        .unwrap_or(if bi.sign() == num_bigint::Sign::Minus {
                            -1
                        } else {
                            i64::MAX
                        })
                }
                _ => return None,
            };
            if n <= 0 {
                return Some(Ok(Value::seq(vec![])));
            }
            let n = n as usize;
            match target.view() {
                ValueView::Array(items, kind) => {
                    let count = n.min(items.len());
                    let values = if kind.is_immutable_list() {
                        items[..count].to_vec()
                    } else {
                        (0..count)
                            .map(|i| {
                                target
                                    .array_slot_ref(i, true)
                                    .unwrap_or_else(|| items[i].clone())
                            })
                            .collect()
                    };
                    Some(Ok(Value::seq(values)))
                }
                ValueView::Range(a, b) => {
                    let items: Vec<Value> = (a..=b).take(n).map(Value::int).collect();
                    Some(Ok(Value::seq(items)))
                }
                // An unbounded range of any element type: step `n` times.
                _ if let Some(mut steps) = crate::runtime::unbounded_range::Steps::new(target) => {
                    Some(Ok(Value::seq(steps.take(n))))
                }
                _ => Some(Ok(Value::seq(runtime::with_receiver_items(
                    target,
                    |items| items[..n.min(items.len())].to_vec(),
                )))),
            }
        }
        // Cost: O(k), k = elements requested, on an Array, a List or a reified Seq;
        // O(e) otherwise, e = elements of the invocant (decomposed into a Vec first).
        "tail" => match target.view() {
            ValueView::Array(items, kind) => {
                let n = match arg.view() {
                    ValueView::Int(i) => i as usize,
                    _ => return None,
                };
                let start = items.len().saturating_sub(n);
                let values = if kind.is_immutable_list() {
                    items[start..].to_vec()
                } else {
                    (start..items.len())
                        .map(|i| {
                            target
                                .array_slot_ref(i, true)
                                .unwrap_or_else(|| items[i].clone())
                        })
                        .collect()
                };
                Some(Ok(Value::seq(values)))
            }
            ValueView::Instance { class_name, .. } if class_name == "Supply" => None,
            _ => {
                let n = match arg.view() {
                    ValueView::Int(i) => i as usize,
                    _ => return None,
                };
                Some(Ok(Value::seq(runtime::with_receiver_items(
                    target,
                    |items| items[items.len().saturating_sub(n)..].to_vec(),
                ))))
            }
        },
        _ => None,
    }
}
