//! Shared stepping logic for the index-advancing `Iterator` protocol methods
//! (`pull-one`, `push-exactly`, `push-at-least`, `push-all`, `push-until-lazy`,
//! `sink-all`, `skip-one`, `skip-at-least`, `skip-at-least-pull-one`) over a
//! built-in `Iterator` instance's materialized `items` prefix.
//!
//! Two dispatch paths consume this single implementation: the mutating path
//! (`methods_mut_dispatch.rs`, variable receiver — writes the advanced `index`
//! back into the instance) and the read-only path (`methods_call_dispatch.rs`,
//! temporary receiver — the cursor is discarded, but the `push-*` family still
//! mutates its array argument by identity).
//!
//! Spec: https://docs.raku.org/type/Iterator

use crate::value::{Value, ValueView};

pub(crate) struct IterProtoStep {
    /// The method's return value.
    pub ret: Value,
    /// The advanced cursor position.
    pub new_index: usize,
    /// Range of `items` the `push-*` family appends to the first array argument.
    pub append: Option<std::ops::Range<usize>>,
}

/// Compute the state transition for one Iterator-protocol method call.
/// Returns `None` for methods outside this family (caller falls through to
/// its generic dispatch).
pub(crate) fn step(
    method: &str,
    items: &[Value],
    index: usize,
    args: &[Value],
) -> Option<IterProtoStep> {
    let len = items.len();
    let end = || Value::str_from("IterationEnd");
    let arg_int = |v: Option<&Value>, default: i64| -> usize {
        v.map(crate::runtime::to_int).unwrap_or(default).max(0) as usize
    };
    let step = match method {
        "pull-one" => {
            if index < len {
                IterProtoStep {
                    ret: items[index].clone(),
                    new_index: index + 1,
                    append: None,
                }
            } else {
                IterProtoStep {
                    ret: end(),
                    new_index: index,
                    append: None,
                }
            }
        }
        // push-exactly(@target, n) / push-at-least(@target, n): append up to n
        // elements; return the count delivered, or IterationEnd when the
        // source ran dry before reaching n.
        "push-exactly" | "push-at-least" => {
            let want = arg_int(args.get(1), 1);
            let take = len.saturating_sub(index).min(want);
            IterProtoStep {
                ret: if take < want {
                    end()
                } else {
                    Value::int(take as i64)
                },
                new_index: index + take,
                append: (take > 0).then(|| index..index + take),
            }
        }
        "push-all" | "push-until-lazy" => IterProtoStep {
            ret: end(),
            new_index: len.max(index),
            append: (index < len).then_some(index..len),
        },
        "sink-all" => IterProtoStep {
            ret: end(),
            new_index: len.max(index),
            append: None,
        },
        "skip-one" => {
            if index < len {
                IterProtoStep {
                    ret: Value::int(1),
                    new_index: index + 1,
                    append: None,
                }
            } else {
                IterProtoStep {
                    ret: Value::int(0),
                    new_index: index,
                    append: None,
                }
            }
        }
        "skip-at-least" => {
            let want = arg_int(args.first(), 0);
            let available = len.saturating_sub(index);
            if available >= want {
                IterProtoStep {
                    ret: Value::int(1),
                    new_index: index + want,
                    append: None,
                }
            } else {
                IterProtoStep {
                    ret: Value::int(0),
                    new_index: len,
                    append: None,
                }
            }
        }
        "skip-at-least-pull-one" => {
            let want = arg_int(args.first(), 0);
            let available = len.saturating_sub(index);
            if available >= want && index + want < len {
                IterProtoStep {
                    ret: items[index + want].clone(),
                    new_index: index + want + 1,
                    append: None,
                }
            } else {
                IterProtoStep {
                    ret: end(),
                    new_index: len,
                    append: None,
                }
            }
        }
        _ => return None,
    };
    Some(step)
}

impl crate::Interpreter {
    /// Append `vals` to the array passed as the `push-*` family's first
    /// argument, updating every binding that shares the array's identity.
    pub(crate) fn iterator_append_to_array_arg(&mut self, args: &[Value], vals: &[Value]) {
        if vals.is_empty() {
            return;
        }
        if let Some(av) = args.first()
            && let ValueView::Array(existing, arr_kind) = av.view()
        {
            let mut next = existing.to_vec();
            next.extend(vals.iter().cloned());
            let updated_array = Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(next)),
                arr_kind,
            );
            self.overwrite_array_bindings_by_identity(&existing, updated_array);
        }
    }
}
