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
        // `Seq.sqrt` and the other list-likes with no table shape: `Cool`'s
        // numeric method on the element count, through the `Cool.sqrt` row's
        // handler (`method_table::math`). A `List` or `Array` is answered by the
        // row itself.
        // Cost: O(1).
        "sqrt" if target.as_list_items().is_some() => {
            Some(Some(crate::builtins::method_table::math::sqrt(target, &[])))
        }
        _ => None,
    }
}
