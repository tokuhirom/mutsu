/// `sqrt` of the list-likes with no table shape. `end`, `flat`, `sort`,
/// `reverse`, `unique` and `repeated` are rows of `Any` (ADR-11276 §9.80).
use crate::value::{RuntimeError, Value};

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    match method {
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
