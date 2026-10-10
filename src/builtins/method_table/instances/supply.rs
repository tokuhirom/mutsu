//! `Supply.list` (ADR-11276 §9.55).
//!
//! A materialized `Supply` instance keeps its emitted values in the `values`
//! attribute; `.list` and `.Array` read them. A supply that has no values yet
//! (an on-demand `supply { ... }` block, a stream backed by a supplier or a
//! channel) declines, so the stateful slow path runs its body or drains it.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "Supply",
    name: "list",
    arity: 0,
    handler: Handler::Narrow(list),
    flags: RowFlags::OWNER_ONLY,
    named: &[],
}];

/// `Supply.list`.
// Cost: O(e), e = values emitted so far.
fn list(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    listify(target, false)
}

/// The values of a `Supply` as a List (`want_array` false) or a real Array, or
/// `None` when only the stateful path can produce them.
// Cost: O(e), e = values emitted so far.
pub(crate) fn listify(target: &Value, want_array: bool) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    // `.Array` builds a REAL Array, whose elements are `Scalar` containers, so
    // aggregates itemize on the way in; `.list` builds a List, which must not.
    let wrap = |items: Vec<Value>| {
        if want_array {
            crate::runtime::utils::itemize_real_array_elements(Value::real_array(items))
        } else {
            Value::array(items)
        }
    };
    // An on-demand supply has no materialized values; its body must be run by
    // the stateful slow path (`supply_list_values`).
    if attributes.as_map().contains_key("on_demand_callback") {
        return None;
    }
    // Likewise `.list` on a live, channel-backed supply (an `IO::Socket::Async`
    // read stream): its values arrive on a channel that only the stateful path
    // can drain, and answering the empty `values` attribute here would report
    // an open stream as an empty one.
    if !want_array {
        let attrs = attributes.as_map();
        if attrs.get("live").is_some_and(Value::truthy)
            && let Some(supplier_id) = attrs.get("supplier_id").and_then(Value::as_int)
            && supplier_id > 0
        {
            return Some(
                crate::runtime::native_methods::collect_supplier_values(
                    supplier_id as u64,
                    Vec::new(),
                    false,
                    true,
                )
                .map(wrap),
            );
        }
        if attrs.contains_key("supply_id") && !attrs.contains_key("proc_output") {
            return None;
        }
    }
    let items = match attributes.as_map().get("values").map(Value::view) {
        Some(ValueView::Array(items, ..)) => items.to_vec(),
        _ => Vec::new(),
    };
    Some(Ok(wrap(items)))
}
