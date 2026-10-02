//! The native payload of an instance of a user subclass of `Int` or `Num`
//! (`class MyInt is Int {}`), kept in a reserved attribute. Below the
//! builtins so `Value`'s own coercions read it without naming
//! `builtins::numeric_subclass` (#10779), which answers the native methods.

use crate::value::{InstanceAttrs, Value, ValueView};

/// The reserved attribute an `is Int` subclass instance keeps its payload in.
pub(crate) const INT_PAYLOAD: &str = "__mutsu_int_value";
/// The reserved attribute an `is Num` subclass instance keeps its payload in.
pub(crate) const NUM_PAYLOAD: &str = "__mutsu_num_value";

/// The native payload (an `Int` or a `Num`) of an instance's attribute map,
/// when the instance is of a user subclass of `Int` or `Num`.
// Cost: O(1) (at most two attribute-map lookups).
pub(crate) fn numeric_payload_of(attributes: &InstanceAttrs) -> Option<Value> {
    let map = attributes.as_map();
    map.get(INT_PAYLOAD)
        .or_else(|| map.get(NUM_PAYLOAD))
        .cloned()
}

/// The native payload of an instance of a user subclass of `Int` or `Num`.
// Cost: O(1) (at most two attribute-map lookups).
pub(crate) fn numeric_subclass_payload(target: &Value) -> Option<Value> {
    match target.view() {
        ValueView::Instance { attributes, .. } => numeric_payload_of(&attributes),
        _ => None,
    }
}
