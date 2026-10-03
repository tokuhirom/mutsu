//! `Map`'s rows (`Hash` inherits them through its MRO).

use super::{Handler, MethodRow};
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Map",
        name: "elems",
        arity: 0,
        handler: Handler::Pure(elems),
    },
    MethodRow {
        owner: "Map",
        name: "Bool",
        arity: 0,
        handler: Handler::Pure(bool),
    },
];

// Cost: O(1), the map's length.
fn elems(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Hash(items) => Ok(Value::int(items.len() as i64)),
        _ => Err(RuntimeError::new("Map.elems: receiver is not a Map")),
    }
}

// Cost: O(1), an emptiness test.
fn bool(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::truth(target.truthy()))
}
