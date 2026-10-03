//! `List`'s rows (`Array` inherits them through its MRO).

use super::{Handler, MethodRow};
use crate::value::{RuntimeError, Value};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "List",
        name: "elems",
        arity: 0,
        handler: Handler::Pure(elems),
    },
    MethodRow {
        owner: "List",
        name: "end",
        arity: 0,
        handler: Handler::Pure(end),
    },
    MethodRow {
        owner: "List",
        name: "Bool",
        arity: 0,
        handler: Handler::Pure(bool),
    },
];

fn len(target: &Value) -> i64 {
    target.as_list_items().map_or(0, |items| items.len() as i64)
}

// Cost: O(1), a length read on the reified items.
fn elems(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::int(len(target)))
}

// Cost: O(1), a length read on the reified items.
fn end(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::int(len(target) - 1))
}

// Cost: O(1), an emptiness test.
fn bool(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::truth(target.truthy()))
}
