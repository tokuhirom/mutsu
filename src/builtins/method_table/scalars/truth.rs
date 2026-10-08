//! `Bool`'s rows and `Uni.Bool` (ADR-11276 §10, slice 3A proof rows for the
//! `Bool` and `Uni` shapes; slice 3B moves the rest of their methods).

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Pure($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

pub(super) static BOOL_ROWS: &[MethodRow] = &[
    row!("Bool", "Bool", truthiness),
    row!("Bool", "key", bool_key),
    row!("Bool", "value", bool_value),
    row!("Bool", "Int", bool_value),
    row!("Bool", "Numeric", bool_value),
    row!("Bool", "Real", bool_value),
];

pub(super) static UNI_ROWS: &[MethodRow] = &[row!("Uni", "Bool", truthiness)];

/// `.Bool`: whether the value is true, by the one truthiness rule
/// (`Value::truthy`). A `Uni` is true when it has a codepoint.
// Cost: O(1).
pub(crate) fn truthiness(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::truth(target.truthy()))
}

/// `Bool.key`: the enum value's name.
// Cost: O(1).
pub(crate) fn bool_key(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Bool(b) => Ok(Value::str_from(if b { "True" } else { "False" })),
        _ => Err(RuntimeError::new("Bool.key: receiver is not a Bool")),
    }
}

/// `Bool.value`: the enum value's integer, 1 for `True` and 0 for `False`.
// Cost: O(1).
pub(crate) fn bool_value(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Bool(b) => Ok(Value::int(i64::from(b))),
        _ => Err(RuntimeError::new("Bool.value: receiver is not a Bool")),
    }
}
