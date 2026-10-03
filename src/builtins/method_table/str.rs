//! `Str`'s rows.

use super::{Handler, MethodRow};
use crate::value::{RuntimeError, Value};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Str",
        name: "chars",
        arity: 0,
        handler: Handler::Pure(chars),
    },
    MethodRow {
        owner: "Str",
        name: "Bool",
        arity: 0,
        handler: Handler::Pure(bool),
    },
];

// Cost: O(1) amortized: the grapheme count comes from the payload's cached
// index (built in O(n) on first use, `grapheme_index`).
fn chars(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::int(crate::builtins::str_prim::chars(target) as i64))
}

// Cost: O(1), an emptiness test.
fn bool(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::truth(target.truthy()))
}
