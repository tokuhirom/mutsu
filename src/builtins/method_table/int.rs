//! `Int`'s rows.

use super::{Handler, MethodRow};
use crate::value::{RuntimeError, Value};

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "Int",
    name: "isNaN",
    arity: 0,
    handler: Handler::Pure(is_nan),
}];

/// `Int.isNaN`: an integer is never NaN.
// Cost: O(1).
fn is_nan(_target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::FALSE)
}
