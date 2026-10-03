//! `Complex`'s rows.

use super::{Handler, MethodRow};
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "Complex",
    name: "isNaN",
    arity: 0,
    handler: Handler::Pure(is_nan),
}];

/// `Complex.isNaN`: either part is NaN (`(NaN+5i).isNaN` is `True`).
// Cost: O(1).
fn is_nan(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Complex(re, im) => Ok(Value::truth(re.is_nan() || im.is_nan())),
        _ => Err(RuntimeError::new("Complex.isNaN: receiver is not a Complex")),
    }
}
