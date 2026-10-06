//! `Num`'s rows.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "Num",
    name: "isNaN",
    arity: 0,
    handler: Handler::Pure(is_nan),
    flags: RowFlags::NONE,
    named: &[],
}];

/// `Num.isNaN`.
// Cost: O(1).
fn is_nan(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Num(f) => Ok(Value::truth(f.is_nan())),
        _ => Err(RuntimeError::new("Num.isNaN: receiver is not a Num")),
    }
}
