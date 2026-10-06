//! Zero-argument successor and predecessor rows. The same arithmetic helpers
//! answer the cascade and increment/decrement operators (ADR-0118).

use super::{Handler, MethodRow};
use crate::value::{RuntimeError, Value};

macro_rules! rows {
    ($owner:literal) => {
        &[
            MethodRow {
                owner: $owner,
                name: "succ",
                arity: 0,
                handler: Handler::Pure(succ),
            },
            MethodRow {
                owner: $owner,
                name: "pred",
                arity: 0,
                handler: Handler::Pure(pred),
            },
        ]
    };
}

pub(super) static STR_ROWS: &[MethodRow] = rows!("Str");
pub(super) static INT_ROWS: &[MethodRow] = rows!("Int");
pub(super) static NUM_ROWS: &[MethodRow] = rows!("Num");
pub(super) static RAT_ROWS: &[MethodRow] = rows!("Rat");
pub(super) static FAT_RAT_ROWS: &[MethodRow] = rows!("FatRat");
pub(super) static COMPLEX_ROWS: &[MethodRow] = rows!("Complex");

/// Successor for a built-in scalar. The cascade still uses this for receiver
/// shapes outside the row table.
// Cost: O(1) for word-sized numbers; O(n^2) for big numbers and O(n) for
// strings, n = the receiver's bit length or character count.
pub(crate) fn succ(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(crate::builtins::value_succ(target).unwrap_or_else(|| target.clone()))
}

/// Predecessor for a built-in scalar.
// Cost: O(1) for word-sized numbers; O(n^2) for big numbers and O(n) for
// strings, n = the receiver's bit length or character count.
pub(crate) fn pred(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(crate::builtins::value_pred(target).unwrap_or_else(|| target.clone()))
}
