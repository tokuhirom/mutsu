//! `Var::PositionalCapture.sigil` and `Var::NamedCapture.sigil`: a match
//! capture variable has no sigil of its own in rakudo, so both answer the
//! empty string; this file registers the rows, which
//! the measured accessor tables do not carry.

use super::super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value};

// Cost: O(1).
fn sigil(_target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::str_from(""))
}

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "RakuAST::Var::PositionalCapture",
        name: "sigil",
        arity: 0,
        handler: Handler::Pure(sigil),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Var::NamedCapture",
        name: "sigil",
        arity: 0,
        handler: Handler::Pure(sigil),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
];
