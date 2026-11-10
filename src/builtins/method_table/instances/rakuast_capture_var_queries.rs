//! `Var::PositionalCapture.sigil`: a match capture variable has no sigil of
//! its own in rakudo, so it answers the empty string. This file registers the
//! row, which the measured accessor tables do not carry (`Var::NamedCapture`'s
//! is generated).

use super::super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value};

// Cost: O(1).
fn sigil(_target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::str_from(""))
}

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "RakuAST::Var::PositionalCapture",
    name: "sigil",
    arity: 0,
    handler: Handler::Pure(sigil),
    flags: RowFlags::OWNER_ONLY,
    named: &[],
}];
