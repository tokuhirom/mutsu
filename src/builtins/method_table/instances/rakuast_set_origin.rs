//! `RakuAST::Node.set-origin($origin)`, answered by [`crate::rakuast::origin`];
//! this file only registers the row.

use super::super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value};

// Cost: O(1).
fn set_origin(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target.rakuast_edit(|node| crate::rakuast::origin::set_origin(node, &args[0]))
}

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "RakuAST::Node",
    name: "set-origin",
    arity: 1,
    handler: Handler::Narrow(set_origin),
    flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
    named: &[],
}];
