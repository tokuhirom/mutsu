//! `RakuAST::Node.to-parse-time` and `to-begin-time`: rakudo runs a node's parse
//! and begin phases against the resolver while it parses. mutsu resolves names
//! when it lowers the tree, so a node built mid-parse (a slang's
//! `$*BLOCK.to-parse-time($*R, $*CU.context)`) has nothing to do at that point
//! and the call answers the node, as rakudo's `to-parse-time` does.

use super::super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value};

// Cost: O(1).
fn phase_noop(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(target.clone())
}

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "RakuAST::Node",
        name: "to-parse-time",
        arity: 2,
        handler: Handler::Pure(phase_noop),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Node",
        name: "to-begin-time",
        arity: 2,
        handler: Handler::Pure(phase_noop),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
];
