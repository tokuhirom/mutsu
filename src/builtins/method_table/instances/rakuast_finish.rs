//! `CompUnit.finish-content`, answered by [`crate::rakuast::unit_finish`];
//! this file only registers the row.

use super::super::{Handler, MethodRow, RowFlags};
use super::rakuast_visit::native_node;
use crate::value::{RuntimeError, Value, ValueView};

// Cost: O(f), f = fields of the node.
fn finish_content(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => Some(crate::rakuast::unit_finish::read(node)),
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "RakuAST::CompUnit",
    name: "finish-content",
    arity: 0,
    handler: Handler::Pure(finish_content),
    flags: RowFlags::OWNER_ONLY,
    named: &[],
}];
