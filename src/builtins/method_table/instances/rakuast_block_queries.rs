//! `RakuAST::Block`'s `has-placeholder-parameters`, answered by
//! [`crate::rakuast::block_queries`]; this file only registers the row.

use super::super::{Handler, MethodRow, RowFlags};
use super::rakuast_visit::native_node;
use crate::value::{RuntimeError, Value, ValueView};

// Cost: O(n), n = size of the block's own body.
fn has_placeholder_parameters(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => Some(
            crate::rakuast::block_queries::has_placeholder_parameters(node),
        ),
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "RakuAST::Block",
    name: "has-placeholder-parameters",
    arity: 0,
    handler: Handler::Pure(has_placeholder_parameters),
    flags: RowFlags::OWNER_ONLY,
    named: &[],
}];
