//! `RakuAST::Block`'s `has-placeholder-parameters` and `replace-body`,
//! answered by [`crate::rakuast::block_queries`]; this file only registers the
//! rows.

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

// Cost: O(f), f = fields of the block.
fn bare_block(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => Some(crate::rakuast::block_queries::bare_block_flag(node)),
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

// Cost: O(f), f = fields of the node.
fn replace_body(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target.rakuast_replace_body(args[0].clone())
}

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "RakuAST::Block",
        name: "has-placeholder-parameters",
        arity: 0,
        handler: Handler::Pure(has_placeholder_parameters),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Block",
        name: "bare-block",
        arity: 0,
        handler: Handler::Pure(bare_block),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Block",
        name: "replace-body",
        arity: 1,
        handler: Handler::Narrow(replace_body),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
];
