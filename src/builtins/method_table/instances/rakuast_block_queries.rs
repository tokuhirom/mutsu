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

// Cost: O(f), f = fields of the node.
fn otherwise(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => Some(crate::rakuast::for_otherwise::otherwise(node)),
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

// Cost: O(f), f = fields of the node.
fn replace_otherwise(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target.rakuast_edit(|node| crate::rakuast::for_otherwise::replace_otherwise(node, &args[0]))
}

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "RakuAST::PlaceholderParameterOwner",
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
    // rakudo declares `replace-body` on `Sub` and `Method` too (`Submethod` inherits `Method`'s); the `Block`
    // row above answers them all (its handler takes every class with a body),
    // these rows only let `.^can` / `.^methods` report the method.
    MethodRow {
        owner: "RakuAST::Sub",
        name: "replace-body",
        arity: 1,
        handler: Handler::Narrow(replace_body),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Method",
        name: "replace-body",
        arity: 1,
        handler: Handler::Narrow(replace_body),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Statement::For",
        name: "otherwise",
        arity: 0,
        handler: Handler::Pure(otherwise),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Statement::For",
        name: "replace-otherwise",
        arity: 1,
        handler: Handler::Narrow(replace_otherwise),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
];
