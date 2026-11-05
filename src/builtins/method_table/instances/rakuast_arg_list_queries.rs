//! `RakuAST::ArgList`'s `arity`, `has-args`, `invocant` and `arg-at-pos`, answered by
//! [`crate::rakuast::arg_list_queries`]; this file only registers the rows.

use super::super::{Handler, MethodRow, RowFlags};
use super::rakuast_visit::native_node;
use crate::value::{RuntimeError, Value, ValueView};

// Cost: O(a), a = arguments of the list.
fn arity(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(with_node(target, crate::rakuast::arg_list_queries::arity))
}

// Cost: O(a), a = arguments of the list.
fn has_args(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(with_node(
        target,
        crate::rakuast::arg_list_queries::has_args,
    ))
}

// Cost: O(a), a = arguments of the list.
fn arg_at_pos(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    let Some(ValueView::Int(index)) = args.first().map(Value::view) else {
        return Err(RuntimeError::new(
            "Type check failed in binding to parameter '$pos'; expected Int",
        ));
    };
    Ok(with_node(target, |node| {
        crate::rakuast::arg_list_queries::arg_at_pos(node, index)
    }))
}

// Cost: O(1).
fn invocant(_target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(crate::rakuast::arg_list_queries::invocant())
}

fn with_node(target: &Value, f: impl FnOnce(&crate::rakuast::RakuAstNode) -> Value) -> Value {
    native_node(target)
        .and_then(|base| match base.view() {
            ValueView::RakuAst(node) => Some(f(node)),
            _ => None,
        })
        .unwrap_or(Value::NIL)
}

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "RakuAST::ArgList",
        name: "arity",
        arity: 0,
        handler: Handler::Pure(arity),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::ArgList",
        name: "has-args",
        arity: 0,
        handler: Handler::Pure(has_args),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::ArgList",
        name: "invocant",
        arity: 0,
        handler: Handler::Pure(invocant),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::ArgList",
        name: "arg-at-pos",
        arity: 1,
        handler: Handler::Pure(arg_at_pos),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
];
