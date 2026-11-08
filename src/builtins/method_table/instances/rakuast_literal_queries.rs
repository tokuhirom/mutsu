//! `return-type`, `compile-time-value` and `has-compile-time-value` of `RakuAST::Literal` (the type of its value),
//! `RakuAST::QuotedString` and `RakuAST::QuoteWordsAtom` (`Str`), answered by
//! [`crate::rakuast::literal_queries`]; this file only registers the rows.

use super::super::{Handler, MethodRow, RowFlags};
use super::rakuast_visit::native_node;
use crate::value::{RuntimeError, Value, ValueView};

// Cost: O(1).
fn literal_return_type(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => {
            Some(crate::rakuast::literal_queries::literal_return_type(node))
        }
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

// Cost: O(1).
fn literal_compile_time_value(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => {
            Some(crate::rakuast::literal_queries::literal_compile_time_value(node))
        }
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

// Cost: O(1).
fn has_compile_time_value(_target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::truth(true))
}

// Cost: O(1).
fn string_return_type(_target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(crate::rakuast::literal_queries::string_return_type())
}

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "RakuAST::Literal",
        name: "return-type",
        arity: 0,
        handler: Handler::Pure(literal_return_type),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Literal",
        name: "compile-time-value",
        arity: 0,
        handler: Handler::Pure(literal_compile_time_value),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Literal",
        name: "has-compile-time-value",
        arity: 0,
        handler: Handler::Pure(has_compile_time_value),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::QuotedString",
        name: "return-type",
        arity: 0,
        handler: Handler::Pure(string_return_type),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::QuoteWordsAtom",
        name: "return-type",
        arity: 0,
        handler: Handler::Pure(string_return_type),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
];
