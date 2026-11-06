//! `properties`, `reducer-name` and `default-operator-properties` of the
//! RakuAST operator nodes, run by [`crate::rakuast::operator_props`]; this
//! file only registers the rows.

use super::super::{Handler, MethodRow, RowFlags};
use super::rakuast_visit::native_node;
use crate::rakuast::operator_props;
use crate::value::{RuntimeError, Value, ValueView};

// Cost: O(d log n), d = nesting of metaoperators, n = built-in operator count.
fn properties(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => operator_props::properties(node),
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

// Cost: O(d log n), d = nesting of metaoperators, n = built-in operator count.
fn reducer_name(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => operator_props::reducer_name(node),
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

macro_rules! row {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Pure($handler),
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
}

macro_rules! rows_for {
    ($($owner:literal),* $(,)?) => {
        &[$(
            row!($owner, "properties", properties),
            row!($owner, "reducer-name", reducer_name),
        )*]
    };
}

pub(super) static ROWS: &[MethodRow] = rows_for![
    "RakuAST::Infix",
    "RakuAST::Prefix",
    // (`model_extension`: rakudo declares both on the `RakuAST::OperatorProperties`
    // role these four compose, not on the classes.)
    "RakuAST::Postfix",
    "RakuAST::Postcircumfix::ArrayIndex",
    "RakuAST::Postcircumfix::HashIndex",
    "RakuAST::Postcircumfix::LiteralHashIndex",
    "RakuAST::DottyInfix::Call",
    "RakuAST::FunctionInfix",
    "RakuAST::MetaInfix::Assign",
    "RakuAST::MetaInfix::Negate",
    "RakuAST::MetaInfix::Reverse",
    "RakuAST::MetaInfix::Cross",
    "RakuAST::MetaInfix::Zip",
    "RakuAST::MetaInfix::Hyper",
];

pub(super) static DEFAULT_ROWS: &[MethodRow] = &[
    row!("RakuAST::Infix", "default-operator-properties", properties),
    row!("RakuAST::Prefix", "default-operator-properties", properties),
    row!(
        "RakuAST::Postfix",
        "default-operator-properties",
        properties
    ),
    row!(
        "RakuAST::Postcircumfix::ArrayIndex",
        "default-operator-properties",
        properties
    ),
    row!(
        "RakuAST::Postcircumfix::HashIndex",
        "default-operator-properties",
        properties
    ),
    row!(
        "RakuAST::Postcircumfix::LiteralHashIndex",
        "default-operator-properties",
        properties
    ),
    row!(
        "RakuAST::DottyInfix::Call",
        "default-operator-properties",
        properties
    ),
];
