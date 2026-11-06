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

/// `OperatorProperties.<category>($symbol)`: the symbol defaults to the empty
/// string, as rakudo's `Str $op = ""` does.
fn class_lookup(category: &str, args: &[Value]) -> Result<Value, RuntimeError> {
    let symbol = args.first().map(|v| v.to_string_value()).unwrap_or_default();
    Ok(operator_props::lookup(category, &symbol).unwrap_or(Value::NIL))
}

macro_rules! lookup_fn {
    ($fn_name:ident, $category:literal) => {
        // Cost: O(log n), n = built-in operator count.
        fn $fn_name(_target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
            class_lookup($category, args)
        }
    };
}

lookup_fn!(lookup_infix, "infix");
lookup_fn!(lookup_prefix, "prefix");
lookup_fn!(lookup_postfix, "postfix");
lookup_fn!(lookup_circumfix, "circumfix");
lookup_fn!(lookup_postcircumfix, "postcircumfix");

macro_rules! lookup_rows {
    ($(($name:literal, $handler:ident)),* $(,)?) => {
        &[$(
            MethodRow {
                owner: "OperatorProperties",
                name: $name,
                arity: 0,
                handler: Handler::Pure($handler),
                flags: RowFlags::OWNER_ONLY,
                named: &[],
            },
            MethodRow {
                owner: "OperatorProperties",
                name: $name,
                arity: 1,
                handler: Handler::Pure($handler),
                flags: RowFlags::OWNER_ONLY,
                named: &[],
            },
        )*]
    };
}

/// The class-level lookups of the prelude's `OperatorProperties`.
pub(super) static LOOKUP_ROWS: &[MethodRow] = lookup_rows![
    ("infix", lookup_infix),
    ("prefix", lookup_prefix),
    ("postfix", lookup_postfix),
    ("circumfix", lookup_circumfix),
    ("postcircumfix", lookup_postcircumfix),
];

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
