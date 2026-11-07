//! The check-time problem lists of `RakuAST::Node`, answered by
//! [`crate::rakuast::diagnostics`]; this file only registers the rows.

use super::super::{Handler, MethodRow, RowFlags};
use super::rakuast_visit::native_node;
use crate::rakuast::diagnostics;
use crate::value::{RuntimeError, Value, ValueView};

// Cost: O(f + p), f = fields of the node, p = problems held.
fn sorries(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(read(target, |n| diagnostics::list(n, false)))
}

// Cost: O(f + p), f = fields of the node, p = problems held.
fn worries(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(read(target, |n| diagnostics::list(n, true)))
}

// Cost: O(f).
fn has_problems(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(read(target, |n| Value::truth(diagnostics::has_problems(n))))
}

fn read(target: &Value, f: impl FnOnce(&crate::rakuast::RakuAstNode) -> Value) -> Value {
    native_node(target)
        .and_then(|base| match base.view() {
            ValueView::RakuAst(node) => Some(f(node)),
            _ => None,
        })
        .unwrap_or(Value::NIL)
}

// Cost: O(f + p).
fn add_sorry(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target.rakuast_edit(|node| Ok(diagnostics::add(node, false, &args[0])))
}

// Cost: O(f + p).
fn add_worry(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target.rakuast_edit(|node| Ok(diagnostics::add(node, true, &args[0])))
}

// Cost: O(f).
fn clear_worries(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target.rakuast_edit(|node| Ok(diagnostics::clear_worries(node)))
}

// Cost: O(f + p).
fn promote(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target.rakuast_edit(|node| Ok(diagnostics::promote(node)))
}

const fn query(
    name: &'static str,
    f: fn(&Value, &[Value]) -> Result<Value, RuntimeError>,
) -> MethodRow {
    MethodRow {
        owner: "RakuAST::Node",
        name,
        arity: 0,
        handler: Handler::Pure(f),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    }
}

const fn edit(name: &'static str, arity: u8, f: super::super::NarrowFn) -> MethodRow {
    MethodRow {
        owner: "RakuAST::Node",
        name,
        arity,
        handler: Handler::Narrow(f),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    }
}

pub(super) static ROWS: &[MethodRow] = &[
    query("sorries", sorries),
    query("worries", worries),
    query("has-check-time-problems", has_problems),
    edit("add-sorry", 1, add_sorry),
    edit("add-worry", 1, add_worry),
    edit("clear-worries", 0, clear_worries),
    edit("promote-worries-to-sorries", 0, promote),
];
