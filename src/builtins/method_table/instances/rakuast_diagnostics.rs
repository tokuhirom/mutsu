//! The check-time problem lists of `RakuAST::Node`, answered by
//! [`crate::rakuast::diagnostics`]; this file only registers the rows.

use super::super::{Handler, MethodRow, RowFlags};
use super::rakuast_visit::native_node;
use crate::rakuast::diagnostics;
use crate::value::{RuntimeError, Value, ValueView};

// Cost: O(f + p), f = fields of the node, p = problems held.
fn sorries(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    if is_comp_unit(target) {
        return Ok(Value::array(crate::unit_diagnostics::peek().0));
    }
    Ok(read(target, |n| diagnostics::list(n, false)))
}

// Cost: O(f + p), f = fields of the node, p = problems held.
fn worries(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    if is_comp_unit(target) {
        return Ok(Value::array(crate::unit_diagnostics::peek().1));
    }
    Ok(read(target, |n| diagnostics::list(n, true)))
}

// Cost: O(f).
fn has_problems(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    if is_comp_unit(target) {
        let (sorries, worries) = crate::unit_diagnostics::peek();
        return Ok(Value::truth(!sorries.is_empty() || !worries.is_empty()));
    }
    Ok(read(target, |n| Value::truth(diagnostics::has_problems(n))))
}

/// `$*CU` keeps its problems in the unit-wide queue the compile phase reports
/// (#15666), not on the node.
// Cost: O(1).
fn is_comp_unit(target: &Value) -> bool {
    native_node(target).is_some_and(|base| {
        matches!(base.view(), ValueView::RakuAst(n) if n.class == crate::rakuast::RakuAstClass::CompUnit)
    })
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
    if is_comp_unit(target) {
        crate::unit_diagnostics::add(&args[0], false);
        return Some(Ok(Value::NIL));
    }
    target.rakuast_edit(|node| Ok(diagnostics::add(node, false, &args[0])))
}

// Cost: O(f + p).
fn add_worry(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if is_comp_unit(target) {
        crate::unit_diagnostics::add(&args[0], true);
        return Some(Ok(Value::NIL));
    }
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

const fn unit_query(
    name: &'static str,
    f: fn(&Value, &[Value]) -> Result<Value, RuntimeError>,
) -> MethodRow {
    MethodRow {
        owner: "RakuAST::CompUnit",
        name,
        arity: 0,
        handler: Handler::Pure(f),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    }
}

const fn edit(name: &'static str, arity: u8, f: super::super::NarrowFn) -> MethodRow {
    MethodRow {
        owner: "RakuAST::CheckTime",
        name,
        arity,
        handler: Handler::Narrow(f),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    }
}

pub(super) static ROWS: &[MethodRow] = &[
    // Rakudo lists these on `CompUnit` as well as on the `CheckTime` role every
    // node does; a name is declared once, so one `CompUnit` row serves both and
    // its handler tells the unit (the `$*CU` queue) from any other node.
    unit_query("sorries", sorries),
    unit_query("worries", worries),
    // Rakudo declares this one on `CompUnit` too; one row serves both (a name
    // is declared once, and the handler tells the unit from other nodes).
    MethodRow {
        owner: "RakuAST::CompUnit",
        name: "has-check-time-problems",
        arity: 0,
        handler: Handler::Pure(has_problems),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::CompUnit",
        name: "add-sorry",
        arity: 1,
        handler: Handler::Narrow(add_sorry),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::CompUnit",
        name: "add-worry",
        arity: 1,
        handler: Handler::Narrow(add_worry),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
    edit("clear-worries", 0, clear_worries),
    edit("promote-worries-to-sorries", 0, promote),
];
