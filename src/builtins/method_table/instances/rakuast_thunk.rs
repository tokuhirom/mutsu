//! Expression thunks exposed by `visit-thunks` and rendered by `dump-extras`.

use super::super::{Handler, MethodRow, Named, RowFlags};
use crate::rakuast::{RakuAstClass, RakuAstNode, modeled_thunks, node_accessor};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

/// Call the visitor once for every thunk directly carried by the expression.
// Cost: O(n + k), n = nodes in the expression, k = callback cost.
fn visit_thunks(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let base = match target.view() {
        ValueView::RakuAst(_) => target.clone(),
        ValueView::Instance { attributes, .. } => attributes.rakuast_base()?,
        _ => return None,
    };
    let ValueView::RakuAst(node) = base.view() else {
        return None;
    };
    let callback = args.first()?;
    for thunk in modeled_thunks(node) {
        if let Err(error) = interp.call_sub_value(callback.clone(), vec![thunk]) {
            return Some(Err(error));
        }
    }
    Some(Ok(Value::NIL))
}

/// The kind of an expression thunk.
// Cost: O(f), f = fields of the thunk.
fn thunk_kind(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::RakuAst(node) = target.view() else {
        return None;
    };
    if !matches!(
        node.class,
        RakuAstClass::PrimeThunk | RakuAstClass::BlockThunk
    ) {
        return None;
    }
    Some(Ok(node_accessor(node, "kind")?))
}

/// The details of an expression thunk.
// Cost: O(f), f = fields of the thunk.
fn thunk_details(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::RakuAst(node) = target.view() else {
        return None;
    };
    if !matches!(
        node.class,
        RakuAstClass::PrimeThunk | RakuAstClass::BlockThunk
    ) {
        return None;
    }
    Some(Ok(node_accessor(node, "details")?))
}

/// `Expression.dump-extras` lines for its carried thunks.
// Cost: O(n + d), n = nodes in the expression, d = output length.
pub(super) fn dump_extras(node: &RakuAstNode, indent: usize) -> String {
    let mut out = String::new();
    for thunk in modeled_thunks(node) {
        let ValueView::RakuAst(thunk) = thunk.view() else {
            continue;
        };
        let kind = node_accessor(thunk, "kind").map(|v| v.to_string_value());
        let details = node_accessor(thunk, "details").map(|v| v.to_string_value());
        if let (Some(kind), Some(details)) = (kind, details) {
            out.push_str(&format!("{}🧠 {kind} {details}\n", " ".repeat(indent)));
        }
    }
    out
}

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "RakuAST::Expression",
        name: "visit-thunks",
        arity: 1,
        handler: Handler::Interp(visit_thunks),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::PrimeThunk",
        name: "thunk-kind",
        arity: 0,
        handler: Handler::Narrow(thunk_kind),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::PrimeThunk",
        name: "thunk-details",
        arity: 0,
        handler: Handler::Narrow(thunk_details),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::BlockThunk",
        name: "thunk-kind",
        arity: 0,
        handler: Handler::Narrow(thunk_kind),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::BlockThunk",
        name: "thunk-details",
        arity: 0,
        handler: Handler::Narrow(thunk_details),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
];
