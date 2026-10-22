//! `RakuAST::Node.visit-children` (ADR-11276 §9, slice 3D).
//!
//! A RakuAST node is the opaque shape `RakuAstNode`, and `RakuAST::Origin` and
//! `RakuAST::Origin::Source` instances are opaque shapes of their own
//! (`value/dispatch_shape/opaque.rs`); the rows are `OWNER_ONLY` under the
//! owners those shapes name.
//!
//! The same file holds the rows of `RakuAST::Origin` (a node's `.origin`) and
//! `RakuAST::Origin::Source` (its `.source`). A statement node records only
//! the range it occupies: `from` and `to` are character offsets into the
//! unit's source, which `.source` answers, and `Source.original-line` finds the
//! line of an offset by binary search (ADR-10723 §4; `rakuast::unit_source`).

use super::super::{Handler, MethodRow, Named, RowFlags};
use crate::rakuast::{RakuAstFieldValue, RakuAstNode};
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! origin_row {
    ($owner:literal, $name:literal, $arity:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Narrow($handler),
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
}

/// A one-argument row of the node class `$owner` that takes any argument (a
/// node).
macro_rules! node_row {
    ($owner:literal, $name:literal, $handler:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 1,
            handler: Handler::Narrow($handler),
            flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "RakuAST::Node",
        name: "visit-children",
        arity: 1,
        handler: Handler::Interp(visit_children),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    // The builders that change a node in place (`$list.add-statement($s)`):
    // each answers only the node class Rakudo declares it on
    // (`Value::rakuast_add_child`, `Value::rakuast_set_field`).
    node_row!("RakuAST::StatementList", "add-statement", |t, a| t
        .rakuast_add_child("add-statement", a[0].clone())),
    node_row!("RakuAST::StatementList", "unshift-statement", |t, a| t
        .rakuast_add_child("unshift-statement", a[0].clone())),
    node_row!("RakuAST::ArgList", "push", |t, a| t
        .rakuast_add_child("push", a[0].clone())),
    node_row!("RakuAST::CompUnit", "replace-statement-list", |t, a| t
        .rakuast_set_field("replace-statement-list", a[0].clone())),
    node_row!(
        "RakuAST::Statement::Expression",
        "set-expression",
        |t, a| t.rakuast_set_field("set-expression", a[0].clone())
    ),
    // One row serves both classes (a name is declared once); the handler
    // refuses any other node.
    node_row!("RakuAST::ApplyInfix", "add-colonpair", |t, a| t
        .rakuast_add_colonpair(a[0].clone())),
    origin_row!("RakuAST::Origin", "from", 0, origin_position),
    origin_row!("RakuAST::Origin", "to", 0, origin_end),
    origin_row!("RakuAST::Origin", "source", 0, origin_source),
    origin_row!("RakuAST::Origin::Source", "original-line", 1, original_line),
    origin_row!("RakuAST::Origin::Source", "orig", 0, source_orig),
];

/// `Origin.from` / `Origin.to`: the character range the node occupies.
// Cost: O(1).
fn origin_position(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let _ = args;
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    attributes.as_map().get("from").cloned().map(Ok)
}

/// `Origin.to`.
// Cost: O(1).
fn origin_end(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    attributes.as_map().get("to").cloned().map(Ok)
}

/// `Origin.source`: the unit source an origin's positions index.
// Cost: O(1).
fn origin_source(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let ValueView::Instance { attributes, .. } = target.view()
        && let Some(source) = attributes.as_map().get("source")
    {
        return Some(Ok(source.clone()));
    }
    Some(Ok(Value::make_instance(
        Symbol::intern("RakuAST::Origin::Source"),
        std::collections::HashMap::new(),
    )))
}

/// `Origin::Source.orig`: the text of the unit.
// Cost: O(1).
fn source_orig(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    attributes
        .as_map()
        .get(crate::rakuast::unit_source::ORIG_ATTR)
        .cloned()
        .map(Ok)
}

/// `Origin::Source.original-line($position)`: the 1-based line the character
/// offset `$position` is on, by binary search over the unit's line starts.
/// A source without an index (one built by hand) answers the position itself.
// Cost: O(log n), n = lines of the unit.
fn original_line(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Int(pos) = args.first()?.view() else {
        return None;
    };
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    let map = attributes.as_map();
    let Some(starts) = map.get(crate::rakuast::unit_source::LINE_STARTS_ATTR) else {
        return Some(Ok(args[0].clone()));
    };
    let ValueView::Array(starts, _) = starts.view() else {
        return Some(Ok(args[0].clone()));
    };
    let line = starts
        .as_slice()
        .partition_point(|start| matches!(start.view(), ValueView::Int(s) if s <= pos));
    Some(Ok(Value::int(line.max(1) as i64)))
}

/// The child nodes of `node`, in field order: every field value that is itself
/// a RakuAST node, and every such element of a list field. Leaf payloads (a
/// name, a number) and the hidden `origin` are not nodes.
// Cost: O(f + c), f = fields of `node`, c = child nodes listed in them.
pub(super) fn children(node: &RakuAstNode) -> Vec<Value> {
    let mut out = Vec::new();
    for field in node
        .fields
        .iter()
        .filter(|field| !crate::rakuast::is_unvisited_field(node, field))
    {
        match &field.value {
            RakuAstFieldValue::Node(v) if crate::rakuast::is_node_value(v) => {
                out.push(v.clone());
            }
            RakuAstFieldValue::List(items) => out.extend(
                items
                    .iter()
                    .filter(|v| crate::rakuast::is_node_value(v))
                    .cloned(),
            ),
            _ => {}
        }
    }
    out
}

/// `visit-children(&callable)`: call `callable` once with each direct child.
// Cost: O(f + c) plus c calls of the callable, f = fields of the node, c = its
// child nodes.
fn visit_children(
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
    let callable = args.first()?;
    for child in children(node) {
        if let Err(e) = interp.call_sub_value(callable.clone(), vec![child]) {
            return Some(Err(e));
        }
    }
    Some(Ok(Value::NIL))
}
