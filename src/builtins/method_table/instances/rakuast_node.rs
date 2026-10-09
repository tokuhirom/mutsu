//! `RakuAST::Node.visit-children` (ADR-11276 §9, slice 3D).
//!
//! A RakuAST node has no shape, so the row is reached through its owner
//! (`RowFlags::OWNER_ONLY`, `invoke_owner`) from the method-call fallback.
//!
//! The same file holds the rows of `RakuAST::Origin` (a node's `.origin`) and
//! `RakuAST::Origin::Source` (its `.source`). A statement node records only
//! the line it began on, so an origin's position is that line: `from` and `to`
//! answer it and `Source.original-line` maps a position back to it unchanged.
//! TODO: carry character offsets and the source text so `from`/`to` are the
//! statement's real span, as rakudo's are.

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

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "RakuAST::Node",
        name: "visit-children",
        arity: 1,
        handler: Handler::Interp(visit_children),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    origin_row!("RakuAST::Origin", "from", 0, origin_position),
    origin_row!("RakuAST::Origin", "to", 0, origin_position),
    origin_row!("RakuAST::Origin", "source", 0, origin_source),
    origin_row!("RakuAST::Origin::Source", "original-line", 1, original_line),
];

/// The owners the rows reachable from `target` are found under: a RakuAST
/// node, or an origin or origin source built by [`origin_value`].
// Cost: O(1).
pub(crate) fn owners_of(target: &Value) -> Option<&'static [&'static str]> {
    match target.view() {
        ValueView::RakuAst(_) => Some(&["RakuAST::Node"]),
        ValueView::Instance { class_name, .. } => match class_name.resolve().as_str() {
            "RakuAST::Origin" => Some(&["RakuAST::Origin"]),
            "RakuAST::Origin::Source" => Some(&["RakuAST::Origin::Source"]),
            _ => None,
        },
        _ => None,
    }
}

/// The `RakuAST::Origin` of a node that began on `line`.
// Cost: O(1).
pub(crate) fn origin_value(line: i64) -> Value {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("position".to_string(), Value::int(line));
    Value::make_instance(Symbol::intern("RakuAST::Origin"), attrs)
}

/// `Origin.from` / `Origin.to`: the position the node was found at.
// Cost: O(1).
fn origin_position(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    attributes.as_map().get("position").cloned().map(Ok)
}

/// `Origin.source`: the source an origin's positions index.
// Cost: O(1).
fn origin_source(_target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(Value::make_instance(
        Symbol::intern("RakuAST::Origin::Source"),
        std::collections::HashMap::new(),
    )))
}

/// `Origin::Source.original-line($position)`: the line a position is on.
// Cost: O(1).
fn original_line(_target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match args.first()?.view() {
        ValueView::Int(_) => Some(Ok(args[0].clone())),
        _ => None,
    }
}

/// The child nodes of `node`, in field order: every field value that is itself
/// a RakuAST node, and every such element of a list field. Leaf payloads (a
/// name, a number) and the hidden `origin` are not nodes.
// Cost: O(f + c), f = fields of `node`, c = child nodes listed in them.
fn children(node: &RakuAstNode) -> Vec<Value> {
    let mut out = Vec::new();
    for field in &node.fields {
        match &field.value {
            RakuAstFieldValue::Node(v) if matches!(v.view(), ValueView::RakuAst(_)) => {
                out.push(v.clone());
            }
            RakuAstFieldValue::List(items) => out.extend(
                items
                    .iter()
                    .filter(|v| matches!(v.view(), ValueView::RakuAst(_)))
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
    let ValueView::RakuAst(node) = target.view() else {
        return None;
    };
    let callable = args.first()?;
    for child in children(node) {
        if let Err(e) = interp.call_sub_value(callable.clone(), vec![child], false) {
            return Some(Err(e));
        }
    }
    Some(Ok(Value::NIL))
}
