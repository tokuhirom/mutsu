//! Origins below a routine declaration (ADR-10723 Stage 2): its `Name`, its
//! `Signature` and `Parameter`s, its `Blockoid` and `StatementList`, from the
//! positions the parser recorded on `Stmt::SubDecl` / `Stmt::MethodDecl`.

use super::{RakuAstFieldValue, RakuAstNode};
use crate::ast::ParamDef;
use crate::ast::parsed::RoutineSpans;
use crate::value::{Value, ValueView};

/// Give the parts of `node` (a `Sub`, `Method` or `Submethod` built from
/// `params`) the origins in `spans`.
// Cost: O(f + p), f = fields of the routine node, p = parameters.
pub(super) fn apply(node: &mut RakuAstNode, spans: Option<&RoutineSpans>, params: &[ParamDef]) {
    let Some(spans) = spans else {
        return;
    };
    with_child(node, "name", |name| super::origin::set_span(name, spans.name));
    with_child(node, "signature", |signature| {
        super::origin::set_span(signature, spans.signature);
        parameters(signature, params);
    });
    with_child(node, "body", |blockoid| {
        super::origin::set_span(blockoid, spans.block.blockoid);
        if let Some(field) = blockoid.fields.first_mut() {
            with_value(&mut field.value, |list| {
                super::origin::set_span(list, spans.block.list)
            });
        }
    });
}

/// Give each `Parameter` of `signature` the origin of the parameter it was
/// built from, when there is one node per parameter.
// Cost: O(p), p = parameters.
pub(super) fn parameters(signature: &mut RakuAstNode, params: &[ParamDef]) {
    let Some(field) = signature
        .fields
        .iter_mut()
        .find(|f| f.name == Some("parameters"))
    else {
        return;
    };
    let RakuAstFieldValue::List(nodes) = &mut field.value else {
        return;
    };
    if nodes.len() != params.len() {
        return;
    }
    for (value, param) in nodes.iter_mut().zip(params) {
        let ValueView::RakuAst(node) = value.view() else {
            continue;
        };
        let mut node = RakuAstNode::clone(node);
        super::origin::set_span(&mut node, param.spans.param);
        with_child(&mut node, "target", |target| {
            super::origin::set_span(target, param.spans.target)
        });
        *value = Value::rakuast(Box::new(node));
    }
}

/// Apply `f` to the node held in `node`'s field `name`, if it holds one.
// Cost: O(f), f = fields of `node`.
fn with_child(node: &mut RakuAstNode, name: &str, f: impl FnOnce(&mut RakuAstNode)) {
    if let Some(field) = node.fields.iter_mut().find(|f| f.name == Some(name)) {
        with_value(&mut field.value, f);
    }
}

/// Apply `f` to the node a field value holds, if it holds one.
// Cost: O(1) plus `f`.
fn with_value(value: &mut RakuAstFieldValue, f: impl FnOnce(&mut RakuAstNode)) {
    let RakuAstFieldValue::Node(held) = value else {
        return;
    };
    let ValueView::RakuAst(node) = held.view() else {
        return;
    };
    let mut node = RakuAstNode::clone(node);
    f(&mut node);
    *held = Value::rakuast(Box::new(node));
}
