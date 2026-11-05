//! The statements rakudo's tree adds around a role body's own: a
//! `Role::ResolveInstantiations` first and `nqp::list(MetaObject, nqp::curlexpad)`
//! last (measured on rakudo 2026.09).
//!
//! Rakudo's `RoleBody` carries them from the moment it is built (by the parser
//! or by `RoleBody.new`): `RakuAST::Deparse` reads a role body as
//! `statements.skip.head(*-1)` to lose "the fabricated return value", and the
//! constructor form (`.raku`) and the lowering leave them out. A body here holds
//! them as well: [`add`] puts them in, [`shown`] is the body without them for
//! the renderer and the lowering.

use super::{RakuAstClass as C, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

fn plain(class: C, fields: Vec<RakuAstField>) -> Value {
    Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class,
        fields,
    }))
}

fn unnamed(value: Value) -> RakuAstField {
    RakuAstField {
        name: None,
        value: RakuAstFieldValue::Node(value),
    }
}

/// The node held by an unnamed-or-named `Node` field.
fn held(field: &RakuAstField) -> Option<&RakuAstNode> {
    match &field.value {
        RakuAstFieldValue::Node(v) => match v.view() {
            ValueView::RakuAst(n) => Some(n),
            _ => None,
        },
        _ => None,
    }
}

/// The `StatementList` field of a role body's `Blockoid`, as `(blockoid field
/// index, list field index)` into `body.fields` and the blockoid's fields.
// Cost: O(f), f = fields of the body and its block.
fn list_position(body: &RakuAstNode) -> Option<(usize, usize)> {
    let at = body
        .fields
        .iter()
        .position(|f| f.name == Some("body") && held(f).is_some_and(|b| b.class == C::Blockoid))?;
    let blockoid = held(&body.fields[at])?;
    let inner = blockoid
        .fields
        .iter()
        .position(|f| held(f).is_some_and(|l| l.class == C::StatementList))?;
    Some((at, inner))
}

fn is_resolve_instantiations(field: &RakuAstField) -> bool {
    held(field).is_some_and(|n| n.class == C::RoleResolveInstantiations)
}

/// The closing `nqp::list(MetaObject, nqp::curlexpad)` statement.
fn is_closing(field: &RakuAstField) -> bool {
    held(field).is_some_and(|stmt| {
        stmt.class == C::StatementExpression
            && stmt.fields.iter().any(|f| {
                held(f).is_some_and(|nqp| {
                    nqp.class == C::Nqp
                        && nqp
                            .fields
                            .iter()
                            .any(|a| held(a).is_some_and(|arg| arg.class == C::RoleMetaObject))
                })
            })
    })
}

/// Rebuild `body` with its statement list passed through `edit`.
// Cost: O(s), s = statements of the body.
fn edit_statements(
    body: &RakuAstNode,
    edit: impl FnOnce(&mut Vec<RakuAstField>),
) -> Option<RakuAstNode> {
    let (at, inner) = list_position(body)?;
    let mut blockoid = RakuAstNode::clone(held(&body.fields[at])?);
    let mut list = RakuAstNode::clone(held(&blockoid.fields[inner])?);
    edit(&mut list.fields);
    blockoid.fields[inner].value = RakuAstFieldValue::Node(Value::rakuast(Box::new(list)));
    let mut out = RakuAstNode::clone(body);
    out.fields[at].value = RakuAstFieldValue::Node(Value::rakuast(Box::new(blockoid)));
    Some(out)
}

/// A `RoleBody` with the implicit statements around its own; one that has them
/// already is returned as it is.
// Cost: O(s), s = statements of the body.
pub(super) fn add(body: RakuAstNode) -> RakuAstNode {
    let with = edit_statements(&body, |statements| {
        if statements.first().is_some_and(is_resolve_instantiations) {
            return;
        }
        let nqp = |op: &str, args: Vec<Value>| {
            let mut fields = vec![unnamed(Value::str(op.to_string()))];
            fields.extend(args.into_iter().map(unnamed));
            plain(C::Nqp, fields)
        };
        let closing = nqp(
            "list",
            vec![
                plain(C::RoleMetaObject, Vec::new()),
                nqp("curlexpad", Vec::new()),
            ],
        );
        let closing = plain(
            C::StatementExpression,
            vec![RakuAstField {
                name: Some("expression"),
                value: RakuAstFieldValue::Node(closing),
            }],
        );
        statements.insert(0, unnamed(plain(C::RoleResolveInstantiations, Vec::new())));
        statements.push(unnamed(closing));
    });
    with.unwrap_or(body)
}

/// A `RoleBody` without the implicit statements, or `None` when it has none.
// Cost: O(s), s = statements of the body.
pub(super) fn shown(body: &RakuAstNode) -> Option<RakuAstNode> {
    if body.class != C::RoleBody {
        return None;
    }
    let (at, inner) = list_position(body)?;
    let list = held(&held(&body.fields[at])?.fields[inner])?;
    if !list.fields.first().is_some_and(is_resolve_instantiations) {
        return None;
    }
    edit_statements(body, |statements| {
        statements.remove(0);
        if statements.last().is_some_and(is_closing) {
            statements.pop();
        }
    })
}
