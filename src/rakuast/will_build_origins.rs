//! `has R $.r .= new(...)` (#18621): an attribute whose default is a `.=`
//! call. Rakudo's declaration has an `Initializer::CallAssign`, which its
//! `visit-children` does not walk, and a `Trait::WillBuild` holding the call
//! on the declared type, whose type is placed where it was written and whose
//! call is placed over its text after `.=`. mutsu's trait holds that
//! `ApplyPostfix` directly. Rakudo wraps it in a generated
//! `Method::Initializer`, which mutsu does not model.

use super::infix_origins::{child, with_child, with_child_at};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// Whether `node` is an attribute declaration (`has` / `HAS`) whose
/// initializer is an `Initializer::CallAssign`.
// Cost: O(f), f = fields of `node`.
pub(super) fn is_attribute_call_assign(node: &RakuAstNode) -> bool {
    node.class == RakuAstClass::VarDeclarationSimple
        && super::decl_origins::leaf(node, "scope").is_some_and(|s| s == "has" || s == "HAS")
        && child(node, "initializer")
            .is_some_and(|i| i.class == RakuAstClass::InitializerCallAssign)
}

/// Give the `ApplyPostfix` an attribute's `Trait::WillBuild` holds the
/// origins of the placed declared type (its operand) and the placed call of
/// the `Initializer::CallAssign` (its postfix).
// Cost: O(t + n), t = traits, n = size of the call (it is copied).
pub(super) fn place(node: &mut RakuAstNode) {
    if !is_attribute_call_assign(node) {
        return;
    }
    let ty = child(node, "type")
        .filter(|t| t.origin.0.is_some())
        .cloned();
    let call = child(node, "initializer")
        .and_then(|i| super::infix_origins::child_at(i, 0))
        .filter(|c| c.origin.0.is_some())
        .cloned();
    let Some(field) = node.fields.iter_mut().find(|f| f.name == Some("traits")) else {
        return;
    };
    let RakuAstFieldValue::List(traits) = &mut field.value else {
        return;
    };
    for value in traits.iter_mut() {
        let ValueView::RakuAst(trait_node) = value.view() else {
            continue;
        };
        if trait_node.class != RakuAstClass::TraitWillBuild {
            continue;
        }
        let mut trait_node = RakuAstNode::clone(trait_node);
        with_child_at(&mut trait_node, 0, |apply| {
            if apply.class != RakuAstClass::ApplyPostfix {
                return;
            }
            if let Some(ty) = &ty {
                with_child(apply, "operand", |operand| {
                    if operand.class == ty.class {
                        *operand = ty.clone();
                    }
                });
            }
            if let Some(call) = &call {
                with_child(apply, "postfix", |postfix| {
                    if postfix.class == call.class {
                        *postfix = call.clone();
                    }
                });
            }
        });
        *value = Value::rakuast(Box::new(trait_node));
    }
}
