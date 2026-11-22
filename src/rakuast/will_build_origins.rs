//! The origins inside the `Method::Initializer` thunk an attribute's
//! `Trait::WillBuild` holds (#18621; `super::method_initializer`). Rakudo
//! places the expression the thunk computes where it was written, though the
//! thunk and its own generated nodes have no origin:
//!
//! - `has R $.r .= new(...)`: the `ApplyPostfix` on the declared type has no
//!   origin, its type operand is placed where the type was written and its
//!   call over its text after `.=`;
//! - `has $.x = 1 + 2`: the returned expression is the placed initializer
//!   expression.

use super::infix_origins::{child, with_child};
use super::{RakuAstClass, RakuAstNode};

/// Whether `node` is an attribute declaration (`has` / `HAS`) whose
/// initializer is an `Initializer::CallAssign`.
// Cost: O(f), f = fields of `node`.
pub(super) fn is_attribute_call_assign(node: &RakuAstNode) -> bool {
    node.class == RakuAstClass::VarDeclarationSimple
        && super::decl_origins::leaf(node, "scope").is_some_and(|s| s == "has" || s == "HAS")
        && child(node, "initializer")
            .is_some_and(|i| i.class == RakuAstClass::InitializerCallAssign)
}

/// Give the expression of `node`'s thunk the origins of the placed declared
/// type and initializer it was built from.
// Cost: O(t + n), t = traits, n = size of the expression (it is copied).
pub(super) fn place(node: &mut RakuAstNode) {
    if !super::method_initializer::is_thunked(node) {
        return;
    }
    let ty = child(node, "type")
        .filter(|t| t.origin.0.is_some())
        .cloned();
    let call_assign = is_attribute_call_assign(node);
    let written = child(node, "initializer")
        .and_then(|i| super::infix_origins::child_at(i, 0))
        .filter(|c| c.origin.0.is_some())
        .cloned();
    super::method_initializer::with_thunk_expression(node, |expression| {
        if !call_assign {
            if let Some(written) = written
                && written.class == expression.class
            {
                *expression = written;
            }
            return;
        }
        if expression.class != RakuAstClass::ApplyPostfix {
            return;
        }
        if let Some(ty) = &ty {
            with_child(expression, "operand", |operand| {
                if operand.class == ty.class {
                    *operand = ty.clone();
                }
            });
        }
        if let Some(call) = &written {
            with_child(expression, "postfix", |postfix| {
                if postfix.class == call.class {
                    *postfix = call.clone();
                }
            });
        }
    });
}
