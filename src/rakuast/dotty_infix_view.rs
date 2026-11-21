//! The view of a method call written at the start of a continuation line
//! (`TERM\n    .meth`), which Rakudo parses as a dotty infix.

use super::view::node_field;
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::scope::NameScope;
use crate::value::RuntimeError;

/// `TERM .meth` (`Spelling::DottyInfixCall`): the method call's postfix
/// application as rakudo's dotty infix,
/// `ApplyDottyInfix(left, DottyInfix::Call, Call::Method)`.
// Cost: O(n), n = size of the call.
pub(super) fn continuation_call(names: &NameScope, call: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let mut node = super::view::view_expr(names, call)?;
    if node.class != RakuAstClass::ApplyPostfix {
        return Ok(node);
    }
    let take = |node: &mut RakuAstNode, name: &str| {
        node.fields
            .iter()
            .position(|f| f.name == Some(name))
            .map(|i| node.fields.remove(i))
    };
    let (Some(mut left), Some(mut right)) =
        (take(&mut node, "operand"), take(&mut node, "postfix"))
    else {
        return Err(super::view::unsupported(
            "a dotty infix call of another shape",
        ));
    };
    // `(1 ~ 2)\n .chars`: Rakudo drops the parentheses around a postfix
    // operand but keeps them around a dotty infix's left side.
    if let Expr::MethodCall { target, .. } = call
        && matches!(**target, Expr::Grouped(_))
    {
        left = node_field(None, super::view::view_expr(names, target)?);
    }
    left.name = Some("left");
    right.name = Some("right");
    Ok(RakuAstNode {
        origin: node.origin,
        class: RakuAstClass::ApplyDottyInfix,
        fields: vec![
            left,
            node_field(
                Some("infix"),
                RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::DottyInfixCall,
                    fields: Vec::new(),
                },
            ),
            right,
        ],
    })
}
