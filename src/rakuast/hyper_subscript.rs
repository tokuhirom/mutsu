//! Hyper subscripts retain their operator identity instead of becoming an
//! explicit AT-POS/AT-KEY method call. Measured against Rakudo 2026.07.
use super::convert::{convert_expr, node_field};
use super::lower::{lower_expr, named_child, named_child_or_positional, unsupported};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::symbol::Symbol;
use crate::value::RuntimeError;

// Cost: O(n), n = index expression size.
pub(super) fn convert(name: &str, args: &[Expr]) -> Option<Result<RakuAstNode, RuntimeError>> {
    let class = match name {
        "postcircumfix:<[ ]>" => RakuAstClass::PostcircumfixArrayIndex,
        "postcircumfix:<{ }>" => RakuAstClass::PostcircumfixHashIndex,
        _ => return None,
    };
    let [index] = args else { return None };
    Some((|| {
        let statement = RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::StatementExpression,
            fields: vec![node_field(Some("expression"), convert_expr(scope, index)?)],
        };
        let semilist = RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::SemiList,
            fields: vec![node_field(None, statement)],
        };
        Ok(RakuAstNode {
            origin: Default::default(),
            class,
            fields: vec![node_field(Some("index"), semilist)],
        })
    })())
}

// Cost: O(1).
pub(super) fn is_subscript(node: &RakuAstNode) -> bool {
    matches!(
        node.class,
        RakuAstClass::PostcircumfixArrayIndex | RakuAstClass::PostcircumfixHashIndex
    )
}

// Cost: O(n), n = index expression size.
pub(super) fn lower(operand: Expr, node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let index = named_child(node, "index")?;
    if index.fields.len() != 1 {
        return Err(unsupported(node));
    }
    let statement = named_child_or_positional(index)?;
    Ok(Expr::HyperMethodCall {
        target: Box::new(operand),
        name: Symbol::intern(if node.class == RakuAstClass::PostcircumfixArrayIndex {
            "postcircumfix:<[ ]>"
        } else {
            "postcircumfix:<{ }>"
        }),
        args: vec![lower_expr(statement)?],
        modifier: None,
        quoted: false,
    })
}
