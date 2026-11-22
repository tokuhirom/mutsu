//! A routine used as an infix: `1 [&plus] 2` (#18469).
//!
//! The parser calls the routine with both operands (`Expr::CallOn`, marked
//! `Spelling::FunctionInfix`); rakudo keeps an `ApplyInfix` whose `infix` is a
//! `FunctionInfix` over the routine's term (`Var::Lexical("&plus")`). Lowering
//! gives the call back.

use super::lower::{lower_expr, named_child, positional_leaf};
use super::view::{node_field, view_expr};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::scope::NameScope;
use crate::value::{RuntimeError, ValueView};

/// The `ApplyInfix(left, FunctionInfix(term), right)` the call `expr` was
/// written as.
// Cost: O(n), n = size of the operands.
pub(super) fn view(names: &NameScope, expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let Expr::CallOn { target, args } = expr else {
        return Err(super::view::unsupported("a function infix that is no call"));
    };
    let [left, right] = args.as_slice() else {
        return Err(super::view::unsupported("a function infix without two operands"));
    };
    let infix = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::FunctionInfix,
        fields: vec![node_field(None, view_expr(names, target)?)],
    };
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyInfix,
        fields: vec![
            node_field(Some("left"), view_expr(names, left)?),
            node_field(Some("infix"), infix),
            node_field(Some("right"), view_expr(names, right)?),
        ],
    })
}

/// `ApplyInfix(left, FunctionInfix(term), right)` -> the call of `term` with
/// both operands; `None` for any other node.
// Cost: O(n), n = size of the node.
pub(super) fn lower(names: &NameScope, node: &RakuAstNode) -> Option<Result<Expr, RuntimeError>> {
    if node.class != RakuAstClass::ApplyInfix {
        return None;
    }
    let infix = named_child(node, "infix").ok()?;
    if infix.class != RakuAstClass::FunctionInfix {
        return None;
    }
    Some((|| {
        let function = positional_leaf(infix)?;
        let ValueView::RakuAst(function) = function.view() else {
            return Err(super::lower::unsupported(node));
        };
        Ok(Expr::CallOn {
            target: Box::new(lower_expr(names, function)?),
            args: vec![
                lower_expr(names, named_child(node, "left")?)?,
                lower_expr(names, named_child(node, "right")?)?,
            ],
        })
    })())
}
