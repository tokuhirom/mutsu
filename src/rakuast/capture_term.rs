//! Capture literals `\(...)` and `\term` across the RakuAST boundary.
//!
//! Measured against rakudo 2026.09, `\(1, 2, :a)` is `Term::Capture(ArgList(…))`
//! and a bare `\$x` / `\3` is `Term::Capture` over the term itself. The parser
//! keeps both as [`Expr::CaptureLiteral`], with a flag for the parentheses.

use super::convert::{arg_list, convert_expr, node_field};
use super::lower::{arg_list_exprs, lower_expr, named_child_or_positional};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::value::RuntimeError;

/// An [`Expr::CaptureLiteral`] as its `Term::Capture` node.
// Cost: O(n), n = nodes of the items.
pub(super) fn convert(items: &[Expr], parenthesized: bool) -> Result<RakuAstNode, RuntimeError> {
    let source = match items {
        [item] if !parenthesized => convert_expr(item)?,
        _ => arg_list(items)?,
    };
    Ok(RakuAstNode {
        class: RakuAstClass::TermCapture,
        fields: vec![node_field(None, source)],
    })
}

/// A `Term::Capture` node as the parser's [`Expr::CaptureLiteral`].
// Cost: O(n), n = nodes of the source.
pub(super) fn lower(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let source = named_child_or_positional(node)?;
    if source.class == RakuAstClass::ArgList {
        return Ok(Expr::CaptureLiteral(arg_list_exprs(source)?, true));
    }
    Ok(Expr::CaptureLiteral(vec![lower_expr(source)?], false))
}
