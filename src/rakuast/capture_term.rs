//! Capture literals `\(...)` and `\term` across the RakuAST boundary.
//!
//! Measured against rakudo 2026.09, `\(1, 2, :a)` is `Term::Capture(ArgList(…))`
//! and a bare `\$x` / `\3` is `Term::Capture` over the term itself. The parser
//! keeps both as [`Expr::CaptureLiteral`], with a flag for the parentheses.

use super::lower::{arg_list_exprs, lower_expr, named_child_or_positional};
use super::view::{arg_list, node_field, view_expr};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::scope::NameScope;
use crate::value::RuntimeError;

/// An [`Expr::CaptureLiteral`] as its `Term::Capture` node.
// Cost: O(n), n = nodes of the items.
pub(super) fn view_node(
    names: &NameScope,
    items: &[Expr],
    parenthesized: bool,
) -> Result<RakuAstNode, RuntimeError> {
    let source = match items {
        [item] if !parenthesized => view_expr(names, item)?,
        _ => arg_list(names, items)?,
    };
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::TermCapture,
        fields: vec![node_field(None, source)],
    })
}

/// A `Term::Capture` node as the parser's [`Expr::CaptureLiteral`].
// Cost: O(n), n = nodes of the source.
pub(super) fn lower(names: &NameScope, node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let source = named_child_or_positional(node)?;
    if source.class == RakuAstClass::ArgList {
        return Ok(Expr::CaptureLiteral(arg_list_exprs(names, source)?, true));
    }
    Ok(Expr::CaptureLiteral(
        vec![lower_expr(names, source)?],
        false,
    ))
}

/// Place the `ArgList` of `node`, a `Term::Capture` with a range written as
/// `\(ARGS)`: from its first argument to its last, or empty just inside the
/// `(` (#15780).
// Cost: O(n), n = length of the capture's source.
pub(super) fn arg_list_origin(node: &mut RakuAstNode) {
    let Some((from, to)) = super::infix_origins::byte_range(node) else {
        return;
    };
    let source = super::infix_origins::child_at(node, 0);
    if source.is_none_or(|s| s.class != RakuAstClass::ArgList) {
        return;
    }
    let parens = super::unit_source::with_text(crate::ast::SrcSpan::new(from, to), |text| {
        (text.starts_with("\\(") && text.ends_with(')')).then_some(())
    });
    if parens.is_none() || to < from + 3 {
        return;
    }
    let inside = crate::ast::SrcSpan::new(from + 2, to - 1);
    let Some((start, end)) = super::call_origins::parenthesized(source, inside) else {
        return;
    };
    super::infix_origins::with_child_at(node, 0, |list| {
        super::origin::set_span(list, crate::ast::SrcSpan::new(start, end))
    });
}
