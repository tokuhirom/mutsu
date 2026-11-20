//! Colonpairs written one after another as a term (`my $c = :a:!b:42c`).
//!
//! The parser runs the run as the list of its pairs and marks it
//! `Spelling::ColonPairs`; rakudo keeps `RakuAST::ColonPairs` with one
//! positional child per pair (measured on 2026.11), and lowering takes it back
//! to the list.

use super::view::{node_field, view_expr};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::scope::NameScope;
use crate::value::RuntimeError;

/// `expr` as the whole expression of a statement: a run of colonpairs there
/// is a comma list (`:a:!b;` is `ApplyListInfix(",")` in rakudo 2026.11), not
/// `ColonPairs`.
// Cost: O(1).
pub(super) fn statement_form(expr: &Expr) -> &Expr {
    match expr {
        Expr::Spelled(s) if matches!(s.spelling, crate::ast::spelled::Spelling::ColonPairs) => &s.expr,
        _ => expr,
    }
}

/// The `ColonPairs` of a marked run of colonpairs.
// Cost: O(n), n = size of the pairs.
pub(super) fn view_node(names: &NameScope, run: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let Expr::ArrayLiteral(pairs) = run else {
        return view_expr(names, run);
    };
    let mut fields = Vec::with_capacity(pairs.len());
    for pair in pairs {
        fields.push(node_field(None, view_expr(names, pair)?));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ColonPairs,
        fields,
    })
}
