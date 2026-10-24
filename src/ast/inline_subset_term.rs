//! Shared parser expansion for an inline `subset` used as a term.

use super::{Expr, Stmt};

/// An inline subset term: kept as the declaration written by a `.AST` parse,
/// lowered at once otherwise (ADR-10723 Stage 2, #14966).
// Cost: O(n), n = size of the declaration.
pub(crate) fn expand(declaration: Stmt) -> Expr {
    if super::spelled::keeping() {
        return Expr::Ast(Box::new(super::parsed::ParsedExpr::InlineSubset(Box::new(
            declaration,
        ))));
    }
    lower(declaration)
}

/// Expand a subset term to its declaration and the value of its generated name.
// Cost: O(n), n = size of the declaration.
pub(crate) fn lower(declaration: Stmt) -> Expr {
    let Stmt::SubsetDecl { name, .. } = &declaration else {
        unreachable!("inline subset term must contain a subset declaration")
    };
    let name = name.resolve();
    Expr::desugar_block(vec![declaration, Stmt::Expr(Expr::BareWord(name))])
}
