use crate::ast::{Expr, Stmt};

/// `supply { BODY }`: kept in its RakuAST shape; `rakuast::supply_lower`
/// lowers it (ADR-15334).
// Cost: O(n), n = size of the body.
pub(crate) fn supply_block(body: Vec<Stmt>) -> Expr {
    Expr::Ast(Box::new(crate::ast::parsed::ParsedExpr::Supply(body)))
}
