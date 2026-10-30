//! The expansions of an assignment to something immutable: both die with
//! `X::Assignment::RO` once the right-hand side has been evaluated. The reserved
//! call that throws is spelled only here.

use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;

const ASSIGNMENT_RO: &str = "__mutsu_assignment_ro";

/// `LHS = RHS` over a target that cannot be assigned: `{ LHS; RHS; die }`.
// Cost: O(1).
pub(crate) fn assignment_ro_expr(lhs: Expr, rhs: Expr) -> Expr {
    Expr::desugar_block(vec![
        Stmt::Expr(lhs),
        Stmt::Expr(rhs),
        Stmt::Expr(Expr::Call {
            name: Symbol::intern(ASSIGNMENT_RO),
            args: Vec::new(),
            listop: false,
        }),
    ])
}

/// `LITERAL = RHS`: assigning to an immutable literal throws
/// `X::Assignment::RO` once the right-hand side has been evaluated, reporting
/// the literal's type and repr. The expansion is `{ RHS; ro(LITERAL) }`.
// Cost: O(1).
pub(crate) fn literal_assign_ro_expr(literal: Expr, rhs: Expr) -> Expr {
    Expr::desugar_block(vec![
        Stmt::Expr(rhs),
        Stmt::Expr(Expr::Call {
            name: Symbol::intern(ASSIGNMENT_RO),
            args: vec![literal],
            listop: false,
        }),
    ])
}
