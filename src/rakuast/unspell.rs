//! The tree `lower` hands the compiler carries no spellings (ADR-12199).
//!
//! The parser wraps a term in `Expr::Spelled` when its source spelling is more
//! than its value (a word list, a negated infix, a subscript with an unknown
//! adverb), for the RakuAST conversion to read. The lowering reuses the
//! parser's builders for the executable shape of a construct, and a builder
//! that spells its result (the written adverbs of a subscript, the sections of
//! a list) hands the wrapper back along with the shape. The wrapper only
//! annotates the expression it holds, so lowering ends by taking each one off.

use crate::ast::{Expr, Stmt};
use crate::ast_visit::{VisitMut, walk_expr_mut};

/// Take every spelling off the lowered statements `stmts`.
// Cost: O(n), n = size of the tree.
pub(super) fn strip(stmts: &mut [Stmt]) {
    let mut pass = Strip;
    for stmt in stmts {
        pass.visit_stmt_mut(stmt);
    }
}

struct Strip;

impl VisitMut for Strip {
    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        while matches!(expr, Expr::Spelled(_)) {
            if let Expr::Spelled(spelled) = std::mem::replace(expr, Expr::Whatever) {
                *expr = spelled.expr;
            }
        }
        walk_expr_mut(self, expr);
    }
}
