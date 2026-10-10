//! Callable identities required by a lowered loop body.

use crate::ast::{Expr, Stmt};
use crate::ast_visit::{Visit, walk_expr};

/// Whether a loop body requires its own callable block value. Descendant
/// references conservatively request it, as the parser's source scan does.
// Cost: O(n), n = nodes in the body.
pub(super) fn uses_block(body: &[Stmt]) -> bool {
    struct Scan(bool);
    impl<'ast> Visit<'ast> for Scan {
        fn visit_expr(&mut self, expr: &'ast Expr) {
            if matches!(expr, Expr::CodeVar(name) if name == "?BLOCK") {
                self.0 = true;
            }
            if !self.0 {
                walk_expr(self, expr);
            }
        }
    }
    let mut scan = Scan(false);
    for stmt in body {
        scan.visit_stmt(stmt);
    }
    scan.0
}
