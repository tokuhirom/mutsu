//! Whether the body of an `nqp::while` / `nqp::until` family loop names a loop
//! control (`last`, `next`, `redo`).
//!
//! A sunk `nqp::` loop is a plain jump loop with no loop handler of its own, so
//! a `last` in its body had nothing to catch it ("last without loop
//! construct"). Such a body is compiled through the ordinary `while` lowering
//! instead, which does register the loop (Math::Combinatorics' iterators write
//! `nqp::while(cond, nqp::if(test, last))`).

use crate::ast::{ControlFlowKind, Expr, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_stmt};

struct FindLoopControl {
    found: bool,
}

impl<'ast> Visit<'ast> for FindLoopControl {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if matches!(stmt, Stmt::Last(_) | Stmt::Next(_) | Stmt::Redo(_)) {
            self.found = true;
            return;
        }
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if let Expr::ControlFlow { kind, .. } = expr
            && matches!(
                kind,
                ControlFlowKind::Last | ControlFlowKind::Next | ControlFlowKind::Redo
            )
        {
            self.found = true;
            return;
        }
        walk_expr(self, expr);
    }
}

/// Whether `body` contains a `last` / `next` / `redo`, anywhere in its tree.
/// A control inside a nested loop is counted too: that only sends the loop
/// down the slower registered-loop lowering, never changes its meaning.
// Cost: O(n), n = size of the loop body's subtree.
pub(super) fn body_names_loop_control(body: &Expr) -> bool {
    let mut finder = FindLoopControl { found: false };
    finder.visit_expr(body);
    finder.found
}
