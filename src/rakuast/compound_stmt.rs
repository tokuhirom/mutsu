//! The conditional target of a compound assignment (`(c ?? $a !! $b) += 1`),
//! restored to the expression form the parser builds for it.

use crate::ast::{AssignOp, Expr, Stmt};

/// A parenthesized conditional assignment is an lvalue when it is the left
/// operand of a compound assignment. Restore the parser's expression-form
/// body at that boundary; ordinary statement modifiers keep statement-form
/// assignments, which matters for loop-body result collection.
// Cost: O(n), n = AST nodes in the conditional target's assignment value.
pub(super) fn restore_conditional_target(target: Expr) -> Expr {
    match target {
        Expr::Grouped(inner) => Expr::Grouped(Box::new(restore_conditional_do_stmt(*inner))),
        other => restore_conditional_do_stmt(other),
    }
}

// Cost: O(n), n = AST nodes in the assignment value copied into expression form.
fn restore_conditional_do_stmt(target: Expr) -> Expr {
    match target {
        Expr::DoStmt(mut statement) => {
            if let Stmt::If {
                then_branch,
                else_branch,
                binding_var: None,
                ..
            } = statement.as_mut()
                && then_branch.len() == 1
                && else_branch.is_empty()
                && let Stmt::Assign {
                    name,
                    expr,
                    op: op @ (AssignOp::Assign | AssignOp::Bind),
                    target_is_sigilless: false,
                } = &then_branch[0]
            {
                then_branch[0] = Stmt::Expr(Expr::AssignExpr {
                    name: name.clone(),
                    expr: Box::new(expr.clone()),
                    is_bind: matches!(*op, AssignOp::Bind),
                });
            }
            Expr::DoStmt(statement)
        }
        other => other,
    }
}
