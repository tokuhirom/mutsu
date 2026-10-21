//! Preserve a compound assignment that the parser wrapped as an Assign stmt.

use super::convert::leaf_field;
use super::{RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{AssignOp, Expr, Stmt};
use crate::value::{RuntimeError, Value, ValueView};

const SOURCE_ASSIGN_NAME: &str = "source-stmt-assign-name";

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

// Cost: O(1).
pub(super) fn mark(mut node: RakuAstNode, name: &str) -> RakuAstNode {
    node.fields.push(leaf_field(
        Some(SOURCE_ASSIGN_NAME),
        Value::str(name.to_string()),
    ));
    node
}

// Cost: O(f), f = fields of the assignment node.
pub(super) fn restore_stmt(node: &RakuAstNode, expr: Expr) -> Result<Stmt, RuntimeError> {
    let Some(field) = node
        .fields
        .iter()
        .find(|field| field.name == Some(SOURCE_ASSIGN_NAME))
    else {
        return Ok(Stmt::Expr(expr));
    };
    let RakuAstFieldValue::Node(value) = &field.value else {
        return Err(super::lower::unsupported(node));
    };
    let ValueView::Str(name) = value.view() else {
        return Err(super::lower::unsupported(node));
    };
    Ok(Stmt::Assign {
        name: name.to_string(),
        expr,
        op: AssignOp::Assign,
        target_is_sigilless: false,
    })
}

// Cost: O(1).
pub(super) fn is_source_field(field: &RakuAstField) -> bool {
    field.name == Some(SOURCE_ASSIGN_NAME)
}
