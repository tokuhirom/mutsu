//! What the text of a statement modifier's operand ends with.
//!
//! `parse_statement_modifier` treats a modifier whose operand ends in `}`
//! followed by a newline as a statement terminator (`return if @a.first: {
//! ... }` then a new statement on the next line). That rule is about *blocks*;
//! a `}` closing a hash subscript (`die "x" if %h{$_}` followed by `for LIST`
//! on the next line, as App::Lorea writes it) is an ordinary term end, so the
//! next line's modifier still belongs to the same statement.

use crate::ast::{Expr, Stmt};

/// Whether the operand of the statement modifier that produced `stmt` (the
/// `if`/`unless`/`while`/`until` condition, the `for` list, the `given`/`with`
/// topic) ends with a subscript such as `%h{...}`, so its final `}` does not
/// close a block.
pub(super) fn modifier_operand_ends_with_subscript(stmt: &Stmt) -> bool {
    let operand = match stmt {
        Stmt::If { cond, .. } | Stmt::While { cond, .. } => cond,
        Stmt::For { iterable, .. } => iterable,
        Stmt::Given { topic, .. } => topic,
        _ => return false,
    };
    crate::parser::expr::is_subscript_expr(rightmost_term(operand))
}

/// The term whose text ends the expression: the right operand of an infix,
/// the else arm of a ternary, the operand of a prefix.
fn rightmost_term(mut expr: &Expr) -> &Expr {
    loop {
        expr = match expr {
            Expr::Binary { right, .. } => right,
            Expr::Ternary { else_expr, .. } => else_expr,
            Expr::Unary { expr, .. } => expr,
            _ => return expr,
        };
    }
}
