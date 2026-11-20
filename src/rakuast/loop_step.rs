//! A C-style loop's comma-separated step (`loop (...; ...; $i++, $j--)`):
//! the parser runs it as a do-block of one statement per item, marked
//! `Spelling::LoopStepComma`; RakuAST keeps one comma list infix.

use super::RakuAstNode;
use super::view::{comma_list_node, unsupported};
use crate::ast::{Expr, Stmt};
use crate::scope::NameScope;
use crate::value::RuntimeError;

/// The comma list of a marked loop step.
// Cost: O(n), n = size of the step's items.
pub(super) fn view_node(names: &NameScope, expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let Expr::DoBlock { body, .. } = expr else {
        return Err(unsupported("loop step comma without its execution block"));
    };
    let mut expressions = Vec::with_capacity(body.len());
    for statement in body {
        let Stmt::Expr(expression) = statement else {
            return Err(unsupported("non-expression loop step item"));
        };
        expressions.push(expression.clone());
    }
    comma_list_node(names, &expressions)
}
