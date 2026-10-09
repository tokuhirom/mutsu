//! Preserve a compound assignment that the parser wrapped as an Assign stmt.

use super::convert::leaf_field;
use super::{RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{AssignOp, Expr, Stmt};
use crate::value::{RuntimeError, Value, ValueView};

const SOURCE_ASSIGN_NAME: &str = "source-stmt-assign-name";

// Cost: O(1).
pub(super) fn mark(mut node: RakuAstNode, name: &str) -> RakuAstNode {
    node.fields.push(leaf_field(
        Some(SOURCE_ASSIGN_NAME),
        Value::str(name.to_string()),
    ));
    node
}

// Cost: O(f), f = fields of the assignment node.
pub(super) fn lower(node: &RakuAstNode, expr: Expr) -> Result<Stmt, RuntimeError> {
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
