//! A statement prefix over a bare statement (`gather say 1`, `try say 1`,
//! `start say 1`, `once say 1`, `BEGIN say 1`), ADR-12199.
//!
//! The parser wraps the statement in the one-statement block the braced form
//! makes (`gather { say 1 }`); rakudo keeps the statement itself:
//!
//! ```text
//! gather { say 1 }   StatementPrefix::Gather(Block(body => Blockoid(StatementList(...))))
//! gather say 1       StatementPrefix::Gather(Statement::Expression(...))
//! ```
//!
//! A spelling-keeping parse marks the bare form with `Spelling::BareStatement`;
//! [`convert`] renders the prefix the usual way and takes the block off again.
//! `lower` needs no marker: [`lower_body`] reads either child.

use super::lower::{lower_block, lower_stmt};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, Stmt};
use crate::value::{RuntimeError, Value, ValueView};

/// `prefix` (already converted, holding the block of its one statement) with
/// the statement in place of the block.
// Cost: O(1), the nodes are moved, not copied.
pub(super) fn convert(inner: &Expr) -> Result<RakuAstNode, RuntimeError> {
    // `do STATEMENT`: the statement under `StatementPrefix::Do`, whatever it is
    // (the converter's own `DoStmt` arm only takes loops and conditionals).
    if let Expr::DoStmt(stmt) = inner {
        let statement = super::convert::convert_stmt(stmt)?
            .ok_or_else(|| super::convert::unsupported("an empty `do` statement"))?;
        return Ok(RakuAstNode {
            class: RakuAstClass::StatementPrefixDo,
            fields: vec![super::convert::node_field(None, statement)],
        });
    }
    let mut node = super::convert::convert_expr(inner)?;
    let Some(field) = node.fields.first_mut() else {
        return Ok(node);
    };
    if let Some(statement) = sole_statement(&field.value) {
        field.value = RakuAstFieldValue::Node(statement);
    }
    Ok(node)
}

fn child(value: &RakuAstFieldValue) -> Option<&RakuAstNode> {
    match value {
        RakuAstFieldValue::Node(value) => match value.view() {
            ValueView::RakuAst(node) => Some(node),
            _ => None,
        },
        _ => None,
    }
}

fn named<'a>(node: &'a RakuAstNode, name: Option<&str>) -> Option<&'a RakuAstField> {
    node.fields.iter().find(|field| field.name == name)
}

/// The one statement of `Block(body => Blockoid(StatementList(STATEMENT)))`.
fn sole_statement(block: &RakuAstFieldValue) -> Option<Value> {
    let block = child(block).filter(|b| b.class == RakuAstClass::Block)?;
    let blockoid = child(&named(block, Some("body"))?.value)?;
    let list = child(&named(blockoid, None)?.value)?;
    match list.fields.as_slice() {
        [RakuAstField {
            value: value @ RakuAstFieldValue::Node(statement),
            ..
        }] if child(value).is_some() => Some(statement.clone()),
        _ => None,
    }
}

/// The statements a prefix holds: the block's, or the bare statement itself.
// Cost: O(n), n = size of the child.
pub(super) fn lower_body(child: &RakuAstNode) -> Result<Vec<Stmt>, RuntimeError> {
    if child.class == RakuAstClass::Block {
        lower_block(child)
    } else {
        Ok(vec![lower_stmt(child)?])
    }
}
