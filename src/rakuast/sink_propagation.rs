//! Sink context of parsed RakuAST trees. A statement list sinks all but its
//! final statement; a sunk block also sinks its final statement.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::{RuntimeError, Value, ValueView};

const MAX_DEPTH: usize = 1024;

// Cost: O(n), n = nodes in the RakuAST tree.
pub(super) fn annotate(node: &mut RakuAstNode) -> Result<(), RuntimeError> {
    visit(node, false, false, 0)
}

// Cost: O(n), n = descendants of this node; depth is bounded.
fn visit(
    node: &mut RakuAstNode,
    sunk: bool,
    block_statement: bool,
    depth: usize,
) -> Result<(), RuntimeError> {
    if depth >= MAX_DEPTH {
        return Err(RuntimeError::new(
            "RakuAST sink propagation exceeded nesting limit",
        ));
    }
    node.origin.5 = sunk;
    node.origin.6 = block_statement;
    let is_list = node.class == RakuAstClass::StatementList;
    let last_statement = if is_list {
        node.fields.iter().rposition(|field| {
            field.name.is_none()
                && matches!(&field.value, RakuAstFieldValue::Node(value) if matches!(value.view(), ValueView::RakuAst(_)))
        })
    } else {
        None
    };
    let class_name = node.class.printed_name();
    for (index, field) in node.fields.iter_mut().enumerate() {
        let child_sunk = if is_list && field.name.is_none() {
            last_statement.is_some_and(|last| index < last || (index == last && sunk))
        } else if sunk && node.class == RakuAstClass::StatementExpression {
            field.name == Some("expression")
        } else if sunk && matches!(node.class, RakuAstClass::Block | RakuAstClass::PointyBlock) {
            field.name == Some("body")
        } else if sunk && node.class == RakuAstClass::Blockoid {
            field.name.is_none()
        } else if sunk && class_name.starts_with("RakuAST::Statement::") {
            matches!(field.name, Some("body" | "then" | "else" | "next"))
        } else {
            false
        };
        let child_block = is_list && field.name.is_none();
        let apply = |value: &mut Value| -> Result<(), RuntimeError> {
            if let ValueView::RakuAst(held) = value.view() {
                let mut owned = RakuAstNode::clone(held);
                visit(&mut owned, child_sunk, child_block, depth + 1)?;
                *value = Value::rakuast(Box::new(owned));
            }
            Ok(())
        };
        match &mut field.value {
            RakuAstFieldValue::Node(value) => apply(value)?,
            RakuAstFieldValue::List(values) => {
                for value in values {
                    apply(value)?;
                }
            }
            RakuAstFieldValue::Adverb(_) => {}
        }
    }
    Ok(())
}
