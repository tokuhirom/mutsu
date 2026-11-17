//! `StatementList.insert-statement($i, $statement)` (#14916): put a statement
//! in front of the `$i`-th one (or after the last, for `$i` = the count), in
//! place, so every holder of the list sees it. As rakudo 2026.09, the answer is
//! the list's statements, and an index outside `0 .. count` dies with
//! `Cannot insert a statement at $i in a list of $n statements`.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{RuntimeError, Value, ValueView};

/// Insert `statement` at statement index `index` of the list `node`.
// Cost: O(f), f = fields of the list.
pub(crate) fn insert_statement(
    node: &mut RakuAstNode,
    index: i64,
    statement: Value,
) -> Result<Value, RuntimeError> {
    if node.class != RakuAstClass::StatementList {
        return Err(RuntimeError::new(
            "insert-statement is only available on RakuAST::StatementList",
        ));
    }
    let slots = statement_slots(node);
    let count = slots.len();
    let at = usize::try_from(index)
        .ok()
        .filter(|i| *i <= count)
        .ok_or_else(|| {
            RuntimeError::new(format!(
                "Cannot insert a statement at {index} in a list of {count} statements"
            ))
        })?;
    let is_node = match statement.view() {
        ValueView::RakuAst(_) => true,
        ValueView::Mixin(inner, _) => matches!(inner.view(), ValueView::RakuAst(_)),
        _ => false,
    };
    if !is_node {
        return Err(RuntimeError::new(
            "RakuAST::StatementList.insert-statement expects a RakuAST node",
        ));
    }
    let position = match slots.get(at) {
        Some(field) => *field,
        None => slots.last().map_or(node.fields.len(), |last| last + 1),
    };
    node.fields.insert(
        position,
        RakuAstField {
            name: None,
            value: RakuAstFieldValue::Node(statement),
        },
    );
    let statements = statement_slots(node)
        .into_iter()
        .map(|i| match &node.fields[i].value {
            RakuAstFieldValue::Node(v) => v.clone(),
            RakuAstFieldValue::List(items) => Value::array(items.clone()),
            RakuAstFieldValue::Adverb(name) => Value::str(name.to_string()),
        })
        .collect();
    Ok(Value::array(statements))
}

/// The field positions of the list's statements (not its origin or hidden
/// bookkeeping fields), in order.
// Cost: O(f), f = fields of the list.
fn statement_slots(node: &RakuAstNode) -> Vec<usize> {
    node.fields
        .iter()
        .enumerate()
        .filter(|(_, f)| !super::origin::is_origin(f) && !super::is_unvisited_field(node, f))
        .map(|(i, _)| i)
        .collect()
}
