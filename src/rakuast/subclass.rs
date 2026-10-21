//! The native node stored by a user subclass of a RakuAST class.

use super::{RakuAstFieldValue, RakuAstNode, render};
use crate::value::{RuntimeError, Value, ValueView};

/// Render a node inherited by a user subclass under that subclass's name.
// Cost: O(n), n = constructor text length.
pub fn gist(class_name: &str, node: &RakuAstNode) -> String {
    gist_at(class_name, node, 0)
}

pub(super) fn gist_at(class_name: &str, node: &RakuAstNode, indent: usize) -> String {
    let rendered = render::render_node(node, indent);
    let parent = node.class.printed_name();
    match rendered.strip_prefix(parent) {
        Some(rest) => format!("{class_name}{rest}"),
        None => rendered,
    }
}

pub(crate) fn is_node_value(value: &Value) -> bool {
    let mut current = value;
    loop {
        match current.view() {
            ValueView::RakuAst(_) => return true,
            ValueView::Instance { attributes, .. } => return attributes.rakuast_base().is_some(),
            ValueView::Mixin(inner, _) => current = inner,
            _ => return false,
        }
    }
}

/// Replace a user subclass in a child slot with the native node its parent
/// constructor built. Only ancestors of such a slot are cloned; ordinary ASTs
/// stay borrowed throughout lowering.
pub(super) fn normalize_for_lower(
    node: &RakuAstNode,
    depth: usize,
) -> Result<Option<RakuAstNode>, RuntimeError> {
    if depth >= 512 {
        return Err(RuntimeError::new(
            "RakuAST: node nesting exceeds 512 levels",
        ));
    }
    let mut normalized = None;
    for (index, field) in node.fields.iter().enumerate() {
        let replacement = match &field.value {
            RakuAstFieldValue::Node(value) => {
                normalize_child_value(value, depth + 1)?.map(RakuAstFieldValue::Node)
            }
            RakuAstFieldValue::List(items) => {
                let mut changed = None;
                for (item_index, item) in items.iter().enumerate() {
                    if let Some(value) = normalize_child_value(item, depth + 1)? {
                        let output = changed.get_or_insert_with(|| items.clone());
                        output[item_index] = value;
                    }
                }
                changed.map(RakuAstFieldValue::List)
            }
            RakuAstFieldValue::Adverb(_) => None,
        };
        if let Some(replacement) = replacement {
            let output = normalized.get_or_insert_with(|| node.clone());
            output.fields[index].value = replacement;
        }
    }
    Ok(normalized)
}

fn normalize_child_value(value: &Value, depth: usize) -> Result<Option<Value>, RuntimeError> {
    if depth >= 512 {
        return Err(RuntimeError::new(
            "RakuAST: node nesting exceeds 512 levels",
        ));
    }
    match value.view() {
        ValueView::Instance { attributes, .. } => {
            let Some(base) = attributes.rakuast_base() else {
                return Ok(None);
            };
            Ok(Some(
                normalize_child_value(&base, depth + 1)?.unwrap_or(base),
            ))
        }
        ValueView::RakuAst(node) => {
            Ok(normalize_for_lower(node, depth + 1)?.map(|node| Value::rakuast(Box::new(node))))
        }
        ValueView::Mixin(inner, mixins) => Ok(normalize_child_value(inner, depth + 1)?
            .map(|inner| Value::mixin_with_state(inner, (**mixins).clone()))),
        _ => Ok(None),
    }
}
