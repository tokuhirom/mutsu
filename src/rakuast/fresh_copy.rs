//! A fresh copy of a RakuAST tree, for a consumer that may edit it in place.
//!
//! Node edits (`set-initializer`, `replace-body`, ...) write through the shared
//! node, as in rakudo, where every parse builds new nodes. The Rust parser may
//! hand back the same node for a production it parses again at the same position
//! (its memo tables), so a slang action that edits the node a core rule gave it
//! (EEL's `:my` locals drop the declaration's initializer) would change what the
//! next parse of that position answers. The slang host's shims give each match its
//! own copy.

use super::{RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// `value` with every RakuAST node below it copied; any other value is shared.
// Cost: O(n), n = nodes of the tree.
pub(crate) fn fresh_copy(value: &Value) -> Value {
    match value.view() {
        ValueView::RakuAst(node) => Value::rakuast(Box::new(copy_node(node))),
        _ => value.clone(),
    }
}

// Cost: O(n), n = nodes of the tree.
fn copy_node(node: &RakuAstNode) -> RakuAstNode {
    RakuAstNode {
        class: node.class,
        origin: node.origin.clone(),
        fields: node
            .fields
            .iter()
            .map(|f| RakuAstField {
                name: f.name,
                value: match &f.value {
                    RakuAstFieldValue::Node(v) => RakuAstFieldValue::Node(fresh_copy(v)),
                    RakuAstFieldValue::List(items) => {
                        RakuAstFieldValue::List(items.iter().map(fresh_copy).collect())
                    }
                    RakuAstFieldValue::Adverb(a) => RakuAstFieldValue::Adverb(a),
                },
            })
            .collect(),
    }
}
