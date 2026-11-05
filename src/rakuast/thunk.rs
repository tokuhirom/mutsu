//! The priming scope the parser planted, kept across the RakuAST boundary.
//!
//! Where a `*` is primed is decided by the parser's grammar positions, not by
//! the shape of the tree alone: `(* + 1).WHAT` and `* + 1.WHAT` differ in the
//! parentheses, and rakudo's tree for the first drops them from the postfix's
//! operand. Re-deriving the scope from the tree therefore primes the whole
//! postfix chain (`WhateverCode.new`) where the source primed `* + 1` only.
//!
//! Rakudo carries the same decision on the node itself (its dump shows the
//! `WhateverCode` thunk beside the `ApplyInfix`). The view does likewise:
//! the node that an [`crate::ast::Expr::WhateverCurry`] wrapped carries a hidden
//! `thunk` field, and lowering puts the marker back around it. A hand-built tree
//! has none, so it is still primed from its shape.

use super::{RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::Value;

/// The hidden field's name.
const FIELD: &str = "thunk";

/// Whether `field` is the hidden marker, which no renderer shows.
// Cost: O(1).
pub(super) fn is_marker(field: &RakuAstField) -> bool {
    field.name == Some(FIELD)
}

/// `node` marked as the scope of a priming. `source` is the text the scope
/// was written as (`*.not`), which a WhateverCode's `.raku` prints; the mark
/// carries it as its value, so it survives `EVAL($ast)`.
// Cost: O(f), f = fields of `node`.
pub(super) fn mark(mut node: RakuAstNode, source: Option<&str>) -> RakuAstNode {
    if !is_thunk(&node) {
        node.fields.push(RakuAstField {
            name: Some(FIELD),
            value: RakuAstFieldValue::Node(match source {
                Some(text) => Value::str(text.to_string()),
                None => Value::truth(true),
            }),
        });
    }
    node
}

/// [`mark`] applied to a viewed scope body, with the scope's recorded source.
// Cost: O(f), f = fields of the node.
pub(super) fn mark_of(
    node: Result<RakuAstNode, crate::value::RuntimeError>,
    source: &Option<String>,
) -> Result<RakuAstNode, crate::value::RuntimeError> {
    node.map(|node| mark(node, source.as_deref()))
}

/// The source text the mark carries, when it has one.
// Cost: O(f), f = fields of `node`.
pub(super) fn source(node: &RakuAstNode) -> Option<String> {
    node.fields
        .iter()
        .find(|f| is_marker(f))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::Node(value) => match value.view() {
                crate::value::ValueView::Str(text) => Some(text.to_string()),
                _ => None,
            },
            _ => None,
        })
}

/// Whether `node` is marked as the scope of a priming.
// Cost: O(f), f = fields of `node`.
pub(super) fn is_thunk(node: &RakuAstNode) -> bool {
    node.fields.iter().any(is_marker)
}

/// `node` without the mark.
// Cost: O(f), f = fields of `node`.
pub(super) fn unmark(node: &RakuAstNode) -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
        class: node.class,
        fields: node
            .fields
            .iter()
            .filter(|f| !is_marker(f))
            .cloned()
            .collect(),
    }
}
