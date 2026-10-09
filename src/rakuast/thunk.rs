//! The priming scope the parser planted, kept across the RakuAST boundary.
//!
//! Where a `*` is primed is decided by the parser's grammar positions, not by
//! the shape of the tree alone: `(* + 1).WHAT` and `* + 1.WHAT` differ in the
//! parentheses, and rakudo's tree for the first drops them from the postfix's
//! operand. Re-deriving the scope from the tree therefore primes the whole
//! postfix chain (`WhateverCode.new`) where the source primed `* + 1` only.
//!
//! Rakudo carries the same decision on the node itself (its dump shows the
//! `WhateverCode` thunk beside the `ApplyInfix`). The converter does likewise:
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

/// `node` marked as the scope of a priming.
// Cost: O(f), f = fields of `node`.
pub(super) fn mark(mut node: RakuAstNode) -> RakuAstNode {
    if !is_thunk(&node) {
        node.fields.push(RakuAstField {
            name: Some(FIELD),
            value: RakuAstFieldValue::Node(Value::truth(true)),
        });
    }
    node
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
        class: node.class,
        fields: node
            .fields
            .iter()
            .filter(|f| !is_marker(f))
            .cloned()
            .collect(),
    }
}
