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

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// The hidden field's name.
const FIELD: &str = "thunk";
const BLOCK_FIELD: &str = "block-thunk";

// Cost: O(1).
fn is_prime_marker(field: &RakuAstField) -> bool {
    field.name == Some(FIELD)
}

/// Whether `field` is the hidden marker, which no renderer shows.
// Cost: O(1).
pub(super) fn is_marker(field: &RakuAstField) -> bool {
    is_prime_marker(field) || field.name == Some(BLOCK_FIELD)
}

/// A lazy list-infix operand that Rakudo evaluates through a block thunk.
// Cost: O(1).
fn mark_block(mut node: RakuAstNode) -> RakuAstNode {
    node.fields.push(RakuAstField {
        name: Some(BLOCK_FIELD),
        value: RakuAstFieldValue::Node(Value::truth(true)),
    });
    node
}

/// Whether the node carries a lazy operand's block thunk.
// Cost: O(f), f = fields of the node.
fn has_block_thunk(node: &RakuAstNode) -> bool {
    node.fields
        .iter()
        .any(|field| field.name == Some(BLOCK_FIELD))
}

/// Mark a nonconstant operand after the first one of a lazy list infix. A
/// WhateverCode curry is already a value-producing prime thunk of its own.
// Cost: O(n), n = nodes in the operand's compile-time-value check.
pub(super) fn lazy_operand(
    node: RakuAstNode,
    index: usize,
    op: &crate::token_kind::TokenKind,
) -> RakuAstNode {
    use crate::token_kind::TokenKind;
    let lazy = matches!(
        op,
        TokenKind::AndThen | TokenKind::OrElse | TokenKind::NotAndThen
    );
    if lazy
        && index > 0
        && !is_thunk(&node)
        && !super::compile_time_value::has_compile_time_value(&node).truthy()
    {
        mark_block(node)
    } else {
        node
    }
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
        .find(|f| is_prime_marker(f))
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
    node.fields.iter().any(is_prime_marker)
}

/// The expression thunks carried by a parsed node. The priming mark already
/// records the exact curry boundary, including boundaries that parentheses
/// remove from the public tree. A nested marked expression owns its own thunk.
// Cost: O(n), n = nodes within the marked expression.
pub(crate) fn modeled_thunks(node: &RakuAstNode) -> Vec<Value> {
    if !is_thunk(node) {
        return if has_block_thunk(node) {
            vec![thunk_value(
                RakuAstClass::BlockThunk,
                "Block thunk",
                String::new(),
            )]
        } else {
            Vec::new()
        };
    }
    let mut arguments = 0usize;
    let mut stack = vec![node];
    while let Some(current) = stack.pop() {
        if current.class == RakuAstClass::WhateverCodeArgument {
            arguments += 1;
        }
        for field in &current.fields {
            let values: &[Value] = match &field.value {
                RakuAstFieldValue::Node(value) => std::slice::from_ref(value),
                RakuAstFieldValue::List(values) => values,
                RakuAstFieldValue::Adverb(_) => continue,
            };
            for value in values {
                if let ValueView::RakuAst(child) = value.view()
                    && !is_thunk(child)
                {
                    stack.push(child);
                }
            }
        }
    }
    vec![thunk_value(
        RakuAstClass::PrimeThunk,
        "WhateverCode",
        format!("⋐{}⋑", "🔆".repeat(arguments)),
    )]
}

/// A modeled thunk with its public kind and details.
// Cost: O(d), d = length of `details`.
fn thunk_value(class: RakuAstClass, kind: &str, details: String) -> Value {
    Value::rakuast(Box::new(RakuAstNode {
        class,
        fields: vec![
            RakuAstField {
                name: Some("kind"),
                value: RakuAstFieldValue::Node(Value::str(kind.to_string())),
            },
            RakuAstField {
                name: Some("details"),
                value: RakuAstFieldValue::Node(Value::str(details)),
            },
        ],
        origin: Default::default(),
    }))
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
