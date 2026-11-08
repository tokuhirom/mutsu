//! The question rakudo 2026.09's `RakuAST::Block` answers about its
//! placeholder parameters (`src/Raku/ast/blocks-and-sub-signatures.rakumod`):
//! `has-placeholder-parameters` is true when the block's own body declares one
//! (`$^a`, `:$^n`, `@_`, `%_`); a placeholder in a nested block, pointy block
//! or routine belongs to that one. Measured over nine shapes.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::{RuntimeError, Value, ValueView};

// Cost: O(n), n = size of the block's own body.
fn declares_placeholder(node: &RakuAstNode) -> bool {
    node.fields.iter().any(|field| {
        let values: Vec<&Value> = match &field.value {
            RakuAstFieldValue::Node(v) => vec![v],
            RakuAstFieldValue::List(items) => items.iter().collect(),
            RakuAstFieldValue::Adverb(_) => Vec::new(),
        };
        values.into_iter().any(|v| match v.view() {
            ValueView::RakuAst(child) => match child.class {
                RakuAstClass::VarDeclarationPlaceholderPositional
                | RakuAstClass::VarDeclarationPlaceholderNamed
                | RakuAstClass::VarDeclarationPlaceholderSlurpyArray
                | RakuAstClass::VarDeclarationPlaceholderSlurpyHash => true,
                // Their placeholders are their own.
                RakuAstClass::Block
                | RakuAstClass::PointyBlock
                | RakuAstClass::Sub
                | RakuAstClass::Method
                | RakuAstClass::Submethod => false,
                _ => declares_placeholder(child),
            },
            _ => false,
        })
    })
}

/// `has-placeholder-parameters`.
// Cost: O(n), n = size of the block's own body.
pub(crate) fn has_placeholder_parameters(node: &RakuAstNode) -> Value {
    Value::truth(declares_placeholder(node))
}

/// Whether `class` is one rakudo's `Block.replace-body` is inherited by: the
/// block, the pointy block and every routine.
fn takes_a_body(class: RakuAstClass) -> bool {
    matches!(
        class,
        RakuAstClass::Block
            | RakuAstClass::PointyBlock
            | RakuAstClass::Sub
            | RakuAstClass::Method
            | RakuAstClass::Submethod
    )
}

/// `replace-body($new-body)`: the node's `body` field becomes `$new-body`,
/// which must be a `RakuAST::Blockoid`.
// Cost: O(f), f = fields of the node.
pub(crate) fn replace_body(node: &mut RakuAstNode, new_body: Value) -> Result<(), RuntimeError> {
    if !takes_a_body(node.class) {
        return Err(RuntimeError::new(
            "replace-body is only available on RakuAST::Block",
        ));
    }
    if !matches!(new_body.view(), ValueView::RakuAst(n) if n.class == RakuAstClass::Blockoid) {
        return Err(RuntimeError::new(format!(
            "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter \
             '$new-body'; expected RakuAST::Blockoid but got {}",
            crate::value::type_name::value_type_name(&new_body)
        )));
    }
    let value = RakuAstFieldValue::Node(new_body);
    match node.fields.iter_mut().find(|f| f.name == Some("body")) {
        Some(slot) => slot.value = value,
        None => node.fields.push(super::RakuAstField {
            name: Some("body"),
            value,
        }),
    }
    Ok(())
}

/// `replace-signature($new-signature)`: the routine's `signature` field becomes
/// `$new-signature`, which must be a `RakuAST::Signature`.
// Cost: O(f), f = fields of the node.
pub(crate) fn replace_signature(
    node: &mut RakuAstNode,
    new_signature: &Value,
) -> Result<(), RuntimeError> {
    if !matches!(
        node.class,
        RakuAstClass::Sub | RakuAstClass::Method | RakuAstClass::Submethod
    ) {
        return Err(RuntimeError::new(
            "replace-signature is only available on RakuAST::Routine",
        ));
    }
    if !matches!(new_signature.view(), ValueView::RakuAst(n) if n.class == RakuAstClass::Signature)
    {
        return Err(RuntimeError::new(format!(
            "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter \
             '$new-signature'; expected RakuAST::Signature but got {}",
            crate::value::type_name::value_type_name(new_signature)
        )));
    }
    let value = RakuAstFieldValue::Node(new_signature.clone());
    match node.fields.iter_mut().find(|f| f.name == Some("signature")) {
        Some(slot) => slot.value = value,
        None => node.fields.push(super::RakuAstField {
            name: Some("signature"),
            value,
        }),
    }
    Ok(())
}

/// The hidden field marking a statement-level block. Rakudo's `bare-block` is
/// parse-time state (`Block.new(:bare-block)` is rejected), `True` for a block
/// that is a whole statement (`{ 2 };`, also nested ones) and `False` for every
/// other block, a hand-built one included, so the statement conversion records
/// it and the row reads it back.
const BARE_BLOCK: &str = "bare-block";

/// Whether `field` is the hidden statement-level marker of a `Block`.
// Cost: O(1).
pub(super) fn is_bare_marker(node: &RakuAstNode, field: &super::RakuAstField) -> bool {
    node.class == RakuAstClass::Block && field.name == Some(BARE_BLOCK)
}

/// `node` marked as a statement-level block (a `PointyBlock` is never one).
// Cost: O(1).
pub(super) fn bare_block(mut node: RakuAstNode) -> RakuAstNode {
    if node.class == RakuAstClass::Block {
        node.fields.push(super::RakuAstField {
            name: Some(BARE_BLOCK),
            value: RakuAstFieldValue::Node(Value::truth(true)),
        });
    }
    node
}

/// `bare-block`: whether the block is a statement of its own.
// Cost: O(f), f = fields of the block.
pub(crate) fn bare_block_flag(node: &RakuAstNode) -> Value {
    Value::truth(node.fields.iter().any(|f| is_bare_marker(node, f)))
}
