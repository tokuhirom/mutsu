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
