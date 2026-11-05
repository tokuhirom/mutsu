//! The question rakudo 2026.09's `RakuAST::Block` answers about its
//! placeholder parameters (`src/Raku/ast/blocks-and-sub-signatures.rakumod`):
//! `has-placeholder-parameters` is true when the block's own body declares one
//! (`$^a`, `:$^n`, `@_`, `%_`); a placeholder in a nested block, pointy block
//! or routine belongs to that one. Measured over nine shapes.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

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
