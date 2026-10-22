//! `.AST` of a conditional the parser built (ADR-10723 Stage 2): the RakuAST
//! node rendered from the parser's [`Conditional`], with the origins the
//! parser recorded on its blocks. Nothing here reverse-engineers a desugaring;
//! the statement's own origin is added by the statement list, as for every
//! statement.

use super::convert::{
    convert_expr, node_field, pointy_block, signature_block_node, topic_block_node,
};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::parsed::{BlockParams, ClauseKind, Conditional, ConditionalKind, ParsedBlock};
use crate::value::{RuntimeError, Value, ValueView};

/// `Statement::If` / `::Unless` / `::With` / `::Without` for `node`.
// Cost: O(n), n = size of the conditional.
pub(super) fn conditional_node(node: &Conditional) -> Result<RakuAstNode, RuntimeError> {
    let topical = matches!(node.kind, ConditionalKind::With | ConditionalKind::Without);
    let (class, block_name) = match node.kind {
        ConditionalKind::If => (RakuAstClass::StatementIf, "then"),
        ConditionalKind::Unless => (RakuAstClass::StatementUnless, "body"),
        ConditionalKind::With => (RakuAstClass::StatementWith, "then"),
        ConditionalKind::Without => (RakuAstClass::StatementWithout, "body"),
    };
    let mut fields = vec![
        node_field(Some("condition"), convert_expr(&node.condition)?),
        node_field(Some(block_name), block_node(&node.then, topical)?),
    ];
    let mut elsifs = Vec::with_capacity(node.clauses.len());
    // An `else` topicalizes the value the clause before it tested, when that
    // clause topicalizes (`with`, `orwith`).
    let mut else_topical = topical;
    for clause in &node.clauses {
        let orwith = clause.kind == ClauseKind::Orwith;
        let clause_node = RakuAstNode {
            class: if orwith {
                RakuAstClass::StatementOrwith
            } else {
                RakuAstClass::StatementElsif
            },
            fields: vec![
                node_field(Some("condition"), convert_expr(&clause.condition)?),
                node_field(Some("then"), block_node(&clause.then, orwith)?),
            ],
        };
        elsifs.push(Value::rakuast(Box::new(clause_node)));
        else_topical = orwith;
    }
    if !elsifs.is_empty() {
        fields.push(RakuAstField {
            name: Some("elsifs"),
            value: RakuAstFieldValue::List(elsifs),
        });
    }
    if let Some(block) = &node.else_block {
        fields.push(node_field(Some("else"), block_node(block, else_topical)?));
    }
    Ok(RakuAstNode { class, fields })
}

/// A clause's `Block` (`implicit-topic` when the clause topicalizes) or
/// `PointyBlock`, with the origins of the block, its `Blockoid` and its
/// `StatementList`.
// Cost: O(n), n = size of the block.
fn block_node(block: &ParsedBlock, topical: bool) -> Result<RakuAstNode, RuntimeError> {
    let node = match &block.params {
        BlockParams::None if topical => topic_block_node(&block.body)?,
        BlockParams::None => signature_block_node(&block.body)?,
        BlockParams::List(defs) => pointy_block(defs, &block.body, None)?,
        BlockParams::Topic { def, .. } => pointy_block(def.as_slice(), &block.body, None)?,
    };
    Ok(with_block_origins(node, block))
}

/// `node` (a `Block` or `PointyBlock` over a `Blockoid` over a
/// `StatementList`) with the three origins the parser recorded for `block`.
// Cost: O(f), f = fields of the three nodes.
fn with_block_origins(mut node: RakuAstNode, block: &ParsedBlock) -> RakuAstNode {
    node.fields.extend(super::origin::span_field(block.span));
    let Some(body) = node.fields.iter_mut().find(|f| f.name == Some("body")) else {
        return node;
    };
    let RakuAstFieldValue::Node(value) = &body.value else {
        return node;
    };
    let ValueView::RakuAst(blockoid) = value.view() else {
        return node;
    };
    let mut blockoid = RakuAstNode::clone(blockoid);
    blockoid
        .fields
        .extend(super::origin::span_field(block.blockoid));
    if let Some(list_field) = blockoid.fields.first_mut()
        && let RakuAstFieldValue::Node(list) = &list_field.value
        && let ValueView::RakuAst(list) = list.view()
    {
        let mut list = RakuAstNode::clone(list);
        list.fields.extend(super::origin::span_field(block.list));
        list_field.value = RakuAstFieldValue::Node(Value::rakuast(Box::new(list)));
    }
    body.value = RakuAstFieldValue::Node(Value::rakuast(Box::new(blockoid)));
    node
}
