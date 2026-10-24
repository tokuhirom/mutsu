//! `.AST` of a construct the parser built in its RakuAST shape (ADR-10723
//! Stage 2): the RakuAST node rendered from the parser's [`ParsedStmt`], with
//! the origins the parser recorded on its blocks. Nothing here reverse-engineers a desugaring;
//! the statement's own origin is added by the statement list, as for every
//! statement.

use super::convert::{
    block_node, convert_expr, label_fields, loop_setup_node, node_field, pointy_block,
    signature_block_node, statement_expression, topic_block_node,
};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::parsed::{
    BlockParams, BlockSpans, ClauseKind, Conditional, ConditionalKind, LoopKind, LoopStmt,
    ParsedBlock, ParsedStmt,
};
use crate::value::{RuntimeError, Value, ValueView};

/// The RakuAST node of a construct the parser built in its RakuAST shape.
// Cost: O(n), n = size of the construct.
pub(super) fn parsed_node(node: &ParsedStmt) -> Result<RakuAstNode, RuntimeError> {
    Ok(match node {
        ParsedStmt::Conditional(c) => conditional_node(c)?,
        ParsedStmt::Given { topic, block } => RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::StatementGiven,
            fields: vec![
                node_field(Some("source"), convert_expr(topic)?),
                node_field(Some("body"), view_block(block, BlockKind::Topic)?),
            ],
        },
        ParsedStmt::When { condition, block } => RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::StatementWhen,
            fields: vec![
                node_field(Some("condition"), convert_expr(condition)?),
                node_field(Some("body"), view_block(block, BlockKind::Signature)?),
            ],
        },
        ParsedStmt::Default(block) => RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::StatementDefault,
            fields: vec![node_field(
                Some("body"),
                view_block(block, BlockKind::Plain)?,
            )],
        },
        ParsedStmt::WhileLoop(w) => {
            let mut fields = label_fields(&w.label);
            fields.push(node_field(Some("condition"), convert_expr(&w.condition)?));
            fields.push(node_field(
                Some("body"),
                view_block(&w.block, BlockKind::Signature)?,
            ));
            RakuAstNode {
                origin: Default::default(),
                class: if w.until {
                    RakuAstClass::StatementLoopUntil
                } else {
                    RakuAstClass::StatementLoopWhile
                },
                fields,
            }
        }
        ParsedStmt::BareBlock(block) => {
            statement_expression(view_block(block, BlockKind::Signature)?)
        }
        ParsedStmt::Loop(l) => loop_node(l)?,
        ParsedStmt::BindDecl(decl) => {
            let parts = super::convert::var_decl_parts(decl)?;
            super::convert::var_decl_statement(parts, true)?
                .ok_or_else(|| super::convert::unsupported("binding declaration"))?
        }
        ParsedStmt::DoesDecl { decl, role, init } => {
            super::convert::does_declaration(decl, role, init.as_ref())?
                .ok_or_else(|| super::convert::unsupported("does declaration"))?
        }
        ParsedStmt::For { stmt, block } => {
            let mut node = super::convert::convert_stmt(stmt)?
                .ok_or_else(|| super::convert::unsupported("empty for loop"))?;
            if let Some(field) = node.fields.iter_mut().find(|f| f.name == Some("body"))
                && let RakuAstFieldValue::Node(value) = &field.value
                && let ValueView::RakuAst(body) = value.view()
            {
                let body = with_block_origins(RakuAstNode::clone(body), *block);
                field.value = RakuAstFieldValue::Node(Value::rakuast(Box::new(body)));
            }
            node
        }
    })
}

/// `Statement::Loop` (C-style or bare) or `Statement::Loop::RepeatWhile` /
/// `::RepeatUntil`.
// Cost: O(n), n = size of the loop.
fn loop_node(l: &LoopStmt) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = label_fields(&l.label);
    if l.kind == LoopKind::Loop {
        if let Some(init) = l.init.as_deref() {
            fields.push(node_field(Some("setup"), loop_setup_node(init)?));
        }
        if let Some(cond) = &l.condition {
            fields.push(node_field(Some("condition"), convert_expr(cond)?));
        }
        if let Some(step) = &l.step {
            fields.push(node_field(Some("increment"), convert_expr(step)?));
        }
        fields.push(node_field(
            Some("body"),
            view_block(&l.block, BlockKind::Plain)?,
        ));
        return Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::StatementLoop,
            fields,
        });
    }
    fields.push(node_field(
        Some("body"),
        view_block(&l.block, BlockKind::Signature)?,
    ));
    if let Some(cond) = &l.condition {
        fields.push(node_field(Some("condition"), convert_expr(cond)?));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: if l.kind == LoopKind::RepeatUntil {
            RakuAstClass::StatementLoopRepeatUntil
        } else {
            RakuAstClass::StatementLoopRepeatWhile
        },
        fields,
    })
}

/// Which `Block` an unparameterized block renders as.
#[derive(Clone, Copy)]
enum BlockKind {
    /// `may-have-signature`: placeholders may give it one.
    Signature,
    /// `implicit-topic`: it topicalizes what its statement tested.
    Topic,
    /// No flags.
    Plain,
}

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
        node_field(Some(block_name), clause_block(&node.then, topical)?),
    ];
    let mut elsifs = Vec::with_capacity(node.clauses.len());
    // An `else` topicalizes the value the clause before it tested, when that
    // clause topicalizes (`with`, `orwith`).
    let mut else_topical = topical;
    for clause in &node.clauses {
        let orwith = clause.kind == ClauseKind::Orwith;
        let clause_node = RakuAstNode {
            origin: Default::default(),
            class: if orwith {
                RakuAstClass::StatementOrwith
            } else {
                RakuAstClass::StatementElsif
            },
            fields: vec![
                node_field(Some("condition"), convert_expr(&clause.condition)?),
                node_field(Some("then"), clause_block(&clause.then, orwith)?),
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
        fields.push(node_field(Some("else"), clause_block(block, else_topical)?));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class,
        fields,
    })
}

/// A clause's `Block` (`implicit-topic` when the clause topicalizes) or
/// `PointyBlock`.
// Cost: O(n), n = size of the block.
fn clause_block(block: &ParsedBlock, topical: bool) -> Result<RakuAstNode, RuntimeError> {
    view_block(
        block,
        if topical {
            BlockKind::Topic
        } else {
            BlockKind::Signature
        },
    )
}

/// `block` as a `PointyBlock` when it has parameters, else as `kind`, with the
/// origins of the block, its `Blockoid` and its `StatementList`.
// Cost: O(n), n = size of the block.
fn view_block(block: &ParsedBlock, kind: BlockKind) -> Result<RakuAstNode, RuntimeError> {
    let node = match &block.params {
        BlockParams::None => match kind {
            BlockKind::Signature => signature_block_node(&block.body)?,
            BlockKind::Topic => topic_block_node(&block.body)?,
            BlockKind::Plain => block_node(&block.body)?,
        },
        BlockParams::List(defs) => pointy_block(defs, &block.body, None)?,
        BlockParams::Topic { def, .. } => pointy_block(
            def.as_deref().map_or(&[], std::slice::from_ref),
            &block.body,
            None,
        )?,
    };
    Ok(with_block_origins(node, block.spans()))
}

/// `node` (a `Block` or `PointyBlock` over a `Blockoid` over a
/// `StatementList`) with the three origins the parser recorded for `block`.
// Cost: O(f), f = fields of the three nodes.
fn with_block_origins(mut node: RakuAstNode, block: BlockSpans) -> RakuAstNode {
    super::origin::set_span(&mut node, block.span);
    // `-> { ... }`: the empty `Signature` sits right after the arrow.
    if node.class == super::RakuAstClass::PointyBlock
        && node.fields.iter().all(|f| f.name != Some("signature"))
        && let Some(arrow) = super::unit_source::with_text(block.span, |text| {
            ["->", "<->"]
                .into_iter()
                .find(|a| text.starts_with(a))
                .map(str::len)
        })
    {
        let at = block.span.from as usize + arrow;
        super::empty_fields::set_span(&mut node, "signature", crate::ast::SrcSpan::new(at, at));
    }
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
    super::origin::set_span(&mut blockoid, block.blockoid);
    if let Some(list_field) = blockoid.fields.first_mut()
        && let RakuAstFieldValue::Node(list) = &list_field.value
        && let ValueView::RakuAst(list) = list.view()
    {
        let mut list = RakuAstNode::clone(list);
        super::origin::set_span(&mut list, block.list);
        list_field.value = RakuAstFieldValue::Node(Value::rakuast(Box::new(list)));
    }
    body.value = RakuAstFieldValue::Node(Value::rakuast(Box::new(blockoid)));
    node
}
