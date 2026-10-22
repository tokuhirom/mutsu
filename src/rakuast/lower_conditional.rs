use super::super::control_signature;
use super::*;
use crate::ast::SrcSpan;
use crate::ast::parsed::{
    BlockParams, ClauseKind, Conditional, ConditionalClause, ConditionalKind, ParsedBlock,
};

/// Lower a `Statement::If` / `::Unless` / `::With` / `::Without` node: read it
/// into the [`Conditional`] the parser builds for the same source and lower
/// that, so a hand-built or round-tripped conditional runs exactly as a parsed
/// one (ADR-10723 Stage 2, one lowering per family).
// Cost: O(n), n = size of the conditional.
pub(super) fn lower_conditional(
    node: &RakuAstNode,
    kind: ConditionalKind,
) -> Result<Stmt, RuntimeError> {
    super::super::conditional_lower::lower(read_conditional(node, kind)?)
}

// Cost: O(n), n = size of the conditional.
fn read_conditional(
    node: &RakuAstNode,
    kind: ConditionalKind,
) -> Result<Conditional, RuntimeError> {
    let block_name = match kind {
        ConditionalKind::If | ConditionalKind::With => "then",
        ConditionalKind::Unless | ConditionalKind::Without => "body",
    };
    let has_chain = node
        .fields
        .iter()
        .any(|f| f.name == Some("else") || f.name == Some("elsifs"));
    // rakudo rejects `unless` / `without` with continuation clauses at compile
    // time, so a node carrying them is not a shape it could have produced.
    if matches!(kind, ConditionalKind::Unless | ConditionalKind::Without) && has_chain {
        return Err(unsupported(node));
    }
    let topical = matches!(kind, ConditionalKind::With | ConditionalKind::Without);
    let condition = lower_expr(named_child(node, "condition")?)?;
    let then = read_block(named_child(node, block_name)?, topical)?;
    let items = match node.fields.iter().find(|f| f.name == Some("elsifs")) {
        Some(field) => match &field.value {
            RakuAstFieldValue::List(items) => items.as_slice(),
            _ => return Err(unsupported(node)),
        },
        None => &[],
    };
    let mut clauses = Vec::with_capacity(items.len());
    for item in items {
        let ValueView::RakuAst(clause) = item.view() else {
            return Err(unsupported(node));
        };
        let kind = match clause.class {
            RakuAstClass::StatementElsif => ClauseKind::Elsif,
            RakuAstClass::StatementOrwith => ClauseKind::Orwith,
            _ => return Err(unsupported(clause)),
        };
        clauses.push(ConditionalClause {
            kind,
            condition: lower_expr(named_child(clause, "condition")?)?,
            then: read_block(named_child(clause, "then")?, kind == ClauseKind::Orwith)?,
        });
    }
    let else_block = match node.fields.iter().find(|f| f.name == Some("else")) {
        // The parser reads the `else` of a clause-less `with` with the `with`
        // block's own parameter rules, and every other `else` with `if`'s.
        Some(_) => Some(read_block(
            named_child(node, "else")?,
            topical && clauses.is_empty(),
        )?),
        None => None,
    };
    Ok(Conditional {
        kind,
        condition,
        then,
        clauses,
        else_block,
    })
}

/// A clause's block. `topic` says the clause binds one topic parameter (the
/// `with` family) rather than a parameter list. A hand-built block has no
/// positions.
// Cost: O(n), n = size of the block.
fn read_block(block: &RakuAstNode, topic: bool) -> Result<ParsedBlock, RuntimeError> {
    let params = if block.class != RakuAstClass::PointyBlock {
        BlockParams::None
    } else if topic {
        let mut defs = control_signature::params(block)?;
        if defs.len() > 1 {
            return Err(RuntimeError::new(
                "RakuAST: with-family pointy block requires one parameter",
            ));
        }
        let def = defs.pop();
        BlockParams::Topic {
            name: def.as_ref().map(|def| def.name.clone()),
            def: def.map(Box::new),
        }
    } else {
        BlockParams::List(control_signature::params(block)?)
    };
    Ok(ParsedBlock {
        params,
        body: lower_block(block)?,
        span: SrcSpan::NONE,
        blockoid: SrcSpan::NONE,
        list: SrcSpan::NONE,
    })
}
