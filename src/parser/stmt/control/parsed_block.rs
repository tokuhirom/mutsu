//! Blocks of the construct families the parser builds in their RakuAST shape
//! (ADR-10723 Stage 2, `ast::parsed`), with the positions Rakudo gives a
//! `Block`, its `Blockoid` and its `StatementList`.

use super::*;
use crate::ast::SrcSpan;
use crate::ast::parsed::{BlockParams, Conditional, ParsedBlock, ParsedStmt};

/// Parse the `{ ... }` at `input` as the block of a migrated construct.
/// `start` is where the block was written from: `input` itself, or the `->`
/// of its parameters, which `params` holds.
// Cost: O(n), n = length of the block's source.
pub(super) fn parsed_block<'a>(
    start: &'a str,
    input: &'a str,
    params: BlockParams,
) -> PResult<'a, ParsedBlock> {
    let scope_params: &[ParamDef] = match &params {
        BlockParams::None => &[],
        BlockParams::List(defs) => defs,
        BlockParams::Topic { def, .. } => def.as_slice(),
    };
    let (rest, body) = block_with_pointy_params(input, scope_params)?;
    let blockoid = crate::parser::primary::source_span(input, rest);
    Ok((
        rest,
        ParsedBlock {
            span: crate::parser::primary::source_span(start, rest),
            list: statement_list_span(&body, blockoid),
            blockoid,
            params,
            body,
        },
    ))
}

/// The range of a block's statements: from the start of the first to the end
/// of the last, its `;` terminators included. Rakudo places an empty list just
/// after the `{`.
// Cost: O(s), s = statements of `body`.
fn statement_list_span(body: &[Stmt], blockoid: SrcSpan) -> SrcSpan {
    let mut spans = body.iter().filter_map(|stmt| match stmt {
        Stmt::SetLine(_, span) if span.is_some() => Some(*span),
        _ => None,
    });
    match spans.next() {
        Some(first) => {
            let last = spans.last().unwrap_or(first);
            crate::parser::primary::extend_over_semicolons(SrcSpan::new(
                first.from as usize,
                last.to as usize,
            ))
        }
        None if blockoid.is_some() && body.iter().all(Stmt::is_marker) => {
            let inside = blockoid.from as usize + 1;
            SrcSpan::new(inside, inside)
        }
        None => SrcSpan::NONE,
    }
}

/// The statement a conditional the parser built becomes: kept in its RakuAST
/// shape for a parse the RakuAST conversion reads, lowered where it is built
/// for every other parse. The kept node is lowered once as well, so the same
/// source raises the same compile-time errors either way.
// Cost: O(n), n = size of the conditional (twice when the node is kept).
pub(super) fn finish(node: Conditional) -> Result<Stmt, crate::value::RuntimeError> {
    if crate::ast::spelled::keeping() {
        crate::rakuast::lower_conditional_node(node.clone())?;
        return Ok(Stmt::Ast(Box::new(ParsedStmt::Conditional(node))));
    }
    crate::rakuast::lower_conditional_node(node)
}
