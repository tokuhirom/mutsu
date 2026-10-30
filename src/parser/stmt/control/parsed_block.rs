//! Blocks of the construct families the parser builds in their RakuAST shape
//! (ADR-10723 Stage 2, `ast::parsed`), with the positions Rakudo gives a
//! `Block`, its `Blockoid` and its `StatementList`.

use super::*;
use crate::ast::SrcSpan;
use crate::ast::parsed::{BlockParams, BlockSpans, ParsedBlock, ParsedStmt};

/// Parse the `{ ... }` at `input` as the block of a migrated construct.
/// `start` is where the block was written from: `input` itself, or the `->`
/// of its parameters, which `params` holds.
// Cost: O(n), n = length of the block's source.
pub(crate) fn parsed_block<'a>(
    start: &'a str,
    input: &'a str,
    params: BlockParams,
) -> PResult<'a, ParsedBlock> {
    let scope_params: &[ParamDef] = match &params {
        BlockParams::None => &[],
        BlockParams::List(defs) => defs,
        BlockParams::Topic { def, .. } => def.as_deref().map_or(&[], std::slice::from_ref),
    };
    let (rest, body) = block_with_pointy_params(input, scope_params)?;
    let spans = block_spans(start, input, rest, &body);
    Ok((
        rest,
        ParsedBlock {
            span: spans.span,
            list: spans.list,
            blockoid: spans.blockoid,
            params,
            body,
        },
    ))
}

/// Where a block whose `{` is at `brace` and whose source ends at `rest` was
/// written; `start` is the `->` of its parameters, or `brace`. When `start`
/// is not a `->`, the block starts at its brace.
// Cost: O(s), s = statements of `body`.
pub(crate) fn block_spans(start: &str, brace: &str, rest: &str, body: &[Stmt]) -> BlockSpans {
    let start = if start.starts_with("->") || start.starts_with("<->") {
        start
    } else {
        brace
    };
    let blockoid = crate::parser::primary::source_span(brace, rest);
    BlockSpans {
        span: crate::parser::primary::source_span(start, rest),
        blockoid,
        list: statement_list_span(body, blockoid),
    }
}

/// The statement a `for` loop becomes: the loop itself, with the positions of
/// its block, for the RakuAST conversion (ADR-10723 Stage 2).
// Cost: O(1).
pub(crate) fn finish_for(stmt: Stmt, block: BlockSpans) -> Stmt {
    Stmt::Ast(Box::new(ParsedStmt::For {
        stmt: Box::new(stmt),
        block,
    }))
}

/// The range of a block's statements (see
/// [`crate::parser::primary::statement_list_range`]). Rakudo places an empty
/// list just after the `{`.
// Cost: O(s + n), s = statements of `body`, n = length of the text around them.
fn statement_list_span(body: &[Stmt], blockoid: SrcSpan) -> SrcSpan {
    let mut spans = body.iter().filter_map(|stmt| match stmt {
        Stmt::SetLine(_, span) if span.is_some() => Some(*span),
        _ => None,
    });
    match spans.next() {
        Some(first) if blockoid.is_some() => {
            let last = spans.next_back().unwrap_or(first);
            let inside = SrcSpan::new(blockoid.from as usize + 1, blockoid.to as usize - 1);
            crate::parser::primary::statement_list_range(
                first.from as usize,
                last.to as usize,
                inside,
            )
        }
        None if blockoid.is_some() && body.iter().all(Stmt::is_marker) => {
            let inside = blockoid.from as usize + 1;
            SrcSpan::new(inside, inside)
        }
        _ => SrcSpan::NONE,
    }
}

/// The statement a construct the parser built becomes: kept in its RakuAST
/// shape for the conversion. The node is lowered once here as well, so the
/// compile-time errors of the construct are raised at parse time.
// Cost: O(n), n = size of the construct (twice).
pub(crate) fn finish(node: ParsedStmt) -> Result<Stmt, crate::value::RuntimeError> {
    crate::rakuast::lower_parsed_stmt(node.clone())?;
    Ok(Stmt::Ast(Box::new(node)))
}

/// [`finish`] for a node whose lowering cannot fail.
// Cost: O(n), n = size of the construct.
pub(crate) fn finish_total(node: ParsedStmt) -> Stmt {
    finish(node).expect("the lowering of this node is total")
}

/// A package-like declaration with its header: kept as a
/// `ParsedStmt::PackageHeader` when it has one, the bare declaration otherwise.
// Cost: O(n), n = size of the declaration.
pub(crate) fn finish_package_header(
    declaration: Stmt,
    name: &str,
    header: crate::ast::package_header::Header,
) -> Stmt {
    if header.adverbs.is_empty() && header.export_tags.is_none() {
        return declaration;
    }
    finish_total(ParsedStmt::PackageHeader {
        decl: Box::new(declaration),
        name: name.to_string(),
        header,
    })
}
