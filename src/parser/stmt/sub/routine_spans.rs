//! Where a routine declaration was written below its statement (ADR-10723
//! Stage 2): its name, its signature and its block.

use crate::ast::parsed::RoutineSpans;
use crate::ast::{ParamDef, SrcSpan, Stmt};

/// The positions of a routine whose name is at `name`, whose parameters (when
/// it has parentheses) start at `signature_start`, and whose `{` is at `brace`,
/// its source ending at `rest`.
// Cost: O(s), s = statements of `body`.
pub(crate) fn routine_spans(
    name: SrcSpan,
    signature_start: Option<&str>,
    params: &[ParamDef],
    brace: &str,
    rest: &str,
    body: &[Stmt],
) -> Option<Box<RoutineSpans>> {
    let signature = match signature_start {
        None => SrcSpan::NONE,
        Some(start) => signature_span(start, params),
    };
    let brace = brace.trim_start();
    let block = crate::parser::stmt::control::parsed_block::block_spans(brace, brace, rest, body);
    Some(Box::new(RoutineSpans {
        name,
        signature,
        block,
    }))
}

/// A signature runs from its first parameter to the end of its last; an empty
/// one sits where its parameters would start.
// Cost: O(1).
fn signature_span(start: &str, params: &[ParamDef]) -> SrcSpan {
    let written: Vec<SrcSpan> = params
        .iter()
        .map(|p| p.spans.param)
        .filter(|s| s.is_some())
        .collect();
    match (written.first(), written.last()) {
        (Some(first), Some(last)) => SrcSpan::new(first.from as usize, last.to as usize),
        _ => crate::parser::primary::source_span(start, start),
    }
}
