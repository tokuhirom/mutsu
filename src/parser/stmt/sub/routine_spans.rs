//! Where a routine declaration was written below its statement (ADR-10723
//! Stage 2): its name, its signature and its block.

use crate::ast::parsed::{PackageSpans, RoutineSpans, TraitSpans};
use crate::ast::{ParamDef, SrcSpan, Stmt};

/// The positions of a routine whose name is at `name`, whose parameters (when
/// it has parentheses) start at `signature_start`, and whose `{` is at `brace`,
/// its source ending at `rest`; `traits` are where its traits were written.
// Cost: O(s), s = statements of `body`.
pub(crate) fn routine_spans(
    name: SrcSpan,
    signature_start: Option<&str>,
    params: &[ParamDef],
    traits: Vec<TraitSpans>,
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
        traits,
        only: false,
        name_written: None,
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

/// The positions of a package declaration whose name is at `name` and whose
/// `{` is at `brace`, its source ending at `rest`.
// Cost: O(s), s = statements of `body`.
pub(crate) fn package_spans(
    name: SrcSpan,
    brace: &str,
    rest: &str,
    body: &[Stmt],
) -> Option<Box<PackageSpans>> {
    let brace = brace.trim_start();
    let block = crate::parser::stmt::control::parsed_block::block_spans(brace, brace, rest, body);
    Some(Box::new(PackageSpans { name, block }))
}

/// Mark a parsed routine declaration as written with the `only` declarator.
// Cost: O(1).
pub(crate) fn mark_only(stmt: &mut Stmt) {
    if let Stmt::SubDecl { spans, .. } | Stmt::MethodDecl { spans, .. } = stmt
        && let Some(spans) = spans
    {
        spans.only = true;
    }
}

/// Remember how an operator routine's name was written when the canonical
/// name spells its symbol differently (`«<< >>»` vs `<<< >>>`).
// Cost: O(n), n = length of the written name.
pub(crate) fn note_written_name(stmt: &mut Stmt, written: &str) {
    if let Stmt::SubDecl { name, spans, .. } = stmt {
        note_written_spans(spans, &name.resolve(), written);
    }
}

/// [`note_written_name`] on the spans themselves, for a declaration whose
/// statement is not built yet (a method).
// Cost: O(n), n = length of the written name.
pub(crate) fn note_written_spans(
    spans: &mut Option<Box<crate::ast::parsed::RoutineSpans>>,
    canonical: &str,
    written: &str,
) {
    if let Some(spans) = spans
        && written != canonical
        && written
            .split_once(':')
            .is_some_and(|(_, quoted)| quoted.starts_with("<<") || quoted.starts_with('\u{ab}'))
    {
        spans.name_written = Some(written.into());
    }
}
