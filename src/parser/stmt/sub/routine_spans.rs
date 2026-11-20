//! Where a routine declaration was written below its statement (ADR-10723
//! Stage 2): its name, its signature and its block.

use crate::ast::parsed::{PackageSpans, RoutineSpans, TraitSpans};
use crate::ast::{ParamDef, SrcSpan, Stmt};

/// The positions of a routine whose name is at `name`, whose parameters (when
/// it has parentheses) run from the first to the second text of `signature`
/// (a `--> Type` return part included), and whose `{` is at `brace`, its
/// source ending at `rest`; `traits` are where its traits were written.
// Cost: O(s + n), s = statements of `body`, n = length of the signature.
pub(crate) fn routine_spans(
    name: SrcSpan,
    signature: Option<(&str, &str)>,
    params: &[ParamDef],
    traits: Vec<TraitSpans>,
    brace: &str,
    rest: &str,
    body: &[Stmt],
) -> Option<Box<RoutineSpans>> {
    let (signature, returns) = match signature {
        None => (SrcSpan::NONE, SrcSpan::NONE),
        Some((start, end)) => signature_span(start, end, params),
    };
    let brace = brace.trim_start();
    let block = crate::parser::stmt::control::parsed_block::block_spans(brace, brace, rest, body);
    Some(Box::new(RoutineSpans {
        name,
        signature,
        returns,
        block,
        traits,
        only: false,
        name_written: None,
    }))
}

/// A signature runs from its first parameter (or its `-->` when it has none)
/// to the end of its last, or of its return part; an empty one sits where its
/// parameters would start. The second range is the return part's (`Int` of
/// `--> Int`), `SrcSpan::NONE` without one.
// Cost: O(p + n), p = parameters, n = length of the signature.
fn signature_span(start: &str, end: &str, params: &[ParamDef]) -> (SrcSpan, SrcSpan) {
    let written: Vec<SrcSpan> = params
        .iter()
        .map(|p| p.spans.param)
        .filter(|s| s.is_some())
        .collect();
    let returns = return_part(start, end);
    let span = match (written.first(), written.last(), returns) {
        (Some(first), _, Some((_, ret))) => SrcSpan::new(first.from as usize, ret.to as usize),
        (None, _, Some((arrow, ret))) => SrcSpan::new(arrow.from as usize, ret.to as usize),
        (Some(first), Some(last), None) => SrcSpan::new(first.from as usize, last.to as usize),
        _ => crate::parser::primary::source_span(start, start),
    };
    (span, returns.map_or(SrcSpan::NONE, |(_, ret)| ret))
}

/// Where the `-->` of a signature written from `start` to `end` is, and the
/// return part after it (blanks around it excluded).
// Cost: O(n), n = length of the signature.
fn return_part(start: &str, end: &str) -> Option<(SrcSpan, SrcSpan)> {
    let text = start.get(..start.len().checked_sub(end.len())?)?;
    let arrow = text.rfind("-->")?;
    let after = &text[arrow + 3..];
    let ret = after.trim_start();
    let ret_text = ret.trim_end();
    if ret_text.is_empty() {
        return None;
    }
    let ret_end = &ret[ret_text.len()..];
    let arrow_text = &text[arrow..];
    let arrow_span = crate::parser::primary::source_span(arrow_text, &arrow_text[3..]);
    let ret_span = crate::parser::primary::source_span(ret, ret_end);
    (arrow_span.is_some() && ret_span.is_some()).then_some((arrow_span, ret_span))
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
