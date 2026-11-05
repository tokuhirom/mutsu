//! Ranges of the declaration `unit module` / `unit package` makes of the rest
//! of its unit (#15780).

use crate::ast::Stmt;

/// The positions of a `unit module` whose body is `rest`: the name the parser
/// recorded, and the block from the first statement of the rest to the end of
/// the last (the `;` terminators after it included).
// Cost: O(s + n), s = statements of `rest`, n = length of the terminators.
pub(super) fn package_spans(
    header: Option<&crate::ast::parsed::PackageSpans>,
    rest: &[Stmt],
) -> Option<Box<crate::ast::parsed::PackageSpans>> {
    let header = header?;
    let mut block = header.block;
    if let Some(list) = rest_span(rest) {
        block.span = list;
        block.blockoid = list;
        block.list = list;
    }
    Some(Box::new(crate::ast::parsed::PackageSpans {
        name: header.name,
        block,
    }))
}

/// The range of the statements `rest`: from the first to the end of the last,
/// the `;` terminators after it included.
// Cost: O(s + n), s = statements of `rest`, n = length of the terminators.
pub(super) fn rest_span(rest: &[Stmt]) -> Option<crate::ast::SrcSpan> {
    let mut spans = rest.iter().filter_map(|stmt| match stmt {
        Stmt::SetLine(_, span) if span.is_some() => Some(*span),
        _ => None,
    });
    let first = spans.next()?;
    let last = spans.next_back().unwrap_or(first);
    let (from, end) = (first.from as usize, last.to as usize);
    let tail =
        super::unit_source::with_text_from(end, crate::source_comments::terminators_end)?;
    Some(crate::ast::SrcSpan::new(from, end + tail))
}
