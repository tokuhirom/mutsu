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
    let tail = super::unit_source::with_text_from(end, crate::source_comments::terminators_end)?;
    Some(crate::ast::SrcSpan::new(from, end + tail))
}

/// The body of a `unit class` / `unit role` / `unit grammar` declaration, which
/// holds the rest of its unit; `None` for any other statement.
// Cost: O(1).
pub(super) fn unit_body(stmt: &Stmt) -> Option<&[Stmt]> {
    let stmt = match stmt {
        Stmt::Ast(node) => node.inner_stmt().unwrap_or(stmt),
        _ => stmt,
    };
    match stmt {
        Stmt::ClassDecl {
            is_unit: true,
            body,
            ..
        } => Some(body),
        // A role has no `is_unit` flag: the parser leaves the block of a
        // `unit role` unknown, as it is the rest of the unit.
        Stmt::RoleDecl {
            body,
            spans: Some(spans),
            ..
        } if !spans.block.span.is_some() => Some(body),
        _ => None,
    }
}

/// `span`, the range of a `unit class` statement, widened over the unit it
/// holds (a statement that is not such a declaration keeps `span`).
// Cost: O(s + n), s = statements of the body, n = length of the terminators.
pub(super) fn widen_statement(span: crate::ast::SrcSpan, stmt: &Stmt) -> crate::ast::SrcSpan {
    match unit_body(stmt).and_then(rest_span) {
        Some(rest) if span.is_some() => {
            crate::ast::SrcSpan::new(span.from as usize, rest.to as usize)
        }
        _ => span,
    }
}
