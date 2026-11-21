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

/// Where a unit's statement list ends when the bodies of the heredocs its
/// last statement (`last`) opens follow it (`say q:to/END/;` then the body and
/// `END`): after the statement's `;` terminators, as anywhere else. `None`
/// unless, past the rest of the statement's line, the text is exactly those
/// bodies, each closed by its terminator line, then whitespace and comments.
// Cost: O(n), n = length of the source from the statement on.
pub(super) fn end_before_heredocs(
    source: &str,
    first: usize,
    last: crate::ast::SrcSpan,
) -> Option<usize> {
    use crate::source_comments::{terminators_end, trivia_len};
    let leading = source.get(..first)?;
    if trivia_len(leading)? != leading.len() {
        return None;
    }
    let (from, to) = (last.from as usize, last.to as usize);
    let stops = heredoc_stops(source.get(from..to)?);
    if stops.is_empty() {
        return None;
    }
    let tail = source.get(to..)?;
    let end = terminators_end(tail)?;
    // The rest of the statement's line, then each body to its terminator.
    let mut rest = &tail[end..];
    let line_end = rest.find('\n')?;
    if trivia_len(&rest[..line_end])? != line_end {
        return None;
    }
    rest = &rest[line_end + 1..];
    for stop in &stops {
        loop {
            let (line, after) = rest.split_once('\n').unwrap_or((rest, ""));
            if line.is_empty() && after.is_empty() {
                return None;
            }
            rest = after;
            if line.trim() == stop {
                break;
            }
        }
    }
    (trivia_len(rest)? == rest.len()).then_some(to + end)
}

/// The terminators of the heredocs a statement's `text` opens
/// (`q:to/END/`, `qq:heredoc<EOT>`), in order.
// Cost: O(n), n = length of `text`.
fn heredoc_stops(text: &str) -> Vec<String> {
    let mut stops = Vec::new();
    let mut at = 0;
    while let Some(found) = text[at..].find(':') {
        let after = &text[at + found + 1..];
        at += found + 1;
        let Some(rest) = after
            .strip_prefix("heredoc")
            .or_else(|| after.strip_prefix("to"))
        else {
            continue;
        };
        let Some(open) = rest.chars().next() else {
            continue;
        };
        let close = match open {
            '/' => '/',
            '<' => '>',
            '[' => ']',
            '{' => '}',
            '(' => ')',
            '\'' | '"' => open,
            _ => continue,
        };
        let body = &rest[open.len_utf8()..];
        if let Some(len) = body.find(close) {
            stops.push(body[..len].trim().to_string());
        }
    }
    stops
}
