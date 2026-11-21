//! Origins of a hyper infix application, `@a >>+<< @b` / `@a «[&plus]» @b`
//! (`ApplyInfix` over a `MetaInfix::Hyper`; ADR-10723 Stage 2, #15780). The
//! operator is read between the operands: a `>>` / `<<` / `»` / `«` marker,
//! the inner operator as written, and a closing marker.

use super::RakuAstNode;
use super::infix_origins::{child, with_child, with_child_at};
use crate::ast::SrcSpan;

/// The hyper markers, ASCII and Unicode.
const MARKERS: [&str; 4] = [">>", "<<", "\u{bb}", "\u{ab}"];

/// Place `node`, an `ApplyInfix` whose infix is a `MetaInfix::Hyper` and
/// whose operands are written at `left` and `right`: the application over
/// both, the hyper operator between them and the operator inside it.
// Cost: O(n), n = length of the source between the operands.
pub(super) fn place(
    node: &mut RakuAstNode,
    left: Option<(usize, usize)>,
    right: Option<(usize, usize)>,
) {
    let (Some((from, left_end)), Some((right_start, to))) = (left, right) else {
        return;
    };
    let Some((op, inner)) = child(node, "infix")
        .and_then(|hyper| child(hyper, "infix"))
        .and_then(super::infix_spelling::written)
    else {
        return;
    };
    let Some((at, open, close)) =
        super::unit_source::with_text(SrcSpan::new(left_end, right_start), |between| {
            let lead = between.len() - between.trim_start().len();
            let rest = &between[lead..];
            let open = MARKERS.into_iter().find(|m| rest.starts_with(m))?;
            let after = rest[open.len()..].strip_prefix(op.as_str())?;
            let close = MARKERS.into_iter().find(|m| after.starts_with(m))?;
            after[close.len()..]
                .chars()
                .all(char::is_whitespace)
                .then_some((left_end + lead, open.len(), close.len()))
        })
    else {
        return;
    };
    let op_at = at + open;
    super::origin::set_span(node, SrcSpan::new(from, to));
    with_child(node, "infix", |hyper| {
        super::origin::set_span(hyper, SrcSpan::new(at, op_at + op.len() + close));
        with_child(hyper, "infix", |infix| {
            super::origin::set_span(infix, SrcSpan::new(op_at, op_at + op.len()));
            if let Some((a, b)) = inner {
                with_child_at(infix, 0, |part| {
                    super::origin::set_span(part, SrcSpan::new(op_at + a, op_at + b))
                });
            }
        });
    });
}

/// Place `node`, an `ApplyInfix` whose infix is a `MetaInfix::Assign` over a
/// `MetaInfix::Hyper` (`@a >>~=>> @b`), with operands written at `left` and
/// `right`. As Rakudo does, the assignment spans the markers, the operator
/// and the `=`, the `Infix` inside is placed and the hyper itself is not.
// Cost: O(n), n = length of the source between the operands.
pub(super) fn place_assign(
    node: &mut RakuAstNode,
    left: Option<(usize, usize)>,
    right: Option<(usize, usize)>,
) {
    let (Some((from, left_end)), Some((right_start, to))) = (left, right) else {
        return;
    };
    let Some(op) = child(node, "infix")
        .filter(|assign| assign.class == super::RakuAstClass::MetaInfixAssign)
        .and_then(|assign| super::infix_origins::child_at(assign, 0))
        .filter(|hyper| hyper.class == super::RakuAstClass::MetaInfixHyper)
        .and_then(|hyper| child(hyper, "infix"))
        .filter(|infix| infix.class == super::RakuAstClass::Infix)
        .and_then(super::infix_origins::leaf_text)
    else {
        return;
    };
    let Some((at, open, end)) =
        super::unit_source::with_text(SrcSpan::new(left_end, right_start), |between| {
            let lead = between.len() - between.trim_start().len();
            let rest = &between[lead..];
            let open = MARKERS.into_iter().find(|m| rest.starts_with(m))?;
            let after = rest[open.len()..].strip_prefix(op.as_str())?.strip_prefix('=')?;
            let close = MARKERS.into_iter().find(|m| after.starts_with(m))?;
            let written = rest.len() - after.len() + close.len();
            after[close.len()..]
                .chars()
                .all(char::is_whitespace)
                .then_some((left_end + lead, open.len(), left_end + lead + written))
        })
    else {
        return;
    };
    super::origin::set_span(node, SrcSpan::new(from, to));
    with_child(node, "infix", |assign| {
        super::origin::set_span(assign, SrcSpan::new(at, end));
        with_child_at(assign, 0, |hyper| {
            with_child(hyper, "infix", |infix| {
                let op_at = at + open;
                super::origin::set_span(infix, SrcSpan::new(op_at, op_at + op.len()))
            })
        });
    });
}
