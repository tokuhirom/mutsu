//! Origins of a statement-control statement in expression position
//! (ADR-10723 Stage 2, #15780): `(for @a { })`, `(if 1 { 2 })`,
//! `do for @a { }`. The parser keeps no range for such a statement; its
//! parts are placed, and the statement runs over all of its own text.

use super::infix_origins::{byte_range, child_at, with_child_at};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;

/// Whether `class` is a statement-control statement that can stand as a
/// term.
// Cost: O(1).
fn is_control(class: RakuAstClass) -> bool {
    matches!(
        class,
        RakuAstClass::StatementFor
            | RakuAstClass::StatementIf
            | RakuAstClass::StatementUnless
            | RakuAstClass::StatementGiven
            | RakuAstClass::StatementWith
            | RakuAstClass::StatementWithout
    )
}

/// Whether `node`'s first child is an unplaced statement-control statement.
// Cost: O(1).
fn unplaced_control(node: &RakuAstNode) -> bool {
    child_at(node, 0).is_some_and(|s| is_control(s.class) && s.origin.0.is_none())
}

/// `(for @a { })`: the lone statement-control statement of `list`, written
/// at `inside` (between the parentheses), runs over the text there, blanks
/// trimmed.
// Cost: O(n), n = length of the text inside the parentheses.
pub(super) fn in_parentheses(list: &mut RakuAstNode, inside: SrcSpan) {
    if list.fields.len() != 1 || !unplaced_control(list) {
        return;
    }
    let from = inside.from as usize;
    let Some(span) = super::unit_source::with_text(inside, |text| {
        let lead = text.len() - text.trim_start().len();
        let written = text.trim();
        written
            .starts_with(char::is_alphabetic)
            .then(|| SrcSpan::new(from + lead, from + lead + written.len()))
    }) else {
        return;
    };
    with_child_at(list, 0, |statement| super::origin::set_span(statement, span));
}

/// `do for @a { }`: the statement-control statement a statement prefix
/// holds runs from after the prefix's keyword and its blanks to the end of
/// the prefix.
// Cost: O(n), n = length of the prefix's text.
pub(super) fn after_prefix(node: &mut RakuAstNode) {
    if !unplaced_control(node) {
        return;
    }
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let Some(start) = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let keyword = text.find(|c: char| !c.is_alphanumeric())?;
        let rest = &text[keyword..];
        let after = rest.trim_start();
        (keyword > 0 && after.len() < rest.len() && after.starts_with(char::is_alphabetic))
            .then(|| to - after.len())
    }) else {
        return;
    };
    with_child_at(node, 0, |statement| {
        super::origin::set_span(statement, SrcSpan::new(start, to))
    });
}
