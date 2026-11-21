//! Origins of a listop call the parser kept no range for (ADR-10723 Stage 2,
//! #15780): `sub { say 1 }`, whose body statements carry no line marks.
//! The call runs from its name, written right before its first argument
//! after blanks, to the end of its last argument; both arguments are placed.

use super::infix_origins::{byte_range, child, child_at, leaf_text};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;

/// How many blanks may separate the name from its first argument.
const WINDOW: usize = 64;

/// Give `node`, a `Call::Name::WithoutParentheses` with no range, the range
/// from its name to the end of its last argument, when its first and last
/// arguments are placed and its name is written right before the first.
// Cost: O(a + n), a = arguments, n = length of the name (and the blanks
// after it, at most `WINDOW`).
fn from_arguments(node: &mut RakuAstNode) {
    if node.class != RakuAstClass::CallNameWithoutParentheses || node.origin.0.is_some() {
        return;
    }
    let Some(name) = child(node, "name").and_then(leaf_text) else {
        return;
    };
    let Some(args) = child(node, "args") else {
        return;
    };
    let Some((first, _)) = child_at(args, 0).and_then(byte_range) else {
        return;
    };
    let Some((_, last)) = args
        .fields
        .len()
        .checked_sub(1)
        .and_then(|i| child_at(args, i))
        .and_then(byte_range)
    else {
        return;
    };
    if first < name.len() + 1 || last < first {
        return;
    }
    // A window before the argument: the name and the blanks after it.
    let window = first.saturating_sub(name.len() + WINDOW);
    let Some(from) = super::unit_source::with_text(SrcSpan::new(window, first), |before| {
        let trimmed = before.trim_end_matches([' ', '\t']);
        if trimmed.len() == before.len() {
            return None;
        }
        let head = trimmed.strip_suffix(name.as_str())?;
        // The name starts a word.
        (!head.ends_with(|c: char| c.is_alphanumeric() || matches!(c, '_' | '-' | ':')))
            .then_some(window + head.len())
    }) else {
        return;
    };
    super::origin::set_span(node, SrcSpan::new(from, last));
}

/// `sub { say 1 }`: a statement with no line mark whose expression is an
/// unplaced listop call. The call is placed from its arguments and its parts
/// derived; the statement takes its range from it.
// Cost: O(a + n), as [`from_arguments`], plus the derivation.
pub(super) fn unmarked_statement(names: &crate::scope::NameScope, statement: &mut RakuAstNode) {
    if statement.class != RakuAstClass::StatementExpression || statement.origin.0.is_some() {
        return;
    }
    let mut placed = None;
    super::infix_origins::with_child_at(statement, 0, |call| {
        from_arguments(call);
        if call.origin.0.is_some() {
            super::infix_origins::derive(names, call);
            placed = call.origin.0.clone();
        }
    });
    if placed.is_some() {
        statement.origin.0 = placed;
    }
}
