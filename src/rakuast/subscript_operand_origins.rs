//! Origins of the operand of a subscript the parser kept no range for
//! (ADR-10723 Stage 2, #15780): `<a b c d>.[*-1]`, `flat(1, 2).hash.<a>`,
//! `self[0]`. The application is placed; its subscript is the last
//! bracketed group of its text, so the operand is the text before it (and
//! before a `.`), and it is placed and derived from that range.

use super::infix_origins::{byte_range, child, with_child};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;

/// Place the unplaced operand of `node`, a placed `ApplyPostfix` whose
/// postfix is a subscript.
// Cost: O(n), n = length of the application's text, plus the operand's
// derivation.
pub(super) fn place(node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let Some(postfix) = child(node, "postfix") else {
        return;
    };
    let (open, close) = match postfix.class {
        RakuAstClass::PostcircumfixArrayIndex => ('[', ']'),
        RakuAstClass::PostcircumfixHashIndex => ('{', '}'),
        RakuAstClass::PostcircumfixLiteralHashIndex => ('<', '>'),
        _ => return,
    };
    if postfix.origin.0.is_some() || child(node, "operand").is_none_or(|o| o.origin.0.is_some()) {
        return;
    }
    let Some(end) = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let opened = opening(text, open, close)?;
        let head = &text[..opened];
        let head = head.strip_suffix('.').unwrap_or(head);
        (!head.is_empty() && !head.ends_with(char::is_whitespace)).then(|| from + head.len())
    }) else {
        return;
    };
    let span = SrcSpan::new(from, end);
    with_child(node, "operand", |operand| {
        super::origin::set_span(operand, span);
        super::origin::named_term(operand, span);
        super::origin::quoted_text(operand, span);
        super::call_origins::postfix_call(operand, span);
        super::infix_origins::derive(&crate::scope::NameScope::default(), operand);
    });
}

/// The byte offset in `text`, which ends with `close`, of the `open` that
/// matches it, counting nested pairs (none for `<`, a word quote's).
// Cost: O(n), n = length of `text`.
fn opening(text: &str, open: char, close: char) -> Option<usize> {
    let body = text.strip_suffix(close)?;
    if open == '<' {
        return body.rfind('<');
    }
    let mut depth = 1usize;
    for (at, c) in body.char_indices().rev() {
        if c == close {
            depth += 1;
        } else if c == open {
            depth -= 1;
            if depth == 0 {
                return Some(at);
            }
        } else if matches!(c, '"' | '\'') {
            // A quote could hold an unbalanced bracket: exact or absent.
            return None;
        }
    }
    None
}
