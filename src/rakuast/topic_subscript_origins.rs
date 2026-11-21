//! Origins of a subscript on the topic, `.[0]` / `.{"a"}` / `.<a>`
//! (`Term::TopicCall` over a `Postcircumfix`; ADR-10723 Stage 2, #15780):
//! the subscript runs from after the `.` to the end of the term, as Rakudo's
//! does, and its index is placed inside the brackets.

use super::infix_origins::{child_at, with_child, with_child_at};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;

/// Place the subscript of `node`, a `Term::TopicCall` written at `span`.
// Cost: O(n + s), n = length of the term's source, s = statements in it.
pub(super) fn place(node: &mut RakuAstNode, span: SrcSpan) {
    let Some(class) = child_at(node, 0)
        .filter(|s| s.origin.0.is_none())
        .map(|s| s.class)
    else {
        return;
    };
    let (from, to) = (span.from as usize + 1, span.to as usize);
    let dotted = super::unit_source::with_text(span, |text| text.starts_with('.').then_some(()));
    if dotted.is_none() || from >= to {
        return;
    }
    let brackets = match class {
        RakuAstClass::PostcircumfixArrayIndex => ('[', ']'),
        RakuAstClass::PostcircumfixHashIndex => ('{', '}'),
        RakuAstClass::PostcircumfixLiteralHashIndex => {
            let Some(inner) = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
                let (open, close) = super::origin::word_brackets_open(text)?;
                text.ends_with(close)
                    .then(|| SrcSpan::new(from + open, to - close.len()))
            }) else {
                return;
            };
            with_child_at(node, 0, |subscript| {
                super::origin::set_span(subscript, SrcSpan::new(from, to));
                with_child(subscript, "index", |quote| {
                    super::origin::word_quote_inner(quote, inner)
                });
            });
            return;
        }
        _ => return,
    };
    let Some(inside) = super::list_origins::bracketed(from, to, brackets) else {
        return;
    };
    with_child_at(node, 0, |subscript| {
        super::origin::set_span(subscript, SrcSpan::new(from, to));
        with_child(subscript, "index", |list| {
            super::list_origins::statement_list(list, inside)
        });
    });
}
