//! Origins of a subscript applied as a hyper postfix (ADR-10723 Stage 2,
//! #15780): `%h{*}»[1]`, `%h{*}».[1]`. The `MetaPostfix::Hyper` runs from
//! its marker to the end; the subscript it holds starts after the marker
//! (and a dot), and its index is the text between the brackets.

use super::infix_origins::{byte_range, child, child_at, with_child, with_child_at};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;

/// Place the subscript inside `node`'s placed `MetaPostfix::Hyper`.
// Cost: O(n), n = length of the subscript's text.
pub(super) fn place(node: &mut RakuAstNode) {
    let Some(hyper) = child(node, "postfix").filter(|p| p.class == RakuAstClass::MetaPostfixHyper)
    else {
        return;
    };
    let Some((from, to)) = byte_range(hyper) else {
        return;
    };
    let Some(subscript) = child_at(hyper, 0) else {
        return;
    };
    let brackets = match subscript.class {
        RakuAstClass::PostcircumfixArrayIndex => ('[', ']'),
        RakuAstClass::PostcircumfixHashIndex => ('{', '}'),
        _ => return,
    };
    if subscript.origin.0.is_some() {
        return;
    }
    let Some(start) = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let rest = text
            .strip_prefix(">>")
            .or_else(|| text.strip_prefix('\u{bb}'))?;
        let rest = rest.strip_prefix('.').unwrap_or(rest);
        Some(to - rest.len())
    }) else {
        return;
    };
    let Some(inside) = super::list_origins::bracketed(start, to, brackets) else {
        return;
    };
    with_child(node, "postfix", |hyper| {
        with_child_at(hyper, 0, |subscript| {
            super::origin::set_span(subscript, SrcSpan::new(start, to));
            with_child(subscript, "index", |list| {
                super::list_origins::statement_list(list, inside)
            });
        });
    });
}
