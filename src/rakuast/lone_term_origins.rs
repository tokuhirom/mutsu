//! Origins of a lone term in parentheses that the view rebuilt rather than
//! read from the source (ADR-10723 Stage 2; #15780): the colonpair argument of
//! an attribute's `is built(:bind)`. The parentheses have a range; the text
//! inside them, trimmed, is the term when it spells it.

use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;

/// Place `term`, a `ColonPair::True` / `False` (`:bind`, `:!bind`), over
/// `span` when that is exactly how it is written.
// Cost: O(n), n = length of the term's text.
pub(super) fn place(term: &mut RakuAstNode, span: SrcSpan) {
    let prefix = match term.class {
        RakuAstClass::ColonPairTrue => ":",
        RakuAstClass::ColonPairFalse => ":!",
        _ => return,
    };
    let Some(key) = super::infix_origins::leaf_text(term) else {
        return;
    };
    let written = super::unit_source::with_text(span, |text| {
        (text == format!("{prefix}{key}")).then_some(())
    });
    if written.is_some() {
        super::origin::set_span(term, span);
    }
}
