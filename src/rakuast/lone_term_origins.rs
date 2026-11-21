//! Origins of a lone term the view rebuilt rather than read from the source,
//! or the conversion could not tell apart from another (ADR-10723 Stage 2;
//! #15780): the colonpair argument of an attribute's `is built(:bind)`,
//! whose parentheses have a range, and the variable a statement list ends
//! with. The text around them, trimmed, is the term when it spells it.

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

/// `{ ...; $x }`: the variable that ends a statement list, when the parser
/// left it unplaced (a parameter's name, read again in the body, is a term
/// of the same shape the conversion cannot tell apart). Its range is its
/// spelling at the end of the text `inside` the braces, after `;`, `{` or a
/// blank.
// Cost: O(v + w), v = length of the variable's name, w = trailing blanks.
pub(super) fn trailing_variable(list: &mut RakuAstNode, inside: SrcSpan) {
    use super::infix_origins::{child_at, leaf_text, with_child_at};
    let Some(last) = list.fields.len().checked_sub(1) else {
        return;
    };
    let Some(name) = child_at(list, last)
        .filter(|s| s.class == RakuAstClass::StatementExpression && s.origin.0.is_none())
        .and_then(|s| child_at(s, 0))
        .filter(|v| {
            v.origin.0.is_none()
                && matches!(v.class, RakuAstClass::VarLexical | RakuAstClass::VarDynamic)
        })
        .and_then(leaf_text)
    else {
        return;
    };
    let Some(at) = super::unit_source::with_text(inside, |text| {
        let body = text
            .trim_end()
            .strip_suffix(';')
            .unwrap_or(text.trim_end())
            .trim_end();
        let head = body.strip_suffix(name.as_str())?;
        (head.is_empty() || head.ends_with([';', '{', ' ', '\t', '\n']))
            .then_some(inside.from as usize + head.len())
    }) else {
        return;
    };
    with_child_at(list, last, |statement| {
        with_child_at(statement, 0, |var| {
            super::origin::set_span(var, SrcSpan::new(at, at + name.len()))
        })
    });
}

/// `(1 if True)` / `($c(1) for @a)`: a lone statement with a modifier inside
/// parentheses written at `inside`, whose modifier is placed but whose
/// expression the parser left unplaced. The expression is the text from the
/// start of the parentheses' contents to the modifier, trimmed (parsed again
/// for its ranges), and the statement runs from it to the modifier's end.
// Cost: O(n), n = length of the statement's text.
pub(super) fn modified_statement(list: &mut RakuAstNode, inside: SrcSpan) {
    use super::infix_origins::{byte_range, child, child_at, with_child, with_child_at};
    let [_] = list.fields.as_slice() else {
        return;
    };
    let Some(statement) = child_at(list, 0)
        .filter(|s| s.class == RakuAstClass::StatementExpression && s.origin.0.is_none())
    else {
        return;
    };
    let Some(modifier) = ["condition-modifier", "loop-modifier"]
        .into_iter()
        .find_map(|name| child(statement, name).and_then(byte_range))
    else {
        return;
    };
    if child(statement, "expression").is_none_or(|e| e.origin.0.is_some()) {
        return;
    }
    let from = inside.from as usize;
    let Some((at, written)) = super::unit_source::with_text(SrcSpan::new(from, modifier.0), |text| {
        let lead = text.len() - text.trim_start().len();
        let written = text.trim();
        (!written.is_empty()).then(|| (from + lead, written.to_string()))
    }) else {
        return;
    };
    // The expression's text is parsed again as a fragment, whose ranges
    // are copied over: exact or absent.
    with_child_at(list, 0, |statement| {
        with_child(statement, "expression", |expression| {
            super::fragment_origins::expression(
                &crate::scope::NameScope::default(),
                expression,
                &written,
                at,
            );
        });
        if child(statement, "expression").is_some_and(|e| e.origin.0.is_some()) {
            super::origin::set_span(statement, SrcSpan::new(at, modifier.1));
        }
    });
}
