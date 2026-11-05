//! The `Blockoid` and `StatementList` ranges of the body of an anonymous
//! `sub` / `method` (#16643). The parser records neither, so they are read
//! off the unit's text: the body runs from its `{` to the end of the routine,
//! and its list from the first statement to the end of the last (the `;`
//! terminators after it included), or sits right after the `{` when empty.

use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;

/// Give the body of `node`, an anonymous routine written at `span`, its
/// ranges, when it has none yet.
// Cost: O(n + s), n = length of the routine's source, s = statements of the body.
pub(super) fn place(node: &mut RakuAstNode, span: SrcSpan) {
    if !matches!(
        node.class,
        RakuAstClass::Sub | RakuAstClass::Method | RakuAstClass::Submethod
    ) || !span.is_some()
        || super::infix_origins::child(node, "name").is_some()
    {
        return;
    }
    let Some(body) = super::infix_origins::child(node, "body") else {
        return;
    };
    if body.origin.0.is_some() {
        return;
    }
    let items = super::infix_origins::child(body, "statement-list")
        .map(statements_range)
        .unwrap_or(None);
    let Some((open, list)) = body_ranges(span, items) else {
        return;
    };
    super::infix_origins::with_child(node, "body", |blockoid| {
        super::origin::set_span(blockoid, SrcSpan::new(open, span.to as usize));
        if list.1 > 0
            && let Some(field) = blockoid.fields.first_mut()
            && let super::RakuAstFieldValue::Node(held) = &mut field.value
            && let crate::value::ValueView::RakuAst(inner) = held.view()
        {
            let mut statements = RakuAstNode::clone(inner);
            super::origin::set_span(&mut statements, SrcSpan::new(list.0, list.1));
            *held = crate::value::Value::rakuast(Box::new(statements));
        }
    });
}

/// The byte range from the start of the first statement of `list` to the end
/// of the last, `None` when it has no statements or one has no range.
// Cost: O(s), s = statements of the list.
fn statements_range(list: &RakuAstNode) -> Option<(usize, usize)> {
    let statements: Vec<(usize, usize)> = list
        .fields
        .iter()
        .filter_map(|f| match &f.value {
            super::RakuAstFieldValue::Node(v) => Some(vec![v.clone()]),
            super::RakuAstFieldValue::List(items) => Some(items.clone()),
            _ => None,
        })
        .flatten()
        .map(|v| match v.view() {
            crate::value::ValueView::RakuAst(n) => super::infix_origins::byte_range(n),
            _ => None,
        })
        .collect::<Option<_>>()?;
    Some((statements.first()?.0, statements.last()?.1))
}

/// Where the body's `{` is, and the range of its list, for a routine written
/// at `span` whose statements run over `items`.
// Cost: O(n), n = length of the routine's source.
fn body_ranges(span: SrcSpan, items: Option<(usize, usize)>) -> Option<(usize, (usize, usize))> {
    let from = span.from as usize;
    super::unit_source::with_text(span, |text| {
        if !text.ends_with('}') {
            return None;
        }
        match items {
            Some((first, last)) => {
                let open = from + text.get(..first.checked_sub(from)?)?.rfind('{')?;
                let tail = super::unit_source::with_text_from(last, |t| {
                    crate::source_comments::terminators_end(t)
                })?;
                Some((open, (first, last + tail)))
            }
            // The statements carry no range of their own (a body that is a
            // single expression): the list is the text between the braces,
            // without the blanks around it.
            None => {
                let open = text.find('{')?;
                let open = match text[..open].rfind(')') {
                    Some(close) => close + text[close..].find('{')?,
                    None => open,
                };
                let inside = &text[open + 1..text.len() - 1];
                // Exact or absent: a `#` may start a comment the statements'
                // end must not include.
                if inside.contains('#') {
                    return Some((from + open, (0, 0)));
                }
                if inside.trim().is_empty() {
                    return Some((from + open, (from + open + 1, from + open + 1)));
                }
                let lead = inside.len() - inside.trim_start().len();
                let list_from = from + open + 1 + lead;
                let list_to = from + open + 1 + inside.trim_end().len();
                Some((from + open, (list_from, list_to.max(list_from))))
            }
        }
    })
}
