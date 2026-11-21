//! Origins of an indirect name (`::("Int")`, `MY::(q[$x])`; ADR-10723
//! Stage 2, #15780): the `Name` spans the term's whole text.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::ValueView;

/// Place `name`, the `Name` of a term written at `span`, when it ends in a
/// `Name::Part::Expression` and the text spells it: `::(...)`, or simple
/// parts then `::(...)`. Whether the name was an indirect one.
// Cost: O(p + n), p = parts of the name, n = length of the term's text.
pub(super) fn place(name: &mut RakuAstNode, span: crate::ast::SrcSpan) -> bool {
    // `::("Int")`: an indirect name, `Name::Part::EmptyEdge` then the
    // `Name::Part::Expression` in parentheses, spans the term's text.
    let parts: Vec<_> = name
        .fields
        .iter()
        .filter(|f| f.name == Some("parts"))
        .flat_map(|f| match &f.value {
            RakuAstFieldValue::List(parts) => parts.as_slice(),
            _ => &[],
        })
        .map(|v| match v.view() {
            ValueView::RakuAst(part) => Some(part.class),
            _ => None,
        })
        .collect();
    if parts
        == [
            Some(RakuAstClass::NamePartEmptyEdge),
            Some(RakuAstClass::NamePartExpression),
        ]
    {
        let indirect = super::unit_source::with_text(span, |text| {
            (text.starts_with("::(") && text.ends_with(')')).then_some(())
        });
        if indirect.is_some() {
            super::origin::set_span(name, span);
        }
        return true;
    }
    // `MY::(q[$x])`: simple parts, then the indirect part.
    if parts.len() > 1
        && parts.last() == Some(&Some(RakuAstClass::NamePartExpression))
        && parts[..parts.len() - 1]
            .iter()
            .all(|p| *p == Some(RakuAstClass::NamePartSimple))
    {
        let prefix: Option<Vec<String>> = name
            .fields
            .iter()
            .filter(|f| f.name == Some("parts"))
            .flat_map(|f| match &f.value {
                RakuAstFieldValue::List(parts) => parts.as_slice(),
                _ => &[],
            })
            .take(parts.len() - 1)
            .map(super::name_parts::part_identifier)
            .collect();
        let prefix = prefix.map(|names| names.join("::"));
        let indirect = prefix.and_then(|prefix| {
            super::unit_source::with_text(span, |text| {
                (text
                    .strip_prefix(prefix.as_str())
                    .is_some_and(|rest| rest.starts_with("::("))
                    && text.ends_with(')'))
                .then_some(())
            })
        });
        if indirect.is_some() {
            super::origin::set_span(name, span);
        }
        return true;
    }
    false
}
