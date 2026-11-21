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
    // `$::($u)::y7`: the indirect part, then simple parts.
    if parts.len() > 2
        && parts[..2]
            == [
                Some(RakuAstClass::NamePartEmptyEdge),
                Some(RakuAstClass::NamePartExpression),
            ]
        && parts[2..]
            .iter()
            .all(|p| *p == Some(RakuAstClass::NamePartSimple))
    {
        let tail: Option<Vec<String>> = name
            .fields
            .iter()
            .filter(|f| f.name == Some("parts"))
            .flat_map(|f| match &f.value {
                RakuAstFieldValue::List(parts) => parts.as_slice(),
                _ => &[],
            })
            .skip(2)
            .map(super::name_parts::part_identifier)
            .collect();
        let indirect = tail.and_then(|tail| {
            super::unit_source::with_text(span, |text| {
                let rest = text.strip_prefix("::(")?;
                let mut end = rest.len();
                for part in tail.iter().rev() {
                    let head = rest[..end].strip_suffix(part.as_str())?;
                    let sep = crate::qualified::last_separator(head)?;
                    if sep + 2 != head.len() {
                        return None;
                    }
                    end = sep;
                }
                rest[..end].ends_with(')').then_some(())
            })
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

/// `$OUR::x` / `$::($n)`: the `Name` of `node`, a `Var::Package` with a
/// range, runs from after its sigil to the end, when it spells it.
// Cost: O(p + n), p = parts of the name, n = length of the variable's text.
pub(super) fn package_variable(node: &mut RakuAstNode) {
    let Some((from, to)) = super::infix_origins::byte_range(node) else {
        return;
    };
    let Some(sigil) = super::decl_origins::leaf(node, "sigil") else {
        return;
    };
    let span = crate::ast::SrcSpan::new(from + sigil.len(), to);
    let written = super::unit_source::with_text(crate::ast::SrcSpan::new(from, to), |text| {
        text.strip_prefix(sigil.as_str()).map(str::to_string)
    });
    let Some(written) = written else {
        return;
    };
    super::infix_origins::with_child(node, "name", |name| {
        if name.origin.0.is_some() || place(name, span) {
            return;
        }
        let spelled = matches!(
            super::name_parts::name_shape(name),
            Some(super::name_parts::NameShape::Identifier(n)) if n == written
        );
        if spelled {
            super::origin::set_span(name, span);
        }
    });
}

/// `prefix:<!>`: the word quote an operator's `Name` holds in its
/// `colonpairs`, when the name written at `span` is `CATEGORY:<...>`: the
/// quote over the brackets, its text inside them.
// Cost: O(n), n = length of the name's text.
pub(super) fn operator_quote(name: &mut RakuAstNode, span: crate::ast::SrcSpan) {
    let Some(field) = name
        .fields
        .iter_mut()
        .find(|f| f.name == Some("colonpairs"))
    else {
        return;
    };
    let RakuAstFieldValue::List(items) = &mut field.value else {
        return;
    };
    let [quote] = items.as_mut_slice() else {
        return;
    };
    let ValueView::RakuAst(node) = quote.view() else {
        return;
    };
    if node.class != RakuAstClass::QuotedString || node.origin.0.is_some() {
        return;
    }
    let from = span.from as usize;
    let Some((open, close)) = super::unit_source::with_text(span, |text| {
        let open = text.find(":<")? + 1;
        (text.ends_with('>') && open + 1 < text.len() - 1).then_some((open, text.len() - 1))
    }) else {
        return;
    };
    let mut node = RakuAstNode::clone(node);
    super::origin::set_span(
        &mut node,
        crate::ast::SrcSpan::new(from + open, from + close + 1),
    );
    super::origin::quoted_text(
        &mut node,
        crate::ast::SrcSpan::new(from + open, from + close + 1),
    );
    *quote = crate::value::Value::rakuast(Box::new(node));
}

/// `prefix:<!>`: how an operator's `Name` (its category, then its symbol as
/// a word quote in `colonpairs`) is written.
// Cost: O(n), n = length of the name.
pub(super) fn written_operator(name: &RakuAstNode) -> Option<String> {
    let category = super::infix_origins::leaf_text(name)?;
    let field = name.fields.iter().find(|f| f.name == Some("colonpairs"))?;
    let RakuAstFieldValue::List(items) = &field.value else {
        return None;
    };
    let [quote] = items.as_slice() else {
        return None;
    };
    let ValueView::RakuAst(quote) = quote.view() else {
        return None;
    };
    let symbol = super::name_parts::quoted_text(quote)?;
    Some(category + ":<" + &symbol + ">")
}

/// `GLOBAL::(q[$x])`: a call of an indirect name with no arguments of its
/// own. Its `Name` spans the call's whole text (`span`), and the empty
/// `ArgList` it visits sits at the end. Whether the name was placed.
// Cost: O(p + n), as [`place`].
pub(super) fn call(node: &mut RakuAstNode, span: crate::ast::SrcSpan) -> bool {
    use super::infix_origins::{child, with_child};
    if node.class != RakuAstClass::CallName || child(node, "args").is_some() {
        return false;
    }
    let mut placed = false;
    with_child(node, "name", |name| {
        if name.origin.0.is_none() && place(name, span) {
            placed = name.origin.0.is_some();
        }
    });
    if placed {
        let end = crate::ast::SrcSpan::new(span.to as usize, span.to as usize);
        super::empty_fields::set_span(node, "args", end);
    }
    placed
}

/// `M::foo if True`: a qualified name written as a bare word is a
/// `Call::Name` with no arguments of its own. Its `Name` spans the call's
/// whole text (`span`) and the empty `ArgList` it visits sits at the end.
/// Whether the name was placed.
// Cost: O(n), n = length of the name.
pub(super) fn bare_call(node: &mut RakuAstNode, span: crate::ast::SrcSpan) -> bool {
    use super::infix_origins::{child, with_child};
    if node.class != RakuAstClass::CallName || child(node, "args").is_some() {
        return false;
    }
    let Some(super::name_parts::NameShape::Identifier(name)) =
        child(node, "name").and_then(super::name_parts::name_shape)
    else {
        return false;
    };
    let exact = super::unit_source::with_text(span, |text| (text == name).then_some(()));
    if exact.is_none() {
        return false;
    }
    with_child(node, "name", |n| super::origin::set_span(n, span));
    let end = crate::ast::SrcSpan::new(span.to as usize, span.to as usize);
    super::empty_fields::set_span(node, "args", end);
    true
}
