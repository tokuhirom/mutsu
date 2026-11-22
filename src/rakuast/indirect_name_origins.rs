//! Origins of an indirect name (`::("Int")`, `MY::(q[$x])`; ADR-10723
//! Stage 2, #15780): the `Name` spans the term's whole text.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::ValueView;

/// Place `name`, the `Name` of a term written at `span`, when it holds a
/// `Name::Part::Expression` and the text spells all of its parts: `::(...)`,
/// `MY::(q[$x])`, `$::($u)::y7`, `CALLER::($s)::not`. Whether the name was
/// an indirect one.
// Cost: O(p + n), p = parts of the name, n = length of the term's text.
pub(super) fn place(name: &mut RakuAstNode, span: crate::ast::SrcSpan) -> bool {
    let parts: Vec<&crate::value::Value> = name
        .fields
        .iter()
        .filter(|f| f.name == Some("parts"))
        .flat_map(|f| match &f.value {
            RakuAstFieldValue::List(parts) => parts.as_slice(),
            _ => &[],
        })
        .collect();
    let class = |v: &crate::value::Value| match v.view() {
        ValueView::RakuAst(part) => Some(part.class),
        _ => None,
    };
    if !parts
        .iter()
        .any(|p| class(p) == Some(RakuAstClass::NamePartExpression))
    {
        return false;
    }
    // Each part as written: `None` for an expression part, whose text is
    // its parenthesized expression.
    let mut written: Vec<Option<String>> = Vec::with_capacity(parts.len());
    for (i, part) in parts.iter().enumerate() {
        match class(part) {
            Some(RakuAstClass::NamePartEmptyEdge) if i == 0 => written.push(Some(String::new())),
            Some(RakuAstClass::NamePartExpression) => written.push(None),
            Some(RakuAstClass::NamePartSimple) => match super::name_parts::part_identifier(part) {
                Some(word) => written.push(Some(word)),
                None => return true,
            },
            _ => return true,
        }
    }
    let spelled = super::unit_source::with_text(span, |text| {
        let mut rest = text;
        for (i, part) in written.iter().enumerate() {
            if i > 0 {
                // The `::` before every part but the first.
                if crate::qualified::separators(rest).next() != Some(0) {
                    return None;
                }
                rest = &rest[2..];
            }
            rest = match part {
                Some(word) => rest.strip_prefix(word.as_str())?,
                None => &rest[parenthesized_len(rest)?..],
            };
        }
        rest.is_empty().then_some(())
    });
    if spelled.is_some() {
        super::origin::set_span(name, span);
    }
    true
}

/// The length of the parenthesized text `text` starts with, through its
/// matching `)`.
// Cost: O(n), n = length of the parenthesized text.
fn parenthesized_len(text: &str) -> Option<usize> {
    if !text.starts_with('(') {
        return None;
    }
    let mut depth = 0usize;
    for (at, c) in text.char_indices() {
        match c {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    return Some(at + 1);
                }
            }
            _ => {}
        }
    }
    None
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
    // `M::term:<ab>`: the operator's `<...>` quote too.
    with_child(node, "name", |n| {
        super::origin::set_span(n, span);
        operator_quote(n, span);
    });
    let end = crate::ast::SrcSpan::new(span.to as usize, span.to as usize);
    super::empty_fields::set_span(node, "args", end);
    true
}

/// How the `Name` `name` is written when it is simple parts ending in `::`
/// (`MY::`, `A::B::`): `None` for any other name.
// Cost: O(p + n), p = parts of the name, n = its length.
pub(super) fn package_spelling(name: &RakuAstNode) -> Option<String> {
    let parts: Vec<_> = name
        .fields
        .iter()
        .filter(|f| f.name == Some("parts"))
        .flat_map(|f| match &f.value {
            RakuAstFieldValue::List(parts) => parts.as_slice(),
            _ => &[],
        })
        .collect();
    let (last, simple) = parts.split_last()?;
    if !matches!(last.view(), ValueView::RakuAst(p) if p.class == RakuAstClass::NamePartEmptyEdge)
        || simple.is_empty()
    {
        return None;
    }
    let words = simple
        .iter()
        .map(|p| match p.view() {
            ValueView::RakuAst(part) if part.class == RakuAstClass::NamePartSimple => {
                super::name_parts::part_identifier(p)
            }
            _ => None,
        })
        .collect::<Option<Vec<String>>>()?;
    Some(format!("{}::", words.join("::")))
}

/// `MY::<$j>`: a package name ending in `::` that a postfix applies to, at
/// the start of `node`'s range. The `Term::Name` and its `Name` span the
/// name with its `::`.
// Cost: O(p + n), p = parts of the name, n = its length.
pub(super) fn package_operand(node: &mut RakuAstNode) {
    use super::infix_origins::{byte_range, child, child_at, with_child, with_child_at};
    let Some((from, _)) = byte_range(node) else {
        return;
    };
    let Some(written) = child(node, "operand")
        .filter(|t| t.class == RakuAstClass::TermName && t.origin.0.is_none())
        .and_then(|t| child_at(t, 0))
        .and_then(package_spelling)
    else {
        return;
    };
    let exact = super::unit_source::with_text_from(from, |text| {
        text.starts_with(written.as_str()).then_some(())
    });
    if exact.is_none() {
        return;
    }
    let span = crate::ast::SrcSpan::new(from, from + written.len());
    with_child(node, "operand", |term| {
        super::origin::set_span(term, span);
        with_child_at(term, 0, |name| super::origin::set_span(name, span));
    });
}
