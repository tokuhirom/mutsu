//! Origins of statement labels (ADR-10723 Stage 2, #15780): `L: for @a
//! { next L }`. A `Label` runs over its name and colon; the statement it
//! labels runs on after it; a label named as an argument (`next L`) is a
//! `Term::Name` over the name.

use super::infix_origins::{child, child_at, with_child, with_child_at};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::value::{Value, ValueView};

/// Place the labels of `node`, a statement written at `span` (labels
/// included), each over its `NAME:`. The range the statement runs on after
/// them: `span` itself when it has none, or when they are not written there.
// Cost: O(l + n), l = labels, n = length of their text.
pub(super) fn place(node: &mut RakuAstNode, span: SrcSpan) -> SrcSpan {
    let Some(field) = node.fields.iter_mut().find(|f| f.name == Some("labels")) else {
        return span;
    };
    let RakuAstFieldValue::List(labels) = &mut field.value else {
        return span;
    };
    let mut at = span.from as usize;
    let mut placed = Vec::with_capacity(labels.len());
    for label in labels.iter() {
        let ValueView::RakuAst(label) = label.view() else {
            return span;
        };
        let Some(name) = label.fields.first().and_then(|f| match &f.value {
            RakuAstFieldValue::Node(v) => v.as_str().map(str::to_string),
            _ => None,
        }) else {
            return span;
        };
        let Some((end, next)) = super::unit_source::with_text_from(at, |text| {
            let rest = text.strip_prefix(name.as_str())?;
            let colon = rest.len() - rest.trim_start_matches([' ', '\t']).len();
            let rest = rest[colon..].strip_prefix(':')?;
            let blanks = rest.len() - rest.trim_start().len();
            let end = name.len() + colon + 1;
            Some((at + end, at + end + blanks))
        }) else {
            return span;
        };
        placed.push(SrcSpan::new(at, end));
        at = next;
    }
    if at >= span.to as usize {
        return span;
    }
    for (label, range) in labels.iter_mut().zip(placed) {
        if let ValueView::RakuAst(inner) = label.view() {
            let mut inner = RakuAstNode::clone(inner);
            super::origin::set_span(&mut inner, range);
            *label = Value::rakuast(Box::new(inner));
        }
    }
    SrcSpan::new(at, span.to as usize)
}

/// `next L`: a call written from `name_end` to `to` whose lone argument is an
/// unplaced `Term::Name` spelled by the text after the call's name: the
/// term, its `Name` and the `ArgList` run over that text.
// Cost: O(n), n = length of the call's text.
pub(super) fn name_argument(node: &mut RakuAstNode, name_end: usize, to: usize) {
    let Some(args) = child(node, "args") else {
        return;
    };
    let Some(term) = child_at(args, 0) else {
        return;
    };
    if args.fields.len() != 1 || term.class != RakuAstClass::TermName || term.origin.0.is_some() {
        return;
    }
    let Some(super::name_parts::NameShape::Identifier(name)) =
        child_at(term, 0).and_then(super::name_parts::name_shape)
    else {
        return;
    };
    let Some(from) = super::unit_source::with_text(SrcSpan::new(name_end, to), |text| {
        let written = text.trim_start_matches([' ', '\t']);
        (written.len() < text.len() && written == name).then(|| to - written.len())
    }) else {
        return;
    };
    let span = SrcSpan::new(from, to);
    with_child(node, "args", |args| {
        with_child_at(args, 0, |term| {
            super::origin::set_span(term, span);
            with_child_at(term, 0, |name| super::origin::set_span(name, span));
        });
    });
}
