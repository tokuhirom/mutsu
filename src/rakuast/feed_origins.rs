//! Origins of a declaration fed from the right (ADR-10723 Stage 2, #15780):
//! `my @o <== map { $_ } <== 1, 2`. The parser keeps the declaration as the
//! sink of the feed with no range of its own; it is the text from the start
//! of the feed up to its first arrow.

use super::infix_origins::{byte_range, child, leaf_text};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::value::{Value, ValueView};

/// Place the unplaced declaration `node`, a placed leftward feed
/// (`ApplyListInfix` with a `<==` / `<<==` `Feed`), starts with: from the
/// feed's start to the blanks before the arrow in front of its second
/// operand.
// Cost: O(n), n = length of the text before the feed's second operand.
pub(super) fn declaration_sink(node: &mut RakuAstNode) {
    let Some(op) = child(node, "infix")
        .filter(|infix| infix.class == RakuAstClass::Feed)
        .and_then(leaf_text)
        .filter(|op| matches!(op.as_str(), "<==" | "<<=="))
    else {
        return;
    };
    let Some((from, _)) = byte_range(node) else {
        return;
    };
    let Some(operands) = node.fields.iter_mut().find_map(|f| match &mut f.value {
        RakuAstFieldValue::List(items) if f.name == Some("operands") => Some(items),
        _ => None,
    }) else {
        return;
    };
    let second = match operands.get(1).map(Value::view) {
        Some(ValueView::RakuAst(second)) => byte_range(second),
        _ => None,
    };
    let Some((next, _)) = second else {
        return;
    };
    let Some(first) = operands.first_mut() else {
        return;
    };
    let ValueView::RakuAst(declaration) = first.view() else {
        return;
    };
    if declaration.class != RakuAstClass::VarDeclarationSimple
        || declaration.origin.0.is_some()
        || next <= from
    {
        return;
    }
    let Some(end) = super::unit_source::with_text(SrcSpan::new(from, next), |text| {
        let written = text
            .trim_end()
            .strip_suffix(op.as_str())?
            .trim_end_matches([' ', '\t']);
        (!written.is_empty() && !written.contains(['\n', '#'])).then(|| from + written.len())
    }) else {
        return;
    };
    let mut declaration = RakuAstNode::clone(declaration);
    super::origin::set_span(&mut declaration, SrcSpan::new(from, end));
    *first = Value::rakuast(Box::new(declaration));
}
