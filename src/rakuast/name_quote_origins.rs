//! Origins of the quote at the end of a routine's name (ADR-10723 Stage 2;
//! #15780): an operator's `<...>` (`infix:<foo>`) and a named colonpair
//! (`comment:sym<#>`), placed from the name's range.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// The origins of the word quote an operator routine's name carries
/// (`term:<forty-two>`, `trait_mod:<is>`): the `QuotedString` is the `<...>` that
/// ends the name, its `StrLiteral` the text between the angle brackets. A
/// named colonpair (`comment:sym<#>`) is a `ColonPair::Value` over `:sym<#>`
/// holding that quote. Only the plain `<word>` spelling, whose delimiters are
/// one character each.
// Cost: O(f + n), f = fields of the name and its quote, n = length of the name.
pub(super) fn place(name: &mut RakuAstNode, span: crate::ast::SrcSpan) {
    if !span.is_some() {
        return;
    }
    let Some(field) = name
        .fields
        .iter_mut()
        .find(|f| f.name == Some("colonpairs"))
    else {
        return;
    };
    let RakuAstFieldValue::List(pairs) = &mut field.value else {
        return;
    };
    let [pair] = pairs.as_mut_slice() else {
        return;
    };
    let ValueView::RakuAst(node) = pair.view() else {
        return;
    };
    let mut node = RakuAstNode::clone(node);
    let to = span.to as usize;
    if node.class == RakuAstClass::ColonPairValue {
        let Some(key) = colonpair_key(&node) else {
            return;
        };
        let mut quote_from = None;
        super::infix_origins::with_child(&mut node, "value", |quote| {
            quote_from = word_quote_at_end(quote, span.from as usize, to);
        });
        let Some(quote_from) = quote_from else {
            return;
        };
        let from = quote_from.saturating_sub(key.len() + 1);
        let written =
            super::unit_source::with_text(crate::ast::SrcSpan::new(from, quote_from), |t| {
                (t.strip_prefix(':') == Some(key.as_str())).then_some(())
            });
        if from <= span.from as usize || written.is_none() {
            return;
        }
        super::origin::set_span(&mut node, crate::ast::SrcSpan::new(from, to));
    } else if word_quote_at_end(&mut node, span.from as usize, to).is_none() {
        return;
    }
    *pair = Value::rakuast(Box::new(node));
}

/// The key of a `ColonPair::Value` (`sym` of `:sym<#>`).
// Cost: O(f), f = fields of the pair.
fn colonpair_key(pair: &RakuAstNode) -> Option<String> {
    pair.fields.iter().find_map(|f| match (&f.name, &f.value) {
        (Some("key"), RakuAstFieldValue::Node(v)) => Some(v.to_string_value()),
        _ => None,
    })
}

/// Place `quote`, a `QuotedString` of one `StrLiteral` written as `<text>`
/// ending at `to` (after `floor`), and its literal; the offset it starts at.
// Cost: O(f + t), f = fields of the quote, t = length of its text.
fn word_quote_at_end(quote: &mut RakuAstNode, floor: usize, to: usize) -> Option<usize> {
    if quote.class != RakuAstClass::QuotedString {
        return None;
    }
    let segments = quote
        .fields
        .iter_mut()
        .find(|f| f.name == Some("segments"))?;
    let RakuAstFieldValue::List(items) = &mut segments.value else {
        return None;
    };
    let [item] = items.as_mut_slice() else {
        return None;
    };
    let ValueView::RakuAst(literal) = item.view() else {
        return None;
    };
    let mut literal = RakuAstNode::clone(literal);
    let text = literal.fields.first().and_then(|f| match &f.value {
        RakuAstFieldValue::Node(v) => Some(v.to_string_value()),
        _ => None,
    })?;
    // `<` + text + `>`, the last characters of the name.
    let from = to.checked_sub(text.len() + 2)?;
    let written = super::unit_source::with_text(crate::ast::SrcSpan::new(from, to), |t| {
        (t.strip_prefix('<').and_then(|t| t.strip_suffix('>')) == Some(text.as_str())).then_some(())
    });
    if from <= floor || written.is_none() {
        return None;
    }
    super::origin::set_span(&mut literal, crate::ast::SrcSpan::new(from + 1, to - 1));
    *item = Value::rakuast(Box::new(literal));
    super::origin::set_span(quote, crate::ast::SrcSpan::new(from, to));
    Some(from)
}
