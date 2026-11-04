//! Origins inside a symbolic subrule call, `<::("name")>` / `<.::('name')>`:
//! the string literal that computes the name. Rakudo places the
//! `QuotedString` over the quotes and its `StrLiteral` over the text between
//! them. Only a plain quoted string (no escape, no interpolation) is read:
//! exact or absent.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::value::{Value, ValueView};

/// The length of the plain quoted string `text` starts with, quotes included.
// Cost: O(n), n = length of the string.
pub(super) fn plain_string_len(text: &str) -> Option<usize> {
    let quote = text.chars().next().filter(|c| matches!(c, '"' | '\''))?;
    let end = text[1..].find(quote)?;
    let body = &text[1..1 + end];
    (!body.contains(['\\', '{', '$', '@', '%', '&'])).then_some(end + 2)
}

/// Give the first `QuotedString` under `node`, and its `StrLiteral`, the
/// ranges of a string written at `from` with the length `len`.
// Cost: O(f), f = nodes searched.
pub(super) fn place_string(node: &mut RakuAstNode, from: usize, len: usize) -> bool {
    if node.class == RakuAstClass::QuotedString {
        super::origin::set_span(node, SrcSpan::new(from, from + len));
        for field in node.fields.iter_mut() {
            let held: Vec<&mut Value> = match &mut field.value {
                RakuAstFieldValue::Node(v) => vec![v],
                RakuAstFieldValue::List(items) => items.iter_mut().collect(),
                RakuAstFieldValue::Adverb(_) => continue,
            };
            for v in held {
                let ValueView::RakuAst(inner) = v.view() else {
                    continue;
                };
                if inner.class == RakuAstClass::StrLiteral {
                    let mut inner = RakuAstNode::clone(inner);
                    super::origin::set_span(&mut inner, SrcSpan::new(from + 1, from + len - 1));
                    *v = Value::rakuast(Box::new(inner));
                }
            }
        }
        return true;
    }
    for field in node.fields.iter_mut() {
        let held: Vec<&mut Value> = match &mut field.value {
            RakuAstFieldValue::Node(v) => vec![v],
            RakuAstFieldValue::List(items) => items.iter_mut().collect(),
            RakuAstFieldValue::Adverb(_) => continue,
        };
        for v in held {
            let ValueView::RakuAst(inner) = v.view() else {
                continue;
            };
            let mut inner = RakuAstNode::clone(inner);
            if place_string(&mut inner, from, len) {
                *v = Value::rakuast(Box::new(inner));
                return true;
            }
        }
    }
    false
}
