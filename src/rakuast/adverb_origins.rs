//! Origins of the adverbs written after a subscript (ADR-10723 Stage 2):
//! `@a[0] :v`, `%h{"a"}:exists`, `%h<a> :!delete`.
//!
//! The subscript's range runs to the end of its last adverb, as Rakudo's
//! does, so the adverbs are read back from the end of that text: each is a
//! `:name` or `:!name` (a `ColonPair::True` / `ColonPair::False`), and what
//! precedes them must end with the subscript's closing bracket. An adverb with
//! a value (`:k(1)`) is not read, and leaves the subscript as it was: exact or
//! absent.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::value::{Value, ValueView};

/// One adverb as written: its range, whether it is negated, and its name.
pub(super) struct Adverb {
    at: SrcSpan,
    negated: bool,
    name: String,
}

/// Split the subscript text written at `from..to` into the part ending with
/// its closing bracket (whose end this returns) and the adverbs after it.
/// `None` when the text after the bracket is not only such adverbs.
// Cost: O(n), n = to - from.
pub(super) fn split(from: usize, to: usize, close: &str) -> Option<(usize, Vec<Adverb>)> {
    super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let mut end = text.len();
        let mut adverbs = Vec::new();
        loop {
            let head = text[..end].trim_end();
            if head.ends_with(close) {
                end = head.len();
                break;
            }
            let colon = head.rfind(':')?;
            let word = &head[colon + 1..];
            let (negated, name) = match word.strip_prefix('!') {
                Some(name) => (true, name),
                None => (false, word),
            };
            let ident = |c: char| c.is_alphanumeric() || c == '_' || c == '-';
            if name.is_empty() || !name.chars().all(ident) || !name.starts_with(char::is_alphabetic)
            {
                return None;
            }
            adverbs.push(Adverb {
                at: SrcSpan::new(from + colon, from + head.len()),
                negated,
                name: name.to_string(),
            });
            end = colon;
        }
        adverbs.reverse();
        Some((from + end, adverbs))
    })
}

/// Give the `colonpairs` of `subscript` the ranges of `adverbs`, when there is
/// one written adverb per pair and each spells its pair.
// Cost: O(a), a = adverbs.
pub(super) fn place(subscript: &mut RakuAstNode, adverbs: &[Adverb]) {
    if adverbs.is_empty() {
        return;
    }
    let Some(field) = subscript
        .fields
        .iter_mut()
        .find(|f| f.name == Some("colonpairs"))
    else {
        return;
    };
    let RakuAstFieldValue::List(items) = &mut field.value else {
        return;
    };
    if items.len() != adverbs.len() {
        return;
    }
    let spelled = items
        .iter()
        .zip(adverbs)
        .all(|(item, adverb)| match item.view() {
            ValueView::RakuAst(pair) => {
                let class = if adverb.negated {
                    RakuAstClass::ColonPairFalse
                } else {
                    RakuAstClass::ColonPairTrue
                };
                pair.class == class
                    && super::infix_origins::leaf_text(pair).is_some_and(|name| name == adverb.name)
            }
            _ => false,
        });
    if !spelled {
        return;
    }
    for (item, adverb) in items.iter_mut().zip(adverbs) {
        if let ValueView::RakuAst(pair) = item.view() {
            let mut pair = RakuAstNode::clone(pair);
            super::origin::set_span(&mut pair, adverb.at);
            *item = Value::rakuast(Box::new(pair));
        }
    }
}
