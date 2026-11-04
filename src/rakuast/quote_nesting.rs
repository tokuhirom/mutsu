//! Nested brackets inside a bracketed `q[...]` / `Q{...}` string (#15780).
//!
//! Rakudo reads a balanced inner bracket pair of such a quote as a quote of
//! its own: `Q[ab [c] d]` is
//! `QuotedString(segments => ("ab [", QuotedString("c"), "] d"))`, the text up
//! to and including the opener, the inner content, then the text from the
//! closer on, recursively. The parser keeps one literal, so the segments are
//! rebuilt from the source text, and only when that text read back is exactly
//! the literal's value (no escape, no adverb, no interpolation): exact or
//! absent.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::value::Value;

/// The piece of a split quote: text, or the quote an inner bracket pair holds.
enum Piece {
    Text {
        text: String,
        from: usize,
        to: usize,
    },
    Quote {
        pieces: Vec<Piece>,
        from: usize,
        to: usize,
    },
}

/// The segments for a quote written at `span` whose single literal reads
/// `value`, when it is a bracketed `q`/`Q` quote holding an inner bracket pair.
// Cost: O(n), n = length of the quote's source.
pub(super) fn segments(value: &str, span: SrcSpan) -> Option<Vec<Value>> {
    let from = span.from as usize;
    let split = super::unit_source::with_text(span, |written| {
        let mut chars = written.chars();
        let letter = chars.next()?;
        let open = chars.next()?;
        let close = match open {
            '[' => ']',
            '{' => '}',
            '<' => '>',
            '(' => ')',
            _ => return None,
        };
        if !matches!(letter, 'q' | 'Q') || !written.ends_with(close) || written.len() < 4 {
            return None;
        }
        let content = &written[2..written.len() - 1];
        if content != value || !content.contains(open) {
            return None;
        }
        pieces(content, open, close, from + 2).filter(|p| p.len() > 1)
    })?;
    Some(split.iter().map(node).collect())
}

/// The pieces of `content`, which starts at offset `base`.
// Cost: O(n), n = length of the content.
fn pieces(content: &str, open: char, close: char, base: usize) -> Option<Vec<Piece>> {
    let mut out = Vec::new();
    let mut start = 0;
    let mut at = 0;
    while at < content.len() {
        let ch = content[at..].chars().next()?;
        if ch == close {
            return None;
        }
        if ch != open {
            at += ch.len_utf8();
            continue;
        }
        let inner_from = at + 1;
        let mut depth = 1;
        let mut end = inner_from;
        while end < content.len() {
            let c = content[end..].chars().next()?;
            if c == open {
                depth += 1;
            } else if c == close {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            end += c.len_utf8();
        }
        if depth != 0 {
            return None;
        }
        out.push(Piece::Text {
            text: content[start..inner_from].to_string(),
            from: base + start,
            to: base + inner_from,
        });
        let inner = &content[inner_from..end];
        let inner_pieces = if inner.contains(open) {
            pieces(inner, open, close, base + inner_from)?
        } else {
            vec![Piece::Text {
                text: inner.to_string(),
                from: base + inner_from,
                to: base + end,
            }]
        };
        out.push(Piece::Quote {
            pieces: inner_pieces,
            from: base + inner_from,
            to: base + end,
        });
        start = end;
        at = end + close.len_utf8();
    }
    out.push(Piece::Text {
        text: content[start..].to_string(),
        from: base + start,
        to: base + content.len(),
    });
    Some(out)
}

// Cost: O(size of the piece).
fn node(piece: &Piece) -> Value {
    let (node, from, to) = match piece {
        Piece::Text { text, from, to } => (
            RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::StrLiteral,
                fields: vec![RakuAstField {
                    name: None,
                    value: RakuAstFieldValue::Node(Value::str(text.clone())),
                }],
            },
            *from,
            *to,
        ),
        Piece::Quote { pieces, from, to } => (
            RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::QuotedString,
                fields: vec![RakuAstField {
                    name: Some("segments"),
                    value: RakuAstFieldValue::List(pieces.iter().map(node).collect()),
                }],
            },
            *from,
            *to,
        ),
    };
    let mut node = node;
    super::origin::set_span(&mut node, SrcSpan::new(from, to));
    Value::rakuast(Box::new(node))
}
