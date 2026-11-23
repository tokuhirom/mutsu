//! A balanced `<...>` inside a `<...>` word quote (`<a <b c> d>`,
//! `$m<value:sym<number>>`) (#18457).
//!
//! The words are the same either way (`<a <b c> d>` is `("a", "<b", "c>",
//! "d")`), but rakudo parses the inner brackets as a nested word quote, a
//! segment of its own between literal runs that keep the brackets:
//! `"a <"`, `QuotedString("b c")`, `"> d"`. The view splits the text that way;
//! lowering joins the segments back into the text.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// Nesting deeper than this stays one literal run.
const MAX_DEPTH: usize = 64;

/// The segments of a word quote over `text`: one literal, or, for a `words`
/// quote (`quotewords` is `false`), literal runs around the nested quotes of
/// its balanced `<...>` pairs.
// Cost: O(n * d), n = length of `text`, d = nesting depth.
pub(super) fn segments(quotewords: bool, text: &str, processors: &[Value]) -> Vec<Value> {
    if quotewords {
        return vec![literal(text)];
    }
    split(text, processors, 0).unwrap_or_else(|| vec![literal(text)])
}

// Cost: O(n * d), as `segments`.
fn split(text: &str, processors: &[Value], depth: usize) -> Option<Vec<Value>> {
    if depth >= MAX_DEPTH {
        return None;
    }
    let mut out = Vec::new();
    let mut run_start = 0;
    let mut chars = text.char_indices().peekable();
    let mut found = false;
    while let Some((at, c)) = chars.next() {
        if c != '<' {
            continue;
        }
        let close = matching_close(&text[at..])? + at;
        // The run up to and including `<`, the nested quote, then `>` starts
        // the next run.
        out.push(literal(&text[run_start..=at]));
        let inner = &text[at + 1..close];
        let nested = split(inner, processors, depth + 1).unwrap_or_else(|| vec![literal(inner)]);
        out.push(Value::rakuast(Box::new(quote(processors, nested))));
        run_start = close;
        found = true;
        while chars.peek().is_some_and(|(i, _)| *i <= close) {
            chars.next();
        }
    }
    if !found {
        return None;
    }
    out.push(literal(&text[run_start..]));
    Some(out)
}

/// The byte offset of the `>` that closes the `<` at the start of `text`.
// Cost: O(n), n = length of `text`.
fn matching_close(text: &str) -> Option<usize> {
    let mut depth = 0usize;
    for (i, c) in text.char_indices() {
        match c {
            '<' => depth += 1,
            '>' => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => {}
        }
    }
    None
}

/// The text of a `words` quote whose segments are literal runs and nested
/// quotes (as [`segments`] builds them); `None` for any other shape.
// Cost: O(n), n = size of the segments.
pub(super) fn joined(segments: &[Value]) -> Option<String> {
    if segments.len() < 2 {
        return None;
    }
    let mut out = String::new();
    join_into(segments, &mut out, 0)?;
    Some(out)
}

// Cost: O(n), as `joined`.
fn join_into(segments: &[Value], out: &mut String, depth: usize) -> Option<()> {
    if depth >= MAX_DEPTH {
        return None;
    }
    for segment in segments {
        let ValueView::RakuAst(node) = segment.view() else {
            return None;
        };
        match node.class {
            RakuAstClass::StrLiteral => {
                out.push_str(&super::infix_origins::leaf_text(node)?);
            }
            RakuAstClass::QuotedString => {
                let inner = node.fields.iter().find(|f| f.name == Some("segments"))?;
                let RakuAstFieldValue::List(items) = &inner.value else {
                    return None;
                };
                join_into(items, out, depth + 1)?;
            }
            _ => return None,
        }
    }
    Some(())
}

// Cost: O(n), n = length of `text`.
fn literal(text: &str) -> Value {
    Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::StrLiteral,
        fields: vec![super::view::leaf_field(None, Value::str(text.to_string()))],
    }))
}

// Cost: O(1) plus the moves.
fn quote(processors: &[Value], segments: Vec<Value>) -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::QuotedString,
        fields: vec![
            RakuAstField {
                name: Some("processors"),
                value: RakuAstFieldValue::List(processors.to_vec()),
            },
            RakuAstField {
                name: Some("segments"),
                value: RakuAstFieldValue::List(segments),
            },
        ],
    }
}
