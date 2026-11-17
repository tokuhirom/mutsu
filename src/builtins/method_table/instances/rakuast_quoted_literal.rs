//! `RakuAST::QuotedString.literal-value` and `maybe-compile-time-value`: the
//! value a quote denotes when the parser can know it, with no interpolation
//! and no run-time processor (#17922).
//!
//! Measured on rakudo 2026.09 (`RAKUDO_RAKUAST=1`):
//! - a quote of `StrLiteral` segments is their concatenation (`"abc"`, `""`);
//! - any other segment (`"a$x"`, `"a{1}b"`) makes it `Nil`, as does a
//!   processor that runs at run time (`qq:x/.../`, `exec`);
//! - `words` (`qw/a b/`, `q:w/.../`) is the list of its words, even of one
//!   (`("a",)`); `quotewords` (`«a "b c"»`) keeps a quoted atom as one word;
//! - `val` turns each unquoted word into its allomorph (`<a 42>` is
//!   `("a", IntStr.new(42, "42"))`), and a `< >` quote of exactly one word is
//!   that word itself (`<42>` is `IntStr.new(42, "42")`, `<a>` is `"a"`);
//! - `:stringify` answers the text before the word processors (`"a 42"`);
//! - a hand-built node's `val` leaves its words `Str`s.
//!
//! `maybe-compile-time-value` answers the same as `literal-value`.

use super::super::{Handler, MethodRow, Named, RowFlags};
use super::rakuast_visit::native_node;
use crate::rakuast::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::{RuntimeError, Value, ValueView};

/// One word of a `words`/`quotewords` quote: its text, and whether any of it
/// was quoted (a quoted word is never an allomorph).
struct Word {
    text: String,
    quoted: bool,
}

/// The processors a quote node lists (`words`, `val`, ...).
// Cost: O(p), p = processors.
fn processors(node: &RakuAstNode) -> Vec<String> {
    match node
        .fields
        .iter()
        .find(|f| f.name == Some("processors"))
        .map(|f| &f.value)
    {
        Some(RakuAstFieldValue::List(items)) => items.iter().map(Value::to_string_value).collect(),
        Some(RakuAstFieldValue::Node(value)) => match value.view() {
            ValueView::Array(items, _) => items.iter().map(Value::to_string_value).collect(),
            _ => vec![value.to_string_value()],
        },
        _ => Vec::new(),
    }
}

/// The segment nodes of a quote node.
// Cost: O(s), s = segments copied.
fn segments(node: &RakuAstNode) -> Vec<Value> {
    match node
        .fields
        .iter()
        .find(|f| f.name == Some("segments"))
        .map(|f| &f.value)
    {
        Some(RakuAstFieldValue::List(items)) => items.clone(),
        Some(RakuAstFieldValue::Node(value)) => match value.view() {
            ValueView::Array(items, _) => items.as_slice().to_vec(),
            _ => vec![value.clone()],
        },
        _ => Vec::new(),
    }
}

/// The text of a `StrLiteral` segment, or `None` for any other node.
// Cost: O(1).
fn str_literal(segment: &Value) -> Option<String> {
    let ValueView::RakuAst(node) = segment.view() else {
        return None;
    };
    (node.class == RakuAstClass::StrLiteral).then(|| {
        crate::rakuast::literal_queries::literal_compile_time_value(node).to_string_value()
    })
}

/// The text a `QuoteWordsAtom` segment protects (`"b c"` of `«a "b c"»`), or
/// `None` for any other node or an atom that interpolates.
// Cost: O(n), n = size of the atom.
fn quoted_atom(segment: &Value) -> Option<String> {
    let ValueView::RakuAst(node) = segment.view() else {
        return None;
    };
    if node.class != RakuAstClass::QuoteWordsAtom {
        return None;
    }
    let inner = node.fields.iter().find_map(|f| match &f.value {
        RakuAstFieldValue::Node(value) if f.name.is_none() => Some(value.clone()),
        _ => None,
    })?;
    let ValueView::RakuAst(inner) = inner.view() else {
        return None;
    };
    match literal_value(inner, false)?.view() {
        ValueView::Str(text) => Some(text.to_string()),
        _ => None,
    }
}

/// The words of a `words`/`quotewords` quote's segments.
// Cost: O(t), t = total text of the segments.
fn split_words(segments: &[Value]) -> Option<Vec<Word>> {
    let mut words = Vec::new();
    let mut current: Option<Word> = None;
    for segment in segments {
        if let Some(text) = str_literal(segment) {
            for ch in text.chars() {
                if ch.is_whitespace() {
                    words.extend(current.take());
                } else {
                    current
                        .get_or_insert_with(|| Word {
                            text: String::new(),
                            quoted: false,
                        })
                        .text
                        .push(ch);
                }
            }
        } else {
            let text = quoted_atom(segment)?;
            let word = current.get_or_insert_with(|| Word {
                text: String::new(),
                quoted: false,
            });
            word.text.push_str(&text);
            word.quoted = true;
        }
    }
    words.extend(current);
    Some(words)
}

/// `QuotedString.literal-value` of `node` (`stringify`: before the word
/// processors), `None` when the quote has no literal value.
// Cost: O(t), t = total text of the segments, plus `val` of each word.
pub(super) fn literal_value(node: &RakuAstNode, stringify: bool) -> Option<Value> {
    if !matches!(
        node.class,
        RakuAstClass::QuotedString | RakuAstClass::Heredoc
    ) {
        return None;
    }
    let processors = processors(node);
    if processors
        .iter()
        .any(|p| !matches!(p.as_str(), "words" | "quotewords" | "val"))
    {
        return None;
    }
    let segments = segments(node);
    let splits = processors
        .iter()
        .any(|p| matches!(p.as_str(), "words" | "quotewords"));
    if !splits || stringify {
        // A lone `val` (only a hand-built node has one) leaves the text a
        // `Str`, as rakudo answers it.
        let mut out = String::new();
        for segment in &segments {
            out.push_str(&str_literal(segment)?);
        }
        return Some(Value::str(out));
    }
    // rakudo's parser applies `val` to a quote's words as it builds the node;
    // a hand-built node (no origin) keeps its words `Str`s
    // (`QuotedString.new(..., :processors<words val>)` answers `("x", "42")`).
    let val = processors.iter().any(|p| p == "val") && node.origin.0.is_some();
    let mut words: Vec<Value> = split_words(&segments)?
        .into_iter()
        .map(|word| {
            let text = Value::str(word.text);
            if val && !word.quoted {
                crate::runtime::builtins_collection::builtin_val(&[text])
            } else {
                text
            }
        })
        .collect();
    // `<42>`: a `< >` quote of one word is that word.
    if val && words.len() == 1 {
        return words.pop();
    }
    Some(Value::array_with_kind(
        crate::gc::Gc::new(crate::value::ArrayData::new(words)),
        crate::value::ArrayKind::List,
    ))
}

// Cost: as `literal_value`.
fn literal_value_row(
    target: &Value,
    _args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let base = native_node(target)?;
    let ValueView::RakuAst(node) = base.view() else {
        return None;
    };
    if !matches!(
        node.class,
        RakuAstClass::QuotedString | RakuAstClass::Heredoc
    ) {
        return None;
    }
    let stringify = named.get("stringify").is_some_and(|v| v.truthy());
    Some(Ok(literal_value(node, stringify).unwrap_or(Value::NIL)))
}

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "RakuAST::QuotedString",
    name: "literal-value",
    arity: 0,
    handler: Handler::Named(literal_value_row),
    flags: RowFlags::OWNER_ONLY,
    named: &["force", "stringify"],
}];
