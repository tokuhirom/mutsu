//! `q:nfc'…'`, `q:nfd'…'`, `q:nfkc'…'`, `q:nfkd'…'` across the RakuAST
//! boundary.
//!
//! S02 specifies these quote adverbs as producing the `Uni` subtype they name
//! (`roast/S15-string-types/NF-types.t`). Rakudo 2026.09 has no node for them:
//! its grammar rejects the source (`Unrecognized adverb: :nfc`), and
//! `QuotedString.new(processors => ("nfc",))` dies with "Unsupported quoted
//! string processor". A quote adverb that post-processes the quoted text is
//! what a `QuotedString`'s `processors` already models (`:w` is `words`, `:v`
//! is `val`, `:exec` is `exec`), so mutsu reads the quote as
//!
//! ```text
//! q:nfc'ẛ̣'   QuotedString(processors => ("nfc",), segments => (StrLiteral("ẛ̣"),))
//! ```
//!
//! The parser folds the quote into its `Uni` value while parsing, so the
//! segment holds the normalized text rather than the text as written; lowering
//! normalizes it again, which leaves a normalized text unchanged, and so
//! restores the same value.

use super::convert::quoted_string;
use super::lower::{list_field, positional_leaf};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::value::{RuntimeError, UniData, Value, ValueView};

/// The processor naming the normalization `form` (`NFC` → `nfc`).
// Cost: O(1).
fn processor(form: &str) -> Option<&'static str> {
    Some(match form {
        "NFC" => "nfc",
        "NFD" => "nfd",
        "NFKC" => "nfkc",
        "NFKD" => "nfkd",
        _ => return None,
    })
}

/// A normalized-quote literal as its `QuotedString`.
// Cost: O(n), n = codepoints of the literal.
pub(super) fn convert(uni: &UniData) -> Result<RakuAstNode, RuntimeError> {
    let Some(processor) = processor(&uni.form) else {
        return Err(super::convert::unsupported(&format!(
            "literal {} value",
            uni.form
        )));
    };
    let mut quoted = quoted_string(Value::str(uni.text()));
    quoted.fields.insert(
        0,
        RakuAstField {
            name: Some("processors"),
            value: RakuAstFieldValue::List(vec![Value::str_from(processor)]),
        },
    );
    Ok(quoted)
}

/// A `QuotedString` whose only processor is a normalization form, over one
/// literal segment, as the `Uni` literal; `None` for any other string.
// Cost: O(n), n = length of the quoted text.
pub(super) fn lower(node: &RakuAstNode) -> Option<Result<Expr, RuntimeError>> {
    if node.class != RakuAstClass::QuotedString
        || !node.fields.iter().any(|f| f.name == Some("processors"))
    {
        return None;
    }
    let processors = list_field(node, "processors").ok()?;
    let [processor] = processors else {
        return None;
    };
    let adverb = processor.as_str()?;
    if !matches!(adverb, "nfc" | "nfd" | "nfkc" | "nfkd") {
        return None;
    }
    Some((|| {
        let segments = list_field(node, "segments")?;
        let [segment] = segments else {
            return Err(super::lower::unsupported(node));
        };
        let ValueView::RakuAst(segment) = segment.view() else {
            return Err(super::lower::unsupported(node));
        };
        if segment.class != RakuAstClass::StrLiteral {
            return Err(super::lower::unsupported(node));
        }
        let text = positional_leaf(segment)?;
        let ValueView::Str(text) = text.view() else {
            return Err(super::lower::unsupported(node));
        };
        crate::parser::normalized_quote(adverb, &text.to_string()).ok_or_else(|| super::lower::unsupported(node))
    })())
}
