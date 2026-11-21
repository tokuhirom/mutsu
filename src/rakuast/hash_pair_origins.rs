//! Origins of a literal-keyed pair in a hash composer (`{"a" => 1}`,
//! `{1 => 2}`; ADR-10723 Stage 2, #15780). The parser keeps the written key
//! beside the value rather than as a term of the unit, so its range is read
//! back from the source: the key ends right before the `=>` that precedes the
//! value, whose range is known.

use super::infix_origins::{byte_range, child, with_child};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;
use crate::scope::NameScope;

/// How far before the value the key and its `=>` are looked for.
const WINDOW: usize = 1024;

/// Place `pair`, an `ApplyInfix` of `=>` whose value has a range and whose
/// key is a quoted string or a number, and then derive its operator and
/// argument list as for any infix application.
// Cost: O(w), w = the bytes read before the value (at most `WINDOW`).
pub(super) fn place(names: &NameScope, pair: &mut RakuAstNode) {
    if pair.class != RakuAstClass::ApplyInfix || pair.origin.0.is_some() {
        return;
    }
    let Some((value_from, value_to)) = child(pair, "right").and_then(byte_range) else {
        return;
    };
    let quoted = match child(pair, "left") {
        Some(left) if left.origin.0.is_none() => left.class == RakuAstClass::QuotedString,
        _ => return,
    };
    let Some(key) = key_before(value_from, quoted) else {
        return;
    };
    with_child(pair, "left", |left| {
        super::origin::set_span(left, key);
        super::origin::quoted_text(left, key);
    });
    super::origin::set_span(pair, SrcSpan::new(key.from as usize, value_to));
    super::infix_origins::derive(names, pair);
}

/// The range of the key written before the `=>` that ends right before byte
/// `value_from`: a `'...'` / `"..."` string without escapes when `quoted`,
/// else a number.
// Cost: O(w), w = the bytes read (at most `WINDOW`).
fn key_before(value_from: usize, quoted: bool) -> Option<SrcSpan> {
    let mut start = value_from.saturating_sub(WINDOW);
    let text = loop {
        if let Some(text) =
            super::unit_source::with_text(SrcSpan::new(start, value_from), |t| Some(t.to_string()))
        {
            break text;
        }
        // Not on a character boundary: step forward to one.
        start += 1;
        if start >= value_from {
            return None;
        }
    };
    let head = text.trim_end().strip_suffix("=>")?.trim_end();
    let end = start + head.len();
    let len = if quoted {
        let quote = head.chars().last().filter(|c| matches!(c, '"' | '\''))?;
        let body = &head[..head.len() - 1];
        let open = body.rfind(quote)?;
        if body[open + 1..].contains('\\') {
            return None;
        }
        head.len() - open
    } else {
        let token = head
            .rsplit(|c: char| !(c.is_alphanumeric() || matches!(c, '_' | '.')))
            .next()?;
        if !token.starts_with(|c: char| c.is_ascii_digit()) {
            return None;
        }
        token.len()
    };
    Some(SrcSpan::new(end - len, end))
}
