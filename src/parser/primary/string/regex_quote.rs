//! The text a quoted regex term (`"x\ny"`, `'a\'b'`) denotes.
//!
//! A double-quoted regex term is a `qq` string, so its backslash escapes
//! decode exactly as in an ordinary `"..."` literal (`process_escape_sequence`).
//! A single-quoted one is a `q` string: only `\\` and the closing quote are
//! escapes.

use super::escapes::process_escape_sequence;

/// The decoded text of the body of a `"..."` regex term, or `None` when the
/// body interpolates (`$x`, `@a[0]`, `{ ... }`, `&f()`) or holds an escape
/// a plain string would not accept. The source-level regex tree has no node
/// for interpolated segments yet, so such a term keeps the runtime parser.
// Cost: O(n), n = length of the body.
pub(crate) fn decode_qq_regex_quote(body: &str) -> Option<String> {
    let mut text = String::new();
    let mut rest = body;
    while let Some(ch) = rest.chars().next() {
        match ch {
            '\\' => {
                rest = process_escape_sequence(rest, &mut text, &['"', '{', '}'])
                    .ok()??
                    .0;
            }
            '$' | '@' | '{' => return None,
            '&' if rest[1..].starts_with(|c: char| c.is_alphabetic() || c == '_') => {
                return None;
            }
            _ => {
                text.push(ch);
                rest = &rest[ch.len_utf8()..];
            }
        }
    }
    Some(text)
}

/// The decoded text of the body of a `'...'` (or `‘...’`) regex term whose
/// closing quote is `close`.
// Cost: O(n), n = length of the body.
pub(crate) fn decode_q_regex_quote(body: &str, close: char) -> String {
    let mut text = String::new();
    let mut chars = body.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '\\'
            && let Some(&next) = chars.peek()
            && (next == '\\' || next == close)
        {
            chars.next();
            text.push(next);
        } else {
            text.push(ch);
        }
    }
    text
}
