//! The text a quoted regex term (`"x\ny"`, `'a\'b'`) denotes.
//!
//! A double-quoted regex term is a `qq` string, so its backslash escapes
//! decode exactly as in an ordinary `"..."` literal (`process_escape_sequence`).
//! A single-quoted one is a `q` string: only `\\` and the closing quote are
//! escapes.

use super::escapes::process_escape_sequence;

/// The decoded text of a qq regex term. Interpolating bodies and unsupported
/// escapes return `None`; callers must use the actual qq expression parser for
/// interpolation rather than guessing a decoded literal.
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
            // A bare `%name` is literal qq text; a postcircumfix interpolates.
            '%' if hash_access(&rest[1..]) => return None,
            '&' if calls_a_routine(&rest[1..]) => return None,
            _ => {
                text.push(ch);
                rest = &rest[ch.len_utf8()..];
            }
        }
    }
    Some(text)
}

/// Whether a hash name is followed by an interpolation postcircumfix.
// Cost: O(k), k = length of the name.
fn hash_access(rest: &str) -> bool {
    let name_len = rest
        .find(|c: char| !(c.is_alphanumeric() || matches!(c, '_' | '-' | '\'')))
        .unwrap_or(rest.len());
    let tail = &rest[name_len..];
    name_len > 0
        && (tail.starts_with(['<', '{', '['])
            || tail.strip_prefix('.').is_some_and(calls_a_routine))
}

/// Whether `rest`, right after a `&`, is a routine name followed by `(`.
// Cost: O(k), k = length of the name.
fn calls_a_routine(rest: &str) -> bool {
    let name_len = rest
        .find(|c: char| !(c.is_alphanumeric() || c == '_' || c == '-' || c == '\''))
        .unwrap_or(rest.len());
    name_len > 0 && rest[name_len..].starts_with('(')
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bare_percent_text_does_not_hide_malformed_escapes() {
        assert_eq!(decode_qq_regex_quote("a%b"), Some("a%b".to_string()));
        assert_eq!(
            decode_qq_regex_quote("%h.keys"),
            Some("%h.keys".to_string())
        );
        assert!(decode_qq_regex_quote("%b\\x[QQ]").is_none());
        assert!(decode_qq_regex_quote("%h<a>").is_none());
        assert!(decode_qq_regex_quote("%h.keys()").is_none());
    }
}
