//! Surrogate codepoint facts shared by the parser's escape decoder and the
//! `Str` construction primitives (`builtins::str_prim::codepoint`, #14416).
//! A leaf module: it names no upper layer.

/// Whether `code` is a UTF-16 surrogate codepoint (U+D800..U+DFFF).
// Cost: O(1).
pub(crate) fn is_surrogate(code: i64) -> bool {
    (0xD800..=0xDFFF).contains(&code)
}

/// Rakudo's `X::InvalidCodepoint` message for a surrogate.
// Cost: O(1).
pub(crate) fn surrogate_message(code: i64) -> String {
    format!("Invalid surrogate codepoint {code}")
}
