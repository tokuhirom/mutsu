//! Codepoint validation for `Str` construction (#14416).
//!
//! A `Str` never holds a surrogate codepoint (U+D800..U+DFFF): the maintainer
//! decided (ADR-12640, Rejected) that every construction path refuses one with
//! `X::InvalidCodepoint` instead of dropping it or reporting a misleading
//! error. `chr`, `.chr`, `chrs`, `nqp::chr`, `Uni.new` and the `\x`/`\o`/`\c`
//! escape decoders all validate through here, so the paths cannot disagree.

use crate::value::RuntimeError;

/// The largest Unicode scalar value.
const MAX_CODEPOINT: i64 = 0x10FFFF;

/// Whether `code` is a UTF-16 surrogate codepoint.
// Cost: O(1).
pub(crate) fn is_surrogate(code: i64) -> bool {
    (0xD800..=0xDFFF).contains(&code)
}

/// Rakudo's `X::InvalidCodepoint` message for a surrogate.
// Cost: O(1).
pub(crate) fn surrogate_message(code: i64) -> String {
    format!("Invalid surrogate codepoint {code}")
}

/// The `X::InvalidCodepoint` error for a surrogate, in the typed-message
/// convention (`code` is derived from the text).
// Cost: O(1).
pub(crate) fn surrogate_error(code: i64) -> RuntimeError {
    RuntimeError::new(format!("X::InvalidCodepoint: {}", surrogate_message(code)))
}

/// The scalar value `Uni.new` accepts for `code`: a surrogate or a value
/// outside Unicode raises `X::InvalidCodepoint` with Rakudo's message.
// Cost: O(1).
pub(crate) fn uni_char(code: i64) -> Result<char, RuntimeError> {
    if is_surrogate(code) {
        return Err(surrogate_error(code));
    }
    if !(0..=MAX_CODEPOINT).contains(&code) {
        return Err(RuntimeError::new(format!(
            "X::InvalidCodepoint: Invalid codepoint {code}; must not exceed 0x10ffff (1114111)"
        )));
    }
    char::from_u32(code as u32).ok_or_else(|| surrogate_error(code))
}

/// The scalar value `chrs`/`nqp::chr` build for `code`: a surrogate raises
/// `X::InvalidCodepoint`; a negative or too large value keeps Rakudo's
/// `X::AdHoc` text.
// Cost: O(1).
pub(crate) fn chr_char(code: i64) -> Result<char, RuntimeError> {
    if is_surrogate(code) {
        return Err(surrogate_error(code));
    }
    if code < 0 {
        return Err(RuntimeError::new(format!(
            "chr codepoint {code} cannot be negative"
        )));
    }
    if code > MAX_CODEPOINT {
        return Err(RuntimeError::new(format!(
            "chr codepoint {code} (0x{code:X}) is out of bounds"
        )));
    }
    char::from_u32(code as u32).ok_or_else(|| surrogate_error(code))
}
