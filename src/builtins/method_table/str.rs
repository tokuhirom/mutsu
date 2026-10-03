//! `Str`'s rows.
//!
//! Rakudo declares each of these on `Str` (the `Str:D` candidates) and again
//! on `Cool`, whose candidates stringify the invocant and call `Str`'s. Here
//! the `Str` rows hold the implementation, and the cascade arms that still
//! answer `Cool` receivers (`42.flip`, `1.5.uc`) call the same handlers, so
//! each method has one implementation however the call arrives. Every handler
//! reads its receiver through `grapheme_index::with_str`, which borrows a
//! `Str`'s payload and stringifies anything else.

use super::{Handler, MethodRow};
use crate::builtins::grapheme_index::with_str;
use crate::value::{RuntimeError, Value};

/// A zero-argument `Str` row.
macro_rules! str_rows {
    ($($name:literal => $handler:ident),* $(,)?) => {
        &[$(MethodRow {
            owner: "Str",
            name: $name,
            arity: 0,
            handler: Handler::Pure($handler),
        }),*]
    };
}

pub(super) static ROWS: &[MethodRow] = str_rows![
    "chars" => chars,
    "Bool" => bool,
    "codes" => codes,
    "ord" => ord,
    "uc" => uc,
    "lc" => lc,
    "fc" => fc,
    "tc" => tc,
    "tclc" => tclc,
    "wordcase" => wordcase,
    "flip" => flip,
    "trim" => trim,
    "trim-leading" => trim_leading,
    "trim-trailing" => trim_trailing,
    "chomp" => chomp,
    "chop" => chop,
];

// Cost: O(1) amortized: the grapheme count comes from the payload's cached
// index (built in O(n) on first use, `grapheme_index`).
fn chars(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::int(crate::builtins::str_prim::chars(target) as i64))
}

// Cost: O(1), an emptiness test.
fn bool(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::truth(target.truthy()))
}

/// `Str.codes`: the number of codepoints.
// Cost: O(n), n = bytes of the invocant.
pub(crate) fn codes(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(with_str(target, |s| Value::int(s.chars().count() as i64)))
}

/// `Str.ord`: the first codepoint, or `Nil` for the empty string.
// Cost: O(1) (borrows the payload).
pub(crate) fn ord(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(with_str(target, |s| match s.chars().next() {
        Some(ch) => Value::int(i64::from(u32::from(ch))),
        None => Value::NIL,
    }))
}

/// `Str.uc`.
// Cost: O(n), n = chars of the invocant (per-grapheme NFD + case map, then NFC).
pub(crate) fn uc(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::str(with_str(
        target,
        crate::builtins::unicode::grapheme_uppercase,
    )))
}

/// `Str.lc`.
// Cost: O(n), n = chars of the invocant (lowercase, then NFC).
pub(crate) fn lc(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::str(with_str(
        target,
        crate::builtins::unicode::grapheme_lowercase,
    )))
}

/// `Str.fc`.
// Cost: O(n), n = chars of the invocant (per-grapheme NFD + fold, then NFC).
pub(crate) fn fc(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::str(with_str(
        target,
        crate::builtins::unicode::grapheme_foldcase,
    )))
}

/// `Str.tc`.
// Cost: O(n), n = chars of the invocant (copies the tail and NFCs the whole
// result).
pub(crate) fn tc(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::str(with_str(
        target,
        crate::builtins::unicode::titlecase_string,
    )))
}

/// `Str.tclc`.
// Cost: O(n), n = chars of the invocant.
pub(crate) fn tclc(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::str(with_str(target, crate::value::tclc_str)))
}

/// `Str.wordcase` with no `:filter` / `:where`.
// Cost: O(n), n = chars of the invocant (one segmenting pass, tclc per word).
pub(crate) fn wordcase(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::str(with_str(target, crate::value::wordcase_str)))
}

/// `Str.flip`: the graphemes in reverse order.
// Cost: O(n), n = chars of the invocant.
pub(crate) fn flip(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(with_str(target, crate::builtins::str_prim::flip))
}

/// `Str.trim`.
// Cost: O(n), n = chars of the invocant (the scan touches only the ends, but
// the result is copied).
pub(crate) fn trim(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(with_str(target, |s| Value::str_from(s.trim())))
}

/// `Str.trim-leading`.
// Cost: O(n), n = chars of the invocant (result copied).
pub(crate) fn trim_leading(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(with_str(target, |s| Value::str_from(s.trim_start())))
}

/// `Str.trim-trailing`.
// Cost: O(n), n = chars of the invocant (result copied).
pub(crate) fn trim_trailing(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(with_str(target, |s| Value::str_from(s.trim_end())))
}

/// `Str.chomp`: drops one trailing line ending.
// Cost: O(1) when nothing is chomped (the receiver is returned); O(n)
// otherwise, n = chars of the invocant.
pub(crate) fn chomp(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(crate::builtins::chomp_value(target))
}

/// `Str.chop`: drops the last character.
// Cost: O(n), n = chars of the invocant (result copied).
pub(crate) fn chop(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(with_str(target, |s| {
        let mut chars = s.chars();
        chars.next_back();
        Value::str_from(chars.as_str())
    }))
}
