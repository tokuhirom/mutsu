//! `Str`'s search and slice rows: `contains`, `starts-with`, `ends-with`,
//! `index` and `rindex` with one needle, and `substr` with a start and an
//! optional length.
//!
//! As with the text rows (`str.rs`), Rakudo declares each method on `Str` and
//! again on `Cool`, whose candidate stringifies the invocant. Both owners'
//! rows point at one handler, which reads the receiver through
//! `grapheme_index::with_str_index` (a `Str` is borrowed, anything else
//! stringified) and the needle through its string form.
//!
//! The table only hands a row plain scalar arguments (`method_table::answer`),
//! so a `Regex`, `Junction`, `Pair`, list or type-object needle never reaches
//! these handlers; those calls take the cascades. `substr` is narrower still:
//! its row binds only a non-negative `Int` start and length inside the string
//! and declines anything else (a negative or `WhateverCode` position, a
//! `Range`, an out-of-range start that must answer a `Failure`).

use super::{Handler, MethodRow};
use crate::builtins::grapheme_index::{find_graphemes, with_str_index};
use crate::builtins::str_prim::{self, Fold};
use crate::value::{RuntimeError, Value};

/// The rows one owner declares.
macro_rules! search_rows {
    ($owner:literal) => {
        &[
            MethodRow {
                owner: $owner,
                name: "contains",
                arity: 1,
                handler: Handler::Pure(contains),
            },
            MethodRow {
                owner: $owner,
                name: "starts-with",
                arity: 1,
                handler: Handler::Pure(starts_with),
            },
            MethodRow {
                owner: $owner,
                name: "ends-with",
                arity: 1,
                handler: Handler::Pure(ends_with),
            },
            MethodRow {
                owner: $owner,
                name: "index",
                arity: 1,
                handler: Handler::Pure(index),
            },
            MethodRow {
                owner: $owner,
                name: "rindex",
                arity: 1,
                handler: Handler::Pure(rindex),
            },
            MethodRow {
                owner: $owner,
                name: "substr",
                arity: 1,
                handler: Handler::Narrow(substr),
            },
        ]
    };
}

pub(super) static STR_ROWS: &[MethodRow] = search_rows!("Str");
pub(super) static COOL_ROWS: &[MethodRow] = search_rows!("Cool");

/// `substr` with a start and a length. Its own table: a row is one arity, and
/// the one-argument form above has the method's first row.
pub(super) static STR_SUBSTR_2: &[MethodRow] = &[MethodRow {
    owner: "Str",
    name: "substr",
    arity: 2,
    handler: Handler::Narrow(substr),
}];
pub(super) static COOL_SUBSTR_2: &[MethodRow] = &[MethodRow {
    owner: "Cool",
    name: "substr",
    arity: 2,
    handler: Handler::Narrow(substr),
}];

/// `Str.contains($needle)`.
// Cost: O(p + m), p = match position, m = chars of the needle (the invocant
// is borrowed).
pub(crate) fn contains(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(with_str_index(target, |text, idx| {
        str_prim::contains(text, idx, 0, &args[0], Fold::Exact)
    }))
}

/// `Str.starts-with($needle)`.
// Cost: O(m), m = chars of the needle.
pub(crate) fn starts_with(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    affix(target, &args[0], true)
}

/// `Str.ends-with($needle)`.
// Cost: O(m), m = chars of the needle.
pub(crate) fn ends_with(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    affix(target, &args[0], false)
}

fn affix(target: &Value, needle: &Value, is_prefix: bool) -> Result<Value, RuntimeError> {
    let needle = needle.to_string_value();
    Ok(Value::truth(str_prim::affix_matches(
        target,
        &needle,
        is_prefix,
        Fold::Exact,
    )))
}

/// `Str.index($needle)`: the grapheme position of the first match, or `Nil`.
// Cost: O(p + m) amortized, p = match position, m = chars of the needle; the
// byte offset is converted through the cached grapheme index.
pub(crate) fn index(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    let needle = args[0].to_string_value();
    Ok(with_str_index(target, |s, idx| {
        match find_graphemes(s, idx, 0, &needle) {
            Some(pos) => Value::int(idx.grapheme_at(s, pos) as i64),
            None => Value::NIL,
        }
    }))
}

/// `Str.rindex($needle)`: the grapheme position of the last match, or `Nil`.
// Cost: O(n - p + m) amortized, p = match position, m = chars of the needle.
pub(crate) fn rindex(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    let needle = args[0].to_string_value();
    Ok(with_str_index(target, |s, idx| {
        match str_prim::rindex(s, idx, idx.len(), &needle) {
            Some(g) => Value::int(g as i64),
            None => Value::NIL,
        }
    }))
}

/// `Str.substr($start, $len?)` for a non-negative `Int` start inside the
/// string and an optional non-negative `Int` length; `None` for any other
/// arguments (see the module docs).
// Cost: O(k) amortized, k = chars returned (`native_substr_slice`).
pub(crate) fn substr(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    crate::builtins::substr::native_substr_slice(target, &args[0], args.get(1))
}
