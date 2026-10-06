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

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::grapheme_index::{find_graphemes, with_str_index};
use crate::builtins::str_prim::{self, Fold};
use crate::value::{RuntimeError, Value, ValueView};

/// The rows one owner declares.
macro_rules! search_rows {
    ($owner:literal) => {
        &[
            MethodRow {
                owner: $owner,
                name: "contains",
                arity: 1,
                handler: Handler::Pure(contains),
                flags: RowFlags::NONE,
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "starts-with",
                arity: 1,
                handler: Handler::Pure(starts_with),
                flags: RowFlags::NONE,
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "ends-with",
                arity: 1,
                handler: Handler::Pure(ends_with),
                flags: RowFlags::NONE,
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "index",
                arity: 1,
                handler: Handler::Pure(index),
                flags: RowFlags::NONE,
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "rindex",
                arity: 1,
                handler: Handler::Pure(rindex),
                flags: RowFlags::NONE,
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "substr",
                arity: 1,
                handler: Handler::Narrow(substr),
                flags: RowFlags::NONE,
                named: &[],
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
    flags: RowFlags::NONE,
    named: &[],
}];
pub(super) static COOL_SUBSTR_2: &[MethodRow] = &[MethodRow {
    owner: "Cool",
    name: "substr",
    arity: 2,
    handler: Handler::Narrow(substr),
    flags: RowFlags::NONE,
    named: &[],
}];

/// The one-needle `contains` candidate shared by Str, Cool and Map.
// Cost: O(p + m), p = match position, m = chars of the needle (the invocant
// is borrowed).
pub(crate) fn contains(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    let result = Ok(with_str_index(target, |text, idx| {
        str_prim::contains(text, idx, 0, &args[0], Fold::Exact)
    }));
    search_warning(target, "contains", result)
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

/// The one-needle `index` candidate: the first grapheme position or `Nil`.
// Cost: O(p + m) amortized, p = match position, m = chars of the needle; the
// byte offset is converted through the cached grapheme index.
pub(crate) fn index(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    let needle = args[0].to_string_value();
    let result = Ok(with_str_index(target, |s, idx| {
        match find_graphemes(s, idx, 0, &needle) {
            Some(pos) => Value::int(idx.grapheme_at(s, pos) as i64),
            None => Value::NIL,
        }
    }));
    search_warning(target, "index", result)
}

/// The one-needle `rindex` candidate: the last grapheme position or `Nil`.
// Cost: O(n - p + m) amortized, p = match position, m = chars of the needle.
pub(crate) fn rindex(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    let needle = args[0].to_string_value();
    let result = Ok(with_str_index(target, |s, idx| {
        match str_prim::rindex(s, idx, idx.len(), &needle) {
            Some(g) => Value::int(g as i64),
            None => Value::NIL,
        }
    }));
    search_warning(target, "rindex", result)
}

/// Add Rakudo's collection-search worry while preserving the ordinary
/// stringified-search answer.
// Cost: O(1), formatting one fixed-size warning and keeping the precomputed result.
fn search_warning(
    target: &Value,
    method: &str,
    result: Result<Value, RuntimeError>,
) -> Result<Value, RuntimeError> {
    let message = match target.dispatch_shape() {
        Some(crate::value::DispatchShape::List) => {
            let advice = match method {
                "contains" => "did you mean '$item (elem) @list'?",
                "index" => "did you mean '.first( ..., :k)'?",
                "rindex" => "did you mean '.first( ..., :k, :end)'?",
                _ => return result,
            };
            format!("Calling '.{method}' on a List, {advice}")
        }
        Some(crate::value::DispatchShape::Array) => {
            let advice = match method {
                "contains" => "did you mean '$item (elem) @list'?",
                "index" => "did you mean '.first( ..., :k)'?",
                "rindex" => "did you mean '.first( ..., :k, :end)'?",
                _ => return result,
            };
            format!("Calling '.{method}' on a Array, {advice}")
        }
        _ if matches!(method, "contains" | "index") => {
            let kind = match target.view() {
                ValueView::Hash(_) => Some(if target.is_immutable_map() {
                    "Map"
                } else {
                    "Hash"
                }),
                ValueView::Scalar(_) if target.is_immutable_map() => Some("Map"),
                _ => None,
            };
            let Some(kind) = kind else {
                return result;
            };
            format!(
                "Applying '.{method}' to a {kind} will look at its .Str representation. Did\nyou mean '{kind}{{needle}}:exists'?"
            )
        }
        _ => return result,
    };
    match result {
        Ok(value) => Err(RuntimeError::warn_signal_with_resume(message, value)),
        Err(error) => Err(error),
    }
}

/// `Str.substr($start, $len?)` for a non-negative `Int` start inside the
/// string and an optional non-negative `Int` length; `None` for any other
/// arguments (see the module docs).
// Cost: O(k) amortized, k = chars returned (`native_substr_slice`).
pub(crate) fn substr(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    crate::builtins::substr::native_substr_slice(target, &args[0], args.get(1))
}
