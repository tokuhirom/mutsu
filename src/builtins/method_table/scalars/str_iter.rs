//! `Str`'s iteration rows: `comb`, `words`, `lines` and `ords` with no
//! arguments, and `comb` with an `Int` or `Str` matcher.
//!
//! As with the text rows (`str.rs`), Rakudo declares each method on `Str`
//! and again on `Cool`, whose candidate stringifies the invocant. Both
//! owners' rows point at one handler. `comb`, `words` and `lines` answer a
//! lazy `Seq` over the receiver's string form (`value::str_iter_seq`), and
//! `ords` an eager `Seq` of the NFC codepoints.

use super::{Handler, MethodRow, Named, RowFlags};
use crate::value::{RuntimeError, StrIterMode, Value};

/// The rows one owner declares.
macro_rules! iter_rows {
    ($owner:literal) => {
        &[
            MethodRow {
                owner: $owner,
                name: "comb",
                arity: 0,
                handler: Handler::Pure(comb),
                flags: RowFlags::NONE,
                named: &[],
            },
            // `comb($matcher)` with an `Int` chunk size or a `Str` needle;
            // a `Regex`, a code object or a `Pair` takes the interpreter.
            MethodRow {
                owner: $owner,
                name: "comb",
                arity: 1,
                handler: Handler::Narrow(crate::builtins::comb::native_comb_method),
                flags: RowFlags::NONE,
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "words",
                arity: 0,
                handler: Handler::Pure(words),
                flags: RowFlags::NONE,
                named: &[],
            },
            // `words($limit)`: `*`, `Inf` or a count, read as `.Int`.
            MethodRow {
                owner: $owner,
                name: "words",
                arity: 1,
                handler: Handler::Narrow(words_limit),
                flags: RowFlags::ANY_ARGS,
                named: &[],
            },
            // `lines(:$chomp, :$count)`: `:count` answers the number of lines.
            MethodRow {
                owner: $owner,
                name: "lines",
                arity: 0,
                handler: Handler::Named(lines),
                flags: RowFlags::NONE,
                named: &["chomp", "count"],
            },
            MethodRow {
                owner: $owner,
                name: "lines",
                arity: 1,
                handler: Handler::Named(lines_limit),
                flags: RowFlags::ANY_ARGS,
                named: &["chomp"],
            },
            MethodRow {
                owner: $owner,
                name: "ords",
                arity: 0,
                handler: Handler::Pure(ords),
                flags: RowFlags::NONE,
                named: &[],
            },
        ]
    };
}

pub(super) static STR_ROWS: &[MethodRow] = iter_rows!("Str");
pub(super) static COOL_ROWS: &[MethodRow] = iter_rows!("Cool");

/// `Str.comb`: a lazy Seq of the graphemes.
// Cost: O(1), a lazy Seq over the invocant (`value::StrIterSpec`).
pub(crate) fn comb(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(crate::value::str_iter_seq(
        target,
        StrIterMode::Graphemes,
        None,
    ))
}

/// `Str.words`: a lazy Seq of the whitespace-separated words.
// Cost: O(1), a lazy Seq over the invocant (`value::StrIterSpec`).
pub(crate) fn words(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(crate::value::str_iter_seq(target, StrIterMode::Words, None))
}

/// `Str.words($limit)`: at most `$limit` words; `*` and `Inf` cap nothing.
/// A limit that is not a number declines (the slow path reports it).
// Cost: O(1), a lazy Seq over the invocant (`value::StrIterSpec`).
fn words_limit(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let limit = crate::value::str_iter_limit(args.first()?)?;
    Some(Ok(crate::value::str_iter_seq(
        target,
        StrIterMode::Words,
        limit,
    )))
}

/// `Str.lines(:$chomp = True, :$count)`: a lazy Seq of the lines, each
/// without its line ending unless `:!chomp`; with a true `:count`, the number
/// of lines.
// Cost: O(1), a lazy Seq over the invocant (`value::StrIterSpec`); `:count`
// is O(n), n = bytes of the invocant, and builds no line.
pub(crate) fn lines(
    target: &Value,
    _args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let mode = StrIterMode::Lines {
        chomp: named.get("chomp").is_none_or(Value::truthy),
    };
    if named.get("count").is_some_and(Value::truthy) {
        let n = crate::value::str_iter_count(target, mode, None);
        return Some(Ok(Value::int(n as i64)));
    }
    Some(Ok(crate::value::str_iter_seq(target, mode, None)))
}

/// `Str.lines($limit, :$chomp = True)`: at most `$limit` lines.
// Cost: O(1), a lazy Seq over the invocant (`value::StrIterSpec`).
fn lines_limit(
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let limit = crate::value::str_iter_limit(args.first()?)?;
    let mode = StrIterMode::Lines {
        chomp: named.get("chomp").is_none_or(Value::truthy),
    };
    Some(Ok(crate::value::str_iter_seq(target, mode, limit)))
}

/// `Str.ords`: a Seq of the codepoints of the NFC form.
// Cost: O(n), n = chars of the invocant.
pub(crate) fn ords(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    use unicode_normalization::UnicodeNormalization;
    let ords: Vec<Value> = crate::builtins::grapheme_index::with_str(target, |s| {
        s.nfc()
            .map(|c| Value::int(i64::from(u32::from(c))))
            .collect()
    });
    Ok(Value::seq(ords))
}
