//! `Str`'s iteration rows: `comb`, `words`, `lines` and `ords` with no
//! arguments.
//!
//! As with the text rows (`str.rs`), Rakudo declares each method on `Str`
//! and again on `Cool`, whose candidate stringifies the invocant. Both
//! owners' rows point at one handler. `comb`, `words` and `lines` answer a
//! lazy `Seq` over the receiver's string form (`value::str_iter_seq`), and
//! `ords` an eager `Seq` of the NFC codepoints.

use super::{Handler, MethodRow, RowFlags};
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
            MethodRow {
                owner: $owner,
                name: "words",
                arity: 0,
                handler: Handler::Pure(words),
                flags: RowFlags::NONE,
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "lines",
                arity: 0,
                handler: Handler::Pure(lines),
                flags: RowFlags::NONE,
                named: &[],
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

/// `Str.lines`: a lazy Seq of the lines, each without its line ending.
// Cost: O(1), a lazy Seq over the invocant (`value::StrIterSpec`).
pub(crate) fn lines(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(crate::value::str_iter_seq(
        target,
        StrIterMode::Lines { chomp: true },
        None,
    ))
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
