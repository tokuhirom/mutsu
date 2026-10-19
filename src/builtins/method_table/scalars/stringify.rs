//! `Str` with no arguments on `Str` and the numeric types.
//!
//! Rakudo has a `Str` method in the method table of `Str`, `Int`, `Num`,
//! `Rat`, `FatRat` and `Complex`. A `Str` is itself. A number renders the
//! way the rest of the runtime renders it (`Value::to_string_value`, also
//! behind `~$n` and interpolation). A rational with a zero denominator
//! cannot be rendered: its `.Str` throws `X::Numeric::DivideByZero` with the
//! interpreter's context, so that row declines it.
//!
//! Every candidate takes `*%_`, so a named argument it does not know is
//! ignored (`1e3.Str(:superscript)` is `"1000"`); `Int.Str(:superscript)` and
//! `Int.Str(:subscript)` render the digits raised or lowered (`:superscript`
//! wins when both are passed).

use super::{Handler, MethodRow, Named, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

/// One `Str` row per owner.
macro_rules! str_rows {
    ($($owner:literal),*) => {
        &[$(MethodRow {
            owner: $owner,
            name: "Str",
            arity: 0,
            handler: Handler::Named(str_named),
            flags: RowFlags::NONE,
            named: STR_NAMED,
        }),*]
    };
}

pub(super) static ROWS: &[MethodRow] = str_rows!("Str", "Num", "Rat", "FatRat", "Complex");

/// `Int.Str(:superscript, :subscript)`.
pub(super) static INT_ROWS: &[MethodRow] = &[MethodRow {
    owner: "Int",
    name: "Str",
    arity: 0,
    handler: Handler::Named(str_named),
    flags: RowFlags::NONE,
    named: STR_NAMED,
}];

/// The named arguments Rakudo's numeric `Str` candidates declare; every other
/// one is swallowed by the implicit `*%_`.
const STR_NAMED: &[&str] = &["subscript", "superscript"];

/// `.Str` with its named arguments: an `Int` reads `:superscript` and
/// `:subscript`, every receiver ignores the rest.
// Cost: O(n), n = chars of the rendering.
fn str_named(
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    if matches!(target.view(), ValueView::Int(_) | ValueView::BigInt(_)) {
        let set = |name| named.get(name).is_some_and(Value::truthy);
        let digits: Option<&[char; 11]> = if set("superscript") {
            Some(&SUPERSCRIPT)
        } else if set("subscript") {
            Some(&SUBSCRIPT)
        } else {
            None
        };
        if let Some(digits) = digits {
            return Some(Ok(Value::str(scripted(&target.to_string_value(), digits))));
        }
    }
    str(target, args)
}

/// The superscript digits `0`-`9`, then the superscript minus.
const SUPERSCRIPT: [char; 11] = [
    '\u{2070}', '\u{00B9}', '\u{00B2}', '\u{00B3}', '\u{2074}', '\u{2075}', '\u{2076}', '\u{2077}',
    '\u{2078}', '\u{2079}', '\u{207B}',
];

/// The subscript digits `0`-`9`, then the subscript minus.
const SUBSCRIPT: [char; 11] = [
    '\u{2080}', '\u{2081}', '\u{2082}', '\u{2083}', '\u{2084}', '\u{2085}', '\u{2086}', '\u{2087}',
    '\u{2088}', '\u{2089}', '\u{208B}',
];

/// An integer's decimal rendering with each digit and the sign replaced from
/// `digits`.
// Cost: O(n), n = chars of `decimal`.
fn scripted(decimal: &str, digits: &[char; 11]) -> String {
    decimal
        .chars()
        .map(|c| match c {
            '-' => digits[10],
            d if d.is_ascii_digit() => digits[(d as u8 - b'0') as usize],
            _ => c,
        })
        .collect()
}

/// `.Str`: the receiver itself for a `Str`, its rendering for a number, or
/// `X::Numeric::DivideByZero` for a zero-denominator rational.
// Cost: O(1) for a `Str` (the value is shared, not copied); O(n) for a
// number, n = chars of the rendering.
fn str(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Str(_) => Some(Ok(target.clone())),
        ValueView::Rat(n, 0) | ValueView::FatRat(n, 0) => Some(Err(
            RuntimeError::rational_to_str_divide_by_zero(Value::int(n)),
        )),
        ValueView::BigRat(n, d) if num_traits::Zero::is_zero(d) => Some(Err(
            RuntimeError::rational_to_str_divide_by_zero(Value::from_bigint(n.clone())),
        )),
        _ => Some(Ok(Value::str(target.to_string_value()))),
    }
}
