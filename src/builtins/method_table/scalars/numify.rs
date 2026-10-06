//! `Cool`'s numification, shared by the numeric rows Rakudo declares on `Cool`.
//!
//! `Cool.sin` is `self.Numeric.sin` in Rakudo, so the one row serves every
//! `Cool` receiver the table reaches: a `Str` numifies by parsing (and a
//! non-numeric one is the `X::Str::Numeric` `Failure` `.Numeric` gives), a
//! `List`, `Array` or `Hash` by its element count, and a number is itself.
//! The numeric owners' rows (`Int.sin`, `Rat.sin`, ...) read their receiver
//! through the same function, so there is one implementation however the call
//! arrives.

use crate::value::{Value, ValueView};
use std::borrow::Cow;

/// The number `target` stands for in a `Cool` numeric method: the receiver
/// itself when it is a number, the parsed value of a `Str`, the element count
/// of a `List`, `Array`, `Seq` or `Hash`. A `Str` that is not numeric is the
/// `Failure` the method answers.
// Cost: O(n) for a Str, n = chars (the parse); O(1) otherwise.
pub(crate) fn numify(target: &Value) -> Result<Cow<'_, Value>, Value> {
    if let ValueView::Str(s) = target.view() {
        return match numify_str(&s) {
            Some(number) => Ok(Cow::Owned(number)),
            None => Err(crate::builtins::methods_0arg::str_numeric_failure(&s)),
        };
    }
    let count = match target.view() {
        ValueView::Hash(map) => Some(map.len()),
        // A `List`, an `Array` and the list-likes (`Seq`, `Slip`, ...).
        _ => target.as_list_items().map(<[Value]>::len),
    };
    Ok(match count {
        Some(count) => Cow::Owned(Value::int(count as i64)),
        None => Cow::Borrowed(target),
    })
}

/// The number a `Str` numifies to the way `.Numeric` does: an integer, a
/// decimal (a `Rat`, so `"-5.9".abs` is the `Rat` 5.9, not the `Num`
/// 5.9000000000000004 an `f64` parse gave), or a `Rat`/`Complex` string
/// (`"6+8i"`, `"1/2"`); `None` when it is not a number.
// Cost: O(n), n = chars of the string.
pub(crate) fn numify_str(s: &str) -> Option<Value> {
    if let Ok(i) = s.parse::<i64>() {
        return Some(Value::int(i));
    }
    crate::runtime::str_numeric::parse_raku_str_to_numeric(s)
        .or_else(|| crate::builtins::methods_0arg::parse_raku_int_from_str(s))
}
