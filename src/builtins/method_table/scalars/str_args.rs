//! `Str`'s and `Cool`'s text methods that take an argument or return another
//! type (ADR-11276 §9.74): `samecase`, `samemark`, `indent` and `encode` on
//! both owners; `parse-base`, `substr-eq`, `naive-word-wrapper`, `Date` and
//! `DateTime` on `Str`; `encode` on `Uni`.
//!
//! As in `str.rs`, a `Cool` row points at the `Str` row's handler, which reads
//! the receiver through `with_str` (a `Str` is borrowed, anything else
//! stringified, which is what Rakudo's `Cool` candidate does). Each handler is
//! the one implementation: the cascade arms that still answer receivers with no
//! shape (an allomorph, a `Cool` subclass instance) call it too.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::grapheme_index::{with_str, with_str_index};
use crate::builtins::method_table::Named;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($owner:literal, $name:literal, $arity:literal, $handler:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: $handler,
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

/// The rows `Str` and `Cool` both declare.
macro_rules! shared_rows {
    ($owner:literal) => {
        &[
            row!($owner, "samecase", 1, Handler::Pure(samecase)),
            row!($owner, "samemark", 1, Handler::Pure(samemark)),
            row!($owner, "indent", 1, Handler::Pure(indent)),
            row!($owner, "encode", 0, Handler::Pure(encode)),
        ]
    };
}

pub(super) static COOL_ROWS: &[MethodRow] = shared_rows!("Cool");
pub(super) static STR_SHARED_ROWS: &[MethodRow] = shared_rows!("Str");
pub(super) static STR_ROWS: &[MethodRow] = &[
    row!("Str", "Stringy", 0, Handler::Pure(stringy)),
    row!("Str", "parse-base", 1, Handler::Narrow(parse_base)),
    row!("Str", "substr-eq", 1, Handler::Narrow(substr_eq)),
    row!("Str", "substr-eq", 2, Handler::Narrow(substr_eq)),
    row!("Str", "Date", 0, Handler::Narrow(date)),
    row!("Str", "DateTime", 0, Handler::Narrow(date_time)),
    MethodRow {
        owner: "Str",
        name: "naive-word-wrapper",
        arity: 0,
        handler: Handler::Named(naive_word_wrapper),
        flags: RowFlags::NONE,
        named: &["indent", "max"],
    },
];
pub(super) static UNI_ROWS: &[MethodRow] = &[row!("Uni", "encode", 0, Handler::Pure(uni_encode))];

/// `Str.Stringy`: the string itself.
// Cost: O(1) (the value is shared, not copied).
fn stringy(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(target.clone())
}

/// `.samecase($pattern)`: the receiver with each character cased like the
/// pattern's character at the same position (the last one repeats).
// Cost: O(n + m), n = chars of the invocant, m = chars of the pattern.
pub(crate) fn samecase(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    let pattern = args[0].to_string_value();
    Ok(with_str(target, |s| {
        Value::str(crate::builtins::samecase_string(s, &pattern))
    }))
}

/// `.samemark($source)`: the receiver's base characters with the source's marks.
// Cost: O(n + m), n = chars of the invocant, m = chars of the mark source.
pub(crate) fn samemark(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    let source = args[0].to_string_value();
    Ok(with_str(target, |s| {
        Value::str(crate::builtins::samemark_string(s, &source))
    }))
}

/// `.indent($steps)`: each line's leading whitespace widened or narrowed by
/// `$steps` columns (`*` removes the common indent). Removing more than a line
/// has warns, with the result as the warning's resumption value.
// Cost: O(n + L * s), n = chars of the invocant, L = lines, s = |steps|.
pub(crate) fn indent(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    let (result, warning) = with_str(target, |s| {
        crate::builtins::methods_narg::indent::str_indent(s, &args[0])
    })?;
    match warning {
        Some(message) => Err(RuntimeError::warn_signal_with_resume(
            message,
            Value::str(result),
        )),
        None => Ok(Value::str(result)),
    }
}

/// `.encode`: the receiver's UTF-8 bytes as a `utf8` blob (a `Uni`'s as a
/// `Buf`, see [`uni_encode`]).
// Cost: O(n), n = bytes of the invocant.
pub(crate) fn encode(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    if let ValueView::Uni(_) = target.view() {
        return uni_encode(target, args);
    }
    Ok(with_str(target, |s| {
        crate::value::value_buf::make_buf_from_bytes(Symbol::intern("utf8"), s.as_bytes())
    }))
}

/// `Uni.encode`: the codepoints as they are (no normalization), UTF-8
/// encoded into a `Buf`, as Rakudo's `Uni.encode` answers.
// Cost: O(n), n = codepoints.
fn uni_encode(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Uni(u) => Ok(crate::value::value_buf::make_buf_from_u8(
            u.text().as_bytes(),
        )),
        _ => Err(RuntimeError::new("Uni.encode: receiver is not a Uni")),
    }
}

/// Rakudo's `Str.parse-base` candidates, as its `X::Multi::NoMatch` lists them
/// (the `"camel"` and `"beer"` ones are an easter egg mutsu does not answer).
const PARSE_BASE_CANDIDATES: [&str; 3] = [
    "(Str:D $:: Int:D $radix, *%_ --> Numeric:D)",
    "(Str:D $:: \"camel\", *%_ --> Int:D)",
    "(Str:D $:: \"beer\", *%_ --> Int:D)",
];

/// `Str.parse-base($radix)`: the number the string spells in that radix.
/// Rakudo's only candidate takes an `Int:D` radix, so any other radix is
/// `X::Multi::NoMatch`; a big `Int` takes the cascades.
// Cost: see `parse_base` (src/builtins/parse_base.rs).
pub(crate) fn parse_base(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let radix = match args[0].view() {
        ValueView::Int(radix) => radix,
        ValueView::BigInt(_) => return None,
        _ => {
            let arg_type = match args[0].view() {
                ValueView::Package(_) | ValueView::Instance { .. } | ValueView::Mixin(..) => {
                    crate::value::types::what_type_name(&args[0])
                }
                _ => crate::runtime::utils::value_type_name(&args[0]).to_string(),
            };
            let smiley = if crate::runtime::types::value_is_defined(&args[0]) {
                ":D"
            } else {
                ":U"
            };
            return Some(Err(
                crate::runtime::methods_signature_errors::make_multi_no_match_error_detailed(
                    "parse-base",
                    "Str",
                    true,
                    &format!("{arg_type}{smiley}"),
                    &PARSE_BASE_CANDIDATES.map(str::to_string),
                ),
            ));
        }
    };
    Some(with_str(target, |s| {
        crate::builtins::parse_base::parse_base(s, radix)
    }))
}

/// `Str.substr-eq($needle, $pos = 0)`: whether the needle occurs at grapheme
/// position `$pos`. A position past the end is `False`, as in Rakudo. Only a
/// non-negative `Int` position binds: a negative one (an `X::OutOfRange`
/// `Failure`), a `WhateverCode` and a numeric string take the interpreter's
/// position resolution.
// Cost: O(m) amortized, m = chars of the needle, once the invocant's grapheme
// index is cached (`$pos` is resolved through it).
pub(crate) fn substr_eq(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let pos = match args.get(1).map(|pos| pos.view()) {
        None => 0,
        Some(ValueView::Int(pos)) if pos >= 0 => pos as usize,
        Some(_) => return None,
    };
    let needle = args[0].to_string_value();
    with_str_index(target, |text, idx| {
        let eq = pos <= idx.len()
            && crate::builtins::str_prim::eq_at(
                text,
                idx,
                pos,
                &needle,
                crate::builtins::str_prim::Fold::Exact,
            );
        Some(Ok(Value::truth(eq)))
    })
}

/// `Str.Date`: the `Date` an ISO `yyyy-mm-dd` string names.
// Cost: O(n), n = chars of the invocant.
pub(crate) fn date(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    use crate::builtins::methods_0arg::temporal::{make_date, parse_date_string};
    let ValueView::Str(s) = target.view() else {
        return None;
    };
    Some(parse_date_string(&s).map(|(y, m, d)| make_date(y, m, d)))
}

/// `Str.DateTime`: the `DateTime` an ISO string names; a bare `yyyy-mm-dd` is
/// midnight UTC.
// Cost: O(n), n = chars of the invocant.
pub(crate) fn date_time(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    use crate::builtins::methods_0arg::temporal::{
        make_datetime, parse_date_string, parse_datetime_string,
    };
    let ValueView::Str(s) = target.view() else {
        return None;
    };
    Some(if s.contains(['T', 't']) {
        parse_datetime_string(&s)
            .map(|(y, mo, d, h, mi, se, tz)| make_datetime(y, mo, d, h, mi, se, tz))
    } else {
        parse_date_string(&s).map(|(y, m, d)| make_datetime(y, m, d, 0, 0, 0.0, 0))
    })
}

/// `Str.naive-word-wrapper(:$max, :$indent)`.
// Cost: O(n), n = bytes of the invocant.
fn naive_word_wrapper(
    target: &Value,
    _args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let pairs: Vec<&Value> = named.pairs().iter().collect();
    crate::builtins::naive_word_wrapper::native_naive_word_wrapper(target, &pairs)
}
