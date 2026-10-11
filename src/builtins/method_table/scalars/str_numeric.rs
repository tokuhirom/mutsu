//! `Str`'s and `Cool`'s conversions to a number: `Str.Numeric`, `Str.Int`,
//! `Str.Num`, and `Cool.Num`, `Cool.Rat`, `Cool.UInt` (ADR-11276 §9.71). `Cool.Int` has
//! no row: `List`, `Map`, `Str` and the numeric shapes all declare their own `Int`, so
//! no shape would reach it (`every_row_is_reached_and_answers`).
//!
//! Rakudo's `Cool` bodies are `self.Numeric.METHOD`, so a `Cool` row numifies its
//! receiver through [`numify`] (a `List`, `Array`, `Hash` is its element count)
//! and calls the numeric types' routine. A `Str` numifies by parsing, and that
//! parse has cases the numeric types do not: an empty string is zero, a string
//! that reads as a `Complex` (`"1+2i".Int`) is coerced as that `Complex`, which
//! needs `$*TOLERANCE` and so the interpreter (the handler declines it), and a
//! string that is no number is the lazy `X::Str::Numeric` `Failure`.
//!
//! The `*_of_str` functions are the one implementation: the rows call them, and
//! so do the cascade arms that still answer receivers with no shape (an
//! allomorph, a user subclass).

use super::numify::numify;
use super::{Handler, MethodRow, RowFlags};
use crate::builtins::methods_0arg::{parse_raku_int_from_str, str_numeric_failure};
use crate::value::str_numeric::str_numifies_to_complex;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! pure {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Pure($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

macro_rules! narrow {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

pub(super) static STR_ROWS: &[MethodRow] = &[
    pure!("Str", "Numeric", str_numeric),
    narrow!("Str", "Int", str_int),
    narrow!("Str", "Num", str_num),
];

pub(super) static COOL_ROWS: &[MethodRow] = &[
    narrow!("Cool", "Num", cool_num),
    narrow!("Cool", "Rat", cool_rat),
    narrow!("Cool", "UInt", cool_uint),
];

/// The error a `Str` row raises for a receiver that is not a `Str`; the table
/// never lets one through.
fn not_a_str(method: &str) -> RuntimeError {
    RuntimeError::new(format!("Str.{method}: receiver is not a Str"))
}

fn str_numeric(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Str(s) => Ok(numeric_of_str(&s)),
        _ => Err(not_a_str("Numeric")),
    }
}

fn str_int(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Str(s) => int_of_str(&s).map(Ok),
        _ => Some(Err(not_a_str("Int"))),
    }
}

fn str_num(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Str(s) => num_of_str(&s).map(Ok),
        _ => Some(Err(not_a_str("Num"))),
    }
}

/// The number a `Cool` receiver other than a `Str` stands for, for the
/// conversions below: `Ok(Some(number))`, `Ok(None)` for a receiver that is no
/// number (the call takes the cascades), or the `Failure` to answer.
// Cost: see `numify`.
fn number_of(target: &Value) -> Result<Option<Value>, Value> {
    let number = numify(target)?.into_owned();
    // A `Complex` is a `Real` only within `$*TOLERANCE`: the interpreter's.
    Ok(match number.view() {
        ValueView::Complex(..) => None,
        _ => Some(number),
    })
}

/// `Cool.Int`, the first step of `Cool.UInt`.
// Cost: O(d^2) for a Str of d digits; O(1) otherwise.
fn cool_int(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let ValueView::Str(s) = target.view() {
        return int_of_str(&s).map(Ok);
    }
    match number_of(target) {
        Ok(number) => super::coerce::int_of(&number?).map(Ok),
        Err(failure) => Some(Ok(failure)),
    }
}

/// `Cool.Num`.
// Cost: as `cool_int`.
fn cool_num(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let ValueView::Str(s) = target.view() {
        return num_of_str(&s).map(Ok);
    }
    match number_of(target) {
        Ok(number) => super::coerce::num_of(&number?).map(Ok),
        Err(failure) => Some(Ok(failure)),
    }
}

/// `Cool.Rat`.
// Cost: as `cool_int`.
fn cool_rat(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let ValueView::Str(s) = target.view() {
        return rat_of_str(&s).map(Ok);
    }
    match number_of(target) {
        Ok(number) => super::numeric_coerce::rat_of(&number?).map(Ok),
        Err(failure) => Some(Ok(failure)),
    }
}

/// `Cool.UInt`: `.Int`, then the range check.
// Cost: as `cool_int`.
fn cool_uint(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let int = cool_int(target, &[])?;
    Some(int.map(uint_of_int))
}

/// `.Numeric` of a string: the number it spells (an `Int`, `Rat`, `Num` or
/// `Complex`), or the `X::Str::Numeric` `Failure`.
// Cost: O(d^2) for a d-digit integer string (radix parse), O(n) otherwise,
// n = chars of the string.
pub(crate) fn numeric_of_str(s: &str) -> Value {
    crate::runtime::str_numeric::parse_raku_str_to_numeric(s)
        .unwrap_or_else(|| str_numeric_failure(s))
}

/// `.Int` of a string. An empty or blank string is `0` (like `"".Numeric`); the
/// full numeric grammar is parsed and truncated (radix `:16<ff>`, rational `3/4`,
/// ...); a string that is no number is the `X::Str::Numeric` `Failure`. `None`
/// for a string that numifies to a `Complex` (`"1+2i"`), which is coerced as
/// that `Complex` (`Interpreter::dispatch_complex_to_real`).
// Cost: O(d^2) for a d-digit string (radix parse; a few O(n) copies first).
pub(crate) fn int_of_str(s: &str) -> Option<Value> {
    if s.trim().is_empty() {
        return Some(Value::int(0));
    }
    if let Some(v) = parse_raku_int_from_str(s) {
        return Some(v);
    }
    if str_numifies_to_complex(s).is_some() {
        return None;
    }
    Some(
        crate::runtime::str_numeric::parse_raku_str_to_numeric(s.trim())
            .as_ref()
            .and_then(super::coerce::int_of)
            .unwrap_or_else(|| str_numeric_failure(s)),
    )
}

/// `.Num` of a string, as [`int_of_str`] but always a `Num`. The sign of a zero
/// is the string's (`"-0".Num` is `-0e0`, roast S32-num/negative-zero.t).
// Cost: as `int_of_str`.
pub(crate) fn num_of_str(s: &str) -> Option<Value> {
    let trimmed = s.trim();
    if trimmed.is_empty() {
        return Some(Value::num(0.0));
    }
    // U+2212 MINUS SIGN reads as a hyphen-minus.
    let normalized = trimmed.replace('\u{2212}', "-");
    if str_numifies_to_complex(&normalized).is_some() {
        return None;
    }
    let Some(v) = crate::runtime::str_numeric::parse_raku_str_to_numeric(&normalized) else {
        return Some(str_numeric_failure(s));
    };
    let f = v.to_f64();
    // "-0" and "-0.0" parse as an Int or Rat zero, which has no sign.
    Some(Value::num(
        if f == 0.0 && f.is_sign_positive() && normalized.starts_with('-') {
            -0.0
        } else {
            f
        },
    ))
}

/// The real number a string numifies to for `.Rat` and `.FatRat`: `Err` is the
/// `X::Str::Numeric` `Failure`, `Ok(None)` a string that numifies to a `Complex`.
fn real_of_str(s: &str) -> Result<Option<Value>, Value> {
    let Some(number) = crate::runtime::str_numeric::parse_raku_str_to_numeric(s.trim()) else {
        return Err(str_numeric_failure(s));
    };
    Ok(match number.view() {
        ValueView::Complex(..) => None,
        _ => Some(number),
    })
}

/// `.Rat` of a string: the `Rat` its number converts to (`"0x1F".Rat` is `31.0`,
/// `"NaN".Rat` is `0/0`, a decimal stays exact). `None` for a string that
/// numifies to a `Complex`.
// Cost: O(d^2) for a d-digit string (the parse, then the rational's gcd).
pub(crate) fn rat_of_str(s: &str) -> Option<Value> {
    match real_of_str(s) {
        Err(failure) => Some(failure),
        Ok(number) => super::numeric_coerce::rat_of(&number?),
    }
}

/// `.FatRat` of a string, as [`rat_of_str`].
// Cost: as `rat_of_str`.
pub(crate) fn fat_rat_of_str(s: &str) -> Option<Value> {
    match real_of_str(s) {
        Err(failure) => Some(failure),
        Ok(number) => super::numeric_coerce::fat_rat_of(&number?),
    }
}

/// `.UInt` from an `.Int` answer: a negative one is a soft `X::OutOfRange`
/// `Failure` (a signature bind may reject it without ever exploding); anything
/// that is not an `Int` (a `Failure`) passes through.
// Cost: O(1).
pub(crate) fn uint_of_int(int: Value) -> Value {
    let negative = match int.view() {
        ValueView::Int(i) => i < 0,
        ValueView::BigInt(n) => n.sign() == num_bigint::Sign::Minus,
        _ => false,
    };
    if !negative {
        return int;
    }
    let msg = format!(
        "Coercion to UInt out of range. Is: {}, should be in 0..^Inf",
        int.to_string_value()
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg));
    attrs.insert(
        "what".to_string(),
        Value::str("Coercion to UInt".to_string()),
    );
    attrs.insert("got".to_string(), int);
    attrs.insert("range".to_string(), Value::str("0..^Inf".to_string()));
    let ex = Value::make_instance(crate::symbol::Symbol::intern("X::OutOfRange"), attrs);
    let mut failure_attrs = std::collections::HashMap::new();
    failure_attrs.insert("exception".to_string(), ex);
    failure_attrs.insert("handled".to_string(), Value::FALSE);
    Value::make_instance(crate::symbol::Symbol::intern("Failure"), failure_attrs)
}
