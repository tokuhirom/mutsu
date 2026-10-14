//! The Unicode methods of `Cool`, `Str` and `Int`: `uniname`, `uninames`,
//! `uniprop`, `uniprops`, `unival`, `univals`, `unimatch`, `uniparse`,
//! `parse-names`, and the normalization forms `NFC`, `NFD`, `NFKC` and `NFKD`
//! (which `Uni` declares too).
//!
//! `Int` reads its receiver as a codepoint; `Str` and `Cool` read the first
//! character of the receiver's string form (all of it for the plural
//! methods). A type object of a built-in type answers the rows flagged
//! `TYPE_OBJECT_OK` the way these methods always did: `Int.uniname` is an
//! `X::Multi::NoMatch`, `Str.NFC` normalizes the type's name.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::grapheme_index::with_str;
use crate::builtins::methods_0arg::make_no_match_error;
use crate::builtins::str_prim::{Normal, normalize};
use crate::builtins::uniprop;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($owner:literal, $name:literal, $arity:literal, $handler:ident, $flags:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Narrow($handler),
            flags: $flags,
            named: &[],
        }
    };
}

/// A row that answers the type object of a built-in type too.
const TOK: RowFlags = RowFlags::TYPE_OBJECT_OK;
const NONE: RowFlags = RowFlags::NONE;
/// A row whose argument is any plain value (the property or value name).
const ARG: RowFlags = RowFlags::ANY_ARGS;
/// Both: an argument form that answers a type object too.
const ARG_TOK: RowFlags = RowFlags::ANY_ARGS.or(RowFlags::TYPE_OBJECT_OK);

pub(super) static STR_ROWS: &[MethodRow] = &[
    row!("Str", "uniname", 0, uniname, TOK),
    row!("Str", "uninames", 0, uninames, NONE),
    row!("Str", "uniprop", 0, uniprop, TOK),
    row!("Str", "uniprop", 1, uniprop_of, ARG_TOK),
    row!("Str", "uniprops", 0, uniprops, NONE),
    row!("Str", "uniprops", 1, uniprops_of, ARG_TOK),
    row!("Str", "unival", 0, unival, TOK),
    row!("Str", "univals", 0, univals, TOK),
    row!("Str", "unimatch", 1, unimatch, ARG_TOK),
    row!("Str", "unimatch", 2, unimatch_in, ARG_TOK),
    row!("Str", "uniparse", 0, uniparse, TOK),
    row!("Str", "NFC", 0, nfc, NONE),
    row!("Str", "NFD", 0, nfd, NONE),
    row!("Str", "NFKC", 0, nfkc, NONE),
    row!("Str", "NFKD", 0, nfkd, NONE),
];

pub(super) static COOL_ROWS: &[MethodRow] = &[
    row!("Cool", "uniname", 0, uniname, TOK),
    row!("Cool", "uninames", 0, uninames, NONE),
    row!("Cool", "uniprop", 0, uniprop, TOK),
    row!("Cool", "uniprop", 1, uniprop_of, ARG_TOK),
    row!("Cool", "uniprops", 0, uniprops, NONE),
    row!("Cool", "uniprops", 1, uniprops_of, ARG),
    row!("Cool", "unival", 0, unival, TOK),
    row!("Cool", "univals", 0, univals, TOK),
    row!("Cool", "unimatch", 1, unimatch, ARG),
    row!("Cool", "unimatch", 2, unimatch_in, ARG),
    row!("Cool", "uniparse", 0, uniparse, NONE),
    row!("Cool", "parse-names", 0, uniparse, TOK),
    row!("Cool", "NFC", 0, nfc, NONE),
    row!("Cool", "NFD", 0, nfd, NONE),
    row!("Cool", "NFKC", 0, nfkc, NONE),
    row!("Cool", "NFKD", 0, nfkd, NONE),
];

pub(super) static INT_ROWS: &[MethodRow] = &[
    row!("Int", "uniname", 0, uniname, TOK),
    row!("Int", "uniprop", 0, uniprop, TOK),
    row!("Int", "uniprop", 1, uniprop_of, ARG_TOK),
    row!("Int", "unival", 0, unival, TOK),
    row!("Int", "unimatch", 1, unimatch, ARG_TOK),
    row!("Int", "unimatch", 2, unimatch_in, ARG_TOK),
];

pub(super) static UNI_ROWS: &[MethodRow] = &[
    row!("Uni", "NFC", 0, uni_nfc, NONE),
    row!("Uni", "NFD", 0, uni_nfd, NONE),
    row!("Uni", "NFKC", 0, uni_nfkc, NONE),
    row!("Uni", "NFKD", 0, uni_nfkd, NONE),
];

/// A type object of a built-in type (`Int`, `Str`, ...) or `Nil`: no
/// candidate of these methods takes one.
fn is_type_object(target: &Value) -> bool {
    matches!(
        target.view(),
        ValueView::Nil | ValueView::Package(_) | ValueView::CustomType { .. }
    )
}

/// The codepoint an `Int` (or a `Bool`, the `Int` enum) receiver stands for.
fn codepoint(target: &Value) -> Option<i64> {
    match target.view() {
        ValueView::Int(i) => Some(i),
        ValueView::Bool(b) => Some(i64::from(b)),
        _ => None,
    }
}

/// The first character of the receiver's string form.
// Cost: O(1) for a Str (borrowed), O(n) for another receiver (stringified).
fn first_char(target: &Value) -> Option<char> {
    with_str(target, |s| s.chars().next())
}

/// `.uniname`: the name of the codepoint (an `Int` receiver) or of the first
/// character, `Nil` for the empty string.
// Cost: O(1) (a table lookup).
pub(crate) fn uniname(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if is_type_object(target) {
        return Some(Err(make_no_match_error("uniname")));
    }
    Some(match codepoint(target) {
        Some(cp) => crate::builtins::unicode::uniname_from_int(cp).map(Value::str),
        None => Ok(first_char(target).map_or(Value::NIL, |ch| {
            Value::str(crate::builtins::unicode::unicode_char_name(ch))
        })),
    })
}

/// `.uninames`: the names of every character, as a `Seq`.
// Cost: O(n), n = chars of the receiver.
pub(crate) fn uninames(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let names = with_str(target, |s| {
        s.chars()
            .map(|ch| Value::str(crate::builtins::unicode::unicode_char_name(ch)))
            .collect::<Vec<_>>()
    });
    Some(Ok(Value::seq(names)))
}

/// How an integer receiver of the Unicode methods reads as a codepoint.
enum IntCodepoint {
    /// A codepoint MoarVM looks up (`0 .. 2**31`, including the surrogates
    /// and the unassigned range past `0x10FFFF`).
    Lookup(u32),
    /// `2**31` or more (a `BigInt` included): past what MoarVM can look up.
    TooLarge,
}

/// Rakudo's error for a codepoint a Unicode routine cannot take:
/// `Codepoint -1 (0x-1) is out of bounds in 'unival'`.
// Cost: O(d), d = the receiver's digits (the message).
fn codepoint_out_of_bounds(target: &Value, routine: &str) -> RuntimeError {
    let (display, hex) = match target.view() {
        ValueView::BigInt(n) => {
            let sign = if n.sign() == num_bigint::Sign::Minus {
                "-"
            } else {
                ""
            };
            (n.to_string(), format!("{sign}{:X}", n.magnitude()))
        }
        _ => {
            let cp = codepoint(target).unwrap_or_default();
            let sign = if cp < 0 { "-" } else { "" };
            (cp.to_string(), format!("{sign}{:X}", cp.unsigned_abs()))
        }
    };
    RuntimeError::new(format!(
        "Codepoint {display} (0x{hex}) is out of bounds in '{routine}'"
    ))
}

/// Classify an `Int`/`BigInt`/`Bool` receiver as a codepoint; `None` for any
/// other receiver (read through its first character instead). A negative
/// codepoint is the X::AdHoc rakudo raises.
// Cost: O(1) for an Int; O(d) for a BigInt, d = limb count (the error message).
fn int_codepoint(target: &Value, routine: &str) -> Option<Result<IntCodepoint, RuntimeError>> {
    let negative = match target.view() {
        ValueView::BigInt(n) => n.sign() == num_bigint::Sign::Minus,
        _ => codepoint(target)? < 0,
    };
    if negative {
        return Some(Err(codepoint_out_of_bounds(target, routine)));
    }
    let cp = match target.view() {
        ValueView::BigInt(_) => None,
        _ => codepoint(target).and_then(|cp| u32::try_from(cp).ok()),
    };
    Some(Ok(cp
        .filter(|&cp| cp < 1 << 31)
        .map_or(IntCodepoint::TooLarge, IntCodepoint::Lookup)))
}

/// `.uniprop` and `.uniprop($property)` (and the `uniprop` routine, which
/// calls these): the property of the codepoint or first character.
// Cost: O(1) (a table lookup) plus the property name's parse.
fn uniprop_value(target: &Value, property: Option<&str>) -> Result<Value, RuntimeError> {
    if is_type_object(target) {
        return Err(make_no_match_error("uniprop"));
    }
    Ok(match int_codepoint(target, "uniprop").transpose()? {
        Some(IntCodepoint::Lookup(cp)) => {
            uniprop::unicode_property_value_for_codepoint(cp, property)
        }
        // MoarVM answers "" for every property here.
        Some(IntCodepoint::TooLarge) => Value::str_from(""),
        None => first_char(target).map_or(Value::NIL, |ch| match property {
            Some(property) => uniprop::unicode_property_value(ch, property),
            None => Value::str_from(crate::builtins::unicode::unicode_general_category(ch)),
        }),
    })
}

/// `.uniprop`: the general category of the codepoint or first character.
// Cost: O(1) (a table lookup).
pub(crate) fn uniprop(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(uniprop_value(target, None))
}

/// `.uniprop($property)`.
// Cost: O(1) (a table lookup) plus the property name's parse.
pub(crate) fn uniprop_of(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(uniprop_value(target, Some(&args[0].to_string_value())))
}

/// `.uniprops`: the general category of every character, as a `Seq`.
// Cost: O(n), n = chars of the receiver.
pub(crate) fn uniprops(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let props = with_str(target, |s| {
        s.chars()
            .map(|ch| Value::str_from(crate::builtins::unicode::unicode_general_category(ch)))
            .collect::<Vec<_>>()
    });
    Some(Ok(Value::seq(props)))
}

/// `.uniprops($property)`: one property value per character, as an `Array`.
// Cost: O(n), n = chars of the receiver.
pub(crate) fn uniprops_of(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let property = args[0].to_string_value();
    let props = with_str(target, |s| {
        s.chars()
            .map(|ch| uniprop::unicode_property_value(ch, &property))
            .collect::<Vec<_>>()
    });
    Some(Ok(Value::array(props)))
}

/// The numeric value Unicode assigns the character: a `Rat` for a fraction,
/// an `Int` for a digit or numeric character, `NaN` otherwise.
// Cost: O(1) (a table lookup).
fn unival_of_char(ch: char) -> Value {
    use crate::builtins::unicode::{
        unicode_decimal_digit_value, unicode_numeric_int_value, unicode_rat_value,
    };
    if let Some((n, d)) = unicode_rat_value(ch) {
        crate::value::make_rat(n, d)
    } else if let Some(n) = unicode_numeric_int_value(ch) {
        Value::int(n)
    } else if let Some(n) = unicode_decimal_digit_value(ch) {
        Value::int(i64::from(n))
    } else {
        Value::num(f64::NAN)
    }
}

/// `.unival`: the numeric value of the codepoint or first character: `Nil`
/// for the empty string or a codepoint past `0x10FFFF`, `NaN` for a surrogate;
/// a negative codepoint or one of `2**31` or more dies.
// Cost: O(1) (a table lookup).
pub(crate) fn unival(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if is_type_object(target) {
        return Some(Err(make_no_match_error("unival")));
    }
    Some(match int_codepoint(target, "unival") {
        Some(Ok(IntCodepoint::Lookup(cp))) => Ok(match char::from_u32(cp) {
            Some(ch) => unival_of_char(ch),
            // A surrogate has no numeric value; past 0x10FFFF there is none.
            None if (0xD800..=0xDFFF).contains(&cp) => Value::num(f64::NAN),
            None => Value::NIL,
        }),
        Some(Ok(IntCodepoint::TooLarge)) => Err(codepoint_out_of_bounds(target, "unival")),
        Some(Err(err)) => Err(err),
        None => Ok(first_char(target).map_or(Value::NIL, unival_of_char)),
    })
}

/// `.univals`: the numeric value of every character of the receiver's
/// string form, as a `Seq` -- an `Int` is not read as a codepoint here
/// (`65.univals` is `(6 5)`), as in rakudo.
// Cost: O(n), n = chars of the receiver.
pub(crate) fn univals(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if is_type_object(target) {
        return Some(Err(make_no_match_error("univals")));
    }
    let vals = with_str(target, |s| s.chars().map(unival_of_char).collect());
    Some(Ok(Value::seq(vals)))
}

/// `.unimatch($value)`: whether the codepoint or first character has the
/// property value.
// Cost: O(1) (a table lookup) plus the value name's parse.
pub(crate) fn unimatch(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    unimatch_with(target, &args[0].to_string_value(), None)
}

/// `.unimatch($value, $property)`.
// Cost: as `unimatch`.
pub(crate) fn unimatch_in(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    unimatch_with(
        target,
        &args[0].to_string_value(),
        Some(&args[1].to_string_value()),
    )
}

fn unimatch_with(
    target: &Value,
    value: &str,
    property: Option<&str>,
) -> Option<Result<Value, RuntimeError>> {
    if is_type_object(target) {
        return Some(Err(make_no_match_error("unimatch")));
    }
    Some(Ok(match int_codepoint(target, "unimatch") {
        Some(Ok(IntCodepoint::Lookup(cp))) => uniprop::unimatch_for_codepoint(cp, value, property),
        Some(Ok(IntCodepoint::TooLarge)) => Value::FALSE,
        Some(Err(err)) => return Some(Err(err)),
        None => first_char(target).map_or(Value::NIL, |ch| {
            Value::truth(uniprop::unimatch(ch, value, property))
        }),
    }))
}

/// `.uniparse` and `.parse-names`: the string of the characters the
/// comma-separated Unicode names stand for.
// Cost: O(n), n = chars of the receiver; a name no table knows also walks the
// CLDR emoji list (O(E) per such name, E = emoji count).
pub(crate) fn uniparse(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(crate::builtins::functions::uniparse_impl(
        &target.to_string_value(),
    ))
}

/// The text a normalization form reads: the codepoints of an `Array` of
/// `Int`s (a `Uni`-like list), else the string form.
// Cost: O(n), n = codepoints of the receiver.
fn normalization_text(target: &Value) -> String {
    match target.view() {
        ValueView::Array(items, ..)
            if items.iter().all(|v| matches!(v.view(), ValueView::Int(_))) =>
        {
            items
                .iter()
                .filter_map(|v| match v.view() {
                    ValueView::Int(cp) => char::from_u32(cp as u32),
                    _ => None,
                })
                .collect()
        }
        _ => target.to_string_value(),
    }
}

/// The `Uni` of `text` normalized to `form`.
// Cost: O(n), n = codepoints of the text.
fn normalized(name: &str, form: Normal, text: &str) -> Value {
    Value::uni(name.to_string(), normalize(text, form).into_owned())
}

macro_rules! normalization {
    ($($handler:ident, $uni_handler:ident: $name:literal => $form:expr;)*) => {
        $(
            // Cost: O(n), n = codepoints of the receiver.
            pub(crate) fn $handler(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
                Some(Ok(normalized($name, $form, &normalization_text(target))))
            }

            // Cost: O(n), n = codepoints of the `Uni`.
            fn $uni_handler(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
                match target.view() {
                    ValueView::Uni(u) => Some(Ok(normalized($name, $form, &u.text()))),
                    _ => None,
                }
            }
        )*
    };
}

normalization! {
    nfc, uni_nfc: "NFC" => Normal::Nfc;
    nfd, uni_nfd: "NFD" => Normal::Nfd;
    nfkc, uni_nfkc: "NFKC" => Normal::Nfkc;
    nfkd, uni_nfkd: "NFKD" => Normal::Nfkd;
}
