//! `Str`'s rows, and the `Cool` text methods that stringify into them.
//!
//! Rakudo declares each text method twice: on `Str` (the `Str:D` candidate,
//! which does the work) and on `Cool` (a candidate that calls `self.Str` and
//! then `Str`'s). Both owners get a row here and both rows point at the same
//! handler, which reads its receiver through `grapheme_index::with_str`: that
//! borrows a `Str`'s payload and stringifies anything else, which is what the
//! `Cool` candidate does. So `"abc".flip` and `42.flip` reach one
//! implementation, and every other shape whose MRO has `Cool` (`Int`, `Num`,
//! the rationals, `Complex`, `List`, `Array`, `Map`, `Hash`) finds the `Cool`
//! row. The cascade arms that still answer receivers with no shape (`Bool`,
//! instances of `Cool` subclasses) call the same handlers.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::grapheme_index::with_str;
use crate::value::{RuntimeError, Value, ValueView};

/// Zero-argument rows owned by `$owner`.
macro_rules! rows {
    ($owner:literal: $($name:literal => $handler:ident),* $(,)?) => {
        &[$(MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Pure($handler),
            flags: RowFlags::NONE,
            named: &[],
        }),*]
    };
}

/// The text methods both `Str` and `Cool` declare.
macro_rules! text_rows {
    ($owner:literal) => {
        rows![$owner:
            "chars" => chars,
            "codes" => codes,
            "ord" => ord,
            "uc" => uc,
            "lc" => lc,
            "fc" => fc,
            "tc" => tc,
            "tclc" => tclc,
            "flip" => flip,
            "trim" => trim,
            "trim-leading" => trim_leading,
            "trim-trailing" => trim_trailing,
            "chomp" => chomp,
            "chop" => chop,
        ]
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Str",
        name: "Bool",
        arity: 0,
        handler: Handler::Pure(bool),
        flags: RowFlags::NONE,
        named: &[],
    },
    chop_n_row("Str"),
    chop_n_row("Cool"),
    MethodRow {
        owner: "Str",
        name: "chomp",
        arity: 1,
        handler: Handler::Narrow(chomp_needle),
        flags: RowFlags::ANY_ARGS,
        named: &[],
    },
    MethodRow {
        owner: "Str",
        name: "leading-whitespace",
        arity: 0,
        handler: Handler::Pure(leading_whitespace),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Str",
        name: "trailing-whitespace",
        arity: 0,
        handler: Handler::Pure(trailing_whitespace),
        flags: RowFlags::NONE,
        named: &[],
    },
];

/// `chop($n)`, which `Str` and `Cool` both declare.
const fn chop_n_row(owner: &'static str) -> MethodRow {
    MethodRow {
        owner,
        name: "chop",
        arity: 1,
        handler: Handler::Narrow(chop_n),
        flags: RowFlags::ANY_ARGS,
        named: &[],
    }
}
pub(super) static STR_TEXT_ROWS: &[MethodRow] = text_rows!("Str");
pub(super) static COOL_TEXT_ROWS: &[MethodRow] = text_rows!("Cool");

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

/// `Str.chomp(Str $needle)`: drops `$needle` from the end when the string
/// ends in it (rakudo's own `RakuAST::Deparse` dedents with it).
// Cost: O(n + m), n = chars of the invocant, m = chars of the needle.
fn chomp_needle(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let needle = args.first()?.to_string_value();
    Some(Ok(with_str(target, |s| {
        match s.strip_suffix(needle.as_str()) {
            Some(rest) => Value::str_from(rest),
            None => Value::str_from(s),
        }
    })))
}

/// `Str.leading-whitespace`: the whitespace `trim-leading` drops.
// Cost: O(n), n = chars of the invocant (result copied).
pub(crate) fn leading_whitespace(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(with_str(target, |s| {
        Value::str_from(&s[..s.len() - s.trim_start().len()])
    }))
}

/// `Str.trailing-whitespace`: the whitespace `trim-trailing` drops.
// Cost: O(n), n = chars of the invocant (result copied).
pub(crate) fn trailing_whitespace(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(with_str(target, |s| {
        Value::str_from(&s[s.trim_end().len()..])
    }))
}

/// `Str.chomp`: drops one trailing line ending.
// Cost: O(1) when nothing is chomped (the receiver is returned); O(n)
// otherwise, n = chars of the invocant.
pub(crate) fn chomp(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(crate::builtins::chomp_value(target))
}

/// `Str.chop(Int() $n)`: drops the last `$n` characters (all of them when
/// `$n` is at least the length; none when it is not positive). `$n` is
/// coerced as `.Int` does: a `Str` is parsed, any other number truncated.
// Cost: O(n), n = chars of the invocant (result copied).
fn chop_n(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let mut arg = args.first()?;
    // An allomorph count reads its number.
    if let ValueView::Mixin(inner, _) = arg.view() {
        arg = inner;
    }
    let n = match arg.view() {
        ValueView::Int(i) => i.max(0) as usize,
        ValueView::BigInt(bi) => {
            use num_traits::{Signed, ToPrimitive};
            if bi.is_negative() {
                0
            } else {
                bi.to_usize().unwrap_or(usize::MAX)
            }
        }
        // A plain string coerces like `.Int` (parse an integer literal).
        ValueView::Str(s) => s.trim().parse::<usize>().unwrap_or(1),
        // Any other number (Num/Rat/FatRat/allomorph) truncates toward zero:
        // `.chop(3.6)` chops 3.
        _ => {
            let f = arg.to_f64();
            if f.is_finite() && f > 0.0 {
                f.trunc() as usize
            } else {
                0
            }
        }
    };
    Some(Ok(with_str(target, |s| {
        let keep = s.chars().count().saturating_sub(n);
        Value::str(s.chars().take(keep).collect())
    })))
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
