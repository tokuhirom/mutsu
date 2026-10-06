//! `Uni`'s rows: a `Uni` (and the normalization forms `NFC`, `NFD`, `NFKC`
//! and `NFKD`, which are `Uni`s) is a `Positional[uint32]` of codepoints.
//!
//! The shape stays closed to its ancestors' rows (ADR-11276 §9.15): Rakudo's
//! `Uni` is `Any` and `Mu`, not `Cool`, and the `Any` rows are written for
//! scalars and lists, so a `Uni` reaches only the rows it owns here and in
//! `truth` (`Bool`) and `unicode` (the normalization forms).

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($name:literal, $arity:literal, $handler:expr) => {
        MethodRow {
            owner: "Uni",
            name: $name,
            arity: $arity,
            handler: $handler,
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("elems", 0, Handler::Pure(elems)),
    row!("codes", 0, Handler::Pure(elems)),
    row!("Int", 0, Handler::Pure(elems)),
    row!("Numeric", 0, Handler::Pure(elems)),
    row!("Str", 0, Handler::Pure(str)),
    row!("list", 0, Handler::Pure(list)),
    row!("gist", 0, Handler::Pure(gist)),
    row!("raku", 0, Handler::Pure(raku)),
    row!("AT-POS", 1, Handler::Narrow(at_pos)),
    row!("EXISTS-POS", 1, Handler::Narrow(exists_pos)),
];

/// The `Uni` a handler was resolved for.
fn uni_of(target: &Value) -> Result<crate::value::UniData, RuntimeError> {
    match target.view() {
        ValueView::Uni(u) => Ok(u.clone()),
        _ => Err(RuntimeError::new("Uni: receiver is not a Uni")),
    }
}

/// `Uni.elems`, `.codes`, `.Int` and `.Numeric`: the number of codepoints.
// Cost: O(1) (the codepoint array's length; no text is built).
pub(crate) fn elems(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::int(uni_of(target)?.len() as i64))
}

/// `Uni.Str`: the codepoints composed to NFC.
// Cost: O(n), n = codepoints.
pub(crate) fn str(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    use unicode_normalization::UnicodeNormalization;
    let uni = uni_of(target)?;
    Ok(Value::str(uni.text().nfc().collect::<String>()))
}

/// `Uni.list`: the codepoints as `Int`s.
// Cost: O(n), n = codepoints.
pub(crate) fn list(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let uni = uni_of(target)?;
    Ok(Value::array(
        uni.text().chars().map(|c| Value::int(c as i64)).collect(),
    ))
}

/// `Uni.gist`: `NFC:0x<0061 0062>` (`Uni:0x<...>` for a plain `Uni`).
// Cost: O(n), n = codepoints.
pub(crate) fn gist(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let uni = uni_of(target)?;
    let codepoints: Vec<String> = uni
        .text()
        .chars()
        .map(|c| format!("{:04X}", c as u32))
        .collect();
    let form = if uni.form.is_empty() {
        "Uni"
    } else {
        uni.form.as_str()
    };
    Ok(Value::str(format!("{}:0x<{}>", form, codepoints.join(" "))))
}

/// `Uni.raku`: `Uni.new(0x0061, 0x0062).NFC`.
// Cost: O(n), n = codepoints.
pub(crate) fn raku(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let uni = uni_of(target)?;
    Ok(Value::str(crate::value::raku_repr::uni_raku_repr(
        &uni.text(),
        &uni.form,
    )))
}

/// The index an `Int`, `Str` or `Num` argument stands for, or the answer for
/// an argument that is no index.
fn index_of(arg: &Value) -> Option<i64> {
    match arg.view() {
        ValueView::Int(i) => Some(i),
        ValueView::Num(f) if f.is_finite() => Some(f as i64),
        ValueView::Str(s) => s.trim().parse::<i64>().ok(),
        _ => None,
    }
}

/// `Uni.AT-POS($index)`: the codepoint at the index, or the
/// `X::OutOfRange` `Failure` Rakudo answers for an index outside the `Uni`.
// Cost: O(1) (the codepoint array is indexed).
pub(crate) fn at_pos(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let uni = uni_of(target).ok()?;
    let index = index_of(&args[0])?;
    let len = uni.len() as i64;
    Some(Ok(match usize::try_from(index) {
        Ok(i) if index < len => Value::int(i64::from(uni.codepoints()[i])),
        _ => RuntimeError::out_of_range_failure(
            "Index",
            Value::int(index),
            &format!("0..{}", len - 1),
        ),
    }))
}

/// `Uni.EXISTS-POS($index)`: whether the index is inside the `Uni`.
// Cost: O(1).
pub(crate) fn exists_pos(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let uni = uni_of(target).ok()?;
    let index = index_of(&args[0])?;
    Some(Ok(Value::truth((0..uni.len() as i64).contains(&index))))
}
