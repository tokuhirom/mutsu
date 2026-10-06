//! `Pair`'s component rows (ADR-11276 §10, slice 3A proof rows for the `Pair`
//! shape; slice 3C moves the rest of its methods).
//!
//! `Pair` has two flavours (ADR-0021): a string-keyed one, which is a named
//! argument at a call site, and a data one with any key. Both are Rakudo's
//! `Pair`, and both answer these rows.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! rows {
    ($($name:literal => $handler:ident),* $(,)?) => {
        &[$(MethodRow {
            owner: "Pair",
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }),*]
    };
}

pub(super) static ROWS: &[MethodRow] = rows![
    "key" => key,
    "value" => value,
    "antipair" => antipair,
    "keys" => keys,
    "values" => values,
    "kv" => kv,
    "pairs" => pairs,
    "antipairs" => antipairs,
    "invert" => invert,
    "Pair" => pair,
];

/// `Pair.key`.
// Cost: O(1) for a data pair; O(k) for a string-keyed one, k = key length.
pub(crate) fn key(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Pair(key, _) => Some(Ok(Value::str(key.clone()))),
        ValueView::ValuePair(key, _) => Some(Ok(key.clone())),
        _ => None,
    }
}

/// `Pair.value`: the live value of a `for %h -> $p` pair's entry
/// (`hash_entry_read`), a plain clone for any other pair.
// Cost: O(1).
pub(crate) fn value(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Pair(_, value) | ValueView::ValuePair(_, value) => {
            Some(Ok(value.hash_entry_read()))
        }
        _ => None,
    }
}

/// `Pair.antipair`: the pair with key and value swapped (a data pair, ADR-0021
/// I2: data-minted pairs default positional).
// Cost: O(k) for a string-keyed pair, k = key length; O(1) otherwise.
pub(crate) fn antipair(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Pair(key, value) => Some(Ok(Value::value_pair(
            value.clone(),
            Value::str(key.clone()),
        ))),
        ValueView::ValuePair(key, value) => Some(Ok(Value::value_pair(value.clone(), key.clone()))),
        _ => None,
    }
}

/// `Pair.keys`: the one key.
// Cost: O(1) for a data pair; O(k) for a string-keyed one, k = key length.
pub(crate) fn keys(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let key = key(target, args)?.ok()?;
    Some(Ok(Value::seq(vec![key])))
}

/// `Pair.values`: the one value.
// Cost: O(1).
pub(crate) fn values(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Pair(_, value) | ValueView::ValuePair(_, value) => {
            Some(Ok(Value::seq(vec![value.clone()])))
        }
        _ => None,
    }
}

/// `Pair.kv`: the key, then the value.
// Cost: O(1) for a data pair; O(k) for a string-keyed one, k = key length.
pub(crate) fn kv(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Pair(key, value) => {
            Some(Ok(Value::seq(vec![Value::str(key.clone()), value.clone()])))
        }
        ValueView::ValuePair(key, value) => Some(Ok(Value::seq(vec![key.clone(), value.clone()]))),
        _ => None,
    }
}

/// `Pair.pairs`: the pair itself.
// Cost: O(1).
pub(crate) fn pairs(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Pair(..) | ValueView::ValuePair(..) => {
            Some(Ok(Value::seq(vec![target.clone()])))
        }
        _ => None,
    }
}

/// `Pair.antipairs`: the one swapped pair.
// Cost: O(1) for a data pair; O(k) for a string-keyed one, k = key length.
pub(crate) fn antipairs(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let pair = antipair(target, args)?.ok()?;
    Some(Ok(Value::seq(vec![pair])))
}

/// `Pair.invert`: the one swapped pair, as every `Map`-like inverts.
// Cost: O(1) for a data pair; O(k) for a string-keyed one, k = key length.
pub(crate) fn invert(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    super::list::invert(target, args)
}

/// `Pair.Pair`: the pair itself.
// Cost: O(1).
pub(crate) fn pair(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Pair(..) | ValueView::ValuePair(..) => Some(Ok(target.clone())),
        _ => None,
    }
}
