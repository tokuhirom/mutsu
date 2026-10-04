//! `Map`'s rows (`Hash` inherits them through its MRO).

use super::{Handler, MethodRow};
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Map",
        name: "elems",
        arity: 0,
        handler: Handler::Pure(elems),
    },
    MethodRow {
        owner: "Map",
        name: "Bool",
        arity: 0,
        handler: Handler::Pure(bool),
    },
    MethodRow {
        owner: "Map",
        name: "keys",
        arity: 0,
        handler: Handler::Pure(keys),
    },
    MethodRow {
        owner: "Map",
        name: "Numeric",
        arity: 0,
        handler: Handler::Pure(elems),
    },
    MethodRow {
        owner: "Map",
        name: "Int",
        arity: 0,
        handler: Handler::Pure(elems),
    },
];

// Cost: O(1), the map's length. Also `Map.Numeric` and `Map.Int`: a map
// numifies to its pair count.
fn elems(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Hash(items) => Ok(Value::int(items.len() as i64)),
        _ => Err(RuntimeError::new("Map.elems: receiver is not a Map")),
    }
}

// Cost: O(1), an emptiness test.
fn bool(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::truth(target.truthy()))
}

/// `Map.keys`: a Seq of the keys. An object hash (`my %h{Any}`, a QuantHash's
/// `.hash`, a `classify` by non-Str values) yields its real key objects; a
/// plain hash yields its decoded `Str` keys.
// Cost: O(e), e = pairs (the keys are copied out).
pub(crate) fn keys(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let ValueView::Hash(map) = target.view() else {
        return Err(RuntimeError::new("Map.keys: receiver is not a Map"));
    };
    let keys: Vec<Value> = if map.has_typed_keys() {
        map.keys()
            .map(|k| crate::runtime::utils::hash_typed_key(target, k))
            .collect()
    } else {
        map.keys().map(|k| Value::hash_key_decode(k)).collect()
    };
    Ok(Value::seq(keys))
}
