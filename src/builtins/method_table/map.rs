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
        name: "values",
        arity: 0,
        handler: Handler::Pure(values),
    },
    MethodRow {
        owner: "Map",
        name: "kv",
        arity: 0,
        handler: Handler::Pure(kv),
    },
    MethodRow {
        owner: "Map",
        name: "pairs",
        arity: 0,
        handler: Handler::Pure(pairs),
    },
    MethodRow {
        owner: "Map",
        name: "antipairs",
        arity: 0,
        handler: Handler::Pure(antipairs),
    },
    MethodRow {
        owner: "Map",
        name: "invert",
        arity: 0,
        handler: Handler::Narrow(invert),
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

/// `Map.values`: the values, with any bound element containers dereferenced.
// Cost: O(e), e = map entries copied into the Seq.
pub(crate) fn values(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let ValueView::Hash(map) = target.view() else {
        return Err(RuntimeError::new("Map.values: receiver is not a Map"));
    };
    Ok(Value::seq(
        map.values().map(Value::deref_container).collect(),
    ))
}

/// `Map.kv`: the original typed keys where available, interleaved with values.
// Cost: O(e), e = map entries copied into the Seq.
pub(crate) fn kv(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let ValueView::Hash(map) = target.view() else {
        return Err(RuntimeError::new("Map.kv: receiver is not a Map"));
    };
    let has_typed_keys = map.has_typed_keys();
    let mut result = Vec::with_capacity(map.len() * 2);
    for (key, value) in map.iter() {
        result.push(if has_typed_keys {
            crate::runtime::utils::hash_typed_key(target, key)
        } else {
            Value::hash_key_decode(key)
        });
        result.push(value.deref_container());
    }
    Ok(Value::seq(result))
}

/// `Map.pairs`: a Seq of key/value Pairs, preserving typed keys where present.
// Cost: O(e), e = map entries copied into the Seq.
pub(crate) fn pairs(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let ValueView::Hash(map) = target.view() else {
        return Err(RuntimeError::new("Map.pairs: receiver is not a Map"));
    };
    let has_typed_keys = map.has_typed_keys();
    Ok(Value::seq(
        map.iter()
            .map(|(key, value)| {
                Value::value_pair(
                    if has_typed_keys {
                        crate::runtime::utils::hash_typed_key(target, key)
                    } else {
                        Value::str(key.clone())
                    },
                    value.deref_container(),
                )
            })
            .collect(),
    ))
}

/// `Map.antipairs`: a Seq of value/key Pairs, preserving typed keys where present.
// Cost: O(e), e = map entries copied into the Seq.
pub(crate) fn antipairs(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let ValueView::Hash(map) = target.view() else {
        return Err(RuntimeError::new("Map.antipairs: receiver is not a Map"));
    };
    Ok(Value::seq(
        map.iter()
            .map(|(key, value)| {
                Value::value_pair(
                    value.deref_container(),
                    crate::runtime::utils::hash_typed_key(target, key),
                )
            })
            .collect(),
    ))
}

/// Map's `.invert` shares the Pair-expansion implementation with List and
/// Array; Hash reaches this row through the Map MRO.
// Cost: O(e + v), e = map entries, v = expanded values in entry payloads.
fn invert(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    crate::builtins::methods_0arg::collection::invert_value(target).map(Ok)
}
