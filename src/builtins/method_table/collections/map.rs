//! `Map`'s rows (`Hash` inherits them through its MRO).

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Map",
        name: "list",
        arity: 0,
        handler: Handler::Narrow(list),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Map",
        name: "List",
        arity: 0,
        handler: Handler::Narrow(list),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Map",
        name: "hash",
        arity: 0,
        handler: Handler::Narrow(hash),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Hash",
        name: "default",
        arity: 0,
        handler: Handler::Narrow(default),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Map",
        name: "elems",
        arity: 0,
        handler: Handler::Pure(elems),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Map",
        name: "Bool",
        arity: 0,
        handler: Handler::Pure(bool),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Map",
        name: "keys",
        arity: 0,
        handler: Handler::Pure(keys),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Map",
        name: "values",
        arity: 0,
        handler: Handler::Pure(values),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Map",
        name: "kv",
        arity: 0,
        handler: Handler::Pure(kv),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Map",
        name: "pairs",
        arity: 0,
        handler: Handler::Pure(pairs),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Map",
        name: "antipairs",
        arity: 0,
        handler: Handler::Pure(antipairs),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Map",
        name: "contains",
        arity: 1,
        handler: Handler::Pure(crate::builtins::method_table::str_search::contains),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Map",
        name: "index",
        arity: 1,
        handler: Handler::Pure(crate::builtins::method_table::str_search::index),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Map",
        name: "invert",
        arity: 0,
        handler: Handler::Narrow(invert),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Map",
        name: "Numeric",
        arity: 0,
        handler: Handler::Pure(elems),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Map",
        name: "Int",
        arity: 0,
        handler: Handler::Pure(elems),
        flags: RowFlags::NONE,
        named: &[],
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

/// The pairs of a hash as `.list`, `.List` and `.Array` build them: each key
/// as the object it was stored as, each value as it is stored (not
/// decontainerized: `.pairs` is the decontainerizing view).
// Cost: O(e), e = entries.
pub(crate) fn list_pairs(map: &crate::value::HashData) -> Vec<Value> {
    map.iter()
        .map(|(k, v)| map.typed_pair(k, v.clone()))
        .collect()
}

/// `Map.list` and `Map.List`: a Hash is its pairs (`%h.List` is `(:a(1),)`,
/// not `({:a(1)},)`).
// Cost: O(e), e = entries.
pub(crate) fn list(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Hash(map) => Some(Ok(Value::array(list_pairs(&map)))),
        _ => None,
    }
}

/// `Map.hash`: `.hash` on a hash IS that hash in Associative context,
/// de-itemized, preserving the backing `HashData` and its type metadata.
// Cost: O(1).
pub(crate) fn hash(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Hash(_) => Some(Ok(target.clone().with_hash_itemized(false))),
        _ => None,
    }
}

/// `Hash.default`: the value a missing key reads as. A value-carried
/// `is default(...)` takes priority over the type default, so it survives
/// raw-parameter binding and list construction.
// Cost: O(1).
pub(crate) fn default(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Hash(h) => Some(Ok(h
            .default
            .as_deref()
            .cloned()
            .unwrap_or_else(|| Value::package(crate::symbol::wk::any())))),
        _ => None,
    }
}
