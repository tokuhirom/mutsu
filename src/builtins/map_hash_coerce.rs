// Pure `.Hash` / `.Map` coercions over List/Array/Seq/Set/Bag/Mix/Pair receivers.
//
// Like `quanthash_coerce`, these carry no interpreter state (env / registry /
// type-metadata side table): they fold a value's elements into a Hash purely
// from the input. `.Map` additionally embeds the `Map` declared-type *in the
// `Hash` Arc* (container metadata has travelled in the value since
// #2952, not in the pointer-keyed `instance_type_metadata` side table), so it is
// a pure value op too. Single authoritative impl shared by the bytecode VM
// (native dispatch) and the tree-walking interpreter fallback.
//
// Spec: https://docs.raku.org/routine/Hash and https://docs.raku.org/routine/Map

use crate::runtime::Interpreter;
use crate::runtime::utils::set_hash_original_keys;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};
use std::collections::HashMap;

/// Build an `X::Hash::Store::OddNumber` error for an odd element count.
fn make_odd_number_error(items: &[Value]) -> RuntimeError {
    let count = items.len();
    let last = items
        .last()
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    let message = if count == 1 {
        format!(
            "Odd number of elements found where hash initializer expected:\n\
             Only saw: {}",
            last
        )
    } else {
        format!(
            "Odd number of elements found where hash initializer expected:\n\
             Found {} (implicit) elements:\nLast element seen: {}",
            count, last
        )
    };
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Hash::Store::OddNumber"), attrs);
    RuntimeError {
        exception: Some(Box::new(ex)),
        ..RuntimeError::new(message)
    }
}

/// Unwrap an itemized Pair (`Scalar`) or a Pair held in a `:=` element cell
/// (`ContainerRef`) for hash-initializer purposes. Non-Pair contents (e.g. an
/// itemized hash, which must die "Odd number" like raku) pass through as-is.
pub(crate) fn unwrap_contained_pair(v: &Value) -> Value {
    let held = match v.view() {
        ValueView::Scalar(inner) => inner.clone(),
        ValueView::ContainerRef(cell) => cell.lock().unwrap().clone(),
        _ => return v.clone(),
    };
    if matches!(held.view(), ValueView::Pair(..) | ValueView::ValuePair(..)) {
        held
    } else {
        v.clone()
    }
}

/// Convert a slice of items to a Hash, optionally checking for odd element count.
fn items_to_hash(items: &[Value], check_odd: bool) -> Result<Value, RuntimeError> {
    // An itemized Pair (`$(:a(1))` = `Scalar`) or a Pair held in a `:=` element
    // cell (`ContainerRef`, e.g. a classify bucket element) still counts as a
    // hash initializer pair; an itemized *hash* stays opaque (raku dies "Odd
    // number"), so only Pair contents are unwrapped.
    let items: Vec<Value> = items.iter().map(unwrap_contained_pair).collect();
    if check_odd {
        let non_pair_count = items
            .iter()
            .filter(|v| !matches!(v.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
            .count();
        if non_pair_count % 2 != 0 {
            return Err(make_odd_number_error(&items));
        }
    }
    let mut map = HashMap::new();
    let mut iter = items.iter();
    while let Some(item) = iter.next() {
        match item.view() {
            ValueView::Pair(k, v) => {
                map.insert(k.clone(), v.clone());
            }
            ValueView::ValuePair(k, v) => {
                map.insert(k.to_string_value(), v.clone());
            }
            _ => {
                let key = item.to_string_value();
                let value = iter.next().cloned().unwrap_or(Value::NIL);
                map.insert(key, value);
            }
        }
    }
    Ok(Value::hash(map))
}

/// Build a Hash from a Set/Bag/Mix's keys, preserving non-`Str` original keys.
fn quanthash_to_hash<I, F>(entries: I, value_for: F) -> Value
where
    I: Iterator<Item = (String, Value, Value)>,
    F: Fn(&Value) -> Value,
{
    let mut map = HashMap::new();
    let mut original_keys = HashMap::new();
    let mut has_typed = false;
    for (k, weight, typed) in entries {
        map.insert(k.clone(), value_for(&weight));
        if !matches!(typed.view(), ValueView::Str(sv) if sv.as_ref() == &k) {
            has_typed = true;
            original_keys.insert(k, typed);
        }
    }
    let mut result = Value::hash(map);
    if has_typed {
        original_keys.insert("__mutsu_setty_origin".to_string(), Value::TRUE);
        result = set_hash_original_keys(result, original_keys);
    }
    result
}

/// Coerce `target` to a `Hash`. `check_odd` controls the odd-element check for
/// flat list receivers. Mirrors the interpreter's `dispatch_to_hash_impl`.
pub(crate) fn to_hash(target: Value, check_odd: bool) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Hash(map) => {
            // A Map (a Hash carrying the `Map` declared-type) coerces to a fresh
            // *mutable* Hash, dropping its immutability; a plain Hash is returned
            // by identity (raku's `%h.Hash =:= %h`).
            let is_map = Interpreter::hashdata_type_info(&map)
                .and_then(|info| info.declared_type)
                .is_some_and(|dt| dt == "Map");
            if is_map {
                let mut result = target.clone();
                result.with_hash_mut(|arc| {
                    let data = crate::gc::Gc::make_mut(arc);
                    data.declared_type = None;
                });
                Ok(result)
            } else {
                Ok(target.clone())
            }
        }
        ValueView::Array(items, ..) => items_to_hash(items.as_ref(), check_odd),
        ValueView::Seq(items) | ValueView::Slip(items) => items_to_hash(items.as_ref(), check_odd),
        ValueView::Set(s, _) => Ok(quanthash_to_hash(
            s.iter().map(|k| (k.clone(), Value::TRUE, s.typed_key(k))),
            |_| Value::TRUE,
        )),
        ValueView::Bag(b, _) => Ok(quanthash_to_hash(
            b.iter()
                .map(|(k, v)| (k.clone(), Value::from_bigint(v.clone()), b.typed_key(k))),
            |w| w.clone(),
        )),
        ValueView::Mix(m, _) => Ok(quanthash_to_hash(
            m.iter().map(|(k, v)| {
                (
                    k.clone(),
                    crate::value::mix_weight_to_value(*v),
                    m.typed_key(k),
                )
            }),
            |w| w.clone(),
        )),
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Match" => {
            // %($/) returns the named captures hash.
            if let Some(named) = attributes.as_map().get("named") {
                Ok(named.clone())
            } else {
                Ok(Value::hash(HashMap::new()))
            }
        }
        _ => {
            if check_odd {
                if let ValueView::Pair(k, v) = target.view() {
                    let mut map = HashMap::new();
                    map.insert(k.to_string(), v.clone());
                    return Ok(Value::hash(map));
                }
                return Err(make_odd_number_error(std::slice::from_ref(&target)));
            }
            let mut map = HashMap::new();
            map.insert(target.to_string_value(), Value::TRUE);
            Ok(Value::hash(map))
        }
    }
}

/// Coerce `target` to a `Map`: decontainerize a Hash's values (or fold any other
/// receiver to a Hash) and embed the `Map` declared-type in the resulting
/// `Hash` Arc. An already-`Map` Hash is returned by identity (its
/// pointer-based `.WHICH` is preserved). Mirrors the interpreter's
/// `dispatch_to_map` + `tag_container_metadata(..., declared_type="Map")`.
pub(crate) fn to_map(target: Value) -> Result<Value, RuntimeError> {
    let mut result = 'hash: {
        if let ValueView::Hash(map) = target.view() {
            // Already a Map (embedded declared-type): identity, no re-tag.
            if Interpreter::hashdata_type_info(&map)
                .and_then(|info| info.declared_type)
                .is_some_and(|dt| dt == "Map")
            {
                return Ok(target.clone());
            }
            // Decontainerize values (Map values are not wrapped in Scalar).
            let deconted: HashMap<String, Value> = map
                .iter()
                .map(|(k, v)| {
                    let deconted = match v.view() {
                        ValueView::Scalar(inner) => inner.clone(),
                        _ => v.clone(),
                    };
                    (k.clone(), deconted)
                })
                .collect();
            break 'hash Value::hash(deconted);
        }
        to_hash(target, true)?
    };
    // Embed the `Map` declared-type in the Hash Arc (pure; no side table).
    result.with_hash_mut(|arc| {
        if arc.declared_type.as_deref() != Some("Map") {
            let data = crate::gc::Gc::make_mut(arc);
            data.declared_type = Some("Map".to_string());
        }
    });
    Ok(result)
}
