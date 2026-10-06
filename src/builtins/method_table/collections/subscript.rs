//! The associative subscript protocol and `ACCEPTS` of the collections
//! (ADR-11276 §10, slice 3C): `AT-KEY`, `EXISTS-KEY` and `ACCEPTS` on `Hash`,
//! `Map`, the six quant hashes, `Pair`, `Capture` and `Range`.
//!
//! The rows take any plain argument (a key may be any object), and the
//! cascade's own arms for the same calls delegate here for the receivers the
//! table does not answer (an argument it does not admit, an itemized hash).

use super::{Handler, MethodRow, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};
use num_bigint::BigInt;
use num_traits::Zero;

macro_rules! row {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 1,
            handler: Handler::Narrow($handler),
            flags: RowFlags::ANY_ARGS,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("Hash", "AT-KEY", at_key),
    // `Map.AT-KEY` has no row of its own: a Map value is a Hash value, and
    // `Hash` declares `AT-KEY` itself, so no shape would reach it.
    row!("Set", "AT-KEY", at_key),
    row!("SetHash", "AT-KEY", at_key),
    row!("Bag", "AT-KEY", at_key),
    row!("BagHash", "AT-KEY", at_key),
    row!("Mix", "AT-KEY", at_key),
    row!("MixHash", "AT-KEY", at_key),
    row!("Pair", "AT-KEY", at_key),
    row!("Capture", "AT-KEY", at_key),
    row!("Map", "EXISTS-KEY", exists_key),
    row!("Set", "EXISTS-KEY", exists_key),
    row!("SetHash", "EXISTS-KEY", exists_key),
    row!("Bag", "EXISTS-KEY", exists_key),
    row!("BagHash", "EXISTS-KEY", exists_key),
    row!("Mix", "EXISTS-KEY", exists_key),
    row!("MixHash", "EXISTS-KEY", exists_key),
    row!("Pair", "EXISTS-KEY", exists_key),
    row!("Capture", "EXISTS-KEY", exists_key),
    row!("Set", "ACCEPTS", accepts_quant),
    row!("SetHash", "ACCEPTS", accepts_quant),
    row!("Bag", "ACCEPTS", accepts_quant),
    row!("BagHash", "ACCEPTS", accepts_quant),
    row!("Mix", "ACCEPTS", accepts_quant),
    row!("MixHash", "ACCEPTS", accepts_quant),
    row!("Pair", "ACCEPTS", accepts_pair),
    row!("Range", "ACCEPTS", accepts_range),
];

/// `.AT-KEY($key)`: the value stored under `key`, or the type's default
/// (`Nil` for a Hash, `0`/`False` for the quant hashes).
// Cost: O(1) expected; O(k) in the key's string form, k = chars.
pub(crate) fn at_key(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let arg = &args[0];
    match target.view() {
        ValueView::Hash(map) => {
            // An object hash stores `.WHICH` keys (fall back to the plain
            // string key for the ordinary Str-keyed hash).
            let v = if map.key_type.is_some() {
                let which = crate::runtime::utils::value_which_key(arg);
                map.get(&which).cloned()
            } else {
                None
            };
            let v = v.or_else(|| map.get(&arg.to_string_value()).cloned());
            Some(Ok(v.unwrap_or(Value::NIL)))
        }
        ValueView::Set(data, _) => {
            let (key, _) = crate::runtime::utils::quanthash_elem_entry(arg);
            Some(Ok(Value::truth(data.elements.contains(&key))))
        }
        ValueView::Bag(data, _) => {
            let (key, _) = crate::runtime::utils::quanthash_elem_entry(arg);
            let count = data.counts.get(&key).cloned().unwrap_or_else(BigInt::zero);
            Some(Ok(Value::from_bigint(count)))
        }
        ValueView::Mix(data, _) => {
            let (key, _) = crate::runtime::utils::quanthash_elem_entry(arg);
            let weight = data.weights.get(&key).copied().unwrap_or(0.0);
            Some(Ok(crate::value::mix_weight_to_value(weight)))
        }
        // A `Pair` does `Associative` with a single entry.
        ValueView::Pair(key, value) => Some(Ok(if *key == arg.to_string_value() {
            value.clone()
        } else {
            Value::NIL
        })),
        ValueView::ValuePair(key, value) => {
            Some(Ok(if key.to_string_value() == arg.to_string_value() {
                (*value).clone()
            } else {
                Value::NIL
            }))
        }
        // A `Capture`'s named part.
        ValueView::Capture { named, .. } => Some(Ok(named
            .get(&arg.to_string_value())
            .cloned()
            .unwrap_or(Value::NIL))),
        _ => None,
    }
}

/// `.EXISTS-KEY($key)`: whether `key` is stored.
// Cost: O(1) expected; O(k) in the key's string form, k = chars.
pub(crate) fn exists_key(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let arg = &args[0];
    match target.view() {
        ValueView::Hash(map) => {
            // An object hash stores `.WHICH` keys (fall back to the plain
            // string key for the ordinary Str-keyed hash).
            let found = (map.key_type.is_some()
                && map.contains_key(&crate::runtime::utils::value_which_key(arg)))
                || map.contains_key(&arg.to_string_value());
            Some(Ok(Value::truth(found)))
        }
        ValueView::Set(data, _) => {
            let (key, _) = crate::runtime::utils::quanthash_elem_entry(arg);
            Some(Ok(Value::truth(data.elements.contains(&key))))
        }
        ValueView::Bag(data, _) => {
            let (key, _) = crate::runtime::utils::quanthash_elem_entry(arg);
            Some(Ok(Value::truth(data.counts.contains_key(&key))))
        }
        ValueView::Mix(data, _) => {
            let (key, _) = crate::runtime::utils::quanthash_elem_entry(arg);
            Some(Ok(Value::truth(data.weights.contains_key(&key))))
        }
        ValueView::Pair(key, _) => Some(Ok(Value::truth(*key == arg.to_string_value()))),
        ValueView::ValuePair(key, _) => Some(Ok(Value::truth(
            key.to_string_value() == arg.to_string_value(),
        ))),
        ValueView::Capture { named, .. } => {
            Some(Ok(Value::truth(named.contains_key(&arg.to_string_value()))))
        }
        _ => None,
    }
}

/// `Set`/`Bag`/`Mix` `.ACCEPTS($other)`: equal contents (the same kind of
/// quant hash, the same elements and counts or weights).
// Cost: O(e), e = elements of the receiver.
pub(crate) fn accepts_quant(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let result = match (target.view(), args[0].view()) {
        (ValueView::Set(set1, _), ValueView::Set(set2, _)) => {
            set1.len() == set2.len() && set1.iter().all(|k| set2.contains(k))
        }
        (ValueView::Bag(bag1, _), ValueView::Bag(bag2, _)) => {
            bag1.len() == bag2.len() && bag1.iter().all(|(k, v)| bag2.get(k) == Some(v))
        }
        (ValueView::Mix(mix1, _), ValueView::Mix(mix2, _)) => {
            mix1.len() == mix2.len()
                && mix1.iter().all(|(k, v)| {
                    mix2.get(k)
                        .copied()
                        .is_some_and(|v2| (v - v2).abs() < f64::EPSILON)
                })
        }
        (ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..), _) => false,
        _ => return None,
    };
    Some(Ok(Value::truth(result)))
}

/// `Pair.ACCEPTS($other)`: whether `other` has the entry the pair names (the
/// same value stored under the same key).
// Cost: O(1) expected; O(k) in the key's string form, k = chars.
pub(crate) fn accepts_pair(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let arg = &args[0];
    let (pk, pv) = match target.view() {
        ValueView::Pair(k, v) => (k.to_string(), v.clone()),
        ValueView::ValuePair(k, v) => (k.to_string_value(), v.clone()),
        _ => return None,
    };
    // Set/Bag/Mix store elements under their `.WHICH` key, so membership
    // lookups must key by the pair-key's `.WHICH`, not its raw string.
    let elem_key = match target.view() {
        ValueView::Pair(k, _) => crate::runtime::utils::str_elem_key(k),
        ValueView::ValuePair(k, _) => crate::runtime::utils::value_which_key(k),
        _ => return None,
    };
    let result = match arg.view() {
        ValueView::Bag(data, _) => {
            let count = data
                .counts
                .get(&elem_key)
                .cloned()
                .unwrap_or_else(BigInt::zero);
            Value::from_bigint(count) == pv
        }
        ValueView::Mix(data, _) => {
            let w = data.weights.get(&elem_key).copied().unwrap_or(0.0);
            let mv = if w.fract() == 0.0 {
                Value::int(w as i64)
            } else {
                Value::num(w)
            };
            mv == pv
        }
        ValueView::Set(data, _) => {
            let in_set = data.elements.contains(&elem_key);
            Value::truth(in_set) == pv
        }
        ValueView::Hash(items) => {
            let hv = items.get(&pk).cloned().unwrap_or(Value::int(0));
            hv == pv
        }
        ValueView::Pair(ok, ov) => pk == ok.as_str() && *ov == pv,
        ValueView::ValuePair(ok, ov) => {
            let tk = match target.view() {
                ValueView::Pair(k, _) => Value::str(k.to_string()),
                ValueView::ValuePair(k, _) => k.clone(),
                _ => return None,
            };
            tk == *ok && *ov == pv
        }
        ValueView::Instance { .. } | ValueView::Package(_) => return None,
        _ => false,
    };
    Some(Ok(Value::truth(result)))
}

/// `Range.ACCEPTS($other)`: containment of a value, or the subset check of a
/// range.
// Cost: O(1) for numeric ends; O(n) for a string range or a range subset
// check, n = chars of the endpoints.
pub(crate) fn accepts_range(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !target.is_range() {
        return None;
    }
    let arg = args[0].descalarize();
    let result = if arg.is_range() {
        // Range ~~ Range: subset check -- delegate to pure_smart_match
        crate::vm::vm_smart_match::pure_smart_match(arg, target).unwrap_or(false)
    } else {
        // Value ~~ Range: containment check
        Interpreter::value_in_range(arg, target)
    };
    Some(Ok(Value::truth(result)))
}
