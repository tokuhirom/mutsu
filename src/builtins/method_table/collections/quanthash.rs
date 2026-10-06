//! The quant hashes' rows (ADR-11276 §10, slice 3C): `Set`, `SetHash`, `Bag`,
//! `BagHash`, `Mix` and `MixHash`.
//!
//! Each owner has its own row for a method Rakudo declares on it; the six
//! share one handler per method, which decides by the receiver's kind
//! (`ValueView::Set`/`Bag`/`Mix`, the mutable and immutable forms alike).

use super::{Handler, MethodRow, RowFlags};
use crate::runtime::utils::quanthash_typed_pair;
use crate::value::{RuntimeError, Value, ValueView, mix_weight_to_value};

/// One row of a pure, no-argument method.
macro_rules! row {
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

pub(super) static ROWS: &[MethodRow] = &[
    // The key/value views, declared on every quant hash.
    row!("Set", "keys", keys),
    row!("SetHash", "keys", keys),
    row!("Bag", "keys", keys),
    row!("BagHash", "keys", keys),
    row!("Mix", "keys", keys),
    row!("MixHash", "keys", keys),
    row!("Set", "values", values),
    row!("SetHash", "values", values),
    row!("Bag", "values", values),
    row!("BagHash", "values", values),
    row!("Mix", "values", values),
    row!("MixHash", "values", values),
    row!("Set", "kv", kv),
    row!("SetHash", "kv", kv),
    row!("Bag", "kv", kv),
    row!("BagHash", "kv", kv),
    row!("Mix", "kv", kv),
    row!("MixHash", "kv", kv),
    row!("Set", "pairs", pairs),
    row!("SetHash", "pairs", pairs),
    row!("Bag", "pairs", pairs),
    row!("BagHash", "pairs", pairs),
    row!("Mix", "pairs", pairs),
    row!("MixHash", "pairs", pairs),
    row!("Set", "antipairs", antipairs),
    row!("SetHash", "antipairs", antipairs),
    row!("Bag", "antipairs", antipairs),
    row!("BagHash", "antipairs", antipairs),
    row!("Mix", "antipairs", antipairs),
    row!("MixHash", "antipairs", antipairs),
    // `Baggy`'s own.
    row!("Bag", "kxxv", kxxv),
    row!("BagHash", "kxxv", kxxv),
    row!("Mix", "kxxv", kxxv),
    row!("MixHash", "kxxv", kxxv),
    row!("Bag", "invert", invert),
    row!("BagHash", "invert", invert),
    row!("Mix", "invert", invert),
    row!("MixHash", "invert", invert),
    // Sizes and the default.
    row!("Set", "total", total),
    row!("SetHash", "total", total),
    row!("Bag", "total", total),
    row!("BagHash", "total", total),
    row!("Mix", "total", total),
    row!("MixHash", "total", total),
    row!("Bag", "Numeric", total),
    row!("BagHash", "Numeric", total),
    row!("Mix", "Numeric", total),
    row!("MixHash", "Numeric", total),
    row!("Set", "elems", elems),
    row!("SetHash", "elems", elems),
    row!("Bag", "elems", elems),
    row!("BagHash", "elems", elems),
    row!("Mix", "elems", elems),
    row!("MixHash", "elems", elems),
    row!("Set", "default", default),
    row!("SetHash", "default", default),
    row!("Bag", "default", default),
    row!("BagHash", "default", default),
    row!("Mix", "default", default),
    row!("MixHash", "default", default),
    row!("Set", "of", of),
    row!("SetHash", "of", of),
    row!("Bag", "of", of),
    row!("BagHash", "of", of),
    row!("Mix", "of", of),
    row!("MixHash", "of", of),
    row!("Set", "hash", hash),
    row!("SetHash", "hash", hash),
    row!("Bag", "hash", hash),
    row!("BagHash", "hash", hash),
    row!("Mix", "hash", hash),
    row!("MixHash", "hash", hash),
];

/// `.keys`: the elements, as the objects they were stored as.
// Cost: O(e), e = elements.
pub(crate) fn keys(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(Value::seq(match target.view() {
        ValueView::Set(s, _) => s.iter().map(|k| s.typed_key(k)).collect(),
        ValueView::Bag(b, _) => b.keys().map(|k| b.typed_key(k)).collect(),
        ValueView::Mix(m, _) => m.keys().map(|k| m.typed_key(k)).collect(),
        _ => return None,
    })))
}

/// `.values`: `True` per element of a `Set`, the counts of a `Bag`, the
/// weights of a `Mix`.
// Cost: O(e), e = elements.
pub(crate) fn values(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(Value::seq(match target.view() {
        ValueView::Set(s, _) => s.iter().map(|_| Value::TRUE).collect(),
        ValueView::Bag(b, _) => b.values().map(|v| Value::from_bigint(v.clone())).collect(),
        ValueView::Mix(m, _) => m.values().map(|v| mix_weight_to_value(*v)).collect(),
        _ => return None,
    })))
}

/// `.kv`: each element followed by its count (or weight, or `True`).
// Cost: O(e), e = elements.
pub(crate) fn kv(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let mut kv = Vec::new();
    match target.view() {
        ValueView::Set(s, _) => {
            for k in s.iter() {
                kv.push(s.typed_key(k));
                kv.push(Value::TRUE);
            }
        }
        ValueView::Bag(b, _) => {
            for (k, v) in b.iter() {
                kv.push(b.typed_key(k));
                kv.push(Value::from_bigint(v.clone()));
            }
        }
        ValueView::Mix(m, _) => {
            for (k, v) in m.iter() {
                kv.push(m.typed_key(k));
                kv.push(mix_weight_to_value(*v));
            }
        }
        _ => return None,
    }
    Some(Ok(Value::seq(kv)))
}

/// `.pairs`: `element => count` (the pair keeps the element's object key).
// Cost: O(e), e = elements.
pub(crate) fn pairs(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(Value::seq(match target.view() {
        ValueView::Set(s, _) => s
            .iter()
            .map(|k| quanthash_typed_pair(s.typed_key(k), Value::TRUE))
            .collect(),
        ValueView::Bag(b, _) => b
            .iter()
            .map(|(k, v)| quanthash_typed_pair(b.typed_key(k), Value::from_bigint(v.clone())))
            .collect(),
        ValueView::Mix(m, _) => m
            .iter()
            .map(|(k, v)| quanthash_typed_pair(m.typed_key(k), mix_weight_to_value(*v)))
            .collect(),
        _ => return None,
    })))
}

/// `.antipairs`: `count => element`.
// Cost: O(e), e = elements.
pub(crate) fn antipairs(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(Value::seq(match target.view() {
        ValueView::Bag(b, _) => b
            .iter()
            .map(|(k, v)| Value::value_pair(Value::from_bigint(v.clone()), b.typed_key(k)))
            .collect(),
        ValueView::Set(s, _) => s
            .iter()
            .map(|k| Value::value_pair(Value::TRUE, s.typed_key(k)))
            .collect(),
        ValueView::Mix(m, _) => m
            .iter()
            .map(|(k, v)| Value::value_pair(mix_weight_to_value(*v), m.typed_key(k)))
            .collect(),
        _ => return None,
    })))
}

/// `Baggy.kxxv`: each element repeated as often as its count (a `Mix` weight
/// is floored, a negative one repeats nothing).
// Cost: O(n), n = elements produced, counting repeats.
pub(crate) fn kxxv(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let mut result = Vec::new();
    match target.view() {
        ValueView::Bag(items, _) => {
            for (k, count) in items.iter() {
                for _ in 0..crate::runtime::utils::bigint_to_i64_sat(count) {
                    result.push(items.typed_key(k));
                }
            }
        }
        ValueView::Mix(items, _) => {
            for (k, weight) in items.iter() {
                for _ in 0..(weight.floor() as i64).max(0) {
                    result.push(items.typed_key(k));
                }
            }
        }
        _ => return None,
    }
    Some(Ok(Value::array(result)))
}

/// `Baggy.invert`: the one `invert` every `Map`-like shares.
// Cost: O(e), e = elements.
pub(crate) fn invert(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    super::list::invert(target, args)
}

/// `.total`, and `Baggy.Numeric`: the sum of the counts or weights; a `Set`
/// totals its element count.
// Cost: O(e), e = elements (one pass over the weights).
pub(crate) fn total(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(match target.view() {
        ValueView::Set(s, _) => Value::int(s.len() as i64),
        ValueView::Bag(b, _) => Value::from_bigint(b.values().sum::<num_bigint::BigInt>()),
        ValueView::Mix(m, _) => {
            // Sort values before summing to ensure deterministic results
            // regardless of HashMap iteration order: a weight that only
            // decodes to `Num` still sums non-associatively (e.g.
            // 1.1+1.1+3.3+3.3 vs 1.1+3.3+1.1+3.3).
            let mut vals: Vec<f64> = m.values().copied().collect();
            vals.sort_by(|a, b| a.total_cmp(b));
            // Sum under the numeric tower and decode the total the same way
            // every other weight read-out does, so `.total` is `Int` for a
            // whole total and an exact `Rat` for a decimal one. The old
            // `f64_to_rat` reconstruction snapped anything within 1e-10 of a
            // whole number, turning `(a => 1.00000000001).Mix.total` into 1.
            mix_weight_to_value(crate::builtins::mix_weight::sum(vals))
        }
        _ => return None,
    }))
}

/// `.elems`: the number of distinct elements.
// Cost: O(1).
pub(crate) fn elems(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(Value::int(match target.view() {
        ValueView::Set(s, _) => s.len(),
        ValueView::Bag(b, _) => b.len(),
        ValueView::Mix(m, _) => m.len(),
        _ => return None,
    } as i64)))
}

/// `.default`: what a missing element is: `False` for a `Set`, `0` for the
/// others.
// Cost: O(1).
pub(crate) fn default(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(match target.view() {
        ValueView::Set(..) => Value::FALSE,
        ValueView::Bag(..) | ValueView::Mix(..) => Value::int(0),
        _ => return None,
    }))
}

/// `.of`: the type of the values (`Bool`, `UInt`, `Real`).
// Cost: O(1).
pub(crate) fn of(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let name = match target.view() {
        ValueView::Set(..) => "Bool",
        ValueView::Bag(..) => "UInt",
        ValueView::Mix(..) => "Real",
        _ => return None,
    };
    Some(Ok(Value::package(crate::symbol::Symbol::intern(name))))
}

/// `.hash`: the Hash `.Hash` makes, one implementation with the hyper ops.
// Cost: O(e), e = elements.
pub(crate) fn hash(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => Some(
            crate::builtins::map_hash_coerce::to_hash(target.clone(), false),
        ),
        _ => None,
    }
}
