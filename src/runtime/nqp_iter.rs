//! `nqp::iterator` and the hash-iteration ops `nqp::iterkey_s` /
//! `nqp::iterval` (#11494).
//!
//! MoarVM's iterator is a `BOOTIter`: truthy while elements remain, advanced
//! by `nqp::shift`. Over a list, `shift` answers the next element; over a
//! hash, it answers the iterator itself, positioned on the next pair, whose
//! key and value `iterkey_s` and `iterval` then read. That is the loop
//! Rakudo's own `Map`/`Hash` internals and nqp-level serializers are built
//! on:
//!
//! ```text
//! my $it := nqp::iterator(nqp::getattr(%h, Map, '$!storage'));
//! while $it { my $e := nqp::shift($it); f(nqp::iterkey_s($e), nqp::iterval($e)) }
//! ```
//!
//! mutsu models it as a `BOOTIter` instance holding the elements and a
//! position. A list iterator reads the live list; a hash iterator holds a
//! snapshot of the pairs taken in the order `.keys` reports, so mutating the
//! hash during the loop cannot invalidate it (MoarVM leaves that unspecified).
//! `Value::truthy` answers "elements remain" for a `BOOTIter`, so
//! `while $it` and `nqp::istrue($it)` need no special case. `nqp::shift`
//! reaches [`iter_shift`] through `Interpreter::nqp_shift`, the one body the
//! op table and TRIR share.

use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

/// The class name of an `nqp::iterator` value, and the attribute keys of its
/// state. `Value::truthy` reads the same keys (`value::types_truthy`).
pub(crate) const BOOT_ITER: &str = "BOOTIter";
const ITEMS: &str = "items";
const POS: &str = "pos";
const IS_HASH: &str = "is-hash";

const NOT_ADVANCED: &str =
    "You have not advanced to the first item of the hash iterator, or have gone past the end";

/// Whether `v` is an `nqp::iterator` value.
// Cost: O(1).
pub(crate) fn is_boot_iter(v: &Value) -> bool {
    matches!(v.view(), ValueView::Instance { class_name, .. } if class_name == BOOT_ITER)
}

/// `nqp::iterator($list-or-hash)`.
// Cost: O(1) for a list; O(e) for a hash, e = entries (the pair snapshot).
pub(crate) fn nqp_iterator(target: &Value) -> Result<Value, RuntimeError> {
    let target = target.deref_container();
    let (items, is_hash) = match target.view() {
        ValueView::Array(..) => (target.clone(), false),
        ValueView::Hash(map) => {
            let pairs = map
                .iter()
                .map(|(k, v)| Value::value_pair(Value::str(k.to_string()), v.clone()))
                .collect();
            (Value::array(pairs), true)
        }
        _ => {
            return Err(RuntimeError::new(format!(
                "nqp::iterator: cannot iterate object with {} representation",
                crate::value::type_name::value_type_name(&target)
            )));
        }
    };
    let mut attrs = std::collections::HashMap::new();
    attrs.insert(ITEMS.to_string(), items);
    attrs.insert(POS.to_string(), Value::int(0));
    attrs.insert(IS_HASH.to_string(), Value::truth(is_hash));
    Ok(Value::make_instance(Symbol::intern(BOOT_ITER), attrs))
}

/// The iterator's state: (items, position, is a hash iterator).
fn state(it: &Value) -> Option<(Value, usize, bool)> {
    let ValueView::Instance { attributes, .. } = it.view() else {
        return None;
    };
    let map = attributes.as_map();
    let items = map.get(ITEMS)?.clone();
    let pos = map.get(POS).and_then(Value::as_int).unwrap_or(0).max(0) as usize;
    let is_hash = map.get(IS_HASH).is_some_and(Value::truthy);
    Some((items, pos, is_hash))
}

fn items_len(items: &Value) -> usize {
    match items.view() {
        ValueView::Array(data, _) => data.len(),
        _ => 0,
    }
}

fn item_at(items: &Value, i: usize) -> Option<Value> {
    match items.view() {
        ValueView::Array(data, _) => data.get(i).cloned(),
        _ => None,
    }
}

/// `nqp::shift` on an iterator: the next element of a list iterator; for a
/// hash iterator, the iterator itself, advanced to the next pair.
// Cost: O(1).
pub(crate) fn iter_shift(it: &Value) -> Result<Value, RuntimeError> {
    let (items, pos, is_hash) =
        state(it).ok_or_else(|| RuntimeError::new("nqp::shift: not an iterator"))?;
    if pos >= items_len(&items) {
        return Err(RuntimeError::new("Iteration past end of iterator"));
    }
    if let ValueView::Instance { attributes, .. } = it.view() {
        attributes.insert(POS, Value::int(pos as i64 + 1));
    }
    if is_hash {
        Ok(it.clone())
    } else {
        Ok(item_at(&items, pos).unwrap_or(Value::NIL))
    }
}

/// The pair a hash iterator was last advanced to.
fn current_pair(it: &Value) -> Result<Value, RuntimeError> {
    let (items, pos, is_hash) = state(it).ok_or_else(|| RuntimeError::new(NOT_ADVANCED))?;
    if !is_hash || pos == 0 {
        return Err(RuntimeError::new(NOT_ADVANCED));
    }
    item_at(&items, pos - 1).ok_or_else(|| RuntimeError::new(NOT_ADVANCED))
}

/// `nqp::iterkey_s($it)`: the key of the current pair.
// Cost: O(k), k = chars of the key (copied out of the pair).
pub(crate) fn iterkey_s(it: &Value) -> Result<Value, RuntimeError> {
    match current_pair(it)?.view() {
        ValueView::ValuePair(k, _) => Ok(Value::str(k.to_string_value())),
        ValueView::Pair(k, _) => Ok(Value::str(k.to_string())),
        _ => Err(RuntimeError::new(NOT_ADVANCED)),
    }
}

/// `nqp::iterval($it)`: the value of the current pair.
// Cost: O(1).
pub(crate) fn iterval(it: &Value) -> Result<Value, RuntimeError> {
    match current_pair(it)?.view() {
        ValueView::ValuePair(_, v) | ValueView::Pair(_, v) => Ok(v.clone()),
        _ => Err(RuntimeError::new(NOT_ADVANCED)),
    }
}

/// Dispatch `nqp::iterator` / `iterkey_s` / `iterval`, or `None` when `op`
/// is not one of them.
pub(super) fn call_nqp_iter_op(op: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let arg = args.first().cloned().unwrap_or(Value::NIL);
    Some(match op {
        // Cost: O(1) for a list; O(e) for a hash, e = entries.
        "iterator" => nqp_iterator(&arg),
        // Cost: O(k), k = chars of the key.
        "iterkey_s" => iterkey_s(&arg),
        // Cost: O(1).
        "iterval" => iterval(&arg),
        _ => return None,
    })
}
