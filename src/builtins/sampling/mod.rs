//! `pick`, `roll` and `pickpairs`: the one implementation of the sampling
//! methods (ADR-11276, remainder: the sampling methods).
//!
//! Rakudo declares `pick` and `roll` on `List`, `Range`, `Map`, `Bool`, `Any` and the
//! six quant hashes, and `pickpairs` on the quant hashes. The rows in
//! `method_table::collections::sampling` and the cascade arms that still answer
//! a `Seq`, a lazy list, a shaped array or an itemized hash (receivers with no
//! dispatch shape) call [`pick`], [`roll`] and [`pickpairs`], so a count means
//! the same thing however the call arrives.
//!
//! The answer is random, so the table flags the rows `RANDOM`.

mod pick;
mod pickpairs;
mod range;
mod roll;

use crate::builtins::rng::builtin_rand;
use crate::value::{RuntimeError, Value, ValueView};

use num_traits::Signed;

/// Whether `arg` is the `Whatever` type object, which binds a count exactly as `*`
/// does (Rakudo's `multi method pick(Whatever)` takes both).
// Cost: O(1).
fn is_whatever_type(arg: &Value) -> bool {
    matches!(arg.view(), ValueView::Package(name) if name == "Whatever")
}

/// Runs `f` on `arg`, with the `Whatever` type object replaced by `*`.
// Cost: O(1) beyond `f`.
fn with_count<T>(arg: &Value, f: impl FnOnce(&Value) -> T) -> T {
    if is_whatever_type(arg) {
        f(&Value::WHATEVER)
    } else {
        f(arg)
    }
}

/// `.pick` / `.pick($count)`; `None` declines (a Callable count, a receiver the
/// pure path cannot sample).
// Cost: see `pick::pick_one` and `pick::pick_n`.
pub(crate) fn pick(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let target = target.descalarize();
    match args {
        [] => pick::pick_one(target),
        [arg] => with_count(arg, |arg| pick::pick_n(target, arg)),
        _ => None,
    }
}

/// `.roll` / `.roll($count)`; `None` declines.
// Cost: see `roll::roll_one` and `roll::roll_n`.
pub(crate) fn roll(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let target = target.descalarize();
    match args {
        [] => roll::roll_one(target),
        [arg] => with_count(arg, |arg| roll::roll_n(target, arg)),
        _ => None,
    }
}

/// `.pickpairs` / `.pickpairs($count)` on a quant hash; `None` declines.
// Cost: see `pickpairs::pickpairs_one` and `pickpairs::pickpairs_n`.
pub(crate) fn pickpairs(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let target = target.descalarize();
    match args {
        [] => pickpairs::pickpairs_one(target),
        [arg] => with_count(arg, |arg| pickpairs::pickpairs_n(target, arg)),
        _ => None,
    }
}

/// One uniformly random element of `items`, `Nil` when there is none.
// Cost: O(1).
fn random_item(items: &[Value]) -> Value {
    if items.is_empty() {
        return Value::NIL;
    }
    let mut idx = (builtin_rand() * items.len() as f64) as usize;
    if idx >= items.len() {
        idx = items.len() - 1;
    }
    items[idx].clone()
}

/// One key of a `Mix`, chosen with probability proportional to its weight.
// Cost: O(n), n = elements.
pub(crate) fn sample_weighted_mix_key(items: &crate::value::MixData) -> Option<Value> {
    let mut total = 0.0;
    for weight in items.values() {
        if weight.is_finite() && *weight > 0.0 {
            total += *weight;
        }
    }
    if total <= 0.0 {
        return None;
    }
    let mut needle = builtin_rand() * total;
    for (key, weight) in items.iter() {
        if !weight.is_finite() || *weight <= 0.0 {
            continue;
        }
        if needle <= *weight {
            return Some(items.typed_key(key));
        }
        needle -= *weight;
    }
    items
        .iter()
        .find_map(|(key, weight)| (*weight > 0.0).then(|| items.typed_key(key)))
}

/// One key of a `Bag`, chosen with probability proportional to its count.
// Cost: O(n), n = elements.
pub(crate) fn sample_weighted_bag_key(items: &crate::value::BagData) -> Option<Value> {
    use crate::runtime::utils::bigint_to_i128_sat;
    let mut total: i128 = 0;
    for count in items.values() {
        let count = bigint_to_i128_sat(count);
        if count > 0 {
            total = total.saturating_add(count);
        }
    }
    if total <= 0 {
        return None;
    }
    let needle_f = builtin_rand() * total as f64;
    let mut needle = needle_f as i128;
    if needle >= total {
        needle = total - 1;
    }
    for (key, count) in items.iter() {
        let count = bigint_to_i128_sat(count);
        if count <= 0 {
            continue;
        }
        if needle < count {
            return Some(items.typed_key(key));
        }
        needle -= count;
    }
    items
        .iter()
        .find_map(|(key, count)| count.is_positive().then(|| items.typed_key(key)))
}
