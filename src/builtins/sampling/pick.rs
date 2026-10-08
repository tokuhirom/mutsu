use crate::runtime;
use crate::value::{RuntimeError, Value, ValueView};

use super::range::{range_pick_n_fast, sample_one_from_range};
use super::{random_item, sample_weighted_bag_key};
use num_traits::Signed;

/// `.pick` with no argument.
// Cost: O(1) on an Array, a List, a reified Seq or an integer Range; O(e) on
// any other list-like, e = elements (decomposed into a Vec to index one slot).
pub(super) fn pick_one(target: &Value) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Mix(_, _) => Some(Err(RuntimeError::new(
            "Cannot call .pick on a Mix (immutable)",
        ))),
        ValueView::Bag(items, _) => Some(Ok(sample_weighted_bag_key(&items).unwrap_or(Value::NIL))),
        ValueView::Set(items, _) => {
            if items.is_empty() {
                Some(Ok(Value::NIL))
            } else {
                let keys: Vec<&String> = items.iter().collect();
                let mut idx = (crate::builtins::rng::builtin_rand() * keys.len() as f64) as usize;
                if idx >= keys.len() {
                    idx = keys.len() - 1;
                }
                Some(Ok(items.typed_key(keys[idx])))
            }
        }
        ValueView::Hash(items) => {
            if items.is_empty() {
                Some(Ok(Value::NIL))
            } else {
                let mut idx = (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                if idx >= items.len() {
                    idx = items.len() - 1;
                }
                let (key, value) = items.iter().nth(idx).expect("index in range");
                // typed_pair reconstructs an object hash's real key object
                // from its `.WHICH` store key (plain hashes get the plain
                // `Pair(str_key, v)` as before).
                Some(Ok(items.typed_pair(key, value.clone())))
            }
        }
        _ => {
            // Try efficient range sampling first
            if let Some(v) = sample_one_from_range(target) {
                return Some(Ok(v));
            }
            Some(Ok(if crate::runtime::utils::is_shaped_array(target) {
                random_item(&crate::runtime::utils::shaped_array_leaves(target))
            } else {
                // ADR-0040: the RECEIVER's own elements, ignoring its own
                // itemization -- borrowed, not copied, to index one slot.
                runtime::with_receiver_items(target, random_item)
            }))
        }
    }
}

/// `.pick($count)`.
// Cost: O(e + k) on a list/array, e = elements of the invocant (copied),
// k = elements picked (each an O(1) swap_remove); `.pick(*)` is an O(e)
// Fisher-Yates shuffle; O(k) on an integer Range.
pub(super) fn pick_n(target: &Value, arg: &Value) -> Option<Result<Value, RuntimeError>> {
    if matches!(target.view(), ValueView::Mix(_, _)) {
        return Some(Err(RuntimeError::new(
            "Cannot call .pick on a Mix (immutable)",
        )));
    }
    // Callable args (e.g. WhateverCode `* / 2`) need interpreter to invoke;
    // fall through to the runtime.
    if matches!(arg.view(), ValueView::Sub(_) | ValueView::WeakSub(_)) {
        return None;
    }
    // For Bag/BagHash, use weighted picking without expanding to a flat list
    if let ValueView::Bag(bag, _) = target.view() {
        if let ValueView::Num(f) = arg.view()
            && f.is_nan()
        {
            return Some(Err(RuntimeError::new("Cannot convert NaN to Int")));
        }
        let total_items: i128 = bag
            .values()
            .map(crate::runtime::utils::bigint_to_i128_sat)
            .sum();
        let count: i128 = match arg.view() {
            ValueView::Whatever => total_items,
            ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => total_items,
            ValueView::Int(n) => n.max(0) as i128,
            ValueView::Num(f) => (f as i64).max(0) as i128,
            ValueView::Rat(n, d) if d != 0 => (n / d).max(0) as i128,
            _ => 0i128,
        };
        if count == 0 || bag.is_empty() {
            return Some(Ok(Value::seq(Vec::new())));
        }
        // Build a mutable copy of counts for without-replacement picking
        // (keys decoded to the element objects via typed_key).
        let mut counts: Vec<(Value, i128)> = bag
            .iter()
            .filter(|(_, c)| c.is_positive())
            .map(|(k, c)| {
                (
                    bag.typed_key(k),
                    crate::runtime::utils::bigint_to_i128_sat(c),
                )
            })
            .collect();
        let mut total: i128 = counts.iter().map(|(_, c)| *c).sum();
        let pick_count = (count as usize).min(total as usize);
        let mut result = Vec::with_capacity(pick_count);
        for _ in 0..pick_count {
            if total <= 0 {
                break;
            }
            let needle_f = crate::builtins::rng::builtin_rand() * total as f64;
            let mut needle = needle_f as i128;
            if needle >= total {
                needle = total - 1;
            }
            let mut picked_idx = counts.len() - 1;
            let mut cum: i128 = 0;
            for (i, (_, c)) in counts.iter().enumerate() {
                cum += *c;
                if needle < cum {
                    picked_idx = i;
                    break;
                }
            }
            result.push(counts[picked_idx].0.clone());
            counts[picked_idx].1 -= 1;
            total -= 1;
            if counts[picked_idx].1 == 0 {
                counts.swap_remove(picked_idx);
            }
        }
        return Some(Ok(Value::seq(result)));
    }
    // For Set, pick returns keys (strings) without replacement
    if let ValueView::Set(set, _) = target.view() {
        if let ValueView::Num(f) = arg.view()
            && f.is_nan()
        {
            return Some(Err(RuntimeError::new("Cannot convert NaN to Int")));
        }
        let mut keys: Vec<Value> = set.iter().map(|k| set.typed_key(k)).collect();
        let count: usize = match arg.view() {
            ValueView::Whatever => keys.len(),
            ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => keys.len(),
            ValueView::Int(n) => n.max(0) as usize,
            ValueView::Num(f) => (f as i64).max(0) as usize,
            ValueView::Rat(n, d) if d != 0 => (n / d).max(0) as usize,
            _ => 0,
        };
        let pick_count = count.min(keys.len());
        // Fisher-Yates shuffle for without-replacement
        let len = keys.len();
        for i in (1..len).rev() {
            let j = (crate::builtins::rng::builtin_rand() * (i + 1) as f64) as usize % (i + 1);
            keys.swap(i, j);
        }
        keys.truncate(pick_count);
        return Some(Ok(Value::seq(keys)));
    }
    // Fast path for integer ranges — avoid materializing
    if let Some(result) = range_pick_n_fast(target, arg) {
        return Some(Ok(result));
    }
    // .pick(**) — lazy infinite shuffled cycles
    if matches!(arg.view(), ValueView::HyperWhatever) {
        let pool = runtime::value_to_list_for_receiver(target);
        if pool.is_empty() {
            return Some(Ok(Value::seq(Vec::new())));
        }
        // Pre-generate several cycles of shuffled picks
        let num_cycles = 4;
        let mut cached = Vec::with_capacity(pool.len() * num_cycles);
        for _ in 0..num_cycles {
            let mut cycle = pool.clone();
            let len = cycle.len();
            for i in (1..len).rev() {
                let j = (crate::builtins::rng::builtin_rand() * (i + 1) as f64) as usize % (i + 1);
                cycle.swap(i, j);
            }
            cached.extend(cycle);
        }
        // `.pick(**)` is a genuinely infinite lazy Seq (`.is-lazy` is
        // True in Rakudo, and `.elems` on it throws X::Cannot::Lazy);
        // the pre-generated cycles are only a cache. Record the logical
        // count so `LazyList::is_genuinely_lazy` can see that -- a bare
        // cache carries no other evidence of infiniteness.
        return Some(Ok(Value::lazy_list(crate::gc::Gc::new(
            crate::value::LazyList::new_cached_infinite(cached),
        ))));
    }
    // NaN check for general .pick path
    if let ValueView::Num(f) = arg.view()
        && f.is_nan()
    {
        return Some(Err(RuntimeError::new("Cannot convert NaN to Int")));
    }
    let mut items = runtime::value_to_list_for_receiver(target);
    Some(Ok(match arg.view() {
        ValueView::Whatever => {
            // .pick(*) — Fisher-Yates shuffle
            let len = items.len();
            for i in (1..len).rev() {
                let j = (crate::builtins::rng::builtin_rand() * (i + 1) as f64) as usize % (i + 1);
                items.swap(i, j);
            }
            Value::seq(items)
        }
        ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => {
            // .pick(Inf) — same as .pick(*)
            let len = items.len();
            for i in (1..len).rev() {
                let j = (crate::builtins::rng::builtin_rand() * (i + 1) as f64) as usize % (i + 1);
                items.swap(i, j);
            }
            Value::seq(items)
        }
        ValueView::Num(f) => {
            // .pick(<num>) — truncate to int
            let count = (f as i64).max(0) as usize;
            if count == 0 || items.is_empty() {
                Value::seq(Vec::new())
            } else {
                let mut result = Vec::with_capacity(count.min(items.len()));
                for _ in 0..count.min(items.len()) {
                    let idx = (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize
                        % items.len();
                    result.push(items.swap_remove(idx));
                }
                Value::seq(result)
            }
        }
        ValueView::Rat(n, d) if d != 0 => {
            // .pick(<rat>) — truncate to int
            let count = (n / d).max(0) as usize;
            if count == 0 || items.is_empty() {
                Value::seq(Vec::new())
            } else {
                let mut result = Vec::with_capacity(count.min(items.len()));
                for _ in 0..count.min(items.len()) {
                    let idx = (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize
                        % items.len();
                    result.push(items.swap_remove(idx));
                }
                Value::seq(result)
            }
        }
        ValueView::Int(n) => {
            let count = n.max(0) as usize;
            if count == 0 || items.is_empty() {
                Value::seq(Vec::new())
            } else {
                let mut result = Vec::with_capacity(count.min(items.len()));
                for _ in 0..count.min(items.len()) {
                    let idx = (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize
                        % items.len();
                    result.push(items.swap_remove(idx));
                }
                Value::seq(result)
            }
        }
        ValueView::Str(s) => {
            let count = s.trim().parse::<i64>().unwrap_or(0).max(0) as usize;
            if count == 0 || items.is_empty() {
                Value::seq(Vec::new())
            } else {
                let mut result = Vec::with_capacity(count.min(items.len()));
                for _ in 0..count.min(items.len()) {
                    let idx = (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize
                        % items.len();
                    result.push(items.swap_remove(idx));
                }
                Value::seq(result)
            }
        }
        _ => return None,
    }))
}
