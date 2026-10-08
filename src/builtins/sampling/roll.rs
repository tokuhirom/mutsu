use crate::runtime;
use crate::value::{RuntimeError, Value, ValueView};

use super::range::sample_one_from_range;
use super::{random_item, sample_weighted_bag_key, sample_weighted_mix_key};

/// `.roll` with no argument.
// Cost: O(1) on an Array, a List, a reified Seq or an integer Range; O(e) on
// any other list-like, e = elements (decomposed into a Vec to index one slot).
pub(super) fn roll_one(target: &Value) -> Option<Result<Value, RuntimeError>> {
    if let ValueView::Mix(items, _) = target.view() {
        return Some(Ok(sample_weighted_mix_key(&items).unwrap_or(Value::NIL)));
    }
    if let ValueView::Bag(items, _) = target.view() {
        return Some(Ok(sample_weighted_bag_key(&items).unwrap_or(Value::NIL)));
    }
    if let ValueView::Set(items, _) = target.view() {
        if items.is_empty() {
            return Some(Ok(Value::NIL));
        }
        let keys: Vec<&String> = items.iter().collect();
        let mut idx = (crate::builtins::rng::builtin_rand() * keys.len() as f64) as usize;
        if idx >= keys.len() {
            idx = keys.len() - 1;
        }
        return Some(Ok(items.typed_key(keys[idx])));
    }
    // ADR-0040: a Hash is decomposed into its OWN key-value pairs
    // here regardless of the hash's own itemization flag -- `.roll`
    // is called ON this hash as the receiver, not flattened as an
    // element of some other container, so the itemization axis
    // (which governs the latter) does not apply. Mirrors `.pick`'s
    // own dedicated `ValueView::Hash` arm above; `value_to_list`
    // below would otherwise treat an itemized Hash (e.g. one
    // produced by nested autovivification, `%h<a><b>++`) as a
    // single opaque item and "roll" the whole hash instead of one
    // of its pairs.
    if let ValueView::Hash(items) = target.view() {
        if items.is_empty() {
            return Some(Ok(Value::NIL));
        }
        let mut idx = (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
        if idx >= items.len() {
            idx = items.len() - 1;
        }
        let (key, value) = items.iter().nth(idx).expect("index in range");
        return Some(Ok(items.typed_pair(key, value.clone())));
    }
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

/// `.roll($count)`.
// Cost: O(k), k = elements rolled, on an Array, a List, a reified Seq or an
// integer Range; O(e + k) on any other list-like, e = elements (decomposed
// into the sampling pool first). `.roll(*)` copies the pool once, O(e).
pub(super) fn roll_n(target: &Value, arg: &Value) -> Option<Result<Value, RuntimeError>> {
    if matches!(target.view(), ValueView::Package(_)) {
        return None;
    }
    let count = match arg.view() {
        ValueView::Int(i) if i > 0 => Some(i as usize),
        ValueView::Int(_) => Some(0),
        ValueView::Num(f) if f.is_nan() => {
            return Some(Err(RuntimeError::new("Cannot convert NaN to Int")));
        }
        ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => None,
        ValueView::Num(f) if f < 0.0 => Some(0),
        ValueView::Num(f) => Some(f as usize),
        ValueView::Whatever => None,
        ValueView::Str(s) => {
            let parsed = s.trim().parse::<i64>().ok()?;
            Some(parsed.max(0) as usize)
        }
        _ => return None,
    };
    if let ValueView::Mix(items, _) = target.view() {
        if count.is_none() {
            let generated = 131_072usize;
            let mut out = Vec::with_capacity(generated);
            for _ in 0..generated {
                if let Some(v) = sample_weighted_mix_key(&items) {
                    out.push(v);
                }
            }
            return Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                crate::value::LazyList::new_cached_infinite(out),
            ))));
        }
        let count = count.unwrap_or(0);
        if count == 0 {
            return Some(Ok(Value::seq(Vec::new())));
        }
        let mut result = Vec::with_capacity(count);
        for _ in 0..count {
            if let Some(v) = sample_weighted_mix_key(&items) {
                result.push(v);
            }
        }
        return Some(Ok(Value::seq(result)));
    }
    if let ValueView::Bag(items, _) = target.view() {
        if count.is_none() {
            let generated = 131_072usize;
            let mut out = Vec::with_capacity(generated);
            for _ in 0..generated {
                if let Some(v) = sample_weighted_bag_key(&items) {
                    out.push(v);
                }
            }
            return Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                crate::value::LazyList::new_cached_infinite(out),
            ))));
        }
        let count = count.unwrap_or(0);
        if count == 0 {
            return Some(Ok(Value::seq(Vec::new())));
        }
        let mut result = Vec::with_capacity(count);
        for _ in 0..count {
            if let Some(v) = sample_weighted_bag_key(&items) {
                result.push(v);
            }
        }
        return Some(Ok(Value::seq(result)));
    }
    if let ValueView::Set(items, _) = target.view() {
        let keys: Vec<&String> = items.iter().collect();
        if keys.is_empty() {
            return Some(Ok(Value::seq(Vec::new())));
        }
        if count.is_none() {
            let generated = 131_072usize;
            let mut out = Vec::with_capacity(generated);
            for _ in 0..generated {
                let mut idx = (crate::builtins::rng::builtin_rand() * keys.len() as f64) as usize;
                if idx >= keys.len() {
                    idx = keys.len() - 1;
                }
                out.push(items.typed_key(keys[idx]));
            }
            return Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                crate::value::LazyList::new_cached_infinite(out),
            ))));
        }
        let count = count.unwrap_or(0);
        if count == 0 {
            return Some(Ok(Value::seq(Vec::new())));
        }
        let mut result = Vec::with_capacity(count);
        for _ in 0..count {
            let mut idx = (crate::builtins::rng::builtin_rand() * keys.len() as f64) as usize;
            if idx >= keys.len() {
                idx = keys.len() - 1;
            }
            result.push(items.typed_key(keys[idx]));
        }
        return Some(Ok(Value::seq(result)));
    }
    let sample_from_range = |range: &Value| -> Option<Value> {
        let random_i64 = |lo: i64, hi: i64| -> Value {
            if hi <= lo {
                return Value::int(lo);
            }
            crate::builtins::methods_0arg::dispatch_core_range::range_pick_one_i64(lo, hi)
        };
        match range.view() {
            ValueView::Range(start, end) => Some(random_i64(start, end)),
            ValueView::RangeExcl(start, end) => {
                if start >= end {
                    Some(Value::NIL)
                } else {
                    Some(random_i64(start, end.saturating_sub(1)))
                }
            }
            ValueView::RangeExclStart(start, end) => {
                if start >= end {
                    Some(Value::NIL)
                } else {
                    Some(random_i64(start.saturating_add(1), end))
                }
            }
            ValueView::RangeExclBoth(start, end) => {
                if start.saturating_add(1) >= end {
                    Some(Value::NIL)
                } else {
                    Some(random_i64(start.saturating_add(1), end.saturating_sub(1)))
                }
            }
            ValueView::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                // Try BigInt-based fast path for integer ranges
                if let Some(result) =
                    crate::builtins::methods_0arg::dispatch_core_range::generic_range_pick_one(
                        start, end, excl_start, excl_end,
                    )
                {
                    return Some(result);
                }
                // Any other endpoint pair — non-integer numeric
                // (Rat/Num/FatRat) or non-numeric (`'a'..'z'`) — is
                // enumerated via `.succ` (value_to_list preserves the
                // endpoint type) and sampled from that, so a picked
                // element keeps its type: `(1.1..3.1).roll(n)` yields
                // Rats, not Nums, and `('a'..'z').roll(n)` yields
                // distinct letters rather than 'a' every time.
                //
                // An unbounded end (`1..*`, `'a'..Inf`) has no
                // enumeration to sample, so it keeps answering the start
                // element rather than trying to reify the range.
                let unbounded = matches!(end.view(), ValueView::Whatever)
                    || matches!(end.view(), ValueView::Num(f) if f.is_infinite());
                if !unbounded {
                    let vals = crate::runtime::utils::value_to_list(range);
                    if !vals.is_empty() {
                        let idx = (crate::builtins::rng::builtin_rand() * vals.len() as f64)
                            as usize
                            % vals.len();
                        return Some(vals[idx].clone());
                    }
                }
                Some(start.as_ref().clone())
            }
            _ => None,
        }
    };

    if count.is_none() {
        let items = if target.is_range() {
            Vec::new()
        } else {
            runtime::value_to_list_for_receiver(target)
        };
        if !target.is_range() && items.is_empty() {
            return Some(Ok(Value::seq(Vec::new())));
        }
        if target.is_range() {
            // A range's pool isn't a finite Vec (it may be huge or
            // infinite), so it still needs a per-pull sampler rather
            // than `SequenceSpec::RollPool`'s static pool. Generate a
            // bounded eager prefix as before.
            let generated = 1024usize;
            let mut out = Vec::with_capacity(generated);
            for _ in 0..generated {
                if let Some(v) = sample_from_range(target) {
                    out.push(v);
                }
            }
            return Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                crate::value::LazyList::new_cached_infinite(out),
            ))));
        }
        // A finite pool: `.roll(*)` is a genuinely infinite Seq (each
        // pull an independent random pick), so represent it as a
        // sequence-spec lazy list (like `1...*`) instead of eagerly
        // generating a fixed-size prefix. This renders Rakudo's
        // `(...)` gist placeholder and can be pulled indefinitely.
        return Some(Ok(Value::lazy_list(crate::gc::Gc::new(
            crate::value::LazyList::new_sequence(
                Vec::new(),
                crate::value::SequenceSpec::RollPool(items),
            ),
        ))));
    }
    let count = count.unwrap_or(0);
    if count == 0 {
        return Some(Ok(Value::seq(Vec::new())));
    }
    if target.is_range() {
        let mut result = Vec::with_capacity(count);
        for _ in 0..count {
            if let Some(v) = sample_from_range(target) {
                result.push(v);
            }
        }
        return Some(Ok(Value::seq(result)));
    }
    // The pool is borrowed, not copied: `.roll(k)` indexes k slots.
    Some(Ok(Value::seq(runtime::with_receiver_items(
        target,
        |items| {
            if items.is_empty() {
                return Vec::new();
            }
            (0..count)
                .map(|_| {
                    let mut idx =
                        (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                    if idx >= items.len() {
                        idx = items.len() - 1;
                    }
                    items[idx].clone()
                })
                .collect()
        },
    ))))
}
