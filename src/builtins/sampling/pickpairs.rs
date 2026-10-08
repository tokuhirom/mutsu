use crate::value::{RuntimeError, Value, ValueView};

/// `.pickpairs` with no argument.
// Cost: O(n) on a quant hash, n = elements (the n-th entry is walked to).
pub(super) fn pickpairs_one(target: &Value) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Bag(items, _) => {
            if items.is_empty() {
                Some(Ok(Value::NIL))
            } else {
                let mut idx = (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                if idx >= items.len() {
                    idx = items.len() - 1;
                }
                let (key, count) = items.iter().nth(idx).expect("index in range");
                Some(Ok(crate::runtime::utils::quanthash_typed_pair(
                    items.typed_key(key),
                    Value::from_bigint(count.clone()),
                )))
            }
        }
        ValueView::Set(items, _) => {
            if items.is_empty() {
                Some(Ok(Value::NIL))
            } else {
                let mut idx = (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                if idx >= items.len() {
                    idx = items.len() - 1;
                }
                let key = items.iter().nth(idx).expect("index in range");
                Some(Ok(crate::runtime::utils::quanthash_typed_pair(
                    items.typed_key(key),
                    Value::TRUE,
                )))
            }
        }
        ValueView::Mix(items, _) => {
            if items.is_empty() {
                Some(Ok(Value::NIL))
            } else {
                let mut idx = (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                if idx >= items.len() {
                    idx = items.len() - 1;
                }
                let (key, weight) = items.iter().nth(idx).expect("index in range");
                Some(Ok(crate::runtime::utils::quanthash_typed_pair(
                    items.typed_key(key),
                    crate::value::mix_weight_to_value(*weight),
                )))
            }
        }
        _ => None,
    }
}

/// The `(element, weight)` pairs of a `Set`, `Bag` or `Mix`, as the values a
/// `Pair` of that element would hold.
// Cost: O(n), n = elements.
fn weighted_pairs(target: &Value) -> Option<Vec<(Value, Value)>> {
    match target.view() {
        ValueView::Bag(bag, _) => Some(
            bag.iter()
                .map(|(k, v)| (bag.typed_key(k), Value::from_bigint(v.clone())))
                .collect(),
        ),
        ValueView::Set(set, _) => Some(
            set.iter()
                .map(|k| (set.typed_key(k), Value::TRUE))
                .collect(),
        ),
        ValueView::Mix(mix, _) => Some(
            mix.iter()
                .map(|(k, w)| (mix.typed_key(k), crate::value::mix_weight_to_value(*w)))
                .collect(),
        ),
        _ => None,
    }
}

/// `.pickpairs($count)`: `$count` distinct pairs, in random order.
// Cost: O(n + k) on a quant hash, n = elements (copied), k = pairs picked.
pub(super) fn pickpairs_n(target: &Value, arg: &Value) -> Option<Result<Value, RuntimeError>> {
    let mut pairs = weighted_pairs(target)?;
    let count = match arg.view() {
        ValueView::Whatever => pairs.len(),
        ValueView::Int(n) => n.max(0) as usize,
        ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => pairs.len(),
        ValueView::Num(f) => (f as i64).max(0) as usize,
        ValueView::Rat(n, d) if d != 0 => (n / d).max(0) as usize,
        _ => return None,
    };
    let pick_count = count.min(pairs.len());
    let mut result = Vec::with_capacity(pick_count);
    for _ in 0..pick_count {
        let idx =
            (crate::builtins::rng::builtin_rand() * pairs.len() as f64) as usize % pairs.len();
        let (key, weight) = pairs.swap_remove(idx);
        result.push(crate::runtime::utils::quanthash_typed_pair(key, weight));
    }
    Some(Ok(Value::seq(result)))
}
