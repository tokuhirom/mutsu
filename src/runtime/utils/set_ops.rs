use super::*;

/// Whether a set operator's left operand makes the result a mutable QuantHash.
/// Raku's set operators (`(|)`/`(&)`/`(-)`/`(^)`/`(.)`/`(+)`) take their
/// result's mutability from the FIRST operand only: an immutable operand
/// (Set/Bag/Mix, a list, or a type object used as an element) yields an
/// immutable result even when a later operand is a SetHash/BagHash/MixHash.
pub(crate) fn set_result_mutability(v: &Value) -> bool {
    matches!(
        v.view(),
        ValueView::Set(_, true) | ValueView::Bag(_, true) | ValueView::Mix(_, true)
    )
}

/// Whether a value is an actual QuantHash instance (Set/Bag/Mix, mutable or
/// not) as opposed to a list, hash, or bare type object. Used by symmetric
/// difference, whose result stays mutable only when BOTH operands are
/// QuantHashes (`SetHash (^) Set` -> SetHash, but `SetHash (^) <a b>` -> Set).
pub(crate) fn is_quanthash_instance(v: &Value) -> bool {
    matches!(
        v.view(),
        ValueView::Set(_, _) | ValueView::Bag(_, _) | ValueView::Mix(_, _)
    )
}

/// Result mutability for symmetric difference (`(^)`). Like the other set
/// operators it follows the first operand, but at the Set level it additionally
/// demotes to an immutable Set when the right operand is not a QuantHash
/// (`SetHash (^) <a b>` -> Set). The demotion does NOT apply once the result is
/// promoted to Bag/Mix level (`BagHash (^) <a b>` -> BagHash).
pub(crate) fn set_sym_diff_mutability(left: &Value, right: &Value) -> bool {
    set_result_mutability(left)
        && (is_quanthash_instance(right)
            || matches!(left.view(), ValueView::Bag(_, _) | ValueView::Mix(_, _)))
}

/// Overlay the given mutability onto a freshly-built set-operator result.
pub(crate) fn with_set_mutability(mut result: Value, mutable: bool) -> Value {
    if result.with_set_mut(|_, m| *m = mutable).is_none()
        && result.with_bag_mut(|_, m| *m = mutable).is_none()
    {
        result.with_mix_mut(|_, m| *m = mutable);
    }
    result
}

/// Standalone set difference: left (-) right
/// Implements Raku type promotion: Mix > Bag > Set
///
/// In Raku, when the RHS is a Hash, it is treated as a Set (truthy keys
/// with weight 1) rather than using its numeric values as weights.
pub(crate) fn set_diff_values(left: &Value, right: &Value) -> Value {
    let mut originals: HashMap<String, Value> = HashMap::new();
    // When the RHS is a Hash, coerce it to a Set for subtraction.
    // The Hash's truthy keys each count as weight 1.
    let right_as_set;
    let effective_right = if matches!(right.view(), ValueView::Hash(_)) {
        right_as_set = Value::set_typed(coerce_to_set(right, &mut originals), HashMap::new());
        &right_as_set
    } else {
        right
    };

    let level = set_type_level(left).max(set_type_level(effective_right));
    match level {
        2 => {
            // Mix-level difference: include all keys, keep non-zero results
            let a = to_mix_map(left, &mut originals);
            let b = to_mix_map(effective_right, &mut originals);
            let mut result = HashMap::new();
            for (k, v) in &a {
                let bv = b.get(k).copied().unwrap_or(0.0);
                let diff = v - bv;
                if diff != 0.0 {
                    result.insert(k.clone(), diff);
                }
            }
            for (k, v) in &b {
                if !a.contains_key(k) && *v != 0.0 {
                    result.insert(k.clone(), -*v);
                }
            }
            Value::mix_with_original_keys(result, originals)
        }
        1 => {
            // Bag-level difference: only positive results
            let a = to_bag_map(left, &mut originals);
            let b = to_bag_map(effective_right, &mut originals);
            let mut result = HashMap::new();
            for (k, v) in &a {
                let bv = b.get(k).copied().unwrap_or(0);
                if *v > bv {
                    result.insert(k.clone(), *v - bv);
                }
            }
            Value::bag_typed(result, originals)
        }
        _ => {
            // Set-level difference
            let a = coerce_to_set(left, &mut originals);
            let b = coerce_to_set(effective_right, &mut originals);
            Value::set_typed(a.difference(&b).cloned().collect(), originals)
        }
    }
}

/// Standalone set intersection: left (&) right
pub(crate) fn set_intersect_values(left: &Value, right: &Value) -> Value {
    // Determine result type level: 0=Set, 1=Bag, 2=Mix
    let type_level = |v: &Value| -> u8 {
        match v.view() {
            ValueView::Mix(_, _) => 2,
            ValueView::Bag(_, _) => 1,
            _ => 0,
        }
    };
    let result_level = type_level(left).max(type_level(right));
    let mut originals: HashMap<String, Value> = HashMap::new();
    match result_level {
        2 => {
            let a = coerce_to_mix(left, &mut originals);
            let b = coerce_to_mix(right, &mut originals);
            let mut result = HashMap::new();
            for (k, v) in a.iter() {
                if let Some(bv) = b.get(k) {
                    result.insert(k.clone(), v.min(*bv));
                }
            }
            Value::mix_with_original_keys(result, originals)
        }
        1 => {
            let a = coerce_to_bag(left, &mut originals);
            let b = coerce_to_bag(right, &mut originals);
            let mut result = HashMap::new();
            for (k, v) in a.iter() {
                if let Some(bv) = b.get(k) {
                    result.insert(k.clone(), (*v).min(*bv));
                }
            }
            Value::bag_typed(result, originals)
        }
        _ => {
            let a = coerce_to_set(left, &mut originals);
            let b = coerce_to_set(right, &mut originals);
            Value::set_typed(a.intersection(&b).cloned().collect(), originals)
        }
    }
}

/// Coerce a value to a Bag (HashMap<String, i64>)
fn coerce_to_bag(val: &Value, originals: &mut HashMap<String, Value>) -> HashMap<String, i64> {
    match val.view() {
        ValueView::Bag(b, _) => {
            extend_quanthash_originals(originals, &b.original_keys);
            resolve_bag_tab_keys(&b)
        }
        ValueView::Set(s, _) => {
            extend_quanthash_originals(originals, &s.original_keys);
            s.iter().map(|k| (k.clone(), 1)).collect()
        }
        ValueView::Mix(m, _) => {
            extend_quanthash_originals(originals, &m.original_keys);
            m.iter().map(|(k, v)| (k.clone(), *v as i64)).collect()
        }
        _ => {
            // Count occurrences for list-like values
            let items = value_to_list(val);
            let mut result = HashMap::new();
            for item in &items {
                let (key, elem) = quanthash_elem_entry(item);
                record_quanthash_original(originals, &key, &elem);
                *result.entry(key).or_insert(0i64) += 1;
            }
            result
        }
    }
}

/// Coerce a value to a Mix (HashMap<String, f64>)
fn coerce_to_mix(val: &Value, originals: &mut HashMap<String, Value>) -> HashMap<String, f64> {
    match val.view() {
        ValueView::Mix(m, _) => {
            extend_quanthash_originals(originals, &m.original_keys);
            m.weights.clone()
        }
        ValueView::Bag(b, _) => {
            extend_quanthash_originals(originals, &b.original_keys);
            let resolved = resolve_bag_tab_keys(&b);
            resolved.into_iter().map(|(k, v)| (k, v as f64)).collect()
        }
        ValueView::Set(s, _) => {
            extend_quanthash_originals(originals, &s.original_keys);
            s.iter().map(|k| (k.clone(), 1.0)).collect()
        }
        _ => {
            // Count occurrences for list-like values
            let items = value_to_list(val);
            let mut result = HashMap::new();
            for item in &items {
                let (key, elem) = quanthash_elem_entry(item);
                record_quanthash_original(originals, &key, &elem);
                *result.entry(key).or_insert(0.0f64) += 1.0;
            }
            result
        }
    }
}

/// Standalone set symmetric difference: left (^) right
/// Implements Raku type promotion: Mix > Bag > Set
///
/// For Bags: the result multiplicity for each key is |a_count - b_count|
/// (entries with zero are dropped).
/// For Mixes: the result weight for each key is |a_weight - b_weight|
/// (entries with zero are dropped).
pub(crate) fn set_sym_diff_values(left: &Value, right: &Value) -> Value {
    let level = set_type_level(left).max(set_type_level(right));
    let mut originals: HashMap<String, Value> = HashMap::new();
    match level {
        2 => {
            // Mix-level symmetric difference: |a - b| for each key
            let a = to_mix_map(left, &mut originals);
            let b = to_mix_map(right, &mut originals);
            let mut result = HashMap::new();
            let mut all_keys: HashSet<String> = a.keys().cloned().collect();
            all_keys.extend(b.keys().cloned());
            for k in all_keys {
                let av = a.get(&k).copied().unwrap_or(0.0);
                let bv = b.get(&k).copied().unwrap_or(0.0);
                let diff = (av - bv).abs();
                if diff != 0.0 {
                    result.insert(k, diff);
                }
            }
            Value::mix_with_original_keys(result, originals)
        }
        1 => {
            // Bag-level symmetric difference: |a - b| for each key
            let a = to_bag_map(left, &mut originals);
            let b = to_bag_map(right, &mut originals);
            let mut result = HashMap::new();
            let mut all_keys: HashSet<String> = a.keys().cloned().collect();
            all_keys.extend(b.keys().cloned());
            for k in all_keys {
                let av = a.get(&k).copied().unwrap_or(0);
                let bv = b.get(&k).copied().unwrap_or(0);
                let diff = (av - bv).unsigned_abs() as i64;
                if diff > 0 {
                    result.insert(k, diff);
                }
            }
            Value::bag_typed(result, originals)
        }
        _ => {
            // Set-level symmetric difference
            let a = coerce_to_set(left, &mut originals);
            let b = coerce_to_set(right, &mut originals);
            Value::set_typed(a.symmetric_difference(&b).cloned().collect(), originals)
        }
    }
}

/// Multi-arg symmetric difference: for each key, result = max_weight - second_max_weight.
/// This is NOT a left-fold; it operates on all inputs simultaneously.
pub(crate) fn set_sym_diff_multi(args: &[Value]) -> Value {
    let level = args.iter().map(set_type_level).max().unwrap_or(0);
    let mut originals: HashMap<String, Value> = HashMap::new();
    match level {
        2 => {
            // Mix-level: collect all weight vectors per key, then max - second_max
            let maps: Vec<HashMap<String, f64>> =
                args.iter().map(|a| to_mix_map(a, &mut originals)).collect();
            let mut all_keys: HashSet<String> = HashSet::new();
            for m in &maps {
                all_keys.extend(m.keys().cloned());
            }
            let mut result = HashMap::new();
            for k in all_keys {
                let mut weights: Vec<f64> = maps
                    .iter()
                    .map(|m| m.get(&k).copied().unwrap_or(0.0))
                    .collect();
                weights.sort_by(|a, b| b.partial_cmp(a).unwrap_or(std::cmp::Ordering::Equal));
                let diff = weights[0] - weights.get(1).copied().unwrap_or(0.0);
                if diff != 0.0 {
                    result.insert(k, diff);
                }
            }
            Value::mix_with_original_keys(result, originals)
        }
        1 => {
            // Bag-level: collect all count vectors per key, then max - second_max
            let maps: Vec<HashMap<String, i64>> =
                args.iter().map(|a| to_bag_map(a, &mut originals)).collect();
            let mut all_keys: HashSet<String> = HashSet::new();
            for m in &maps {
                all_keys.extend(m.keys().cloned());
            }
            let mut result = HashMap::new();
            for k in all_keys {
                let mut counts: Vec<i64> = maps
                    .iter()
                    .map(|m| m.get(&k).copied().unwrap_or(0))
                    .collect();
                counts.sort_by(|a, b| b.cmp(a));
                let diff = counts[0] - counts.get(1).copied().unwrap_or(0);
                if diff > 0 {
                    result.insert(k, diff);
                }
            }
            Value::bag_typed(result, originals)
        }
        _ => {
            // Set-level: element is in result iff it appears in exactly 1 input
            let sets: Vec<HashSet<String>> = args
                .iter()
                .map(|a| coerce_to_set(a, &mut originals))
                .collect();
            let mut counts: HashMap<String, usize> = HashMap::new();
            for s in &sets {
                for k in s {
                    *counts.entry(k.clone()).or_insert(0) += 1;
                }
            }
            Value::set_typed(
                counts
                    .into_iter()
                    .filter(|(_, count)| *count == 1)
                    .map(|(k, _)| k)
                    .collect(),
                originals,
            )
        }
    }
}
