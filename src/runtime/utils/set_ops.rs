use super::*;
use crate::value::ValueMap;

/// Whether a set operator's left operand makes the result a mutable QuantHash.
/// Raku's set operators (`(|)`/`(&)`/`(-)`/`(^)`/`(.)`/`(+)`) take their
/// result's mutability from the FIRST operand only: an immutable operand
/// (Set/Bag/Mix, a list, or a type object used as an element) yields an
/// immutable result even when a later operand is a SetHash/BagHash/MixHash.
pub(crate) fn set_result_mutability(v: &Value) -> bool {
    matches!(
        set_operand(v).view(),
        ValueView::Set(_, true) | ValueView::Bag(_, true) | ValueView::Mix(_, true)
    )
}

/// Whether a value is an actual QuantHash instance (Set/Bag/Mix, mutable or
/// not) as opposed to a list, hash, or bare type object. Used by symmetric
/// difference, whose result stays mutable only when BOTH operands are
/// QuantHashes (`SetHash (^) Set` -> SetHash, but `SetHash (^) <a b>` -> Set).
pub(crate) fn is_quanthash_instance(v: &Value) -> bool {
    matches!(
        set_operand(v).view(),
        ValueView::Set(_, _) | ValueView::Bag(_, _) | ValueView::Mix(_, _)
    )
}

/// Result mutability for symmetric difference (`(^)`). Like the other set
/// operators it follows the first operand, but at the Set level it additionally
/// demotes to an immutable Set when the right operand is not a QuantHash
/// (`SetHash (^) <a b>` -> Set). The demotion does NOT apply once the result is
/// promoted to Bag/Mix level (`BagHash (^) <a b>` -> BagHash).
pub(crate) fn set_sym_diff_mutability(left: &Value, right: &Value) -> bool {
    set_result_mutability(left) && (is_quanthash_instance(right) || set_level(left) > SetLevel::Set)
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

/// Multi-arg symmetric difference: for each key, result = max_weight - second_max_weight.
/// This is NOT a left-fold; it operates on all inputs simultaneously.
pub(crate) fn set_sym_diff_multi(args: &[Value]) -> Value {
    let level = args.iter().map(set_level).max().unwrap_or(SetLevel::Set);
    let mut originals: ValueMap = ValueMap::default();
    match level {
        SetLevel::Mix => {
            // Mix-level: collect all weight vectors per key, then max - second_max
            let maps: Vec<HashMap<String, f64>> = args
                .iter()
                .map(|a| operand_mix_weights(a, &mut originals))
                .collect();
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
                let diff = crate::builtins::mix_weight::sub(
                    weights[0],
                    weights.get(1).copied().unwrap_or(0.0),
                );
                if diff != 0.0 {
                    result.insert(k, diff);
                }
            }
            Value::mix_with_original_keys(result, originals)
        }
        SetLevel::Bag => {
            // Bag-level: collect all count vectors per key, then max - second_max
            let maps: Vec<HashMap<String, BigInt>> = args
                .iter()
                .map(|a| operand_bag_counts(a, &mut originals))
                .collect();
            let mut all_keys: HashSet<String> = HashSet::new();
            for m in &maps {
                all_keys.extend(m.keys().cloned());
            }
            let mut result = HashMap::new();
            let zero = BigInt::from(0);
            for k in all_keys {
                let mut counts: Vec<BigInt> = maps
                    .iter()
                    .map(|m| m.get(&k).unwrap_or(&zero).clone())
                    .collect();
                counts.sort_by(|a, b| b.cmp(a));
                let diff = &counts[0] - counts.get(1).unwrap_or(&zero);
                if diff.is_positive() {
                    result.insert(k, diff);
                }
            }
            Value::bag_typed_big(result, originals)
        }
        SetLevel::Set => {
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
