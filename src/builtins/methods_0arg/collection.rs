use crate::value::{ListGen, RuntimeError, Value, ValueView};
fn make_inverted_pair(key: Value, value: Value) -> Value {
    // Positional flavour regardless of key type — see `quanthash_typed_pair`.
    Value::value_pair(key, value)
}

fn should_expand_invert_value(value: &Value) -> bool {
    matches!(
        value.view(),
        ValueView::Array(_, _)
            | ValueView::Seq(_)
            | ValueView::Slip(_)
            | ValueView::Hash(_)
            | ValueView::Set(_, _)
            | ValueView::Bag(_, _)
            | ValueView::Mix(_, _)
    ) || value.is_range()
}

fn extend_inverted_pairs(out: &mut Vec<Value>, key: Value, value: &Value) {
    // ADR-0040 slices 1-2: a `Hash` value (and an `Array` element) that holds
    // an aggregate is itemized at the store. `.invert` is `self.map: *.antipair`
    // over the pairs, and `Pair.antipair` READS `$!value` — an attribute read
    // decontainerizes — so the value expands into its OWN elements here
    // whatever itemization it carries as an element:
    // `{a => (1,2), b => 3..4}.invert` is `(1 => "a", 2 => "a", 3 => "b",
    // 4 => "b").Seq`. This is the same "receiver decomposition vs element
    // flattening" distinction `value_to_list_for_receiver` draws.
    let value = &value.clone().deitemize_element();
    let values = match value.view() {
        ValueView::Str(_) => vec![value.clone()],
        _ if should_expand_invert_value(value) => crate::runtime::utils::value_to_list(value),
        _ => vec![value.clone()],
    };
    for item in values {
        out.push(make_inverted_pair(item, key.clone()));
    }
}

/// Invert one list element for `.invert`: a `Pair`/`ValuePair` contributes its
/// `value => key` entries (a list value expands to one entry per member).
/// Returns `false` for a non-pair element so the caller can reject the whole
/// list (raku throws `X::TypeCheck` for `(1, 2).invert`).
fn extend_inverted_pairs_from_element(out: &mut Vec<Value>, item: &Value) -> bool {
    match item.view() {
        ValueView::Pair(key, value) => {
            extend_inverted_pairs(out, Value::str(key.clone()), &value.clone());
            true
        }
        ValueView::ValuePair(key, value) => {
            extend_inverted_pairs(out, key.clone(), &value.clone());
            true
        }
        _ => false,
    }
}

pub(crate) fn invert_value(target: &Value) -> Option<Value> {
    let mut result = Vec::new();
    match target.view() {
        ValueView::Hash(items) => {
            for (k, v) in items.iter() {
                let orig_key = crate::runtime::utils::hash_typed_key(target, k);
                extend_inverted_pairs(&mut result, orig_key, v);
            }
        }
        ValueView::Bag(items, _) => {
            for (k, count) in items.iter() {
                result.push(make_inverted_pair(
                    Value::from_bigint(count.clone()),
                    items.typed_key(k),
                ));
            }
        }
        ValueView::Set(items, _) => {
            for k in items.iter() {
                result.push(make_inverted_pair(Value::TRUE, items.typed_key(k)));
            }
        }
        ValueView::Mix(items, _) => {
            for (k, weight) in items.iter() {
                result.push(make_inverted_pair(
                    crate::value::mix_weight_to_value(weight.clone()),
                    items.typed_key(k),
                ));
            }
        }
        // Instance types that compose Baggy: delegate to internal bag data
        ValueView::Instance { attributes, .. } => {
            if let Some(bag_data) = attributes.as_map().get(crate::symbol::wk::baggy_data()) {
                return invert_value(bag_data);
            }
            return None;
        }
        ValueView::Pair(key, value) => {
            extend_inverted_pairs(&mut result, Value::str(key.clone()), value);
        }
        ValueView::ValuePair(key, value) => {
            extend_inverted_pairs(&mut result, key.clone(), value);
        }
        ValueView::Array(items, _) => {
            // Iterate the array's own elements directly, NOT via `value_to_list`:
            // a List held in a `$` scalar (`my $l = List.new(...)`; the documented
            // `.invert` example) is an *itemized* Array, and `value_to_list` treats
            // an itemized array as a single element — which made `$l.invert` see one
            // `Array`, not its `Pair`s, and throw `X::TypeCheck`. Its elements are
            // still the pairs regardless of the itemization flag.
            for item in items.iter() {
                if !extend_inverted_pairs_from_element(&mut result, item) {
                    return None;
                }
            }
        }
        ValueView::Seq(_) => {
            for item in crate::runtime::utils::value_to_list(target) {
                if !extend_inverted_pairs_from_element(&mut result, &item) {
                    return None;
                }
            }
        }
        ValueView::Slip(_) => {
            for item in crate::runtime::utils::value_to_list(target) {
                if !extend_inverted_pairs_from_element(&mut result, &item) {
                    return None;
                }
            }
        }
        _ => return None,
    }
    Some(Value::seq(result))
}

/// The lazy Seq of every combination of `items` whose size lies in
/// `k_min..=k_max` (clamped to `0..=items.len()`), in Rakudo's order.
// Cost: O(1) beyond the `items` snapshot; O(k) per combination of size k pulled.
pub(crate) fn combinations_seq(items: Vec<Value>, k_min: i64, k_max: i64) -> Value {
    Value::seq_list_gen(ListGen::combinations(items, k_min, k_max), false)
}

/// The lazy Seq of every permutation of `items`, in Rakudo's order.
// Cost: O(e), e = elements of `items`; O(e) per permutation pulled.
pub(crate) fn permutations_seq(items: Vec<Value>) -> Value {
    Value::seq_list_gen(ListGen::permutations(items), false)
}

/// Add two values with junction threading support.
/// Used by `.sum` to correctly fold arrays containing junctions.
pub(crate) fn add_with_junction_threading(
    left: Value,
    right: Value,
) -> Result<Value, RuntimeError> {
    if let ValueView::Junction { kind, values } = left.view() {
        let results: Result<Vec<Value>, RuntimeError> = values
            .iter()
            .cloned()
            .map(|v| add_with_junction_threading(v, right.clone()))
            .collect();
        return Ok(Value::junction(kind, results?));
    }
    if let ValueView::Junction { kind, values } = right.view() {
        let results: Result<Vec<Value>, RuntimeError> = values
            .iter()
            .cloned()
            .map(|v| add_with_junction_threading(left.clone(), v))
            .collect();
        return Ok(Value::junction(kind, results?));
    }
    crate::builtins::arith_add(left, right)
}
