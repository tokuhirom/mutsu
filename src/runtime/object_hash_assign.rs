//! Assigning a list of pairs to an object-hash attribute through its rw
//! accessor (`$o.hash = $o.hash.grep(...)`, `$o.hash .= grep: ...`).
//!
//! The accessor store builds the new hash from the pairs with the ordinary,
//! key-type-blind builder, which keys every entry by its stringified key. For
//! an object hash (`has %.hash{Any}`) that loses the key *objects*: a hash used
//! as a key came back as the string `"x\t1"`. This recovers them from the
//! pairs being assigned, so the result can be re-keyed by `.WHICH`.

use super::*;

/// The key objects of the pairs `value` assigns, keyed by the stringified key
/// the plain hash builder stores each entry under. A `Hash` item contributes
/// its own recorded key objects.
// Cost: O(n), n = pairs assigned.
pub(crate) fn assigned_pair_key_objects(value: &Value) -> ValueMap {
    let mut keys = ValueMap::default();
    let value = value.descalarize();
    let items: Vec<Value> = match value.view() {
        ValueView::Array(..) | ValueView::Seq(_) | ValueView::Slip(_) => {
            crate::runtime::utils::value_to_list(value)
        }
        _ => vec![value.clone()],
    };
    for item in &items {
        match item.descalarize().view() {
            ValueView::ValuePair(key, _) => {
                let key = key.descalarize().clone();
                keys.insert(key.to_string_value(), key);
            }
            ValueView::Hash(map) => {
                if let Some(orig) = &map.original_keys {
                    for (k, obj) in orig.iter() {
                        keys.insert(k.clone(), obj.clone());
                    }
                }
            }
            _ => {}
        }
    }
    keys
}

/// Turn the plain hash an accessor store built into an object hash with the
/// target's `key_type`, keyed by the `.WHICH` of the assigned key objects.
// Cost: O(n), n = entries.
pub(crate) fn retag_assigned_object_hash(result: Value, source: &Value, key_type: &str) -> Value {
    let mut result = result;
    let key_objects = assigned_pair_key_objects(source);
    result.with_hash_mut(|arc| {
        let data = crate::gc::Gc::make_mut(arc);
        let orig = data.original_keys.get_or_insert_with(ValueMap::default);
        for (k, obj) in key_objects {
            if data.map.contains_key(&k) {
                orig.insert(k, obj);
            }
        }
    });
    crate::runtime::utils::into_object_hash(result, key_type)
}
