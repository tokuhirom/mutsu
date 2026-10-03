//! Autovivification of a typed QuantHash scalar on its first element store:
//! `my MixHash $m; $m{$k} = $w` turns the `MixHash` type object into a
//! one-entry `MixHash`.
//!
//! The store key is the element's `.WHICH` string, so the element object
//! itself is recorded alongside it (as every later store into the live
//! container does); otherwise the first key of a non-`Str` element (a List,
//! a Bool, an object) would read back from `.keys` as its `.WHICH` string.
use super::*;
use crate::value::ValueMap;

impl Interpreter {
    /// The one-entry QuantHash `type_name` (`MixHash`, `BagHash` or
    /// `SetHash`) holding `elem` under the store key `key` with the assigned
    /// `val`. A zero weight/count or a false membership yields the empty
    /// container, as an assignment into an existing one removes the key.
    // Cost: O(k), k = key length (one map insert of the key and its object).
    pub(super) fn autoviv_quanthash_with_key(
        type_name: &str,
        key: &str,
        elem: &Value,
        val: &Value,
    ) -> Result<Value, RuntimeError> {
        let mut originals = ValueMap::default();
        crate::runtime::utils::record_quanthash_original(
            &mut originals,
            key,
            &elem.deref_container(),
        );
        Ok(match type_name {
            "MixHash" => {
                let mut weights = std::collections::HashMap::new();
                let weight = Self::mix_assignment_weight(val)?;
                if weight != 0.0 {
                    weights.insert(key.to_string(), weight);
                }
                Value::mix_hash_with_original_keys(weights, originals)
            }
            "BagHash" => {
                let mut counts = std::collections::HashMap::new();
                let count = Self::bag_assignment_count(val)?;
                if num_traits::Signed::is_positive(&count) {
                    counts.insert(key.to_string(), count);
                }
                let mut bag = Value::bag_typed_big(counts, originals);
                let _ = bag.with_bag_mut(|_, m| *m = true);
                bag
            }
            "SetHash" => {
                let mut items = std::collections::HashSet::new();
                if val.truthy() {
                    items.insert(key.to_string());
                }
                Value::set_hash_typed(items, originals)
            }
            other => unreachable!("not a mutable QuantHash type: {other}"),
        })
    }

    /// The empty mutable QuantHash `type_name` (`MixHash`, `BagHash` or
    /// `SetHash`): what a typed scalar holding the type object becomes before
    /// a slice store distributes its keys into it.
    // Cost: O(1).
    pub(super) fn empty_mutable_quanthash(type_name: &str) -> Value {
        match type_name {
            "MixHash" => Value::mix_hash(std::collections::HashMap::new()),
            "BagHash" => {
                let mut bag = Value::bag_typed_big(Default::default(), ValueMap::default());
                let _ = bag.with_bag_mut(|_, m| *m = true);
                bag
            }
            "SetHash" => Value::set_hash_typed(Default::default(), ValueMap::default()),
            other => unreachable!("not a mutable QuantHash type: {other}"),
        }
    }
}
