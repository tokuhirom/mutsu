//! Element stores into an `is Hash` / `is Array` subclass instance's backing
//! storage, for the nested subscript-assign paths (#12907): such an instance
//! is an existing container to descend into, never one to autovivify over.

use super::*;

impl Interpreter {
    /// Whether `v` is an `is Hash` / `is Array` subclass instance, whose
    /// subscripts address its backing storage.
    // Cost: O(1).
    pub(super) fn is_storage_backed_instance(v: &Value) -> bool {
        matches!(v.view(), ValueView::Instance { attributes, .. }
            if attributes.contains_key("__mutsu_hash_storage")
                || attributes.contains_key("__mutsu_array_storage"))
    }

    /// Store `val` at `key` of an `is Hash` / `is Array` subclass instance's
    /// backing storage, in place through its shared attribute cell: a hash
    /// key for an associative subscript, an index for a positional one. A
    /// `bind` installs `val` as the slot (a `:=` cell); otherwise it stores
    /// through any container already there. `false` when `target` has no such
    /// storage for this subscript (or a positional key is no index).
    // Cost: O(1) amortized (one storage insert; an index past the end extends
    // the array to it, O(i)).
    pub(super) fn store_into_storage_backed_instance(
        target: &Value,
        key: &str,
        positional: bool,
        val: Value,
        bind: bool,
    ) -> bool {
        let ValueView::Instance { attributes, .. } = target.view() else {
            return false;
        };
        if positional {
            let Ok(i) = key.parse::<usize>() else {
                return false;
            };
            if !attributes.contains_key("__mutsu_array_storage") {
                return false;
            }
            attributes.with_attr_mut("__mutsu_array_storage", |storage| {
                storage.with_array_mut(|gc, _| {
                    let arr = crate::value::gc_data_mut(gc);
                    while arr.len() <= i {
                        arr.push(Value::package(crate::symbol::wk::any()));
                    }
                    if bind {
                        arr[i] = val.clone();
                    } else {
                        Value::assign_element_slot(&mut arr[i], val.clone());
                    }
                });
            });
            return true;
        }
        if !attributes.contains_key("__mutsu_hash_storage") {
            return false;
        }
        attributes.with_attr_mut("__mutsu_hash_storage", |storage| {
            storage.with_hash_mut(|gc| {
                let hash = &mut crate::value::gc_data_mut(gc).map;
                if bind {
                    hash.insert(key.to_string(), val.clone());
                } else {
                    Value::hash_insert_through(hash, key.to_string(), val.clone());
                }
            });
        });
        true
    }
}
