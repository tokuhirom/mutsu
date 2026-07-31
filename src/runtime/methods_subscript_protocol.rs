//! Value-level subscript protocol (`DELETE-KEY` / `DELETE-POS` / `EXISTS-POS`)
//! — the raku methods called on a container *value* rather than on a named
//! variable.
//!
//! mutsu implements `%h<k>:delete` / `@a[i]:delete` directly in the delete
//! opcode, which resolves its container by variable name, so the delete
//! protocol existed only on that name-keyed path (`vm_call_method_mut_ops`). A
//! container reached through anything else found no method at all — notably the
//! inner container of a `%h but R` mixin, which is what `:delete` on a mixin
//! dispatches through once a composed role supplies no protocol method itself.
//!
//! Both deletes here mutate the container *through its shared backing node*, so
//! every holder of the same container observes the change — including the
//! `Arc<Value>` a `Mixin` wraps, which no env-scanning writeback can reach.

use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// `%h.DELETE-KEY($key)` on a hash value: remove the entry and return the
    /// value it held, or the hash's `is default(...)` value (else its element
    /// type object) when the key was absent.
    pub(super) fn hash_delete_key_value(&mut self, target: &Value, key_arg: &Value) -> Value {
        let ValueView::Hash(map) = target.view() else {
            return Value::NIL;
        };
        // An object hash stores `.WHICH` keys; a plain hash the stringified key.
        let key = if map.key_type.is_some() {
            crate::runtime::utils::value_which_key(key_arg)
        } else {
            key_arg.to_string_value()
        };
        let old = if map.contains_key(&key) {
            self.resolve_hash_entry(&map, &key)
        } else {
            map.default.as_deref().cloned().unwrap_or_else(|| {
                Value::package(Symbol::intern(map.value_type.as_deref().unwrap_or("Any")))
            })
        };
        let mut container = target.clone();
        container.with_hash_mut(|gc| {
            let data = crate::value::gc_data_mut(gc);
            data.remove(&key);
            if let Some(original) = data.original_keys.as_mut() {
                original.remove(&key);
            }
        });
        old
    }

    /// `@a.DELETE-POS($index)` on an array value: leave a hole at `$index` and
    /// return what it held. Trailing holes are then trimmed, so deleting the
    /// last element shortens the array (`[1,2,3].DELETE-POS(2)` leaves
    /// `[1, 2]`) — the same trimming the `:delete` opcode performs. An
    /// explicitly-assigned type object is not a hole, so `[1, 2, Any]` keeps
    /// its length, matching raku.
    pub(super) fn array_delete_pos_value(&mut self, target: &Value, index: usize) -> Value {
        let mut container = target.clone();
        let deleted = container.with_array_mut(|gc, _| {
            let data = crate::value::gc_data_mut(gc);
            if index >= data.items.len() {
                return Value::NIL;
            }
            let old = std::mem::replace(&mut data.items[index], Value::NIL);
            if let Some(initialized) = data.initialized.as_mut() {
                initialized.remove(&index);
            }
            while !data.items.is_empty() && data.hole_at(data.items.len() - 1) {
                let last = data.items.len() - 1;
                data.items.pop();
                if let Some(set) = data.initialized.as_mut() {
                    set.remove(&last);
                }
            }
            match old.view() {
                ValueView::Scalar(inner) => inner.clone(),
                ValueView::ContainerRef(cell) => cell.lock().unwrap().clone(),
                _ => old.clone(),
            }
        });
        deleted.unwrap_or(Value::NIL)
    }
}
