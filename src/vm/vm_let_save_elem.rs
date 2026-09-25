//! Element saves for `let`/`temp` (`temp @a[i]`, `temp $t[1]<k>[1]`): the
//! `LetSaveElem` / `LetSaveElemVivified` opcodes (#9434).

use super::*;

impl Interpreter {
    /// `LetSaveElem` / `LetSaveElemVivified`: save the element `container[key]`
    /// (every element of a slice key), to be written back into it in place at
    /// scope exit. Rakudo temporizes the element container the same way, so
    /// the rest of the container -- and anything else bound to a part of it --
    /// is left alone (#9434). `vivified` is the second half of a save whose
    /// container did not exist yet: the temporizing assignment just created
    /// the element, so it is saved as holding `Any`.
    // Cost: O(1) for one key; O(k) for a slice, k = its keys.
    pub(super) fn exec_let_save_elem_op(
        &mut self,
        is_temp: bool,
        is_positional: bool,
        vivified: bool,
    ) {
        let key = self.stack.pop().unwrap_or(Value::NIL);
        let raw_container = self.stack.pop().unwrap_or(Value::NIL);
        let container = raw_container.deref_container().descalarize().clone();
        let is_container = matches!(container.view(), ValueView::Array(..) | ValueView::Hash(_));
        if !is_container {
            // Nothing to address yet: ask the compiled code to save the element
            // again once the assignment has vivified the path. The vivified
            // pass on a still-missing container (a `temp` with no assignment)
            // has nothing to restore.
            if !vivified {
                self.stack.push(Value::TRUE);
            }
            return;
        }
        let key = if is_positional && matches!(container.view(), ValueView::Array(..)) {
            self.resolve_whatever_index_for_target(key, Some(&container))
        } else {
            key
        };
        let keys: Vec<Value> = match key.view() {
            ValueView::Array(items, ..) => items.items().to_vec(),
            ValueView::Seq(_) | ValueView::Slip(_) => Self::value_to_list(&key),
            _ if key.is_range() => Self::value_to_list(&key),
            _ => vec![key.clone()],
        };
        for key in keys {
            let old = if vivified {
                Value::package(crate::symbol::wk::any())
            } else {
                Self::element_value_for_save(&container, &key)
            };
            self.let_saves_push_elem(container.clone(), key, old, is_temp);
        }
        if !vivified {
            self.stack.push(Value::FALSE);
        }
    }

    /// The value the element `container[key]` holds, decontainerized (the
    /// restore writes it back THROUGH the element's container), or `Any` for a
    /// missing element.
    fn element_value_for_save(container: &Value, key: &Value) -> Value {
        let found = match container.view() {
            ValueView::Array(items, _) => {
                Self::index_to_usize(key).and_then(|i| items.items().get(i).cloned())
            }
            ValueView::Hash(map) => map.map.get(&Value::hash_key_encode(key)).cloned(),
            _ => None,
        };
        match found {
            Some(v) => v.deref_container(),
            None => Value::package(crate::symbol::wk::any()),
        }
    }
}
