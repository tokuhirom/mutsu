//! The native `Hash.STORE` / `Array.STORE` methods: (re-)initialize a
//! container from a list of values, replacing its contents IN PLACE.
//!
//! The container's backing `Gc` node is shared by every alias of it (the
//! variable, a `\param`, a `Mixin` wrapping it), so storing into that node
//! is what keeps a role mixed into the container — `%h does R`, the
//! `is restricted` trait of `Hash::Restricted` — attached across a
//! reinitialization, and what lets a role's own `STORE` reach the native
//! one through `callsame`.

use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// Store `values` into the hash node `gc`, the way `%h = values` builds
    /// its contents: Pairs, alternating keys and values, or a flattened hash,
    /// with `X::Hash::Store::OddNumber` for a dangling key. The declared key
    /// and value constraints and the default stay on the node.
    // Cost: O(n), n = number of values stored.
    pub(crate) fn store_into_hash(
        &mut self,
        gc: &crate::gc::Gc<crate::value::HashData>,
        values: &Value,
    ) -> Result<Value, RuntimeError> {
        let items = crate::runtime::utils::value_to_list(values);
        let built = self.build_hash_from_items_warning(items)?;
        let ValueView::Hash(new_gc) = built.view() else {
            return Ok(built);
        };
        Ok(Self::hash_inplace_reassign_inheriting_meta(gc, &new_gc))
    }

    /// `Hash.STORE` on a hash whose mixed-in role declares `STORE_AT_KEY`:
    /// the node is emptied and every pair is stored through that method (which
    /// may `nextwith` into the native one), as Rakudo's `Hash.STORE` does.
    // Cost: O(n) plus the role's STORE_AT_KEY per pair, n = number of values stored.
    fn store_into_hash_via_store_at_key(
        &mut self,
        gc: &crate::gc::Gc<crate::value::HashData>,
        target: &Value,
        values: &Value,
    ) -> Result<Value, RuntimeError> {
        let items = crate::runtime::utils::value_to_list(values);
        let built = self.build_hash_from_items_warning(items)?;
        let ValueView::Hash(new_gc) = built.view() else {
            return Ok(built);
        };
        let pairs: Vec<(String, Value)> = new_gc
            .iter()
            .map(|(k, v)| (k.to_string(), v.clone()))
            .collect();
        let empty = crate::gc::Gc::new(crate::value::HashData::default());
        Self::hash_inplace_reassign_inheriting_meta(gc, &empty);
        for (key, value) in pairs {
            self.call_method_with_values(target.clone(), "STORE_AT_KEY", vec![Value::str(key), value])?;
        }
        Ok(target.clone())
    }

    /// Store `values` into the array node `gc`, keeping its kind and declared
    /// element metadata.
    // Cost: O(n), n = number of values stored.
    pub(crate) fn store_into_array(
        gc: &crate::gc::Gc<crate::value::ArrayData>,
        kind: crate::value::ArrayKind,
        values: &Value,
    ) -> Value {
        let items = crate::runtime::utils::value_to_list(values);
        let new_gc = crate::gc::Gc::new(crate::value::ArrayData::new(items));
        Self::array_inplace_reassign_inheriting_meta(gc, &new_gc, kind)
    }

    /// `STORE` on what the method-table rows cannot name from the value alone:
    /// a user `is Hash`/`is Map` instance (its backing storage) and a `Mixin`
    /// over a native Hash or Array. The storage is the row's receiver
    /// (`Hash.STORE`, `Array.STORE`, `Map.STORE`, ADR-11276 §9.174); the answer
    /// is the invocant. `None` for any other invocant.
    // Cost: O(n), n = number of values stored.
    pub(crate) fn native_container_store(
        &mut self,
        invocant: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let target = invocant.deref_container();
        // A user `is Map`/`is Hash` instance keeps its entries in a backing
        // hash; its native `STORE` (what a user `STORE` reaches through
        // `Map.^lookup('STORE')`) stores there and answers the instance.
        if let ValueView::Instance { attributes, .. } = target.view()
            && let Some(mut storage) = attributes.as_map().get(crate::symbol::wk::hash_storage()).cloned()
            && matches!(storage.view(), ValueView::Hash(..))
        {
            let stored = self.native_array_storage_mut(&mut storage, "STORE", args)?;
            return Some(stored.map(|_| target.clone()));
        }
        let ValueView::Mixin(inner, _) = target.view() else {
            return None;
        };
        let mut storage = inner.as_ref().clone();
        if matches!(storage.view(), ValueView::Hash(..))
            && self.mixin_composes_method(&target, "STORE_AT_KEY")
        {
            let values = args
                .iter()
                .find(|a| {
                    !matches!(a.view(), ValueView::Pair(k, _) if matches!(k.as_str(), "INITIALIZE" | "DECONT"))
                })
                .cloned()
                .unwrap_or(Value::NIL);
            let ValueView::Hash(gc) = storage.view() else {
                return None;
            };
            return Some(self.store_into_hash_via_store_at_key(&gc, &target, &values));
        }
        let stored = self.native_array_storage_mut(&mut storage, "STORE", args)?;
        Some(stored.map(|_| target.clone()))
    }
}
