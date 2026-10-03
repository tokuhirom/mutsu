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

    /// `STORE` on a native Hash or Array (or a `Mixin` over one): store into
    /// its node and hand back the invocant. `None` for any other invocant.
    // Cost: O(n), n = number of values stored.
    pub(crate) fn native_container_store(
        &mut self,
        invocant: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let values = args
            .iter()
            .find(|a| !matches!(a.view(), ValueView::Pair(k, _) if k == "INITIALIZE"))
            .cloned()
            .unwrap_or(Value::NIL);
        let target = invocant.deref_container();
        let inner = match target.view() {
            ValueView::Mixin(inner, _) => inner.as_ref().clone(),
            _ => target.clone(),
        };
        match inner.view() {
            ValueView::Hash(gc) => Some(self.store_into_hash(&gc, &values).map(|_| target)),
            ValueView::Array(gc, kind) if !kind.is_immutable_list() => {
                Self::store_into_array(&gc, kind, &values);
                Some(Ok(target))
            }
            _ => None,
        }
    }
}
