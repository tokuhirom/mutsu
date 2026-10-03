//! Storing new contents into an existing QuantHash container in place.
//!
//! `%s = <a b>` on a `my %s is SetHash`, and `$obj!values = ...` through a
//! `method !values is rw { %!values }` over a `has %!values is BagHash`, are
//! both a STORE into a container that already exists: every holder of that
//! container (the variable's env mirror, a `:=` bind, the object's attribute
//! slot) must observe the new contents. So the coerced contents are written
//! into the existing backing node, which keeps its own type metadata.

use super::*;

impl Value {
    /// Write `coerced` into this `Set`/`Bag`/`Mix` container's backing node
    /// when both are the same QuantHash kind, returning this container (with
    /// its own mutability) holding the new contents. `None` when the kinds
    /// differ or `coerced` already is this node.
    // Cost: O(n), n = elements of `coerced` (one clone of its data).
    pub(crate) fn store_quanthash_in_place(&self, coerced: &Value) -> Option<Value> {
        match (self.view(), coerced.view()) {
            (ValueView::Set(old, mutable), ValueView::Set(new, _)) if !Gc::ptr_eq(&old, &new) => {
                let mut data = (**new).clone();
                data.value_type = old.value_type.clone();
                data.key_type = old.key_type.clone();
                data.declared_type = old.declared_type.clone();
                // SAFETY: audited aliased in-place container write; see
                // `value::aliased_mut` (no other borrow live, single write).
                unsafe {
                    *crate::value::gc_contents_mut(&old) = data;
                }
                Some(Value::set_parts(old.clone(), mutable))
            }
            (ValueView::Bag(old, mutable), ValueView::Bag(new, _)) if !Gc::ptr_eq(&old, &new) => {
                let mut data = (**new).clone();
                data.value_type = old.value_type.clone();
                data.key_type = old.key_type.clone();
                data.declared_type = old.declared_type.clone();
                // SAFETY: as above.
                unsafe {
                    *crate::value::gc_contents_mut(&old) = data;
                }
                Some(Value::bag_parts(old.clone(), mutable))
            }
            (ValueView::Mix(old, mutable), ValueView::Mix(new, _)) if !Gc::ptr_eq(&old, &new) => {
                let mut data = (**new).clone();
                data.value_type = old.value_type.clone();
                data.key_type = old.key_type.clone();
                data.declared_type = old.declared_type.clone();
                // SAFETY: as above.
                unsafe {
                    *crate::value::gc_contents_mut(&old) = data;
                }
                Some(Value::mix_parts(old.clone(), mutable))
            }
            _ => None,
        }
    }
}
