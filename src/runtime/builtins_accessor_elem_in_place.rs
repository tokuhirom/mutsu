//! `$obj.attr<k> = v` / `$obj.attr[i] = v` stored in place when the accessor
//! handed back the attribute's own container.

use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// Store `value` at `index` of `current`, in place, when `current` is the
    /// very `Hash`/`Array` the instance `target` holds in its `method`
    /// attribute. Returns whether it stored.
    ///
    /// The attribute is one container in raku, shared by everything bound to
    /// it (`my $t := $a.u`), so an element write through the accessor lands in
    /// it. Rebuilding the container and rebinding its holders by identity
    /// (the general path in `builtin_index_assign_method_lvalue`) reaches the
    /// env and other instances, but not a `:=` alias in a local slot, which
    /// kept the old container (#10897).
    ///
    /// Anything else -- an accessor returning a copy, a computed container, an
    /// argumented accessor -- is left to that general path.
    // Cost: O(1) expected, plus the attribute lookup.
    pub(super) fn store_accessor_element_in_place(
        &mut self,
        target: &Value,
        method: &str,
        current: &Value,
        index: &Value,
        value: &Value,
    ) -> Result<bool, RuntimeError> {
        let ValueView::Instance { attributes, .. } = target.view() else {
            return Ok(false);
        };
        let held = {
            let map = attributes.as_map();
            let Some(slot) = map.get(method) else {
                return Ok(false);
            };
            Self::deref_lvalue_value(slot.clone())
        };
        // An element is a Scalar and cannot hold `Nil`: it takes the
        // container's own default (ADR-0049), as `decay_nil_container_elements`
        // gives the general path's rebuilt container.
        let value = if value.is_nil() {
            let default = self.typed_container_default(current);
            if default.is_nil() {
                value.clone()
            } else {
                default
            }
        } else {
            value.clone()
        };
        let value = &value;
        match (current.view(), held.view()) {
            (ValueView::Hash(cur), ValueView::Hash(attr)) if crate::gc::Gc::ptr_eq(&cur, &attr) => {
                if cur.key_type.is_some() {
                    let which = crate::runtime::utils::value_which_key(index);
                    current.hash_record_original_key(&which, index);
                    // SAFETY: aliased in-place mutation of a shared container;
                    // see `gc_contents_mut`. No borrow into the map is live.
                    let data = unsafe { crate::value::gc_contents_mut(&cur) };
                    Value::hash_insert_through(&mut data.map, which, value.clone());
                } else {
                    // The same key the general path stores under: a bare type
                    // object coerces to "" with its warning.
                    let key = if matches!(index.view(), ValueView::Package(_)) {
                        self.coerce_type_object_hash_key(index)?
                    } else {
                        index.to_string_value()
                    };
                    let data = unsafe { crate::value::gc_contents_mut(&cur) };
                    Value::hash_insert_through(&mut data.map, key, value.clone());
                }
                Ok(true)
            }
            (ValueView::Array(cur, _), ValueView::Array(attr, _))
                if crate::gc::Gc::ptr_eq(&cur, &attr) =>
            {
                let Ok(idx) = usize::try_from(crate::runtime::to_int(index)) else {
                    return Ok(false);
                };
                if idx >= cur.len() {
                    // A shaped array does not grow; its bounds error is the
                    // general path's.
                    if crate::runtime::utils::is_shaped_array(current) {
                        return Ok(false);
                    }
                    current.array_grow_to(idx);
                }
                Ok(current.array_set_in_place(idx, value.clone()))
            }
            _ => Ok(false),
        }
    }
}
