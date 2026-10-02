use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// Mutate the `ArrayData` behind an `is Array` instance's
    /// `__mutsu_array_storage` attribute value. The node is written in place
    /// when this attribute is its only holder and copied first otherwise
    /// (`Gc::make_mut`), so a store never leaks into another holder of the
    /// storage array -- the same visibility the per-store copy it replaces
    /// gave, without copying a singly-owned storage array on every store
    /// (#9157). A non-Array `storage` starts as an empty Array, so `f` always
    /// runs (the `Option` only mirrors `Value::with_array_mut`).
    // Cost: O(1) when the storage node is singly owned; O(e), e = its elements,
    // when it is shared (the one copy that detaches it).
    pub(crate) fn with_array_storage_mut<R>(
        storage: &mut Value,
        f: impl FnOnce(&mut crate::value::ArrayData) -> R,
    ) -> Option<R> {
        if !matches!(storage.view(), ValueView::Array(..)) {
            *storage = Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(Vec::new())),
                crate::value::ArrayKind::Array,
            );
        }
        storage.with_array_mut(|gc, _| f(gc.make_mut()))
    }

    /// Store one element into an `is Array` / `is Hash` (or `is List` / `is Map`)
    /// subclass instance that a method accessor handed back (`$obj.self[1] = 1`,
    /// `$o.arr[0] = 7` where `.arr` holds such an object).
    ///
    /// The element lives in the instance's `__mutsu_array_storage` /
    /// `__mutsu_hash_storage` attribute, and the instance shares its attribute
    /// cell with every alias of the object, so the write needs no setter
    /// write-back -- the same in-place rule the computed-target store
    /// (`exec_index_assign_generic_op`) applies to this shape. Without it the
    /// accessor-lvalue route fell through to "call the accessor as a setter",
    /// which died for a method with no setter (`.self`: "No matching candidates
    /// for method: self") and silently dropped the store for a read-only
    /// attribute accessor.
    ///
    /// Only a single index/key is handled; returns `false` (nothing stored)
    /// for any other shape so the caller's general path takes over.
    // Cost: O(1) amortized for an `is Array` or `is Hash` instance.
    pub(super) fn store_into_storage_instance_element(
        current: &Value,
        index: &Value,
        value: &Value,
    ) -> bool {
        let ValueView::Instance { attributes, .. } = current.view() else {
            return false;
        };
        let single = match index.view() {
            ValueView::Array(items, _) if items.len() == 1 => items[0].clone(),
            ValueView::Seq(items) if items.len() == 1 => items[0].clone(),
            ValueView::Slip(items) if items.len() == 1 => items[0].clone(),
            ValueView::Array(..) | ValueView::Seq(_) | ValueView::Slip(_) => return false,
            _ => index.clone(),
        };
        if attributes.contains_key("__mutsu_array_storage") {
            let Ok(i) = single.to_string_value().parse::<usize>() else {
                return false;
            };
            attributes.with_attr_mut("__mutsu_array_storage", |storage| {
                Self::with_array_storage_mut(storage, |items| {
                    if i >= items.items().len() {
                        items.resize(i + 1, Value::package(crate::symbol::wk::any()));
                    }
                    Value::assign_element_slot(&mut items.live_mut()[i], value.clone());
                });
            });
            return true;
        }
        if attributes.contains_key("__mutsu_hash_storage") {
            let key = single.to_string_value();
            let stored = attributes
                .with_attr_mut("__mutsu_hash_storage", |storage| {
                    storage.with_hash_mut(|gc| {
                        Value::hash_insert_through(
                            &mut crate::value::gc_data_mut(gc).map,
                            key,
                            value.clone(),
                        );
                    })
                })
                .flatten();
            return stored.is_some();
        }
        false
    }
}
