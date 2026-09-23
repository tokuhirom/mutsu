use super::*;
use crate::value::ValueView;

impl Interpreter {
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
    // Cost: O(e) for an `is Array` instance, e = its elements (the storage
    // array is copied per store); O(1) amortized for an `is Hash` instance.
    // Rakudo: O(1) -- see #9157.
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
                let (mut items, kind) = match storage.view() {
                    ValueView::Array(items, kind) => ((**items).clone(), kind),
                    _ => (
                        crate::value::ArrayData::new(Vec::new()),
                        crate::value::ArrayKind::Array,
                    ),
                };
                if i >= items.items().len() {
                    items
                        .items_mut()
                        .resize(i + 1, Value::package(crate::symbol::wk::any()));
                }
                Value::assign_element_slot(&mut items.items_mut()[i], value.clone());
                *storage = Value::array_with_kind(crate::gc::Gc::new(items), kind);
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
