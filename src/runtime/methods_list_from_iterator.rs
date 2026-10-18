//! `List.from-iterator` / `Array.from-iterator`, the body of their rows
//! (ADR-11276 §9.164).

use super::*;

impl Interpreter {
    /// Build the container `container` names (`List`, `Array`, or `Slip`,
    /// which inherits `List`'s) by draining a Raku `Iterator`. Used by custom
    /// `does Iterable` types (Hash::Agnostic's
    /// `method List { List.from-iterator(self.iterator) }`).
    ///
    /// mutsu's `.iterator` yields a materialized `Iterator` instance whose
    /// `items`/`index` attributes hold the remaining elements; `pull-one`
    /// advances by writing the bumped index back to the ITERATOR'S VARIABLE, so
    /// driving `pull-one` on this temporary (unnamed) value would never advance
    /// and would loop forever. The elements are read straight out of the
    /// instance instead; any other value takes the generic list coercion.
    // Cost: O(n), n = elements the iterator yields (one `pull-one` call each
    // for a user iterator).
    pub(crate) fn list_from_iterator(
        &mut self,
        container: &str,
        iterator: &Value,
    ) -> Result<Value, RuntimeError> {
        let user_iterator = matches!(iterator.view(), ValueView::Instance { class_name, .. }
            if class_name != "Iterator" && self.class_does_role(&class_name.resolve(), "Iterator"));
        let items: Vec<Value> = if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = iterator.view()
            && class_name == "Iterator"
        {
            if let Some(rest) = self.map_grep_stream_drain(&attributes) {
                rest?
            } else {
                self.iterator_remaining_items(&attributes.as_map())?
            }
        } else if user_iterator {
            // A user-defined `does Iterator` instance: drive its `pull-one`
            // until IterationEnd. Unlike the built-in `Iterator` (whose index
            // write-back needs a named variable), a user iterator persists its
            // own attribute mutations across `pull-one` calls, so driving the
            // temporary directly reifies the elements the class produces.
            let mut pulled = Vec::new();
            loop {
                let val = self.call_method_with_values(iterator.clone(), "pull-one", vec![])?;
                if val.is_iteration_end() {
                    break;
                }
                pulled.push(val);
            }
            pulled
        } else {
            crate::runtime::utils::value_to_list(iterator)
        };
        Ok(match container {
            "Slip" => Value::slip_arc(std::sync::Arc::new(items)),
            "Array" => Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(items)),
                crate::value::ArrayKind::Array,
            ),
            _ => Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(items)),
                crate::value::ArrayKind::List,
            ),
        })
    }

    /// The elements a user `Iterable` instance's own `iterator` yields, as a
    /// `List` (`Iterable.hyper`/`race`/`lazy` read them so, #12929).
    // Cost: O(n), n = elements yielded (one `pull-one` call each).
    pub(crate) fn list_via_user_iterator(&mut self, target: &Value) -> Result<Value, RuntimeError> {
        let iterator = self.call_method_with_values(target.clone(), "iterator", Vec::new())?;
        let mut items = Vec::new();
        loop {
            let item = self.call_method_with_values(iterator.clone(), "pull-one", Vec::new())?;
            if item.is_iteration_end() {
                break;
            }
            items.push(item);
        }
        Ok(Value::array_with_kind(
            crate::gc::Gc::new(crate::value::ArrayData::new(items)),
            crate::value::ArrayKind::List,
        ))
    }
}
