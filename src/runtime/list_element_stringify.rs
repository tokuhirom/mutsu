//! Stringifying a list whose elements define their own `Str`.
//!
//! `Value::to_string_value` is a pure renderer: it knows the list shape rules
//! (space-separated, nested lists flattened) but cannot call a user-defined
//! `Str` method, so an `Instance` element rendered as the `ClassName()`
//! fallback. `.join` had always resolved elements through the interpreter;
//! `.Str` / `.Stringy` / prefix `~` had not, so `~@a` and `@a.join("")`
//! disagreed for the same array.
//!
//! Rather than duplicate the shape rules in an interpreter-aware renderer,
//! the elements are resolved *in place* — each `Instance` replaced by the
//! `Str` its class dispatches — and the resulting list handed to the same
//! pure renderer.

use crate::value::{RuntimeError, Value, ValueView};

impl crate::Interpreter {
    /// Whether stringifying this value needs the interpreter: it is a list
    /// (at any nesting depth) holding an `Instance` element. Everything else
    /// renders correctly from the pure `to_string_value`, so the native fast
    /// path keeps it.
    pub(crate) fn list_str_needs_interpreter(value: &Value) -> bool {
        // The list may arrive itemized (`$[...]`, e.g. bound to a `Mu $got`
        // parameter) or behind a `ContainerRef` cell; stringification looks
        // through both.
        let value = value.deref_container();
        let value = value.descalarize();
        match value.view() {
            ValueView::Array(items, ..) => items.iter().any(Self::element_needs_interpreter),
            ValueView::Slip(items) => items.iter().any(Self::element_needs_interpreter),
            _ => false,
        }
    }

    fn element_needs_interpreter(item: &Value) -> bool {
        let item = item.deref_container();
        matches!(item.view(), ValueView::Instance { .. }) || Self::list_str_needs_interpreter(&item)
    }

    /// Replace every `Instance` element with the string its class's `Str`
    /// dispatches, recursing into nested lists. Non-list values, and lists
    /// with no such element, are returned unchanged.
    pub(crate) fn resolve_list_element_stringifiers(
        &mut self,
        value: &Value,
    ) -> Result<Value, RuntimeError> {
        if !Self::list_str_needs_interpreter(value) {
            return Ok(value.clone());
        }
        let value = value.deref_container();
        let value = value.descalarize();
        match value.view() {
            ValueView::Array(items, kind) => {
                let resolved = self.resolve_elements(items.iter())?;
                Ok(Value::array_with_kind(
                    crate::gc::Gc::new(crate::value::ArrayData::new(resolved)),
                    kind,
                ))
            }
            ValueView::Slip(items) => Ok(Value::slip(self.resolve_elements(items.iter())?)),
            _ => Ok(value.clone()),
        }
    }

    fn resolve_elements<'a>(
        &mut self,
        items: impl Iterator<Item = &'a Value>,
    ) -> Result<Vec<Value>, RuntimeError> {
        let mut out = Vec::new();
        for item in items {
            // Decontainerize a `ContainerRef` element (a `:=`-bound slot, a
            // grep rw alias) so a cell-wrapped Instance still gets its
            // user-defined `.Str` -- the same decont `.join` does.
            let item = item.deref_container();
            if matches!(item.view(), ValueView::Instance { .. }) {
                let s = self.call_method_with_values(item.clone(), "Str", vec![])?;
                out.push(Value::str(s.to_string_value()));
            } else if Self::list_str_needs_interpreter(&item) {
                out.push(self.resolve_list_element_stringifiers(&item)?);
            } else {
                out.push(item);
            }
        }
        Ok(out)
    }
}
