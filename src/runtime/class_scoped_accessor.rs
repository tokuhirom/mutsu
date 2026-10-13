//! Class-scoped attribute accessors (`my @.list`) against built-in method names.

use super::*;

impl Interpreter {
    /// Whether `method` called with `args` on `class_name` (its type object or
    /// an instance) is a class-scoped attribute's accessor (`my @.list`,
    /// `our $.elems`), which shadows the built-in method of the same name.
    /// A per-instance attribute's accessor of that name hides it instead.
    // Cost: O(d), d = MRO depth (two MRO walks with one hash probe per level).
    pub(crate) fn class_scoped_accessor_shadows(
        &mut self,
        class_name: &str,
        method: &str,
        args: &[Value],
    ) -> bool {
        args.is_empty()
            && self.has_class_level_attr(class_name, method)
            && !self.has_public_accessor(class_name, method)
    }

    /// The value an assignment through a class-scoped accessor stores
    /// (`C.list = ()` for `my @.list of F`): like the declaration's
    /// initializer, it is coerced to the attribute's sigil and keeps the
    /// declared element type, so the attribute stays an `Array[F]` rather
    /// than becoming the assigned `List`.
    // Cost: O(n), n = elements assigned (the sigil coercion and element checks).
    pub(crate) fn class_scoped_attr_store_value(
        &mut self,
        class_name: &str,
        attr_name: &str,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let Some(current) = self.get_class_level_attr(class_name, attr_name) else {
            return Ok(value);
        };
        let sigil = match current.view() {
            ValueView::Array(..) => '@',
            ValueView::Hash(..) => '%',
            _ => return Ok(value),
        };
        let coerced = Self::coerce_attr_value_by_sigil(value, sigil).detach_shared_container();
        let Some(meta) = self.container_type_metadata(&current) else {
            return Ok(coerced);
        };
        let elem_type = match &meta.key_type {
            Some(key) => format!("{}{{{}}}", meta.value_type, key),
            None => meta.value_type.clone(),
        };
        self.finalize_typed_container_attr(attr_name, sigil, &elem_type, coerced)
    }
}
