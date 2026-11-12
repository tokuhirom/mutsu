//! Class-scoped attribute accessors (`my @.list`) against built-in method names.

use super::*;

impl Interpreter {
    /// Return the class-scoped accessor that wins the method race at its MRO
    /// level. The probe memo stores the owner; the value stays live in the
    /// registry so later assignments are visible without invalidating it.
    // Cost: O(1) amortized; the first probe after a registry write is O(d),
    // d = MRO depth.
    pub(crate) fn winning_class_scoped_accessor(
        &mut self,
        class_name: &str,
        method: crate::symbol::Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !self.registry().class_level_attr_names.contains(method.as_str()) {
            return None;
        }
        let Some(super::UserMethodOrAccessor::ClassScopedAccessor(owner)) =
            self.resolve_user_method_or_accessor_sym(class_name, method)
        else {
            return None;
        };
        if !args.is_empty() {
            return Some(Err(RuntimeError::new(
                super::types::arity_message::too_many_positionals(1, 1, args.len() + 1),
            )));
        }
        self.registry()
            .classes
            .get(owner.as_str())?
            .class_level_attrs
            .get(method.as_str())
            .cloned()
            .map(Ok)
    }

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
