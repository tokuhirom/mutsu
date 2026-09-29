use super::*;

impl Interpreter {
    /// The element (and, for a hash, key) type a `:=`-bound `@`/`%` container
    /// carries, in the format [`Interpreter::set_var_type_constraint`] expects
    /// (`"ValueType"` or `"ValueType{KeyType}"`), or `None` when it is untyped.
    // Cost: O(1).
    pub(crate) fn bound_container_constraint(&self, name: &str, value: &Value) -> Option<String> {
        let info = self.container_type_metadata(value)?;
        if info.value_type.is_empty() {
            return None;
        }
        Some(match info.key_type {
            Some(kt) if name.starts_with('%') => format!("{}{{{}}}", info.value_type, kt),
            _ => info.value_type,
        })
    }

    /// A `:=` bind of an `@`/`%` variable reached BY NAME (`SetGlobal`: a
    /// closure or nested routine rebinding a captured free variable).
    ///
    /// Checks the RHS against the variable's declared element type, then makes
    /// the bound container's own element type the one element operations
    /// enforce — see `runtime_var_bind_meta` for why the two are kept apart.
    /// QuantHash values keep their typing on the container alone (their ctor
    /// coerces keys), like the SetLocal bind path.
    // Cost: O(1).
    pub(crate) fn bind_container_by_name(
        &mut self,
        name: &str,
        value: &Value,
    ) -> Result<(), RuntimeError> {
        if name.starts_with('@') {
            self.check_array_bind_value_type(name, value)?;
        } else {
            self.check_hash_bind_value_type(name, value)?;
            if matches!(
                value.view(),
                ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..)
            ) {
                return Ok(());
            }
        }
        let constraint = self.bound_container_constraint(name, value);
        self.loan_env_for(|i| i.set_var_bound_type_constraint(name, constraint));
        Ok(())
    }
}
