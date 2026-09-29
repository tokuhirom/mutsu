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

    /// The value an `@`/`%` ASSIGNMENT (not a bind) stores into `name`, with
    /// container type metadata still attached from a typed source dropped when
    /// `name` itself is untyped. The value may share its backing `Arc` with a
    /// typed source container (`my @a = @typed`), and an untyped variable must
    /// not present its value as typed. Attribute variables (`.h`, `!h`) are
    /// left alone: their element type comes from the class definition, not
    /// the by-name lane. Shared by the statement (`SetLocal`) and expression
    /// (`AssignExprLocal`, `True and @h = @typed`) store paths.
    // Cost: O(1) unless metadata is cleared, which is O(1) amortized
    // copy-on-write of the container header.
    pub(crate) fn untyped_container_assign_value(
        &mut self,
        name: &str,
        name_sym: Option<crate::symbol::Symbol>,
        val: Value,
    ) -> Value {
        if (name.starts_with('%') || name.starts_with('@'))
            && !name.contains('.')
            && !name.contains('!')
            && loan_env!(self, var_type_constraint_for(name, name_sym)).is_none()
            && self.container_type_metadata(&val).is_some()
        {
            return crate::runtime::Interpreter::clear_hash_type_metadata(val);
        }
        val
    }
}
