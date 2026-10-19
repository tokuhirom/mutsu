use super::*;

impl Interpreter {
    /// The compiled method binder checks the original argument, then detaches
    /// its copy, just as the general binder does. An aggregate constraint
    /// describes the argument's container type, not each copied element.
    // Cost: O(n + c), n = copied container elements, c = parameter type check cost.
    pub(crate) fn checked_method_param_copy(
        &mut self,
        pd: &ParamDef,
        value: Value,
        predicate_verified: bool,
    ) -> Result<Value, RuntimeError> {
        let value = self.check_and_coerce_param_type(pd, value, None, None, predicate_verified)?;
        Ok(if pd.name.starts_with('@') {
            value.deitemize_for_sigil_bind().into_param_copy('@')
        } else {
            value.detach_shared_container()
        })
    }
}
