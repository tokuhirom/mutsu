//! `%h = ...` / `@a = ...` on a variable whose container has a role mixed in
//! (`%h does R`, a `Hash::Restricted` `is restricted` trait).
//!
//! In raku the assignment is the container's own `STORE`: a role that
//! declares one sees it (and can `callsame` into the native one), and the
//! container keeps its identity, mixin included. Replacing the variable's
//! value with a fresh plain Hash/Array dropped the role.

use super::*;

impl Interpreter {
    /// The `Mixin` over a native Hash/Array that `value` holds, if any.
    fn mixin_container(value: &Value) -> Option<Value> {
        let value = value.deref_container();
        match value.view() {
            ValueView::Mixin(inner, _)
                if matches!(inner.view(), ValueView::Hash(_) | ValueView::Array(..)) =>
            {
                Some(value.clone())
            }
            _ => None,
        }
    }

    /// Route an assignment to the `@`/`%` variable `name` (local slot `slot`,
    /// or its env binding) through `STORE` when the variable holds a mixed-in
    /// container. Leaves the container on the stack, as an assignment does.
    /// `None` (stack untouched) for every other target.
    // Cost: O(n) plus the role's STORE, n = number of values stored.
    pub(super) fn maybe_mixin_container_store(
        &mut self,
        slot: Option<usize>,
        name: &str,
    ) -> Result<Option<()>, RuntimeError> {
        if !name.starts_with(['%', '@'])
            || self.bind_context().get()
            || self.scalar_bind_context().get()
        {
            return Ok(None);
        }
        let held = slot
            .and_then(|s| self.locals.get(s).cloned())
            .and_then(|v| Self::mixin_container(&v))
            .or_else(|| {
                self.get_env_with_main_alias(name)
                    .and_then(|v| Self::mixin_container(&v))
            });
        let Some(container) = held else {
            return Ok(None);
        };
        let rhs = self.stack.pop().unwrap_or(Value::NIL);
        let values = Value::array(crate::runtime::utils::value_to_list(&rhs));
        self.try_compiled_method_or_interpret(container.clone(), "STORE", vec![values])?;
        self.stack.push(container);
        Ok(Some(()))
    }
}
