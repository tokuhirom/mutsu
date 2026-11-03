//! `++%h<k>` / `%h<k>--` on a Hash or Array that a role has mixed `AT-KEY`
//! (`AT-POS`) into.
//!
//! The increment of an element is a read-modify-write of the container the
//! role's `AT-KEY` hands back (Hash::LRU's `is LRU` hashes; their `AT-KEY` is
//! `is raw`): rakudo calls `AT-KEY` once and bumps what it returned, and never
//! calls `ASSIGN-KEY`. The ordinary increment reads the backing storage directly
//! and never calls the role, so it started from an undefined element and lost
//! the store.

use super::*;

impl Interpreter {
    /// The role-mixed container the increment of `name` addresses, when its role
    /// supplies the read of the subscript.
    // Cost: O(1) (one variable read and two role-method probes).
    fn incdec_mixin_target(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        slot: Option<u32>,
        is_positional: bool,
    ) -> Option<Value> {
        let name = Self::const_str(code, name_idx);
        let name_sym = code.const_sym(name_idx);
        let target = self
            .gate_local_slot_value_at(code, slot, name)
            .or_else(|| self.get_env_with_main_alias_sym(name, name_sym))?
            .deref_container();
        let read = if is_positional { "AT-POS" } else { "AT-KEY" };
        (matches!(target.view(), ValueView::Mixin(..)) && self.mixin_composes_method(&target, read))
            .then_some(target)
    }

    /// Run the increment/decrement through the role's subscript accessor, leaving
    /// the answer (the new value for a prefix op, the old one for a postfix op)
    /// on the stack. `Ok(false)` leaves the stack untouched for the ordinary path.
    // Cost: O(1) plus the role's two method calls.
    pub(crate) fn try_incdec_through_mixin_subscript(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        slot: Option<u32>,
        increment: bool,
        return_new: bool,
        is_positional: bool,
    ) -> Result<bool, RuntimeError> {
        let Some(target) = self.incdec_mixin_target(code, name_idx, slot, is_positional) else {
            return Ok(false);
        };
        let Some(key) = self.stack.pop() else {
            return Ok(false);
        };
        let read = if is_positional { "AT-POS" } else { "AT-KEY" };
        // One call of the accessor, asked for its container: the value it holds is
        // the old one, and the new one is stored back through the same container.
        let slot_value = self.call_method_with_values(target, read, vec![key])?;
        let old = slot_value.deref_container();
        let effective = Self::normalize_incdec_source(
            if old.is_nil() || !crate::runtime::types::value_is_defined(&old) {
                Value::int(0)
            } else {
                old
            },
        );
        let new_val = if increment {
            self.increment_value_smart(&effective)?
        } else {
            self.decrement_value_smart(&effective)?
        };
        self.assign_through_rw_result(slot_value, new_val.clone())?;
        self.stack
            .push(if return_new { new_val } else { effective });
        Ok(true)
    }
}
