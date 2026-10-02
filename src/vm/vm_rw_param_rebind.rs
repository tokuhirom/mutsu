//! Detaching a scalar `is rw` parameter from its caller when the body
//! rebinds it with `:=` (#10361).
//!
//! The full-call path (`call_compiled_function_named`) binds a scalar `is rw`
//! parameter copy-in/copy-out: the body reads and writes its local slot, and
//! on return the slot's final value is written back to the caller's variable.
//! A `$p := $other` inside the body rebinds the NAME `$p` to another container,
//! so from that point the body no longer refers to the caller's variable at
//! all -- copying the slot's final value back would clobber the caller with
//! whatever `$p` was rebound to (`sub f($p is rw) { for 1 {}; $p := $p<a> }`
//! turned the caller's `{}` into `Any`).
//!
//! The call records which slots hold such a parameter in
//! `Interpreter::rw_param_rebinds` (only for slots the compiler saw rebound,
//! see `CompiledCode::rebound_slots`); the first rebind of one snapshots the
//! slot's value at that moment, which is exactly the caller variable's value
//! as of the detachment, and the writeback uses that snapshot instead of the
//! slot. The list is per call frame (`VmCallFrame::saved_rw_param_rebinds`).

use super::*;

impl Interpreter {
    /// Arm rebind tracking for the scalar `is rw` parameters of the call being
    /// entered: every `rw_bindings` parameter whose slot `code` rebinds.
    // Cost: O(r * l), r = rw params, l = locals of `code`; only for a body with a `:=` rebind.
    pub(super) fn arm_rw_param_rebinds(
        &mut self,
        code: &CompiledCode,
        rw_bindings: &[(String, String)],
    ) {
        if code.rebound_slots.is_empty() {
            return;
        }
        for (param_name, _source) in rw_bindings {
            if param_name.starts_with(['@', '%', '&']) {
                continue;
            }
            if let Some(slot) = code.locals.iter().position(|n| n == param_name)
                && code.rebound_slots.contains(&(slot as u32))
            {
                self.rw_param_rebinds.push((slot as u32, None));
            }
        }
    }

    /// A `:=` is about to store into slot `idx`: when that slot holds a tracked
    /// `is rw` parameter not yet detached, snapshot its current value as the
    /// value to write back to the caller.
    // Cost: O(r), r = tracked rw params of the current frame.
    pub(super) fn note_rw_param_rebind(&mut self, idx: u32) {
        if let Some(entry) = self
            .rw_param_rebinds
            .iter_mut()
            .find(|(slot, snap)| *slot == idx && snap.is_none())
        {
            let cur = self.locals.get(idx as usize).cloned().unwrap_or(Value::NIL);
            entry.1 = Some(cur);
        }
    }

    /// The value to write back to the caller for the rw parameter in slot
    /// `slot`: the pre-rebind snapshot when the body rebound it, else `None`
    /// (the slot's own final value applies).
    // Cost: O(r), r = tracked rw params of the current frame.
    pub(super) fn rw_param_detached_value(&self, slot: usize) -> Option<Value> {
        self.rw_param_rebinds
            .iter()
            .find(|(s, _)| *s as usize == slot)
            .and_then(|(_, snap)| snap.clone())
    }

    /// [`Self::rw_param_detached_value`] for a writeback that names the
    /// parameter rather than its slot.
    // Cost: O(1) when no rw param of the frame can be rebound, else O(l), l = locals of `code`.
    pub(super) fn rw_param_detached_by_name(
        &self,
        code: &CompiledCode,
        param_name: &str,
    ) -> Option<Value> {
        if self.rw_param_rebinds.is_empty() {
            return None;
        }
        let slot = code.locals.iter().position(|n| n == param_name)?;
        self.rw_param_detached_value(slot)
    }
}
