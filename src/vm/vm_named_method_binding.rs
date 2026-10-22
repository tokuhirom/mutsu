use super::vm_call_method_ops::MethodName;
use super::*;

impl Interpreter {
    /// Publish the binding a reflective method site actually addresses, before
    /// the call's rebound-receiver snapshot. A same-named slot is never a
    /// substitute for the receiver slot selected by the compiler.
    // Cost: O(1), an indexed local read and an env publication when needed.
    pub(super) fn publish_reflected_binding(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        slot: Option<u32>,
    ) {
        let sym = code.const_sym(name_idx);
        let binding = match slot {
            Some(slot) => self.locals.get(slot as usize).cloned(),
            None => None,
        };
        if let Some(binding) = binding
            && !self
                .env()
                .get_sym(sym)
                .is_some_and(|old| old.same_binding(&binding))
        {
            self.env_mut().insert_sym(sym, binding);
        }
    }

    /// Resolve a dynamic method name before recording the named receiver's
    /// writeback boundary, so reflection observes the selected binding too.
    // Cost: O(a + n), a = arguments, n = receiver name length, plus the call.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_call_method_dynamic_mut_op(
        &mut self,
        code: &CompiledCode,
        ip: usize,
        arity: u32,
        target_name_idx: u32,
        target_slot: Option<u32>,
        modifier_idx: Option<u32>,
        quoted: bool,
        arg_sources_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        self.sync_source_line(code, ip);
        self.explode_if_fatal_failure_in_call_args("", arity as usize)?;
        let name = self
            .take_dynamic_method_name(arity as usize, quoted, "CallMethodDynamicMut")
            .inspect_err(|error| self.record_call_resume_point(code, ip, error))?;
        if arity == 0 && matches!(&name, Ok(method) if method == "VAR") {
            self.publish_reflected_binding(code, target_name_idx, target_slot);
        }
        self.call_method_mut_site_around(code, ip, arity, target_name_idx, |vm| match name {
            Err(callable) => {
                vm.accessor_ref_pending = false;
                vm.exec_dynamic_callable_method(
                    code,
                    callable,
                    arity as usize,
                    modifier_idx,
                    arg_sources_idx,
                )
            }
            Ok(method) => vm.exec_call_method_mut_named_op(
                code,
                MethodName::dynamic(&method),
                arity,
                target_name_idx,
                target_slot,
                modifier_idx,
                arg_sources_idx,
            ),
        })
    }
}
