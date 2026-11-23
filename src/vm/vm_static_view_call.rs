//! A by-name call of a routine that the running code declares below its
//! unit's top level, under the static-outer latch (ADR-12529 amendment A1
//! (b), #18787): the call runs the routine's code object of this activation,
//! whose capture is the declaring scope's static view, so the callee's frame
//! links to its declaring block (`$CALLER::OUTER::x` read from code it calls).

use super::*;
use crate::vm::vm_call_func_ops::CodeVarBinding;

impl Interpreter {
    /// Run the by-name call of `name_idx` on the activation's code object
    /// when this code declares the routine with a static view
    /// (`CompiledSubDeclPlan::static_view_slots`); `None` otherwise.
    // Cost: O(p) to find the plan, p = this code's routine declarations;
    // plus the capture (`capture_lexsub_activation`) for a routine that has
    // a static view.
    pub(super) fn try_static_view_call(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
        compiled_fns: &CompiledFns,
    ) -> Option<Result<(), RuntimeError>> {
        let name = code.const_sym(name_idx);
        if !code
            .sub_decl_plans
            .iter()
            .any(|p| p.name == name && !p.static_view_slots.is_empty())
        {
            return None;
        }
        let own = self.resolve_code_var_unshadowed(name.as_str());
        if !matches!(own.view(), ValueView::Sub(_)) {
            return None;
        }
        let captured = self.capture_lexsub_activation(code, own);
        Some(self.exec_call_on_code_var_op(
            code,
            name_idx,
            arity,
            arg_sources_idx,
            false,
            CodeVarBinding::Bound(captured),
            compiled_fns,
        ))
    }
}
