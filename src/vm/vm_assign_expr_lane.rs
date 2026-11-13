//! The expression-form scalar store lane of `AssignExprLocal`.

use super::*;

impl Interpreter {
    /// The expression-form scalar store lane: see the call site in
    /// [`Self::exec_assign_expr_local_op_inner`]. On success the stored value,
    /// itemized as the full path's result is, is left on the stack.
    // Cost: O(1): one readonly-registry probe plus the `SetLocal` scalar lane's
    // tag probes and latches.
    #[inline]
    pub(super) fn assign_expr_local_scalar_fast(
        &mut self,
        code: &CompiledCode,
        idx: usize,
    ) -> bool {
        if code.simple_scalar_local_desc(idx).is_none() {
            return false;
        }
        let Some(name_sym) = code.locals_sym.get(idx).copied() else {
            return false;
        };
        if self.readonly_kind_sym(name_sym).is_some() {
            return false;
        }
        if !self.exec_set_local_scalar_fast(code, idx as u32) {
            return false;
        }
        let val = self.locals[idx].clone();
        self.stack
            .push(Self::itemize_scalar_assign_result(&code.locals[idx], val));
        true
    }
}
