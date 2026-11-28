//! The JIT shim for an element read or store (#19286, #19285).
//!
//! `Index` and `IndexAssignLocal` are straight-line ops the JIT runs through
//! the generic `step` shim: an `exec_one` call, its dispatch prologue and the
//! opcode-group match, ~310 instructions before the op's own fast lane runs —
//! more than the lane itself costs. This shim calls the lane directly, the way
//! `vm_jit_native_int_step` does for a `++` on a proven `my int`: the plain
//! `@a[$i]` / `%h{$k}` read (`Interpreter::index_fast_path_element`) and the
//! lean `@a[$i] = $v` / `%h{$k} = $v` store
//! (`Interpreter::try_lean_local_element_store`). Every
//! shape either lane declines is handed to `step`, which runs the op
//! generically, so the shim adds no semantics of its own. Same safety
//! contract as every shim in `vm_jit_helpers`.

use super::vm_jit::JIT_STATUS_OK;
use super::*;

/// Whether `op` is one this shim serves.
// Cost: O(1).
pub(super) fn is_element_step_op(op: &OpCode) -> bool {
    matches!(op, OpCode::Index { .. } | OpCode::IndexAssignLocal { .. })
}

/// `extern "C"` shim: run the element op at `op_idx` through its fast lane,
/// or generically.
///
/// # Safety
/// As every `vm_jit_helpers` shim: the pointers are the live interpreter,
/// chunk and function table the JIT entry received.
// Cost: O(1) on a fast lane; the generic op's cost otherwise.
pub(super) unsafe extern "C" fn element_step(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    op_idx: u32,
    fns: *const CompiledFns,
) -> u32 {
    {
        // SAFETY: see the function's safety contract; the borrows end before
        // `step` re-derives its own.
        let (vm, chunk) = unsafe { (&mut *interp, &*code) };
        let served = match chunk.ops.get(op_idx as usize) {
            Some(OpCode::IndexAssignLocal {
                slot,
                name_idx,
                is_positional,
                index_first,
                concat_append: false,
                element_share: false,
            }) => vm
                .try_lean_local_element_store(chunk, *slot, *name_idx, *is_positional, *index_first)
                .then_some(Ok(())),
            Some(OpCode::Index { is_positional }) => vm.try_index_fast_path_step(*is_positional),
            _ => None,
        };
        match served {
            Some(Ok(())) => return JIT_STATUS_OK,
            Some(Err(e)) => return super::vm_jit_helpers::park_err(vm, e),
            None => {}
        }
    }
    // SAFETY: the same pointers, passed on unchanged.
    unsafe { super::vm_jit_helpers::step(interp, code, op_idx, fns) }
}

impl Interpreter {
    /// The `Index` arm's ordinary-read prefix, for the JIT shim: outside an
    /// `is rw` return context, answer the read from `index_fast_path_element`
    /// and apply the arm's `decont_native_pos_ref_on_top`, exactly as
    /// `exec_index_op_with_positional` does on a fast-path hit. `None` =
    /// declined, stack untouched; `Some` = the arm's result.
    // Cost: O(1).
    pub(super) fn try_index_fast_path_step(
        &mut self,
        is_positional: bool,
    ) -> Option<Result<(), RuntimeError>> {
        if self.rw_return_context {
            return None;
        }
        let elem = self.index_fast_path_element(is_positional)?;
        self.stack.truncate(self.stack.len() - 2);
        self.stack.push(elem);
        Some(self.decont_native_pos_ref_on_top())
    }
}
