//! The JIT shim for a `++`/`--` on a proven `my int` local (ADR-18357 D5,
//! stage 1).
//!
//! A `PostIncrement`/`PreIncrement`/`PostDecrement`/`PreDecrement` whose slot
//! [`CompiledCode::native_int_step_slot`] proves is called here, instead of
//! through the generic `step` shim and a full interpreter dispatch. The shim
//! runs the same narrow arm the interpreter's op runs first
//! (`Interpreter::exec_native_int_step`), and hands any step that arm declines
//! to `step`, which runs the generic op. Same safety contract as every shim in
//! `vm_jit_helpers`.

use super::vm_jit::JIT_STATUS_OK;
use super::vm_scalar_incdec::IncDec;
use super::*;

/// The `IncDec` and slot of a slotted scalar `++`/`--` op, or `None` for any
/// other op.
// Cost: O(1).
pub(super) fn native_int_step_operands(op: &OpCode) -> Option<(u32, IncDec)> {
    let (slot, increment, prefix) = match op {
        OpCode::PostIncrement(_, Some(slot)) => (*slot, true, false),
        OpCode::PreIncrement(_, Some(slot)) => (*slot, true, true),
        OpCode::PostDecrement(_, Some(slot)) => (*slot, false, false),
        OpCode::PreDecrement(_, Some(slot)) => (*slot, false, true),
        _ => return None,
    };
    Some((slot, IncDec { increment, prefix }))
}

/// `extern "C"` shim: step the proven slot of the `++`/`--` at `op_idx`, or
/// run the op generically.
///
/// # Safety
/// As every `vm_jit_helpers` shim: the pointers are the live interpreter,
/// chunk and function table the JIT entry received.
// Cost: O(1) on the narrow arm; the generic op's cost otherwise.
pub(super) unsafe extern "C" fn native_int_step(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    op_idx: u32,
    fns: *const CompiledFns,
) -> u32 {
    {
        // SAFETY: see the function's safety contract; the borrows end before
        // `step` re-derives its own.
        let (vm, chunk) = unsafe { (&mut *interp, &*code) };
        if let Some((slot, k)) = chunk
            .ops
            .get(op_idx as usize)
            .and_then(native_int_step_operands)
            && vm.exec_native_int_step(chunk, slot as usize, k)
        {
            return JIT_STATUS_OK;
        }
    }
    // SAFETY: the same pointers, passed on unchanged.
    unsafe { super::vm_jit_helpers::step(interp, code, op_idx, fns) }
}
