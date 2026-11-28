//! The JIT shim for a string interpolation (`StringConcat`).
//!
//! A JIT body ran `"key-$i"` through the generic `step` shim: an `exec_one`
//! call, its dispatch prologue and the opcode-group match before the join
//! itself. This shim calls the op's body (`Interpreter::exec_string_concat_at`,
//! which the interpreter's arm runs too) directly, the way
//! `vm_jit_element_step` does for element reads and stores. Same safety
//! contract as every shim in `vm_jit_helpers`.

use super::vm_jit::{JIT_STATUS_HALT, JIT_STATUS_OK};
use super::*;

/// `extern "C"` shim: the `StringConcat` op at `op_idx`.
///
/// # Safety
/// As every `vm_jit_helpers` shim: the pointers are the live interpreter,
/// chunk and function table the JIT entry received.
// Cost: as `Interpreter::exec_string_concat_at`.
pub(super) unsafe extern "C" fn concat_step(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    op_idx: u32,
    fns: *const CompiledFns,
) -> u32 {
    {
        // SAFETY: see the function's safety contract; the borrows end before
        // `step` re-derives its own.
        let (vm, chunk) = unsafe { (&mut *interp, &*code) };
        if let Some(&OpCode::StringConcat(n)) = chunk.ops.get(op_idx as usize) {
            return super::vm_jit_helpers::panic_boundary(|| {
                // A user `infix:<~>` or `.Str` may run Raku code, which may
                // `exit`; report it as `step` does.
                let r = vm.exec_string_concat_at(chunk, op_idx as usize, n);
                vm.current_code = chunk as *const CompiledCode as usize;
                match r {
                    Ok(()) if vm.is_halted() => JIT_STATUS_HALT,
                    Ok(()) => JIT_STATUS_OK,
                    Err(e) => super::vm_jit_helpers::park_err(vm, e),
                }
            });
        }
    }
    // SAFETY: the same pointers, passed on unchanged.
    unsafe { super::vm_jit_helpers::step(interp, code, op_idx, fns) }
}
