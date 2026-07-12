//! `extern "C"` opcode helper shims called from JIT-compiled code (Tier A).
//!
//! Each shim reproduces exactly one `exec_one` dispatch arm. Values travel on
//! `Interpreter::stack` as in interpreted execution, so no `Value` crosses the
//! FFI boundary; errors are parked in `Interpreter::jit_error` and signalled
//! by a nonzero status (see `vm_jit::JIT_STATUS_*`).
//!
//! Safety contract for every shim: `interp`, `code` and `fns` are the live
//! `&mut Interpreter` / `&CompiledCode` / `&HashMap` the JIT entry wrapper
//! (`vm_jit::try_enter`) received, valid and unaliased for the duration of
//! the native call.
//!
//! TODO(J2): a Rust panic inside a shim aborts the process at the `extern
//! "C"` boundary instead of being converted to X::AdHoc by the run-loop
//! `catch_unwind`; decide between `extern "C-unwind"` + JIT unwind info or
//! per-shim `catch_unwind` once Tier A coverage grows.

use super::vm_jit::{JIT_STATUS_ERR, JIT_STATUS_HALT, JIT_STATUS_OK};
use super::*;

/// `OpCode::LoadConst`
pub(super) unsafe extern "C" fn load_const(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    idx: u32,
) {
    let (interp, code) = unsafe { (&mut *interp, &*code) };
    interp.stack.push(code.constants[idx as usize].clone());
}

/// `OpCode::SetSourceLine`
pub(super) unsafe extern "C" fn set_source_line(interp: *mut Interpreter, line: i64) {
    unsafe { &mut *interp }.cur_source_line = line;
}

/// `OpCode::ContainerizePair`
pub(super) unsafe extern "C" fn containerize_pair(interp: *mut Interpreter) {
    let interp = unsafe { &mut *interp };
    let val = interp.stack.pop().unwrap();
    let containerized = match val.view() {
        ValueView::Pair(k, v) => Value::value_pair(Value::str(k.clone()), v.clone()),
        _ => val,
    };
    interp.stack.push(containerized);
}

/// GC safepoint poll emitted on native backedges (ADR-0004 §2.4).
pub(super) unsafe extern "C" fn safepoint(_interp: *mut Interpreter) {
    crate::gc::gc_safepoint(crate::gc::SafepointKind::Backedge);
}

/// Park `e` in the interpreter's JIT error slot and report error status.
#[inline]
fn park_err(interp: &mut Interpreter, e: RuntimeError) -> u32 {
    interp.jit_error = Some(e);
    JIT_STATUS_ERR
}

/// `OpCode::GetLocal`
pub(super) unsafe extern "C" fn get_local(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    idx: u32,
) -> u32 {
    let (interp, code) = unsafe { (&mut *interp, &*code) };
    match interp.exec_get_local_op(code, idx) {
        Ok(()) => JIT_STATUS_OK,
        Err(e) => park_err(interp, e),
    }
}

/// `OpCode::SetLocal`
pub(super) unsafe extern "C" fn set_local(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    idx: u32,
) -> u32 {
    let (interp, code) = unsafe { (&mut *interp, &*code) };
    match interp.exec_set_local_op(code, idx) {
        Ok(()) => JIT_STATUS_OK,
        Err(e) => park_err(interp, e),
    }
}

/// Dedicated shims for the payload-free fallible opcodes (arith / compare /
/// string): each delegates to the interpreter's own `exec_*_op`, skipping the
/// dispatch match entirely. These are the hot loop-body opcodes, so they get
/// a direct shim instead of the generic `step` below.
macro_rules! fallible_noarg_shims {
    ($($shim:ident => $method:ident),+ $(,)?) => {$(
        #[doc = concat!("Shim delegating to `", stringify!($method), "`.")]
        pub(super) unsafe extern "C" fn $shim(interp: *mut Interpreter) -> u32 {
            let interp = unsafe { &mut *interp };
            match interp.$method() {
                Ok(()) => JIT_STATUS_OK,
                Err(e) => park_err(interp, e),
            }
        }
    )+};
}

fallible_noarg_shims! {
    add => exec_add_op,
    sub => exec_sub_op,
    mul => exec_mul_op,
    div => exec_div_op,
    modulo => exec_mod_op,
    pow => exec_pow_op,
    negate => exec_negate_op,
    num_lt => exec_num_lt_op,
    num_le => exec_num_le_op,
    num_gt => exec_num_gt_op,
    num_ge => exec_num_ge_op,
    num_eq => exec_num_eq_op,
    num_ne => exec_num_ne_op,
    concat => exec_concat_op,
    str_eq => exec_str_eq_op,
    str_ne => exec_str_ne_op,
}

/// Generic single-opcode step for straight-line opcodes without a dedicated
/// shim: runs the interpreter's own dispatch arm at `op_idx` via `exec_one`,
/// so the arm's full behavior (resume-point recording, rw writeback, ...) is
/// reproduced verbatim. Only opcodes on the `step_supported` whitelist
/// (vm_jit_compile.rs) are emitted through here — each is verified to always
/// leave `ip == op_idx + 1` on Ok (no control flow), so the native caller's
/// fall-through to the next opcode matches the interpreter exactly.
/// `current_code` is restored afterwards: a re-entrant arm (CallMethod, ...)
/// overwrites it, and unlike the interpreter loop the following native
/// opcodes do not reset it per step.
pub(super) unsafe extern "C" fn step(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    op_idx: u32,
    fns: *const HashMap<String, CompiledFunction>,
) -> u32 {
    let (interp, code, fns) = unsafe { (&mut *interp, &*code, &*fns) };
    let mut ip = op_idx as usize;
    let r = interp.exec_one(code, &mut ip, fns);
    interp.current_code = code as *const CompiledCode as usize;
    match r {
        Ok(()) => {
            debug_assert_eq!(
                ip,
                op_idx as usize + 1,
                "non-straight-line opcode on the Tier A step whitelist"
            );
            if interp.is_halted() {
                JIT_STATUS_HALT
            } else {
                JIT_STATUS_OK
            }
        }
        Err(e) => park_err(interp, e),
    }
}

/// `OpCode::JumpIfFalse` condition: pops the tested value; returns 1 when the
/// jump must be taken (falsy), 0 to fall through. Mirrors the dispatch arm's
/// Failure-handled marking on both paths.
pub(super) unsafe extern "C" fn jump_if_false_cond(interp: *mut Interpreter) -> u32 {
    let interp = unsafe { &mut *interp };
    Interpreter::mark_failure_handled_on_stack(&mut interp.stack);
    let val = interp.stack.pop().unwrap();
    if !interp.eval_truthy(&val) {
        Interpreter::mark_failure_handled_on_stack(&mut interp.stack);
        1
    } else {
        0
    }
}

/// `OpCode::JumpIfTrue` condition: PEEKS the tested value (the interpreter
/// arm keeps it on the stack on both paths — `||`-style short-circuit);
/// returns 1 when the jump must be taken (truthy), 0 to fall through.
pub(super) unsafe extern "C" fn jump_if_true_cond(interp: *mut Interpreter) -> u32 {
    let interp = unsafe { &mut *interp };
    Interpreter::mark_failure_handled_on_stack(&mut interp.stack);
    let val = interp.stack.last().unwrap().clone();
    if interp.eval_truthy(&val) { 1 } else { 0 }
}

/// `OpCode::JumpIfNotNil` condition: PEEKS the tested value (kept on the
/// stack on both paths — `//`-style short-circuit); returns 1 when the jump
/// must be taken (defined), 0 to fall through.
pub(super) unsafe extern "C" fn jump_if_not_nil_cond(interp: *mut Interpreter) -> u32 {
    let interp = unsafe { &mut *interp };
    Interpreter::mark_failure_handled_on_stack(&mut interp.stack);
    let val = interp.stack.last().unwrap();
    if crate::runtime::types::value_is_defined(val) {
        1
    } else {
        0
    }
}

/// `OpCode::Return`. Status OK means a rebound `&return` ran and execution
/// continues at the next opcode; otherwise the return signal (or the rebound
/// call's error) is parked and reported as ERR, exactly like the interpreter
/// arm's `Err(RuntimeError::return_signal(..))`.
pub(super) unsafe extern "C" fn ret(interp: *mut Interpreter) -> u32 {
    let interp = unsafe { &mut *interp };
    let val = interp.stack.pop().unwrap_or(Value::NIL);
    if let Some(rebound) = interp.env().get("&return").cloned()
        && matches!(
            rebound.view(),
            ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
        )
    {
        return match interp.vm_call_on_value(rebound, vec![val], None) {
            Ok(result) => {
                interp.stack.push(result);
                JIT_STATUS_OK
            }
            Err(e) => park_err(interp, e),
        };
    }
    park_err(interp, RuntimeError::return_signal(val))
}

/// `OpCode::CallMethod`. `op_idx` addresses the opcode in `code.ops` so the
/// payload is read in place. Reproduces the dispatch arm exactly: resume-point
/// recording on error, and the `is rw` writeback / pending-local drain on
/// success. Restores `current_code` after the re-entrant dispatch.
pub(super) unsafe extern "C" fn call_method(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    op_idx: u32,
) -> u32 {
    let (interp, code) = unsafe { (&mut *interp, &*code) };
    let OpCode::CallMethod {
        name_idx,
        arity,
        modifier_idx,
        quoted,
        arg_sources_idx,
    } = &code.ops[op_idx as usize]
    else {
        unreachable!("jit call_method shim on a non-CallMethod opcode")
    };
    let r = interp.exec_call_method_op(
        code,
        *name_idx,
        *arity,
        *modifier_idx,
        *quoted,
        *arg_sources_idx,
    );
    interp.current_code = code as *const CompiledCode as usize;
    match r {
        Ok(()) => {
            interp.apply_pending_rw_writeback(code);
            interp.drain_pending_local_updates_after_call(code);
            if interp.is_halted() {
                JIT_STATUS_HALT
            } else {
                JIT_STATUS_OK
            }
        }
        Err(e) => {
            if !e.is_resume() && interp.resume_ip.is_none() {
                interp.resume_ip = Some(op_idx as usize + 1);
            }
            park_err(interp, e)
        }
    }
}

/// `OpCode::CallMethodMut`. Mirrors the dispatch arm: attr-cell snapshot
/// before, receiver writeback + rw writeback + pending-local drain + attr-cell
/// mirror after, resume-point recording on error.
pub(super) unsafe extern "C" fn call_method_mut(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    op_idx: u32,
) -> u32 {
    let (interp, code) = unsafe { (&mut *interp, &*code) };
    let OpCode::CallMethodMut {
        name_idx,
        arity,
        target_name_idx,
        modifier_idx,
        quoted,
        arg_sources_idx,
    } = &code.ops[op_idx as usize]
    else {
        unreachable!("jit call_method_mut shim on a non-CallMethodMut opcode")
    };
    let pre = interp.array_hash_attr_env_snapshot(code, *target_name_idx);
    let r = interp.exec_call_method_mut_op(
        code,
        *name_idx,
        *arity,
        *target_name_idx,
        *modifier_idx,
        *quoted,
        *arg_sources_idx,
    );
    interp.current_code = code as *const CompiledCode as usize;
    match r {
        Ok(()) => {
            {
                let target_name = Interpreter::const_str(code, *target_name_idx);
                if !target_name.is_empty() {
                    interp
                        .pending_rw_writeback_sources
                        .push(target_name.to_string());
                }
            }
            interp.apply_pending_rw_writeback(code);
            interp.drain_pending_local_updates_after_call(code);
            interp.mirror_array_hash_attr_to_cell(code, *target_name_idx, pre);
            if interp.is_halted() {
                JIT_STATUS_HALT
            } else {
                JIT_STATUS_OK
            }
        }
        Err(e) => {
            if !e.is_resume() && interp.resume_ip.is_none() {
                interp.resume_ip = Some(op_idx as usize + 1);
            }
            park_err(interp, e)
        }
    }
}

/// `OpCode::CallFunc`. `op_idx` addresses the opcode in `code.ops` so the
/// payload (`name_idx`/`arity`/`arg_sources_idx`) is read in place instead of
/// being marshalled through the native frame. Restores `current_code` after
/// the call (the callee's dispatch overwrote it) and reproduces the dispatch
/// arm's resume-point recording.
pub(super) unsafe extern "C" fn call_func(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    op_idx: u32,
    fns: *const HashMap<String, CompiledFunction>,
) -> u32 {
    let (interp, code, fns) = unsafe { (&mut *interp, &*code, &*fns) };
    let OpCode::CallFunc {
        name_idx,
        arity,
        arg_sources_idx,
    } = &code.ops[op_idx as usize]
    else {
        unreachable!("jit call_func shim on a non-CallFunc opcode")
    };
    let r = interp.exec_call_func_op(code, *name_idx, *arity, *arg_sources_idx, fns);
    interp.current_code = code as *const CompiledCode as usize;
    match r {
        Ok(()) => {
            if interp.is_halted() {
                JIT_STATUS_HALT
            } else {
                JIT_STATUS_OK
            }
        }
        Err(e) => {
            if !e.is_resume() && interp.resume_ip.is_none() {
                interp.resume_ip = Some(op_idx as usize + 1);
            }
            park_err(interp, e)
        }
    }
}
