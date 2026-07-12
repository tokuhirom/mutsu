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

/// `OpCode::Add`
pub(super) unsafe extern "C" fn add(interp: *mut Interpreter) -> u32 {
    let interp = unsafe { &mut *interp };
    match interp.exec_add_op() {
        Ok(()) => JIT_STATUS_OK,
        Err(e) => park_err(interp, e),
    }
}

/// `OpCode::Sub`
pub(super) unsafe extern "C" fn sub(interp: *mut Interpreter) -> u32 {
    let interp = unsafe { &mut *interp };
    match interp.exec_sub_op() {
        Ok(()) => JIT_STATUS_OK,
        Err(e) => park_err(interp, e),
    }
}

/// `OpCode::NumLt`
pub(super) unsafe extern "C" fn num_lt(interp: *mut Interpreter) -> u32 {
    let interp = unsafe { &mut *interp };
    match interp.exec_num_lt_op() {
        Ok(()) => JIT_STATUS_OK,
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
