//! `extern "C"` opcode helper shims called from JIT-compiled code (Tier A).
//!
//! Each shim executes exactly one `exec_one` dispatch arm, by calling the same
//! function the arm calls (`vm_call_site_ops.rs` holds the bodies shared with
//! a dedicated shim), so a shim has no opcode logic of its own. Values travel on
//! `Interpreter::stack` as in interpreted execution, so no `Value` crosses the
//! FFI boundary; errors are parked in `Interpreter::jit_error` and signalled
//! by a nonzero status (see `vm_jit::JIT_STATUS_*`).
//!
//! Safety contract for every shim: `interp`, `code` and `fns` are the live
//! `&mut Interpreter` / `&CompiledCode` / `&HashMap` the JIT entry wrapper
//! (`vm_jit::try_enter`) received, valid and unaliased for the duration of
//! the native call.
//!
//! Panic boundary: a Rust panic raised by interpreter machinery (index OOB,
//! capacity overflow, ...) must not unwind through a shim's `extern "C"`
//! frame — that is `panic_cannot_unwind`, an instant abort. Every shim that
//! delegates to fallible interpreter machinery therefore runs its body under
//! [`panic_boundary`], which parks the payload and returns
//! `JIT_STATUS_PANIC`; the generated code returns any nonzero status straight
//! up, and `vm_jit::try_enter*` resumes the unwind on the Rust side of the
//! native frame, so the panic reaches the same run-loop / worker
//! `catch_unwind` boundaries as interpreted execution (pinned by
//! t/hyper-race-panic-boundary.t under MUTSU_JIT_THRESHOLD=2). Residual gap:
//! the void shims (`load_const`, ...) and the 0/1-returning jump-condition
//! shims below cannot signal a status, so a panic there still aborts — their
//! bodies only touch infallible stack/clone machinery.

use super::vm_jit::{JIT_STATUS_ERR, JIT_STATUS_HALT, JIT_STATUS_OK, JIT_STATUS_PANIC, park_panic};
use super::*;

/// Run a shim body under a catch-all panic boundary (see the module doc):
/// a caught payload is parked for `vm_jit::try_enter*` to resume and
/// `JIT_STATUS_PANIC` is returned in place of the body's status.
#[inline]
fn panic_boundary(f: impl FnOnce() -> u32) -> u32 {
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)) {
        Ok(status) => status,
        Err(payload) => {
            park_panic(payload);
            JIT_STATUS_PANIC
        }
    }
}

/// `OpCode::LoadConst`
pub(super) unsafe extern "C" fn load_const(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    idx: u32,
) {
    let (interp, code) = unsafe { (&mut *interp, &*code) };
    interp.stack.push(code.constants[idx as usize].clone());
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

/// GC-only VM poll emitted on native backedges (ADR-0004 §2.4).  The JIT
/// selects this one-argument ABI while the profiler is disarmed, preserving
/// the zero-cost native backedge promised by ADR-0106 Slice 1.
pub(super) unsafe extern "C" fn safepoint(_interp: *mut Interpreter) {
    crate::vm::vm_poll::poll(crate::gc::SafepointKind::Backedge, 0);
}

/// Location-carrying VM poll emitted on native backedges while profiling is
/// armed.  The code pointer is the live compiled chunk received by the JIT
/// entry, and `site` is the compile-time bytecode ip.
pub(super) unsafe extern "C" fn profile_safepoint(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    site: u32,
) {
    // Same safety contract as every other shim: `interp` is the live
    // interpreter the JIT entry wrapper received. The sampler only reads its
    // Raku frame stack.
    let (interp, code) = unsafe { (&*interp, &*code) };
    crate::vm::vm_poll::poll_code(crate::gc::SafepointKind::Backedge, site, code, interp);
}

/// Exact line-entry hook emitted at native basic-block boundaries while the
/// profiler is armed.  It records counts without becoming another GC poll.
pub(super) unsafe extern "C" fn profile_line(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    site: u32,
) {
    let (interp, code) = unsafe { (&*interp, &*code) };
    crate::vm::vm_poll::record_line(code, site, interp);
}

/// Mark a `Failure` at the current top of stack as handled. The Tier B
/// `JumpIfFalse` fast path calls this on the branch-taken path only,
/// mirroring the interpreter arm's post-pop `mark_failure_handled_on_stack`.
pub(super) unsafe extern "C" fn mark_failure_top(interp: *mut Interpreter) {
    let interp = unsafe { &mut *interp };
    Interpreter::mark_failure_handled_on_stack(&mut interp.stack);
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
    panic_boundary(|| match interp.exec_get_local_op(code, idx) {
        Ok(()) => JIT_STATUS_OK,
        Err(e) => park_err(interp, e),
    })
}

/// The METAOP_ASSIGN identity seed of `OpCode::MetaAssignIdentity` and of the
/// fused `OpCode::GetLocalMetaAssign`, for the two *infallible* identities
/// (`Zero` / `One`) — the overwhelmingly common ones, and the reason this is a
/// void shim: the caller then needs no status check or error block around it.
/// `identity` is the `MetaAssignIdentity` discriminant; the `code` pointer is
/// unused (the signature is shared with the other `(interp, code, u32)` shims).
pub(super) unsafe extern "C" fn meta_assign_identity(
    interp: *mut Interpreter,
    _code: *const CompiledCode,
    identity: u32,
) {
    let interp = unsafe { &mut *interp };
    let _ = interp
        .exec_meta_assign_identity_op(crate::token_kind::MetaAssignIdentity::from_u32(identity));
}

/// The fallible half: `/=` and `%=` have no zero-argument meaning, so seeding an
/// undefined container throws.
pub(super) unsafe extern "C" fn meta_assign_identity_fallible(
    interp: *mut Interpreter,
    _code: *const CompiledCode,
    identity: u32,
) -> u32 {
    let interp = unsafe { &mut *interp };
    panic_boundary(|| {
        match interp
            .exec_meta_assign_identity_op(crate::token_kind::MetaAssignIdentity::from_u32(identity))
        {
            Ok(()) => JIT_STATUS_OK,
            Err(e) => park_err(interp, e),
        }
    })
}

/// `OpCode::SetLocal`
///
/// Publishes to the `state` store on the same terms as the interpreter's
/// `SetLocal` dispatch arm (`vm_exec_dispatch.rs`) — `publish_state_local` is
/// a free `is_empty` check for the overwhelmingly common state-free
/// `CompiledCode`, now that `scoped_state_key` is a Copy tuple rather than a
/// `format!`ed `String` (see `todo/tickets/state-write-through-is-skipped-in-a-jit-compiled-range.md`).
pub(super) unsafe extern "C" fn set_local(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    idx: u32,
) -> u32 {
    let (interp, code) = unsafe { (&mut *interp, &*code) };
    panic_boundary(|| match interp.exec_set_local_op(code, idx) {
        Ok(()) => {
            interp.publish_state_local(code, idx);
            JIT_STATUS_OK
        }
        Err(e) => park_err(interp, e),
    })
}

/// `OpCode::ConcatAssignLocal` — the fused `$local ~= rhs` (#8695). Listed so
/// a hot loop that happens to contain one keeps its JIT body: without a shim
/// the opcode would reject the whole range.
pub(super) unsafe extern "C" fn concat_assign_local(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    slot: u32,
) -> u32 {
    let (interp, code) = unsafe { (&mut *interp, &*code) };
    panic_boundary(|| match interp.exec_concat_assign_local_op(code, slot) {
        Ok(()) => JIT_STATUS_OK,
        Err(e) => park_err(interp, e),
    })
}

/// `OpCode::ConcatAssignLocal(_, false)` — the fused `$local = $local ~ rhs`
/// (#9141); same shim as `concat_assign_local`, without the `''` seed.
pub(super) unsafe extern "C" fn concat_reassign_local(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    slot: u32,
) -> u32 {
    let (interp, code) = unsafe { (&mut *interp, &*code) };
    panic_boundary(|| match interp.exec_concat_reassign_local_op(code, slot) {
        Ok(()) => JIT_STATUS_OK,
        Err(e) => park_err(interp, e),
    })
}

/// `OpCode::SetLocalDecl` — the fused `my $x = <expr>` store (ADR-0006 §2.3).
/// `marks` is 1 when the declaration had an explicit initializer.
/// See `set_local` above for why the `publish_state_local` call is free.
pub(super) unsafe extern "C" fn set_local_decl(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    idx: u32,
    explicit_init: u32,
) -> u32 {
    let (interp, code) = unsafe { (&mut *interp, &*code) };
    interp
        .explicit_initializer_context()
        .set(explicit_init != 0);
    interp.vardecl_context().set(true);
    panic_boundary(|| match interp.exec_set_local_op(code, idx) {
        Ok(()) => {
            interp.publish_state_local(code, idx);
            JIT_STATUS_OK
        }
        Err(e) => park_err(interp, e),
    })
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
            panic_boundary(|| match interp.$method() {
                Ok(()) => JIT_STATUS_OK,
                Err(e) => park_err(interp, e),
            })
        }
    )+};
}

fallible_noarg_shims! {
    add => exec_add_op,
    sub => exec_sub_op,
    mul => exec_mul_op,
    div => exec_div_op,
    modulo => exec_mod_op,
    int_div => exec_int_div_op,
    int_mod => exec_int_mod_op,
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
    bit_and => exec_bit_and_op,
    bit_or => exec_bit_or_op,
    bit_xor => exec_bit_xor_op,
    bit_shift_left => exec_bit_shift_left_op,
    bit_shift_right => exec_bit_shift_right_op,
    int_bit_neg => exec_int_bit_neg_op,
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
    fns: *const CompiledFns,
) -> u32 {
    let (interp, code, fns) = unsafe { (&mut *interp, &*code, &*fns) };
    panic_boundary(|| {
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
    })
}

/// `OpCode::JumpIfFalse` condition (`Interpreter::jump_if_false_taken`):
/// pops the tested value; returns 1 when the jump must be taken, 0 to fall
/// through.
pub(super) unsafe extern "C" fn jump_if_false_cond(interp: *mut Interpreter) -> u32 {
    let interp = unsafe { &mut *interp };
    interp.jump_if_false_taken() as u32
}

/// `OpCode::JumpIfTrue` condition (`Interpreter::jump_if_true_taken`): PEEKS
/// the tested value; returns 1 when the jump must be taken, 0 to fall through.
pub(super) unsafe extern "C" fn jump_if_true_cond(interp: *mut Interpreter) -> u32 {
    let interp = unsafe { &mut *interp };
    interp.jump_if_true_taken() as u32
}

/// `OpCode::JumpIfNotNil` condition (`Interpreter::jump_if_not_nil_taken`):
/// PEEKS the tested value; returns 1 when the jump must be taken, 0 to fall
/// through.
pub(super) unsafe extern "C" fn jump_if_not_nil_cond(interp: *mut Interpreter) -> u32 {
    let interp = unsafe { &mut *interp };
    interp.jump_if_not_nil_taken() as u32
}

/// `OpCode::StateVarInitGuard` condition
/// (`Interpreter::state_var_init_guard_taken`), keyed on the opcode's own
/// `key_idx` rather than a stack value: returns 1 to jump past the RHS
/// initializer, 0 to fall through and run it.
pub(super) unsafe extern "C" fn state_var_init_guard_cond(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    op_idx: u32,
) -> u32 {
    let (interp, code) = unsafe { (&mut *interp, &*code) };
    let OpCode::StateVarInitGuard(key_idx, _) = &code.ops[op_idx as usize] else {
        unreachable!("jit state_var_init_guard shim on a non-StateVarInitGuard opcode")
    };
    interp.state_var_init_guard_taken(*key_idx) as u32
}

/// `OpCode::Return` (`Interpreter::exec_return_site`). Status OK means a
/// rebound `&return` ran and execution continues at the next opcode;
/// otherwise the return signal (or the rebound call's error) is parked and
/// reported as ERR.
/// `_op_idx` is unused; it only lets the shim share the `s_code_u32` ABI.
pub(super) unsafe extern "C" fn ret(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    _op_idx: u32,
) -> u32 {
    let (interp, code) = unsafe { (&mut *interp, &*code) };
    panic_boundary(|| match interp.exec_return_site(code) {
        Ok(()) => JIT_STATUS_OK,
        Err(e) => park_err(interp, e),
    })
}

/// Adapt a call site's result (`Interpreter::exec_call_*_site`) to a shim
/// status. Restores `current_code`: the callee's dispatch overwrote it, and
/// unlike the interpreter loop the following native opcodes do not reset it
/// per step. Native code runs no per-op line update, which is why every call
/// site body starts with `sync_source_line`.
#[inline]
fn call_site_status(
    interp: &mut Interpreter,
    code: &CompiledCode,
    r: Result<(), RuntimeError>,
) -> u32 {
    interp.current_code = code as *const CompiledCode as usize;
    match r {
        Ok(()) if interp.is_halted() => JIT_STATUS_HALT,
        Ok(()) => JIT_STATUS_OK,
        Err(e) => park_err(interp, e),
    }
}

/// `OpCode::CallMethod` (`Interpreter::exec_call_method_site`). `op_idx`
/// addresses the opcode in `code.ops` so the payload is read in place.
pub(super) unsafe extern "C" fn call_method(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    op_idx: u32,
) -> u32 {
    let (interp, code) = unsafe { (&mut *interp, &*code) };
    panic_boundary(|| {
        let r = interp.exec_call_method_site(code, op_idx as usize);
        call_site_status(interp, code, r)
    })
}

/// `OpCode::CallMethodMut` (`Interpreter::exec_call_method_mut_site`).
pub(super) unsafe extern "C" fn call_method_mut(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    op_idx: u32,
) -> u32 {
    let (interp, code) = unsafe { (&mut *interp, &*code) };
    panic_boundary(|| {
        let r = interp.exec_call_method_mut_site(code, op_idx as usize);
        call_site_status(interp, code, r)
    })
}

/// `OpCode::CallFunc` / `OpCode::CallFuncNamed`
/// (`Interpreter::exec_call_func_site`).
pub(super) unsafe extern "C" fn call_func(
    interp: *mut Interpreter,
    code: *const CompiledCode,
    op_idx: u32,
    fns: *const CompiledFns,
) -> u32 {
    let (interp, code, fns) = unsafe { (&mut *interp, &*code, &*fns) };
    panic_boundary(|| {
        let r = interp.exec_call_func_site(code, op_idx as usize, fns);
        call_site_status(interp, code, r)
    })
}
