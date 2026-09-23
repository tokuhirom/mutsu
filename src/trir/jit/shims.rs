//! `extern "C"` helpers the native TRIR code calls (ADR-0116).
//!
//! Safety contract for every shim: `interp`, `chunk`, `frame` and `fns` are
//! the live, unaliased pointers [`super::try_run`] handed the native entry,
//! valid for the duration of the call; `io` points at the native frame's
//! operand buffer of at least `max(nin, nout)` words.
//!
//! A Rust panic must not unwind through an `extern "C"` frame, so a shim that
//! runs interpreter machinery catches it, parks it, and reports
//! `JIT_STATUS_PANIC`; [`super::try_run`] resumes it on the Rust side.

use super::super::TrChunk;
use super::super::exec::{TrFlow, TrOutcome};
use super::super::frame::TrFrame;
use super::{STATUS_BAIL, STATUS_RET};
use crate::opcode::CompiledFns;
use crate::runtime::Interpreter;
use crate::vm::vm_jit::{JIT_STATUS_ERR, JIT_STATUS_OK, JIT_STATUS_PANIC, park_panic};

/// Execute op `ip` through [`Interpreter::trir_step`], with its `nin`
/// int-bank operands taken from `io[..nin]` (deepest first) and its `nout`
/// int-bank results written back to `io[..nout]`.
///
/// The int bank lives in the native code's SSA values, so the operands an op
/// reads are pushed onto the interpreter's bank for the op and its results
/// popped off again. Nothing else of the frame moves: the boxed bank and the
/// boxed slots stay where the switch loop keeps them.
// Cost: O(1) plus the op's own cost; `nin`, `nout` <= the call's argument count.
#[allow(clippy::too_many_arguments)]
pub(super) unsafe extern "C" fn trir_jit_step(
    interp: *mut Interpreter,
    chunk: *const TrChunk,
    frame: *const TrFrame,
    fns: *const CompiledFns,
    ip: u32,
    io: *mut i64,
    nin: u32,
    nout: u32,
) -> u32 {
    let run = move || {
        // SAFETY: the module contract above.
        let (interp, chunk, frame, fns) = unsafe { (&mut *interp, &*chunk, *frame, &*fns) };
        for k in 0..nin as usize {
            // SAFETY: `io` holds at least `nin` words.
            interp.trir.ns.push(unsafe { *io.add(k) });
        }
        match interp.trir_step(chunk, frame, fns, ip as usize) {
            Ok(TrFlow::Next) => {
                for k in (0..nout as usize).rev() {
                    let v = interp.ipop();
                    // SAFETY: `io` holds at least `nout` words.
                    unsafe { *io.add(k) = v };
                }
                JIT_STATUS_OK
            }
            Ok(TrFlow::Done(TrOutcome::Value(v))) => {
                interp.trir.jit_ret = Some(v);
                STATUS_RET
            }
            Ok(TrFlow::Done(TrOutcome::Bail)) => STATUS_BAIL,
            Ok(TrFlow::Jump(_)) => {
                // The lowering emits every jump itself; a stepped op that
                // jumps is a lowering bug, reported rather than obeyed.
                interp.jit_error = Some(crate::value::RuntimeError::new(
                    "internal error: a stepped TRIR op jumped",
                ));
                JIT_STATUS_ERR
            }
            Err(e) => {
                interp.jit_error = Some(e);
                JIT_STATUS_ERR
            }
        }
    };
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(run)) {
        Ok(status) => status,
        Err(payload) => {
            park_panic(payload);
            JIT_STATUS_PANIC
        }
    }
}

/// The address of the native frame-slot bank. Re-read after every call out,
/// because a callee that opens a frame may reallocate it.
// Cost: O(1).
pub(super) unsafe extern "C" fn trir_jit_nl(interp: *mut Interpreter) -> *mut i64 {
    // SAFETY: the module contract above.
    unsafe { (*interp).trir.nl.as_mut_ptr() }
}

/// The GC / thread-stop safepoint on a native backedge (ADR-0004 §2.4): a
/// loop that never calls out must still let a stop-the-world proceed.
// Cost: O(1) when no stop is requested.
pub(super) unsafe extern "C" fn trir_jit_poll(_interp: *mut Interpreter) {
    crate::vm::vm_poll::poll(crate::gc::SafepointKind::Backedge, 0);
}
