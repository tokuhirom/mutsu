//! Native lowering of TRIR chunks — ADR-0116 (ADR-0112 Step 4).
//!
//! A hot [`TrChunk`] is lowered to one Cranelift function. The int bank and
//! the native frame slots become SSA values, and every jump a native branch,
//! so the typed half of a chunk runs as machine code rather than through
//! [`Interpreter::trir_step`]'s switch. Every other op (the boxed bank, the
//! calls, the returns) is a call to [`shims::trir_jit_step`], which runs
//! that same `trir_step` for the one op, so an op has one definition whichever
//! tier executes it.
//!
//! Nothing here speculates: TRIR already proved every operand's kind, so the
//! native code has no guard and no deoptimization. Its only exits are the
//! chunk's own: a return, an error, and the existing whole-routine bail,
//! which the TRIR compiler admits only where re-running the routine from the
//! start is equivalent (ADR-0110). The interpreter stays the reference: a
//! chunk the lowering declines keeps running there, and `MUTSU_TRIR_JIT=off`
//! switches the lowering off for the A/B.

mod lower;
mod shape;
mod shims;

use super::TrChunk;
use super::exec::TrOutcome;
use super::frame::TrFrame;
use crate::opcode::CompiledFns;
use crate::runtime::Interpreter;
use crate::value::RuntimeError;
use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};

/// The native entry: `(interp, chunk, frame, compiled_fns) -> status`.
pub(super) type TrJitFn = unsafe extern "C" fn(
    *mut Interpreter,
    *const TrChunk,
    *const TrFrame,
    *const CompiledFns,
) -> u32;

/// The chunk bailed ([`TrOutcome::Bail`]).
pub(super) const STATUS_BAIL: u32 = 4;
/// The chunk returned; the value is parked in `TrStacks::jit_ret`.
pub(super) const STATUS_RET: u32 = 5;

/// `TrJitState::entry` when the lowering declined the chunk.
const ENTRY_DECLINED: u64 = 1;

/// A chunk's native-code state: cold and counting, declined, or compiled.
///
/// Cloning a chunk yields a cold state: the native code is a function of the
/// chunk's ops alone, but a clone is a different chunk as far as anyone
/// holding its address is concerned, and recompiling it is cheap.
#[derive(Debug, Default)]
pub(crate) struct TrJitState {
    /// `0` = cold, [`ENTRY_DECLINED`], or the native function's address.
    entry: AtomicU64,
    /// Runs so far while cold.
    calls: AtomicU32,
}

impl Clone for TrJitState {
    fn clone(&self) -> Self {
        Self::default()
    }
}

/// Whether TRIR chunks are lowered at all: the bytecode JIT's own switch
/// (`MUTSU_JIT`), narrowed by `MUTSU_TRIR_JIT=off`.
fn enabled() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| {
        crate::vm::vm_jit::jit_enabled()
            && !matches!(
                std::env::var("MUTSU_TRIR_JIT").as_deref(),
                Ok("off") | Ok("0") | Ok("false")
            )
    })
}

/// Run `chunk` as native code when it has (or, now hot, gets) some. `None`
/// sends the caller to the switch loop.
// Cost: O(1) per run once compiled or declined; the one compile is O(n), n =
// the chunk's op count.
pub(super) fn try_run(
    interp: &mut Interpreter,
    chunk: &TrChunk,
    frame: TrFrame,
    compiled_fns: &CompiledFns,
) -> Option<Result<TrOutcome, RuntimeError>> {
    if !enabled() {
        return None;
    }
    let state = &chunk.jit;
    let entry = state.entry.load(Ordering::Acquire);
    let f: TrJitFn = if entry == 0 {
        let calls = state.calls.fetch_add(1, Ordering::Relaxed) + 1;
        if calls < crate::vm::vm_jit::jit_threshold() {
            return None;
        }
        match lower::compile(chunk) {
            Some(f) => {
                state.entry.store(f as usize as u64, Ordering::Release);
                super::stats::record_native(super::stats::TrirNative::Compiled);
                f
            }
            None => {
                state.entry.store(ENTRY_DECLINED, Ordering::Release);
                super::stats::record_native(super::stats::TrirNative::Declined);
                return None;
            }
        }
    } else if entry == ENTRY_DECLINED {
        return None;
    } else {
        // SAFETY: the word only ever goes 0 -> function address (or the
        // decline sentinel, handled above), and the function lives in the
        // process-lifetime JIT module.
        unsafe { std::mem::transmute::<usize, TrJitFn>(entry as usize) }
    };
    super::stats::record_native(super::stats::TrirNative::Ran);
    // SAFETY: the pointers are live, unaliased borrows for the duration of
    // the call, which is the shims' contract (`shims.rs`).
    let status = unsafe { f(interp, chunk, &frame, compiled_fns) };
    Some(match status {
        STATUS_RET => Ok(TrOutcome::Value(
            interp
                .trir
                .jit_ret
                .take()
                .unwrap_or(crate::value::Value::NIL),
        )),
        STATUS_BAIL => Ok(TrOutcome::Bail),
        crate::vm::vm_jit::JIT_STATUS_PANIC => crate::vm::vm_jit::resume_parked_panic(),
        _ => Err(interp.jit_error.take().unwrap_or_else(|| {
            RuntimeError::new("internal error: native TRIR code failed without an error")
        })),
    })
}
