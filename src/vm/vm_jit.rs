//! JIT entry point and hotness tracking (ADR-0004 layer 4, phase J1).
//!
//! Method JIT on Cranelift: a `CompiledCode` chunk whose opcodes all belong to
//! the supported Tier A set is translated, once hot, into a native function
//! that calls the interpreter's opcode helpers in sequence (subroutine
//! threading) with branches lowered to native control flow. A chunk containing
//! any unsupported opcode is marked bailout and stays on the interpreter
//! forever — no guards, no deopt, no OSR (ADR-0004 §2.3).
//!
//! Off by default: compiled in under the `jit` cargo feature, activated at
//! runtime with `MUTSU_JIT=on` (threshold tunable via `MUTSU_JIT_THRESHOLD`).

use super::*;

/// Status codes returned by JIT-compiled bodies and fallible opcode helpers.
/// `ERR` means the `RuntimeError` (including the `&return` signal and `fail`)
/// is parked in `Interpreter::jit_error`; `HALT` means `exit`-style halt with
/// no error (the caller loop re-checks `is_halted()`).
pub(crate) const JIT_STATUS_OK: u32 = 0;
pub(crate) const JIT_STATUS_ERR: u32 = 1;
pub(crate) const JIT_STATUS_HALT: u32 = 2;

/// `CompiledCode::jit.entry` sentinel: the chunk was scanned and rejected
/// (contains an unsupported opcode); never retry. `0` means cold/counting;
/// any other value is the native entry pointer.
pub(crate) const JIT_ENTRY_BAILOUT: u64 = 1;

/// Native entry signature: `(interp, code, compiled_fns) -> status`.
#[cfg(feature = "jit")]
pub(crate) type JitEntryFn = unsafe extern "C" fn(
    *mut Interpreter,
    *const CompiledCode,
    *const HashMap<String, CompiledFunction>,
) -> u32;

/// True when the JIT is switched on for this process (`MUTSU_JIT=on|1`).
#[cfg(feature = "jit")]
#[inline]
pub(crate) fn jit_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        std::env::var("MUTSU_JIT")
            .map(|v| v == "1" || v.eq_ignore_ascii_case("on"))
            .unwrap_or(false)
    })
}

/// Call-count threshold before a chunk is considered hot and compiled.
#[cfg(feature = "jit")]
fn jit_threshold() -> u32 {
    static THRESHOLD: std::sync::OnceLock<u32> = std::sync::OnceLock::new();
    *THRESHOLD.get_or_init(|| {
        std::env::var("MUTSU_JIT_THRESHOLD")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(100)
    })
}

/// Try to run a function body natively. Returns `None` when the body must run
/// on the interpreter (JIT off / still cold / bailout chunk); `Some(result)`
/// when the whole body ran natively — the result has the same shape as the
/// interpreter's body loop outcome (`Err` carries explicit return / fail /
/// error exactly like `exec_one`), so the caller threads it through the same
/// match arms.
#[cfg(feature = "jit")]
pub(crate) fn try_enter(
    interp: &mut Interpreter,
    code: &CompiledCode,
    compiled_fns: &HashMap<String, CompiledFunction>,
) -> Option<Result<(), RuntimeError>> {
    use std::sync::atomic::Ordering;
    if !jit_enabled() {
        return None;
    }
    let entry = code.jit.entry.load(Ordering::Acquire);
    let f: JitEntryFn = if entry == 0 {
        let calls = code.jit.calls.fetch_add(1, Ordering::Relaxed) + 1;
        if calls < jit_threshold() {
            return None;
        }
        match super::vm_jit_compile::compile_chunk(code) {
            Some(f) => {
                code.jit.entry.store(f as usize as u64, Ordering::Release);
                crate::vm::vm_stats::record_jit_compile();
                f
            }
            None => {
                code.jit.entry.store(JIT_ENTRY_BAILOUT, Ordering::Release);
                return None;
            }
        }
    } else if entry == JIT_ENTRY_BAILOUT {
        return None;
    } else {
        // The entry word only ever transitions 0 -> fn-pointer (never back),
        // and the pointee is a finalized function in the leaked JIT module,
        // so reconstructing the pointer from the atomic is sound.
        unsafe { std::mem::transmute::<usize, JitEntryFn>(entry as usize) }
    };
    crate::vm::vm_stats::record_jit_entry();
    interp.current_code = code as *const CompiledCode as usize;
    let status = unsafe { f(interp, code, compiled_fns) };
    match status {
        JIT_STATUS_ERR => {
            let e = interp
                .jit_error
                .take()
                .expect("JIT returned error status without a parked error");
            Some(Err(e))
        }
        // OK (fell off the end; result on the stack) or HALT (the caller
        // loop re-checks `is_halted()` right after this step).
        _ => Some(Ok(())),
    }
}

/// JIT-less builds: never enters native code.
#[cfg(not(feature = "jit"))]
#[inline(always)]
pub(crate) fn try_enter(
    _interp: &mut Interpreter,
    _code: &CompiledCode,
    _compiled_fns: &HashMap<String, CompiledFunction>,
) -> Option<Result<(), RuntimeError>> {
    None
}
