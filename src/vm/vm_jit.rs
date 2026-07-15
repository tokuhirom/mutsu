//! JIT entry point and hotness tracking (ADR-0004 layer 4, phase J1).
//!
//! Method JIT on Cranelift: a `CompiledCode` chunk whose opcodes all belong to
//! the supported Tier A set is translated, once hot, into a native function
//! that calls the interpreter's opcode helpers in sequence (subroutine
//! threading) with branches lowered to native control flow. A chunk containing
//! any unsupported opcode is marked bailout and stays on the interpreter
//! forever — no guards, no deopt, no OSR (ADR-0004 §2.3).
//!
//! **On by default** since the ADR-0004 J5 gates passed (2026-07-13):
//! compiled in under the `jit` cargo feature, disabled at runtime with
//! `MUTSU_JIT=off` (threshold tunable via `MUTSU_JIT_THRESHOLD`).

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

/// Process-wide, monotonic count of user-declared infix operator
/// registrations (`sub infix:<+> ...` and module-exported infix ops). The
/// Tier B inline `Add` fast path (vm_jit_tier_b.rs) loads this word and
/// requires it to be zero: a per-interpreter `user_declared_infix_ops` set
/// can only be non-empty after some registration bumped this counter, so
/// zero proves no override exists anywhere and nonzero conservatively routes
/// every inline fast path through the helper (which re-checks precisely).
/// Never decremented — save/restore sites that shrink a set leave the
/// counter high, which only costs speed, never correctness.
pub(crate) static USER_INFIX_DECLS: std::sync::atomic::AtomicU32 =
    std::sync::atomic::AtomicU32::new(0);

/// Record one user infix-operator registration (see `USER_INFIX_DECLS`).
#[inline]
pub(crate) fn note_user_infix_decl() {
    USER_INFIX_DECLS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
}

/// Process-wide, monotonic count of `ContainerRef` cell words ever packed
/// (bumped at the single NaN-box encode chokepoint for `Kind::ContainerRef`).
/// Zero proves no shared `:=`/capture cell exists anywhere in the process, so
/// the Tier B inline `GetLocal` fast path (vm_jit_tier_b.rs) can skip the
/// env cell-adoption probe (`exec_get_local_op`'s per-read `env().get_sym`)
/// outright; nonzero conservatively routes every inline read through the
/// helper, which re-runs the full interpreter arm. Never decremented — a
/// program that stops using cells only loses speed, never correctness.
pub(crate) static CONTAINER_CELLS: std::sync::atomic::AtomicU32 =
    std::sync::atomic::AtomicU32::new(0);

/// Record one `ContainerRef` cell creation (see `CONTAINER_CELLS`).
#[inline]
pub(crate) fn note_container_cell() {
    CONTAINER_CELLS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
}

/// Process-wide, monotonic count of `$CALLER::x := ...` variable-binding
/// aliases ever created (`Interpreter::var_bindings` inserts). Zero proves
/// `resolve_binding` is a no-op everywhere, letting the Tier B inline
/// `GetLocal` fast path skip it; same conservative monotonic contract as
/// [`CONTAINER_CELLS`].
pub(crate) static CALLER_VAR_BINDS: std::sync::atomic::AtomicU32 =
    std::sync::atomic::AtomicU32::new(0);

/// Record one caller-variable binding registration (see `CALLER_VAR_BINDS`).
#[inline]
pub(crate) fn note_caller_var_binding() {
    CALLER_VAR_BINDS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
}

/// Native entry signature: `(interp, code, compiled_fns) -> status`.
#[cfg(feature = "jit")]
pub(crate) type JitEntryFn =
    unsafe extern "C" fn(*mut Interpreter, *const CompiledCode, *const CompiledFns) -> u32;

/// True when the JIT is switched on for this process. **Default ON** since
/// the ADR-0004 J5 gates passed (2026-07-13): gc-stress × jit-stress green,
/// startup budget unchanged, and every bench-CI `+jit` series at or ahead of
/// the interpreter — `MUTSU_JIT=off` is the explicit opt-out (interpreter
/// baselines, debugging the JIT itself).
///
/// Resolved once from `MUTSU_JIT` (`off`/`0` = off; `on`/`1` = on; an
/// unrecognized value warns once and keeps the default on). Unlike
/// `gc_enabled()` there is no `cfg!(test)` exception: JIT state is
/// per-`CompiledCode` (plus a `Mutex`-guarded engine), so parallel in-process
/// unit tests cannot cross-talk through it.
#[cfg(feature = "jit")]
#[inline]
pub(crate) fn jit_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| match std::env::var("MUTSU_JIT").ok().as_deref() {
        Some(v) if v == "0" || v.eq_ignore_ascii_case("off") => false,
        Some(v) if v == "1" || v.eq_ignore_ascii_case("on") => true,
        None => true,
        Some(other) => {
            eprintln!("[mutsu jit] warning: unrecognized MUTSU_JIT={other:?}, defaulting to on");
            true
        }
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
    compiled_fns: &CompiledFns,
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

/// Range flavor of [`try_enter`] (ADR-0004 J4b hot-loop entry): run the
/// opcode sub-range `[start, end)` — a compound loop's body or condition —
/// natively once it is hot. `None` means the range must run on the
/// interpreter (JIT off / cold / bailout); `Some(result)` has the exact
/// shape of `run_range`'s interpreter-loop outcome for a body that never
/// resumes in place (the caller re-enters the interpreter for the two
/// resumable signals, goto and warn).
#[cfg(feature = "jit")]
pub(crate) fn try_enter_range(
    interp: &mut Interpreter,
    code: &CompiledCode,
    start: usize,
    end: usize,
    compiled_fns: &CompiledFns,
) -> Option<Result<(), RuntimeError>> {
    use std::sync::atomic::Ordering;
    if !jit_enabled() || start >= end || end > code.ops.len() {
        return None;
    }
    // Settled-range fast path (ADR-0004 J4d): a range whose entry word is
    // final is published in the lock-free `range_cache`, so the steady state
    // of a hot loop pays a couple of atomic loads here instead of the mutex +
    // linear scan below (which profiled at ~12% of a hot numeric loop).
    let cache_key = (((start as u64) << 32) | end as u64).wrapping_add(1);
    let mut entry = 0u64;
    for (k, e) in &code.jit.range_cache {
        // Acquire pairs with the publishing store: a visible key implies the
        // entry word (stored before the key) is visible too.
        if k.load(Ordering::Acquire) == cache_key {
            entry = e.load(Ordering::Relaxed);
            break;
        }
    }
    if entry == JIT_ENTRY_BAILOUT {
        return None;
    }
    let f: JitEntryFn = if entry != 0 {
        // Same soundness argument as `try_enter`: 0 -> fn-pointer only.
        unsafe { std::mem::transmute::<usize, JitEntryFn>(entry as usize) }
    } else {
        let st = {
            let mut ranges = code.jit.ranges.lock().unwrap();
            let key = (start as u32, end as u32);
            match ranges.iter().find(|(k, _)| *k == key) {
                Some((_, s)) => s.clone(),
                None => {
                    let s = std::sync::Arc::new(crate::opcode::JitRangeState::default());
                    ranges.push((key, s.clone()));
                    s
                }
            }
        };
        let entry = st.entry.load(Ordering::Acquire);
        if entry == 0 {
            let calls = st.calls.fetch_add(1, Ordering::Relaxed) + 1;
            if calls < jit_threshold() {
                return None;
            }
            match super::vm_jit_compile::compile_range(code, start, end) {
                Some(f) => {
                    st.entry.store(f as usize as u64, Ordering::Release);
                    crate::vm::vm_stats::record_jit_compile();
                    publish_range_entry(code, cache_key, f as usize as u64);
                    f
                }
                None => {
                    st.entry.store(JIT_ENTRY_BAILOUT, Ordering::Release);
                    publish_range_entry(code, cache_key, JIT_ENTRY_BAILOUT);
                    return None;
                }
            }
        } else if entry == JIT_ENTRY_BAILOUT {
            publish_range_entry(code, cache_key, JIT_ENTRY_BAILOUT);
            return None;
        } else {
            publish_range_entry(code, cache_key, entry);
            // Same soundness argument as `try_enter`: 0 -> fn-pointer only.
            unsafe { std::mem::transmute::<usize, JitEntryFn>(entry as usize) }
        }
    };
    crate::vm::vm_stats::record_jit_entry();
    // The interpreter loop polls the GC safepoint once per opcode; a native
    // body polls only its own backedges. The enclosing compound loop's
    // per-iteration poll therefore lands here, before each native body run.
    crate::gc::gc_safepoint(crate::gc::SafepointKind::Backedge);
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
        _ => Some(Ok(())),
    }
}

/// Publish a settled range entry (compiled fn pointer or `JIT_ENTRY_BAILOUT`)
/// into the chunk's lock-free `range_cache`. Slot claiming is serialized by
/// taking the `ranges` mutex; the entry word is stored *before* the key is
/// released so a reader that acquires the key always sees the entry. A full
/// cache (more settled ranges than slots) or an already-published key is
/// fine — the range just keeps resolving through the mutex path.
#[cfg(feature = "jit")]
fn publish_range_entry(code: &CompiledCode, cache_key: u64, entry: u64) {
    use std::sync::atomic::Ordering;
    let _guard = code.jit.ranges.lock().unwrap();
    for (k, e) in &code.jit.range_cache {
        let cur = k.load(Ordering::Relaxed);
        if cur == cache_key {
            return; // another thread already published it
        }
        if cur == 0 {
            e.store(entry, Ordering::Relaxed);
            k.store(cache_key, Ordering::Release);
            return;
        }
    }
}

/// JIT-less builds: never enters native code.
#[cfg(not(feature = "jit"))]
#[inline(always)]
pub(crate) fn try_enter(
    _interp: &mut Interpreter,
    _code: &CompiledCode,
    _compiled_fns: &CompiledFns,
) -> Option<Result<(), RuntimeError>> {
    None
}
