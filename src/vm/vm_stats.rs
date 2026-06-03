//! Lightweight VM -> Interpreter fallback instrumentation.
//!
//! The bytecode VM is intended to execute everything natively, but today it
//! still delegates a large fraction of method dispatch to the tree-walking
//! `Interpreter` (see `ANALYSIS.md` section 1). This module counts how often
//! that delegation happens so progress on decoupling the VM can be measured
//! per change instead of guessed at.
//!
//! Disabled by default (a single relaxed atomic load guards every counter).
//! Enable with `MUTSU_VM_STATS=1`; a one-line summary is printed to stderr at
//! the end of the run via `crate::dump_vm_stats()`.

use std::sync::OnceLock;
use std::sync::atomic::{AtomicU64, Ordering};

static METHOD_TOTAL: AtomicU64 = AtomicU64::new(0);
static METHOD_FALLBACK: AtomicU64 = AtomicU64::new(0);
static FUNCTION_TOTAL: AtomicU64 = AtomicU64::new(0);
static FUNCTION_FALLBACK: AtomicU64 = AtomicU64::new(0);

/// Whether instrumentation is active. Resolved once from the environment so the
/// hot path is a single cached boolean load when the feature is off.
fn enabled() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("MUTSU_VM_STATS").is_some())
}

/// Record that an explicit method-call opcode (`.foo(...)`) was executed.
#[inline]
pub(crate) fn record_method_dispatch() {
    if enabled() {
        METHOD_TOTAL.fetch_add(1, Ordering::Relaxed);
    }
}

/// Record that a method dispatch fell back to the tree-walking interpreter
/// (`Interpreter::call_method_with_values`) instead of running compiled code.
#[inline]
pub(crate) fn record_method_fallback() {
    if enabled() {
        METHOD_FALLBACK.fetch_add(1, Ordering::Relaxed);
    }
}

/// Record that an explicit function-call opcode (`foo(...)`) was executed.
#[inline]
pub(crate) fn record_function_dispatch() {
    if enabled() {
        FUNCTION_TOTAL.fetch_add(1, Ordering::Relaxed);
    }
}

/// Record that a function dispatch fell back to the tree-walking interpreter
/// (`Interpreter::call_function` / `call_function_fallback`) instead of running
/// compiled or native code.
#[inline]
pub(crate) fn record_function_fallback() {
    if enabled() {
        FUNCTION_FALLBACK.fetch_add(1, Ordering::Relaxed);
    }
}

/// Print a one-line summary of VM fallback statistics to stderr.
///
/// No-op unless `MUTSU_VM_STATS` is set. Counts aggregate across worker threads
/// (Promise/Proc::Async/hyper) because the counters are process-global.
pub(crate) fn dump() {
    if !enabled() {
        return;
    }
    // `*_opcodes` count explicit call opcodes; `*_fallbacks` count executions
    // delegated to the tree-walking interpreter. The two are measured at
    // different layers, so a `fallback` count may exceed its opcode count for
    // code dominated by calls that reach the interpreter without going through
    // a call opcode (implicit coercions like .Str/.Numeric/.Bool for methods;
    // Routine-value and meta-operator calls for functions).
    let m_total = METHOD_TOTAL.load(Ordering::Relaxed);
    let m_fallback = METHOD_FALLBACK.load(Ordering::Relaxed);
    let m_pct = if m_total == 0 {
        0.0
    } else {
        m_fallback as f64 * 100.0 / m_total as f64
    };
    let f_total = FUNCTION_TOTAL.load(Ordering::Relaxed);
    let f_fallback = FUNCTION_FALLBACK.load(Ordering::Relaxed);
    let f_pct = if f_total == 0 {
        0.0
    } else {
        f_fallback as f64 * 100.0 / f_total as f64
    };
    eprintln!(
        "[mutsu vm-stats] method-call opcodes={m_total} interpreter_fallbacks={m_fallback} ({m_pct:.1}% of opcodes)"
    );
    eprintln!(
        "[mutsu vm-stats] function-call opcodes={f_total} interpreter_fallbacks={f_fallback} ({f_pct:.1}% of opcodes)"
    );
}
