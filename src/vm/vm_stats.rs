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

/// Print a one-line summary of VM fallback statistics to stderr.
///
/// No-op unless `MUTSU_VM_STATS` is set. Counts aggregate across worker threads
/// (Promise/Proc::Async/hyper) because the counters are process-global.
pub(crate) fn dump() {
    if !enabled() {
        return;
    }
    let total = METHOD_TOTAL.load(Ordering::Relaxed);
    let fallback = METHOD_FALLBACK.load(Ordering::Relaxed);
    let pct = if total == 0 {
        0.0
    } else {
        fallback as f64 * 100.0 / total as f64
    };
    // `total` counts explicit method-call opcodes; `fallback` counts method
    // executions delegated to the tree-walking interpreter. The two are
    // measured at different layers, so `fallback` may exceed `total` for code
    // dominated by implicit coercion calls (.Str/.Numeric/.Bool) that reach the
    // interpreter without going through a method-call opcode.
    eprintln!(
        "[mutsu vm-stats] method-call opcodes={total} interpreter_fallbacks={fallback} ({pct:.1}% of opcodes)"
    );
}
