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

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Mutex, OnceLock};

static METHOD_TOTAL: AtomicU64 = AtomicU64::new(0);
static METHOD_FALLBACK: AtomicU64 = AtomicU64::new(0);
static FUNCTION_TOTAL: AtomicU64 = AtomicU64::new(0);
static FUNCTION_FALLBACK: AtomicU64 = AtomicU64::new(0);

/// Per-name function-fallback histogram (only populated when stats are on).
/// Tells us *which* builtins/subs still route through the interpreter, so
/// decoupling work can target the highest-count names first. See
/// docs/vm-decoupling.md.
fn function_fallback_by_name() -> &'static Mutex<HashMap<String, u64>> {
    static BY_NAME: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();
    BY_NAME.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Per-name method-fallback histogram (only populated when stats are on). Same
/// purpose as [`function_fallback_by_name`] but for `.method(...)` dispatch.
fn method_fallback_by_name() -> &'static Mutex<HashMap<String, u64>> {
    static BY_NAME: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();
    BY_NAME.get_or_init(|| Mutex::new(HashMap::new()))
}
// Dual-store (locals <-> env) sync cost. See docs/vm-dual-store.md.
static CLONE_ENV: AtomicU64 = AtomicU64::new(0);
static ENV_DEEP_COPY: AtomicU64 = AtomicU64::new(0);
static ENV_FLUSH: AtomicU64 = AtomicU64::new(0);
static ENV_SLOTS_FLUSHED: AtomicU64 = AtomicU64::new(0);
static LOCALS_PULL: AtomicU64 = AtomicU64::new(0);

/// Whether instrumentation is active. Resolved once from the environment so the
/// hot path is a single cached boolean load when the feature is off.
#[inline]
pub(crate) fn enabled() -> bool {
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
pub(crate) fn record_method_fallback(name: &str) {
    if enabled() {
        METHOD_FALLBACK.fetch_add(1, Ordering::Relaxed);
        if let Ok(mut map) = method_fallback_by_name().lock() {
            *map.entry(name.to_string()).or_insert(0) += 1;
        }
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
pub(crate) fn record_function_fallback(name: &str) {
    if enabled() {
        FUNCTION_FALLBACK.fetch_add(1, Ordering::Relaxed);
        if let Ok(mut map) = function_fallback_by_name().lock() {
            *map.entry(name.to_string()).or_insert(0) += 1;
        }
    }
}

/// Record a `clone_env()` of the interpreter env (one per pushed call frame).
/// Note: `Env` is copy-on-write (`Arc<HashMap>`), so this is an O(1) Arc bump,
/// *not* a deep copy. The deep copy is counted separately by
/// `record_env_deep_copy` when a shared env is first mutated.
#[inline]
pub(crate) fn record_clone_env() {
    if enabled() {
        CLONE_ENV.fetch_add(1, Ordering::Relaxed);
    }
}

/// Record an actual O(env_size) deep copy of the env HashMap, triggered when
/// `Arc::make_mut` clones a shared env on first mutation (e.g. the first env
/// write inside a method body whose frame holds a clone of the env). This is
/// the real cost the dual-store work targets, not `clone_env`.
#[inline]
pub(crate) fn record_env_deep_copy() {
    if enabled() {
        ENV_DEEP_COPY.fetch_add(1, Ordering::Relaxed);
    }
}

/// Record an `ensure_env_synced` flush of dirty locals into env, along with how
/// many slots were actually written.
#[inline]
pub(crate) fn record_env_flush(slots: u64) {
    if enabled() {
        ENV_FLUSH.fetch_add(1, Ordering::Relaxed);
        ENV_SLOTS_FLUSHED.fetch_add(slots, Ordering::Relaxed);
    }
}

/// Record a `sync_locals_from_env` pull (env -> locals after an interpreter
/// bridge modified env).
#[inline]
pub(crate) fn record_locals_pull() {
    if enabled() {
        LOCALS_PULL.fetch_add(1, Ordering::Relaxed);
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
    let clone_env = CLONE_ENV.load(Ordering::Relaxed);
    let deep_copy = ENV_DEEP_COPY.load(Ordering::Relaxed);
    let env_flush = ENV_FLUSH.load(Ordering::Relaxed);
    let slots = ENV_SLOTS_FLUSHED.load(Ordering::Relaxed);
    let locals_pull = LOCALS_PULL.load(Ordering::Relaxed);
    eprintln!(
        "[mutsu vm-stats] dual-store: clone_env={clone_env} (O(1) Arc bumps) env_deep_copies={deep_copy} (O(env) make_mut) env_flushes={env_flush} slots_flushed={slots} locals_pulls={locals_pull}"
    );
    if let Ok(map) = function_fallback_by_name().lock()
        && !map.is_empty()
    {
        let mut entries: Vec<(&String, &u64)> = map.iter().collect();
        entries.sort_by(|a, b| b.1.cmp(a.1).then_with(|| a.0.cmp(b.0)));
        let top: Vec<String> = entries
            .iter()
            .take(25)
            .map(|(name, count)| format!("{name}={count}"))
            .collect();
        eprintln!(
            "[mutsu vm-stats] function-fallback by name (top {}): {}",
            top.len(),
            top.join(" ")
        );
    }
    if let Ok(map) = method_fallback_by_name().lock()
        && !map.is_empty()
    {
        let mut entries: Vec<(&String, &u64)> = map.iter().collect();
        entries.sort_by(|a, b| b.1.cmp(a.1).then_with(|| a.0.cmp(b.0)));
        let top: Vec<String> = entries
            .iter()
            .take(25)
            .map(|(name, count)| format!("{name}={count}"))
            .collect();
        eprintln!(
            "[mutsu vm-stats] method-fallback by name (top {}): {}",
            top.len(),
            top.join(" ")
        );
    }
}
