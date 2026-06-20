//! Lightweight Interpreter -> Interpreter fallback instrumentation.
//!
//! The bytecode Interpreter is intended to execute everything natively, but today it
//! still delegates a large fraction of method dispatch to the tree-walking
//! `Interpreter` (see `ANALYSIS.md` section 1). This module counts how often
//! that delegation happens so progress on decoupling the Interpreter can be measured
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
/// Dispatches that enter the interpreter purely as a *carrier*, not as a
/// tree-walk fallback. `EVAL`/`EVALFILE` compile the supplied source to
/// bytecode and run it on a sub-Interpreter (`run_compiled_block`); pseudo-package reads
/// (`CALLER::`/`OUTER::`/`SETTING::`/`DYNAMIC::`) are reflective env lookups.
/// Neither tree-walks user code, so counting them as fallbacks overstates the
/// real decoupling gap. They are tracked separately here (lever A). See
/// docs/vm-decoupling.md.
static FUNCTION_CARRIER: AtomicU64 = AtomicU64::new(0);

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

/// Per-name carrier-dispatch histogram (only populated when stats are on).
/// Same purpose as the fallback histograms, but for interpreter-as-carrier
/// dispatch ([`record_function_carrier`]) that does not tree-walk user code.
fn function_carrier_by_name() -> &'static Mutex<HashMap<String, u64>> {
    static BY_NAME: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();
    BY_NAME.get_or_init(|| Mutex::new(HashMap::new()))
}
// Dual-store (locals <-> env) sync cost. See docs/vm-dual-store.md.
static CLONE_ENV: AtomicU64 = AtomicU64::new(0);
static ENV_DEEP_COPY: AtomicU64 = AtomicU64::new(0);
static ENV_FLUSH: AtomicU64 = AtomicU64::new(0);
static ENV_SLOTS_FLUSHED: AtomicU64 = AtomicU64::new(0);
static LOCALS_PULL: AtomicU64 = AtomicU64::new(0);
/// Pulls that actually changed at least one local slot. The difference
/// `LOCALS_PULL - LOCALS_PULL_EFFECTIVE` is *spurious* reverse-sync — the env
/// was flagged dirty but no slotted lexical was genuinely stale, so the precise
/// per-carrier-boundary model (docs/vm-single-store.md) can eliminate it.
static LOCALS_PULL_EFFECTIVE: AtomicU64 = AtomicU64::new(0);
/// Total individual local slots that a pull actually overwrote with a *different*
/// value (the real reverse-sync traffic the single-store design must preserve as
/// targeted writebacks).
static LOCALS_PULL_STALE_SLOTS: AtomicU64 = AtomicU64::new(0);

/// Per-name histogram of which slotted lexicals were genuinely stale during a
/// pull. Tells the single-store design *which* names need reverse write-through
/// (R1) vs carrier-boundary writeback (R2). Only populated when stats are on.
fn stale_slot_by_name() -> &'static Mutex<HashMap<String, u64>> {
    static BY_NAME: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();
    BY_NAME.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Whether instrumentation is active. Resolved once from the environment so the
/// hot path is a single cached boolean load when the feature is off.
#[inline]
pub(crate) fn enabled() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("MUTSU_VM_STATS").is_some())
}

/// Stage 3 (docs/env-locals-coherence.md §6.8): the reverse `sync_locals_from_env`
/// pull is **disabled by default**. The Slice F write-through campaign reached an
/// OFF surface of 0 across all `t/` — every by-name caller-lexical env write is
/// now reconciled by a precise write-through (`pending_rw_writeback_sources` /
/// `reconcile_locals_from_env_at_site` / carrier logging), so the blanket reverse
/// pull is no longer load-bearing for correctness. It is retained only as an
/// opt-in escape hatch (`MUTSU_REVERSE_SYNC=1`) for A/B diagnosis during the
/// transition; once CI's full roast confirms no remaining dependence the pull and
/// its `env_dirty` net will be deleted outright. Cached once; a single bool load
/// (zero steady-state cost).
#[inline]
pub(crate) fn reverse_sync_disabled() -> bool {
    static OFF: OnceLock<bool> = OnceLock::new();
    *OFF.get_or_init(|| std::env::var_os("MUTSU_REVERSE_SYNC").is_none())
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

/// Record a dispatch that enters the interpreter as a *carrier* rather than a
/// tree-walk fallback (`EVAL`/`EVALFILE`, pseudo-package reads). Counted in its
/// own bucket so the fallback metric reflects only genuine tree-walk delegation.
#[inline]
pub(crate) fn record_function_carrier(name: &str) {
    if enabled() {
        FUNCTION_CARRIER.fetch_add(1, Ordering::Relaxed);
        if let Ok(mut map) = function_carrier_by_name().lock() {
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

/// Record a write-through mirror of a local slot into env (`flush_local_to_env`).
/// Each call mirrors one name-observable slot, so `slots` is 1 per call.
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

/// Record that a pull genuinely changed >=1 slot (an *effective* pull). Called
/// once per pull that found any stale slot. Stats-gated by the caller.
#[inline]
pub(crate) fn record_locals_pull_effective() {
    LOCALS_PULL_EFFECTIVE.fetch_add(1, Ordering::Relaxed);
}

/// Record that one slotted lexical was genuinely stale during a pull (the env
/// value differed from the local). Bumps the total and the per-name histogram.
/// Stats-gated by the caller (called only when `enabled()`), so it skips the
/// re-check that the other recorders do.
#[inline]
pub(crate) fn record_stale_slot(name: &str) {
    LOCALS_PULL_STALE_SLOTS.fetch_add(1, Ordering::Relaxed);
    if let Ok(mut map) = stale_slot_by_name().lock() {
        *map.entry(name.to_string()).or_insert(0) += 1;
    }
}

/// Print a one-line summary of Interpreter fallback statistics to stderr.
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
    let f_carrier = FUNCTION_CARRIER.load(Ordering::Relaxed);
    eprintln!(
        "[mutsu vm-stats] function-call opcodes={f_total} interpreter_fallbacks={f_fallback} ({f_pct:.1}% of opcodes) interpreter_carrier={f_carrier} (EVAL/pseudo-package, not tree-walk)"
    );
    let clone_env = CLONE_ENV.load(Ordering::Relaxed);
    let deep_copy = ENV_DEEP_COPY.load(Ordering::Relaxed);
    let env_flush = ENV_FLUSH.load(Ordering::Relaxed);
    let slots = ENV_SLOTS_FLUSHED.load(Ordering::Relaxed);
    let locals_pull = LOCALS_PULL.load(Ordering::Relaxed);
    eprintln!(
        "[mutsu vm-stats] dual-store: clone_env={clone_env} (O(1) Arc bumps) env_deep_copies={deep_copy} (O(env) make_mut) env_flushes={env_flush} slots_flushed={slots} locals_pulls={locals_pull}"
    );
    // Reverse-sync precision (docs/vm-single-store.md Slice A): of the
    // `locals_pulls` above, how many actually changed a slot, and how many
    // total slots were stale. `spurious = locals_pulls - effective` is the pure
    // waste the precise model removes.
    let pull_effective = LOCALS_PULL_EFFECTIVE.load(Ordering::Relaxed);
    let stale_slots = LOCALS_PULL_STALE_SLOTS.load(Ordering::Relaxed);
    let spurious = locals_pull.saturating_sub(pull_effective);
    eprintln!(
        "[mutsu vm-stats] reverse-sync: locals_pulls={locals_pull} effective={pull_effective} spurious={spurious} stale_slots={stale_slots}"
    );
    if let Ok(map) = stale_slot_by_name().lock()
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
            "[mutsu vm-stats] stale-slot by name (top {}): {}",
            top.len(),
            top.join(" ")
        );
    }
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
    if let Ok(map) = function_carrier_by_name().lock()
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
            "[mutsu vm-stats] function-carrier by name (top {}): {}",
            top.len(),
            top.join(" ")
        );
    }
}
