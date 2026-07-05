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

/// Per-name histogram of method calls that entered the slow-path resolver dispatch
/// `run_instance_method` (resolve candidate + frame setup + env clone). §B #3680
/// deleted the tree-walk of the method body, so these now execute the body as
/// COMPILED bytecode — this counter measures the residual *dispatch* overhead (the
/// next target: VM-native multi/submethod resolution caching so a `CallMethod` op
/// dispatches without entering `run_instance_method`), NOT tree-walk execution.
fn resolver_method_by_name() -> &'static Mutex<HashMap<String, u64>> {
    static BY_NAME: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();
    BY_NAME.get_or_init(|| Mutex::new(HashMap::new()))
}
// Dual-store (locals <-> env) sync cost. See docs/vm-dual-store.md.
static CLONE_ENV: AtomicU64 = AtomicU64::new(0);
static ENV_DEEP_COPY: AtomicU64 = AtomicU64::new(0);
static ENV_FLUSH: AtomicU64 = AtomicU64::new(0);
static ENV_SLOTS_FLUSHED: AtomicU64 = AtomicU64::new(0);

// GC Level 1a counters (ADR-0001/0002, docs/gc-level1-detailed-design.md
// §8/§9.4a). As of §11 step 4 the candidate buffer exists, so
// `candidate_pushes`/`dedup_hits` are live (they increment when `MUTSU_GC` is
// on and a `Gc` handle is dropped with survivors). The collection counters
// still read 0 — the synchronous collector lands in §11 step 8. Note that no
// `Value` variant is migrated to `Gc<T>` yet (§11 step 5+), so ordinary program
// runs push nothing today. Success criterion once migration lands (§8):
// `gc_candidate_pushes == 0` on the `fib` benchmark, proving the
// scalar/container type filter keeps int-heavy hot paths GC-cost-free.
static GC_CANDIDATE_PUSHES: AtomicU64 = AtomicU64::new(0);
static GC_CANDIDATE_DEDUP_HITS: AtomicU64 = AtomicU64::new(0);
static GC_COLLECTIONS: AtomicU64 = AtomicU64::new(0);
static GC_RECLAIMED_NODES: AtomicU64 = AtomicU64::new(0);
static GC_RECLAIMED_CYCLES: AtomicU64 = AtomicU64::new(0);
static GC_PAUSE_NS_TOTAL: AtomicU64 = AtomicU64::new(0);
static GC_PAUSE_NS_MAX: AtomicU64 = AtomicU64::new(0);
static GC_ROOTS_SCANNED: AtomicU64 = AtomicU64::new(0);

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

/// Record that a method call entered the slow-path resolver dispatch
/// `run_instance_method` (resolve + setup; the body itself runs compiled since #3680).
#[inline]
pub(crate) fn record_resolver_method_dispatch(name: &str) {
    if enabled()
        && let Ok(mut map) = resolver_method_by_name().lock()
    {
        *map.entry(name.to_string()).or_insert(0) += 1;
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

/// Record a GC cycle-candidate buffer push: a mutation chokepoint flagged a
/// GC-managed node as a possible cycle member (design doc §4.2). Wired from
/// `gc::gc_ptr::buffer_candidate` (§11 step 4), but only reachable once a
/// `Value` variant is `Gc`-managed (§11 step 5) — dead until then.
#[inline]
#[allow(dead_code)]
pub(crate) fn record_gc_candidate_push() {
    if enabled() {
        GC_CANDIDATE_PUSHES.fetch_add(1, Ordering::Relaxed);
    }
}

/// Record that a candidate push deduplicated against an already-buffered node
/// instead of adding a new entry. Wired from `gc::gc_ptr::buffer_candidate`,
/// reachable only once a `Value` variant is `Gc`-managed (§11 step 5).
#[inline]
#[allow(dead_code)]
pub(crate) fn record_gc_candidate_dedup_hit() {
    if enabled() {
        GC_CANDIDATE_DEDUP_HITS.fetch_add(1, Ordering::Relaxed);
    }
}

/// Record one completed collect cycle: `roots_scanned` nodes visited from the
/// root set, `reclaimed_nodes`/`reclaimed_cycles` freed, taking `pause_ns`.
/// Wired from `gc::collect::collect_cycles`, which has no production caller
/// until safepoint wiring lands (§11 step 8), so this stays dead until then.
#[inline]
#[allow(dead_code)]
pub(crate) fn record_gc_collection(
    roots_scanned: u64,
    reclaimed_nodes: u64,
    reclaimed_cycles: u64,
    pause_ns: u64,
) {
    if enabled() {
        GC_COLLECTIONS.fetch_add(1, Ordering::Relaxed);
        GC_ROOTS_SCANNED.fetch_add(roots_scanned, Ordering::Relaxed);
        GC_RECLAIMED_NODES.fetch_add(reclaimed_nodes, Ordering::Relaxed);
        GC_RECLAIMED_CYCLES.fetch_add(reclaimed_cycles, Ordering::Relaxed);
        GC_PAUSE_NS_TOTAL.fetch_add(pause_ns, Ordering::Relaxed);
        GC_PAUSE_NS_MAX.fetch_max(pause_ns, Ordering::Relaxed);
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
    eprintln!(
        "[mutsu vm-stats] dual-store: clone_env={clone_env} (O(1) Arc bumps) env_deep_copies={deep_copy} (O(env) make_mut) env_flushes={env_flush} slots_flushed={slots}"
    );
    // GC Level 1a: candidate_pushes/dedup_hits are live as of §11 step 4
    // (nonzero only with MUTSU_GC=on once a Value variant is Gc-managed);
    // the collection counters stay zero until the collector lands (§11 step 8).
    let gc_collections = GC_COLLECTIONS.load(Ordering::Relaxed);
    let gc_candidate_pushes = GC_CANDIDATE_PUSHES.load(Ordering::Relaxed);
    let gc_dedup_hits = GC_CANDIDATE_DEDUP_HITS.load(Ordering::Relaxed);
    let gc_reclaimed_nodes = GC_RECLAIMED_NODES.load(Ordering::Relaxed);
    let gc_reclaimed_cycles = GC_RECLAIMED_CYCLES.load(Ordering::Relaxed);
    let gc_pause_ns_total = GC_PAUSE_NS_TOTAL.load(Ordering::Relaxed);
    let gc_pause_ns_max = GC_PAUSE_NS_MAX.load(Ordering::Relaxed);
    let gc_roots_scanned = GC_ROOTS_SCANNED.load(Ordering::Relaxed);
    // The ADR-0003 size trigger's effective threshold at exit (BASE unless a
    // collect adapted it; 0 = size trigger disabled). Observable proof of the
    // adaptive backoff for tests/operators.
    let gc_threshold = crate::gc::gc_current_size_threshold();
    eprintln!(
        "[mutsu vm-stats] gc: collections={gc_collections} candidate_pushes={gc_candidate_pushes} dedup_hits={gc_dedup_hits} reclaimed_nodes={gc_reclaimed_nodes} reclaimed_cycles={gc_reclaimed_cycles} pause_ns_total={gc_pause_ns_total} pause_ns_max={gc_pause_ns_max} roots_scanned={gc_roots_scanned} gc_threshold={gc_threshold}"
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
    if let Ok(map) = resolver_method_by_name().lock()
        && !map.is_empty()
    {
        let total: u64 = map.values().sum();
        let mut entries: Vec<(&String, &u64)> = map.iter().collect();
        entries.sort_by(|a, b| b.1.cmp(a.1).then_with(|| a.0.cmp(b.0)));
        let top: Vec<String> = entries
            .iter()
            .take(25)
            .map(|(name, count)| format!("{name}={count}"))
            .collect();
        eprintln!(
            "[mutsu vm-stats] resolver-path method dispatches total={} (resolve+setup; body runs compiled) by name (top {}): {}",
            total,
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
