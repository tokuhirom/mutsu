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

/// Per-opcode execution histogram (only populated when stats are on). Keyed by
/// the opcode's `Discriminant` so the per-instruction cost is one discriminant
/// hash + counter bump under the mutex; the human-readable variant name is
/// derived once per variant from the `Debug` representation (truncated at the
/// first non-identifier character). This is the empirical basis for
/// instruction-set decisions (which ops to fuse/shrink/remove) — see
/// docs/opcode-design-review.md.
#[allow(clippy::type_complexity)]
fn opcode_histogram()
-> &'static Mutex<HashMap<std::mem::Discriminant<crate::opcode::OpCode>, (String, u64)>> {
    static HIST: OnceLock<
        Mutex<HashMap<std::mem::Discriminant<crate::opcode::OpCode>, (String, u64)>>,
    > = OnceLock::new();
    HIST.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Per-name histogram of `resolve_function_with_types` invocations (the full
/// registry-scanning resolution walk). Only populated when stats are on. This
/// is the empirical basis for resolution-caching work: a name with thousands
/// of entries here is paying the candidate scan per call.
fn function_full_resolve_by_name() -> &'static Mutex<HashMap<String, u64>> {
    static BY_NAME: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();
    BY_NAME.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Record one `resolve_function_with_types` invocation.
#[inline]
pub(crate) fn record_function_full_resolve(name: &str) {
    if enabled()
        && let Ok(mut map) = function_full_resolve_by_name().lock()
    {
        *map.entry(name.to_string()).or_insert(0) += 1;
    }
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

// Constant-pool interning (ADR-0006 §2.4). `CONST_POOL_ADDS` counts every
// `CompiledCode::add_constant` call, `CONST_POOL_DEDUP_HITS` the ones that
// reused an existing slot instead of pushing a copy. Compile-time counters:
// they are fully accumulated before the program runs (plus whatever EVAL and
// runtime-compiled blocks add later).
static CONST_POOL_ADDS: AtomicU64 = AtomicU64::new(0);
static CONST_POOL_DEDUP_HITS: AtomicU64 = AtomicU64::new(0);

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

// ADR-0016 P2 diagnostics: how often a stored regex capture node
// (`Arc<RegexCaptures>`) is mutated through `Arc::make_mut` while shared
// (strong_count > 1), which deep-clones the entire descendant subtree. The
// `TOTAL` counter is every such make_mut; `SHARED` is the subset that actually
// copied. These quantify the reduce-walk / alias-action_name copy cost the
// CapNode split (ADR-0016 P2) removes structurally.
static REGEX_CAP_MAKEMUT_TOTAL: AtomicU64 = AtomicU64::new(0);
static REGEX_CAP_MAKEMUT_SHARED: AtomicU64 = AtomicU64::new(0);
// ADR-0016 P3: how many leaf captures reached the Match builder WITHOUT a
// span carrier (text-axis only — their offsets are reported as 0..len of the
// captured text, the position search having been retired), vs. leaves whose
// span came from a recorded carrier node. Non-zero `searches` means an
// exploded-builder caller still passes bare text.
static REGEX_MATCH_LEAF_SEARCHES: AtomicU64 = AtomicU64::new(0);
static REGEX_MATCH_LEAF_SPANS: AtomicU64 = AtomicU64::new(0);
// ADR-0016 P5 guard: every first `view()` of a lazy Match forces its
// Instance-shaped attribute map. This makes accidental `view()`-based tag
// probes visible in instrumented grammar/regex runs instead of silently
// eroding the lazy representation.
static REGEX_MATCH_MATERIALIZATIONS: AtomicU64 = AtomicU64::new(0);

// Regex embedded-code parse cache (REGEX_CODE_PARSE_CACHE) effectiveness.
static REGEX_CODE_PARSE_HITS: AtomicU64 = AtomicU64::new(0);
static REGEX_CODE_PARSE_MISSES: AtomicU64 = AtomicU64::new(0);

/// Record one lookup in the regex embedded-code parse cache.
#[inline]
pub(crate) fn record_regex_code_parse(hit: bool) {
    if enabled() {
        if hit {
            REGEX_CODE_PARSE_HITS.fetch_add(1, Ordering::Relaxed);
        } else {
            REGEX_CODE_PARSE_MISSES.fetch_add(1, Ordering::Relaxed);
        }
    }
}

/// Record one `Arc::make_mut` on a stored regex capture node; `shared` means
/// the node had other holders (strong_count > 1) so make_mut deep-copied it.
#[inline]
pub(crate) fn record_regex_cap_makemut(shared: bool) {
    if enabled() {
        REGEX_CAP_MAKEMUT_TOTAL.fetch_add(1, Ordering::Relaxed);
        if shared {
            REGEX_CAP_MAKEMUT_SHARED.fetch_add(1, Ordering::Relaxed);
        }
    }
}

/// A Match-builder leaf capture arrived without a span carrier (`searched ==
/// true` — the legacy text-only shape) or read a recorded span (`false`).
pub(crate) fn record_regex_match_leaf(searched: bool) {
    if enabled() {
        if searched {
            REGEX_MATCH_LEAF_SEARCHES.fetch_add(1, Ordering::Relaxed);
        } else {
            REGEX_MATCH_LEAF_SPANS.fetch_add(1, Ordering::Relaxed);
        }
    }
}

/// Record the first materialization of one lazy `Match` node.
#[inline]
pub(crate) fn record_regex_match_materialization() {
    if enabled() {
        REGEX_MATCH_MATERIALIZATIONS.fetch_add(1, Ordering::Relaxed);
    }
}

// Registry copy-on-write (docs/per-task-clone-slimming.md slice 1): counts
// the deep clones `Arc::make_mut` actually performs in `RegistryWriteGuard`,
// i.e. how often a registry write hit a still-shared `Arc<Registry>`. Should
// stay near-zero on a spawn-heavy benchmark where neither side writes the
// registry between spawns; a per-task count means some write path touches
// `registry_mut()` every spawn (see the plan doc's "stop and ask" condition).
static REGISTRY_COW_CLONES: AtomicU64 = AtomicU64::new(0);

/// Record one `Arc::make_mut` deep clone of the copy-on-write registry.
#[inline]
pub(crate) fn record_registry_cow_clone() {
    if enabled() {
        REGISTRY_COW_CLONES.fetch_add(1, Ordering::Relaxed);
    }
}

// Per-spawn lineage seeding (docs/per-task-clone-slimming.md slice 5 step A):
// `SPAWN_SEED_KEYS` counts env entries walked by the `clone_for_thread`
// seeding loop; `SPAWN_SEED_INSERTS` the subset that actually landed in the
// shared store (`declare` or a `seed_if_absent` that inserted). On a
// same-scope spawn loop, keys grows as env_size x spawns while inserts
// saturates at ~env_size — the gap quantifies the redundant re-walk that the
// (review-gated) step B generation skip would eliminate.
static SPAWN_SEED_KEYS: AtomicU64 = AtomicU64::new(0);
static SPAWN_SEED_INSERTS: AtomicU64 = AtomicU64::new(0);

/// Record one spawn's lineage-seeding walk: `keys` env entries walked,
/// `inserts` of them actually inserted into the shared store.
#[inline]
pub(crate) fn record_spawn_seeding(keys: u64, inserts: u64) {
    if enabled() {
        SPAWN_SEED_KEYS.fetch_add(keys, Ordering::Relaxed);
        SPAWN_SEED_INSERTS.fetch_add(inserts, Ordering::Relaxed);
    }
}

// ADR-0020 worker pool: `POOL_TASKS` counts every task submitted to the
// elastic pool; `POOL_SPAWNS` the subset that grew the pool (no idle worker
// at submit time). `tasks - spawns` is therefore the warm-reuse count the
// pool exists to maximize — on a short-task churn shape (ripemd) spawns
// should flatline at ~pool-floor while tasks keeps counting.
static POOL_TASKS: AtomicU64 = AtomicU64::new(0);
static POOL_SPAWNS: AtomicU64 = AtomicU64::new(0);

/// Record one task submitted to the ADR-0020 worker pool.
#[inline]
pub(crate) fn record_pool_task() {
    if enabled() {
        POOL_TASKS.fetch_add(1, Ordering::Relaxed);
    }
}

/// Record one pool growth (a task found no idle worker and spawned one).
#[inline]
pub(crate) fn record_pool_spawn() {
    if enabled() {
        POOL_SPAWNS.fetch_add(1, Ordering::Relaxed);
    }
}

// JIT (ADR-0004 layer 4) counters: how many chunks compiled to native code,
// how many body executions entered native code, and how many chunks bailed
// out (contain a not-yet-supported opcode; see `jit_bailout_by_opcode` for
// which opcode blocked them — the empirical basis for Tier A coverage work).
static JIT_COMPILES: AtomicU64 = AtomicU64::new(0);
static JIT_ENTRIES: AtomicU64 = AtomicU64::new(0);
static JIT_BAILOUTS: AtomicU64 = AtomicU64::new(0);

/// Per-opcode-name histogram of JIT bailout causes (first unsupported opcode
/// seen in each rejected chunk; only populated when stats are on).
fn jit_bailout_by_opcode() -> &'static Mutex<HashMap<String, u64>> {
    static BY_NAME: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();
    BY_NAME.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Record one chunk compiled to native code by the JIT.
#[inline]
pub(crate) fn record_jit_compile() {
    if enabled() {
        JIT_COMPILES.fetch_add(1, Ordering::Relaxed);
    }
}

/// Record one body execution entering JIT-compiled native code.
#[inline]
pub(crate) fn record_jit_entry() {
    if enabled() {
        JIT_ENTRIES.fetch_add(1, Ordering::Relaxed);
    }
}

/// Record a chunk rejected by the JIT because it contains `op` (the first
/// unsupported opcode encountered during the static scan).
#[inline]
pub(crate) fn record_jit_bailout(op: &crate::opcode::OpCode) {
    if enabled() {
        JIT_BAILOUTS.fetch_add(1, Ordering::Relaxed);
        let dbg = format!("{op:?}");
        let name: String = dbg
            .chars()
            .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
            .collect();
        if let Ok(mut map) = jit_bailout_by_opcode().lock() {
            *map.entry(name).or_insert(0) += 1;
        }
    }
}

// ADR-0019 D3-8a: how often `compile_method_def_in_place_with_dist` still
// compiles a method/submethod body at registration time — the throwaway
// per-registration compile that this box's main-pass `Compiler::compile_method_body`
// is meant to make obsolete for statically-named class/role methods (see
// `docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md`, D3 box).
// D3-8a itself only measures the baseline (nothing installs main-pass bytecode
// yet, so this stays nonzero); D3-8b/c's exit criterion is this counter
// reaching zero across a `t/` + roast S12/S14 sweep except for the
// enumerated dynamic shapes (`augment class`, `.^add_method`, computed names).
static METHOD_BODY_RUNTIME_COMPILES: AtomicU64 = AtomicU64::new(0);

/// Record one runtime (registration-time) compile of a method/submethod body
/// by `compile_method_def_in_place_with_dist`.
#[inline]
pub(crate) fn record_method_body_runtime_compile() {
    if enabled() {
        METHOD_BODY_RUNTIME_COMPILES.fetch_add(1, Ordering::Relaxed);
    }
}

// ADR-0019 Phase E box E1a: shadow-mode comparison of the new TypeId-based receiver
// classifier (`crate::runtime::receiver_class`) against the four dispatch sites'
// EXISTING string-based owner decisions. `OWNER_SHADOW_CHECKS`/`_MISMATCHES` are the
// totals across all four sites; `owner_shadow_mismatch_by_site` breaks mismatches down
// by `"<site> [old=... new=... definedness=... exec=...]"` so a bucket can be matched
// against the E1a PR's accepted-mismatch ledger. E1a's exit criterion is NOT a raw
// zero mismatch count -- it is that every bucket here is either zero or explained in
// that ledger (see `todo/deep/adr0019-e1-typeid-receiver-owner.md`). Nothing reads
// these counters to make a dispatch decision: shadow-only, zero behavior change.
static OWNER_SHADOW_CHECKS: AtomicU64 = AtomicU64::new(0);
static OWNER_SHADOW_MISMATCHES: AtomicU64 = AtomicU64::new(0);

fn owner_shadow_mismatch_by_site() -> &'static Mutex<HashMap<String, u64>> {
    static BY_SITE: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();
    BY_SITE.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Record one E1a shadow comparison at `site`. `detail` is only evaluated on a
/// mismatch (it formats the old/new owner and the classifier's definedness/exec, which
/// is otherwise wasted work on the — expected to be overwhelmingly common — match
/// path).
#[inline]
pub(crate) fn record_owner_shadow_check(
    site: &str,
    matched: bool,
    detail: impl FnOnce() -> String,
) {
    if !enabled() {
        return;
    }
    OWNER_SHADOW_CHECKS.fetch_add(1, Ordering::Relaxed);
    if !matched {
        OWNER_SHADOW_MISMATCHES.fetch_add(1, Ordering::Relaxed);
        if let Ok(mut map) = owner_shadow_mismatch_by_site().lock() {
            *map.entry(format!("{site} [{}]", detail())).or_insert(0) += 1;
        }
    }
}

// ADR-0019 Phase E box E2a: coverage gap between the native-method-row catalog
// (`crate::builtins::native_method_row`) and what the real `native_method_{0,1,2}arg`
// cascades actually recognize. Bumped whenever a cascade call returns `Some(..)`
// for an `(owner, name)` pair whose row does not admit that call's arity (an
// absent row, per `native_method_row`'s conservative default, counts as "does
// not admit any arity"). Every hit is a missing/wrong row for E2b to add —
// see `todo/deep/adr0019-e2-e4-resolver-core.md` decision 2's counter-to-zero
// discipline. `native_call_unmodeled_by_site` breaks hits down by
// `"<owner>x<name> [<call site>]"` so E2b can work through them file-by-file.
// Nothing reads this counter to make a dispatch decision: shadow-only, zero
// behavior change.
static NATIVE_CALL_UNMODELED: AtomicU64 = AtomicU64::new(0);

fn native_call_unmodeled_by_key() -> &'static Mutex<HashMap<String, u64>> {
    static BY_KEY: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();
    BY_KEY.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Record one E2a native-call recognition check at `site`: `owner`/`name` are
/// the receiver's dispatch owner and the resolved method name, `covered` is
/// whether the catalog row admits the arity the cascade just recognized the
/// call at.
#[inline]
pub(crate) fn record_native_call_recognition(site: &str, owner: &str, name: &str, covered: bool) {
    if !enabled() || covered {
        return;
    }
    NATIVE_CALL_UNMODELED.fetch_add(1, Ordering::Relaxed);
    if let Ok(mut map) = native_call_unmodeled_by_key().lock() {
        *map.entry(format!("{owner}x{name} [{site}]")).or_insert(0) += 1;
    }
}

// ADR-0019 Phase E box E4a: shadow comparison between
// `resolution_sequence::resolve_sequence`'s user-candidate winner and the existing
// `resolve_method_with_owner_impl` answer, at the two `resolve_method_cached`
// resolution boundaries (multi-cache miss and fresh resolve).
// `RESOLVER_SHADOW_CHECKS`/`_MISMATCHES` are the totals; `resolver_shadow_mismatch_by_site`
// breaks mismatches down by `"<site> [class=... method=... real=... shadow=...]"` for
// the E4a PR's accepted-mismatch ledger — the sequence builder does not yet model
// `resolve_method_with_owner_impl`'s early-stopping rule that a non-multi method
// resolves by name alone, independent of whether the call's arguments actually bind
// it (see `runtime::resolution_sequence`'s module doc and
// `todo/deep/adr0019-e2-e4-resolver-core.md`). Nothing reads these counters to make a
// dispatch decision: shadow-only, zero behavior change.
static RESOLVER_SHADOW_CHECKS: AtomicU64 = AtomicU64::new(0);
static RESOLVER_SHADOW_MISMATCHES: AtomicU64 = AtomicU64::new(0);

fn resolver_shadow_mismatch_by_site() -> &'static Mutex<HashMap<String, u64>> {
    static BY_SITE: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();
    BY_SITE.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Record one E4a shadow comparison at `site`. `detail` is only evaluated on a
/// mismatch, mirroring [`record_owner_shadow_check`].
#[inline]
pub(crate) fn record_resolver_shadow_check(
    site: &str,
    matched: bool,
    detail: impl FnOnce() -> String,
) {
    if !enabled() {
        return;
    }
    RESOLVER_SHADOW_CHECKS.fetch_add(1, Ordering::Relaxed);
    if !matched {
        RESOLVER_SHADOW_MISMATCHES.fetch_add(1, Ordering::Relaxed);
        if let Ok(mut map) = resolver_shadow_mismatch_by_site().lock() {
            *map.entry(format!("{site} [{}]", detail())).or_insert(0) += 1;
        }
    }
}

// ADR-0019 Phase E box E4b (step 1, scoping doc
// `todo/deep/adr0019-e4b-should-bypass-native-fastpath-decomposition.md`): shadow
// comparison between `should_bypass_native_fastpath`'s "does a user
// method/accessor/class-level-attr (or, for an Instance, a NativeCall binding) win"
// categories (2 and 3 in the scoping doc) and `resolve_user_method_or_accessor`'s
// single-MRO-walk answer, at the receiver's own class. `BYPASS_SHADOW_CHECKS`/
// `_MISMATCHES` are the totals; `bypass_shadow_mismatch_by_key` breaks mismatches
// down by `"[class=... method=... real=... shadow=...]"`. Nothing reads these
// counters to make a dispatch decision: shadow-only, zero behavior change.
static BYPASS_SHADOW_CHECKS: AtomicU64 = AtomicU64::new(0);
static BYPASS_SHADOW_MISMATCHES: AtomicU64 = AtomicU64::new(0);

fn bypass_shadow_mismatch_by_key() -> &'static Mutex<HashMap<String, u64>> {
    static BY_KEY: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();
    BY_KEY.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Record one E4b step-1 shadow comparison. `detail` is only evaluated on a
/// mismatch, mirroring [`record_resolver_shadow_check`].
#[inline]
pub(crate) fn record_bypass_shadow_check(matched: bool, detail: impl FnOnce() -> String) {
    if !enabled() {
        return;
    }
    BYPASS_SHADOW_CHECKS.fetch_add(1, Ordering::Relaxed);
    if !matched {
        BYPASS_SHADOW_MISMATCHES.fetch_add(1, Ordering::Relaxed);
        if let Ok(mut map) = bypass_shadow_mismatch_by_key().lock() {
            *map.entry(detail()).or_insert(0) += 1;
        }
    }
}

// ADR-0019 Phase E box E4b (step 4/9, design decision 4's `Native` row-catalog
// candidate): shadow comparison between `resolve_sequence`'s new `Native`
// candidate presence and whether `native_method_{0,1,2}arg` actually served
// the call -- a real, already-computed production result (`call_method_with_values`
// only calls this when the cascade was actually consulted, i.e.
// `!bypass_native_fastpath`), not a second invocation, so this carries no
// double-invocation side-effect risk even for a mutating row.
// `NATIVE_ROW_SHADOW_CHECKS`/`_MISMATCHES` are the totals; `native_row_shadow_mismatch_by_key`
// breaks mismatches down by `"method=... arity=... real=... shadow=... native_row_owner=..."`.
// Nothing reads these counters to make a dispatch decision: shadow-only, zero behavior change.
static NATIVE_ROW_SHADOW_CHECKS: AtomicU64 = AtomicU64::new(0);
static NATIVE_ROW_SHADOW_MISMATCHES: AtomicU64 = AtomicU64::new(0);

fn native_row_shadow_mismatch_by_key() -> &'static Mutex<HashMap<String, u64>> {
    static BY_KEY: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();
    BY_KEY.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Record one E4b step-4/9 shadow comparison. `detail` is only evaluated on a
/// mismatch, mirroring [`record_bypass_shadow_check`].
#[inline]
pub(crate) fn record_native_row_shadow_check(matched: bool, detail: impl FnOnce() -> String) {
    if !enabled() {
        return;
    }
    NATIVE_ROW_SHADOW_CHECKS.fetch_add(1, Ordering::Relaxed);
    if !matched {
        NATIVE_ROW_SHADOW_MISMATCHES.fetch_add(1, Ordering::Relaxed);
        if let Ok(mut map) = native_row_shadow_mismatch_by_key().lock() {
            *map.entry(detail()).or_insert(0) += 1;
        }
    }
}

// ADR-0019 Phase E box E7 step 4 (`.^can`, `todo/deep/adr0019-e5-e7-entry-
// routing.md` "E7 step 4"): shadow comparison between `collect_can_methods`'s
// existing dummy-`Value::NIL`-arg native probe and the new E2-row-catalog
// existence check (`Interpreter::e2_native_method_exists`), for the same
// `(receiver, method_name)` question. Deliberately a SEPARATE counter pair
// from `RESOLVER_SHADOW_*`/`NATIVE_ROW_SHADOW_*` above: those compare a
// dispatch-WINNER pick against `resolve_sequence`; this compares two
// EXISTENCE predicates that never invoke `resolve_sequence` at all, so
// mixing them into a shared total would repeat the "false lead" step 1 had
// to disentangle (see the ADR's E7 step 1 progress note). Nothing reads
// these counters to make a dispatch decision: shadow-only, zero behavior
// change.
static CAN_SHADOW_CHECKS: AtomicU64 = AtomicU64::new(0);
static CAN_SHADOW_MISMATCHES: AtomicU64 = AtomicU64::new(0);

fn can_shadow_mismatch_by_key() -> &'static Mutex<HashMap<String, u64>> {
    static BY_KEY: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();
    BY_KEY.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Record one E7 step-4 `.^can` shadow comparison. `detail` is only
/// evaluated on a mismatch, mirroring [`record_native_row_shadow_check`].
#[inline]
pub(crate) fn record_can_shadow_check(matched: bool, detail: impl FnOnce() -> String) {
    if !enabled() {
        return;
    }
    CAN_SHADOW_CHECKS.fetch_add(1, Ordering::Relaxed);
    if !matched {
        CAN_SHADOW_MISMATCHES.fetch_add(1, Ordering::Relaxed);
        if let Ok(mut map) = can_shadow_mismatch_by_key().lock() {
            *map.entry(detail()).or_insert(0) += 1;
        }
    }
}

// ADR-0019 Phase E box E7 step 6 (`.^methods`, `todo/deep/adr0019-e5-e7-
// entry-routing.md` "E7 step 6"): shadow comparison between the existing
// `class_mro`-based chain `dispatch_classhow_methods` walks to enumerate
// `.^methods()` and the E4 resolver's own canonical chain
// (`Interpreter::dispatch_owner_chain`) for the same receiver. A dedicated
// counter pair, not the shared `RESOLVER_SHADOW_*` infra: this compares two
// whole MRO CHAINS, not a single dispatch-winner pick, the same reasoning
// that kept E7 step 4's `.^can` check on its own `CAN_SHADOW_*` pair.
// Nothing reads these counters to make a dispatch decision: shadow-only,
// zero behavior change.
static METHODS_SHADOW_CHECKS: AtomicU64 = AtomicU64::new(0);
static METHODS_SHADOW_MISMATCHES: AtomicU64 = AtomicU64::new(0);

fn methods_shadow_mismatch_by_key() -> &'static Mutex<HashMap<String, u64>> {
    static BY_KEY: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();
    BY_KEY.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Record one E7 step-6 `.^methods` chain-shadow comparison. `detail` is only
/// evaluated on a mismatch, mirroring [`record_can_shadow_check`].
#[inline]
pub(crate) fn record_methods_shadow_check(matched: bool, detail: impl FnOnce() -> String) {
    if !enabled() {
        return;
    }
    METHODS_SHADOW_CHECKS.fetch_add(1, Ordering::Relaxed);
    if !matched {
        METHODS_SHADOW_MISMATCHES.fetch_add(1, Ordering::Relaxed);
        if let Ok(mut map) = methods_shadow_mismatch_by_key().lock() {
            *map.entry(detail()).or_insert(0) += 1;
        }
    }
}

// ADR-0019 Phase E box E7 step 7 (`.WALK`, `todo/deep/adr0019-e5-e7-entry-
// routing.md` "E7 step 7"): shadow comparison between the CLASS-kind portion
// of the chain `try_walk_method`'s default (`:canonical`) ordering walks
// (`Interpreter::class_mro_readonly`, via `build_walk_targets`) and the E4
// resolver's own canonical chain (`Interpreter::dispatch_owner_chain`) for
// the same receiver. Scoped to `:canonical`-order-only, unlike E7 step 6's
// `.^methods` check: WALK's OTHER orderings (`:super`/`:breadth`/`:ascendant`/
// `:descendant`) are legitimate alternate traversals documented by raku's own
// WALK spec, not MRO restatements, so comparing them against the resolver's
// MRO chain would be a guaranteed, meaningless mismatch. A dedicated counter
// pair for the same "whole MRO CHAIN, not a single dispatch-winner pick"
// reason as `CAN_SHADOW_*`/`METHODS_SHADOW_*`. Shadow-only, zero behavior
// change: `class_mro_readonly` alone still drives WALK's own chain.
static WALK_SHADOW_CHECKS: AtomicU64 = AtomicU64::new(0);
static WALK_SHADOW_MISMATCHES: AtomicU64 = AtomicU64::new(0);

fn walk_shadow_mismatch_by_key() -> &'static Mutex<HashMap<String, u64>> {
    static BY_KEY: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();
    BY_KEY.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Record one E7 step-7 `.WALK` (`:canonical` order only) chain-shadow
/// comparison. `detail` is only evaluated on a mismatch, mirroring
/// [`record_methods_shadow_check`].
#[inline]
pub(crate) fn record_walk_shadow_check(matched: bool, detail: impl FnOnce() -> String) {
    if !enabled() {
        return;
    }
    WALK_SHADOW_CHECKS.fetch_add(1, Ordering::Relaxed);
    if !matched {
        WALK_SHADOW_MISMATCHES.fetch_add(1, Ordering::Relaxed);
        if let Ok(mut map) = walk_shadow_mismatch_by_key().lock() {
            *map.entry(detail()).or_insert(0) += 1;
        }
    }
}

// ADR-0019 Phase E box E8a (`todo/deep/adr0019-e8-e11-candidate-sequence-
// semantics.md`, design decision 1): shadow comparison between the
// `resolve_sequence` candidate's new `level`/`stored_idx` fields -- filtered
// per-call and with the winner's fingerprint removed -- and the
// `nextsame`/`callsame` deferral list `push_method_dispatch_frame` builds
// today via `resolve_all_methods_with_owner` + fingerprint-based winner
// removal (`Interpreter::shadow_check_deferral_sequence`). A dedicated
// counter pair, not the shared `RESOLVER_SHADOW_*` infra: this compares an
// ORDERED LIST of remaining candidates, not a single dispatch-winner pick,
// the same reasoning that kept E7 steps 4/6/7's chain/existence checks on
// their own pairs. Nothing reads these counters to make a dispatch
// decision: shadow-only, zero behavior change.
static DEFERRAL_SHADOW_CHECKS: AtomicU64 = AtomicU64::new(0);
static DEFERRAL_SHADOW_MISMATCHES: AtomicU64 = AtomicU64::new(0);

fn deferral_shadow_mismatch_by_key() -> &'static Mutex<HashMap<String, u64>> {
    static BY_KEY: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();
    BY_KEY.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Record one E8a deferral-list shadow comparison. `detail` is only
/// evaluated on a mismatch, mirroring [`record_walk_shadow_check`].
#[inline]
pub(crate) fn record_deferral_shadow_check(matched: bool, detail: impl FnOnce() -> String) {
    if !enabled() {
        return;
    }
    DEFERRAL_SHADOW_CHECKS.fetch_add(1, Ordering::Relaxed);
    if !matched {
        DEFERRAL_SHADOW_MISMATCHES.fetch_add(1, Ordering::Relaxed);
        if let Ok(mut map) = deferral_shadow_mismatch_by_key().lock() {
            *map.entry(detail()).or_insert(0) += 1;
        }
    }
}

// ADR-0019 Phase E box E8b (`todo/deep/adr0019-e8-e11-candidate-sequence-
// semantics.md`): shadow comparison between `Interpreter::
// lookup_proto_method`'s real MRO walk over the standalone `Registry::
// proto_methods` table and the same walk read against the new `MethodEntry.
// proto` column (`Registry::method_entry_proto`) both tables are written to
// together by `Registry::set_proto_method`. A dedicated counter pair (not
// `RESOLVER_SHADOW_*`) for the same reason every other Phase E probe family
// gets its own pair: this measures one specific consolidation, not a general
// resolver-winner comparison. Nothing reads these counters to make a
// dispatch decision: shadow-only, zero behavior change.
static PROTO_METHOD_SHADOW_CHECKS: AtomicU64 = AtomicU64::new(0);
static PROTO_METHOD_SHADOW_MISMATCHES: AtomicU64 = AtomicU64::new(0);

fn proto_method_shadow_mismatch_by_key() -> &'static Mutex<HashMap<String, u64>> {
    static BY_KEY: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();
    BY_KEY.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Record one E8b proto-method shadow comparison. `detail` is only
/// evaluated on a mismatch, mirroring [`record_deferral_shadow_check`].
#[inline]
pub(crate) fn record_proto_method_shadow_check(matched: bool, detail: impl FnOnce() -> String) {
    if !enabled() {
        return;
    }
    PROTO_METHOD_SHADOW_CHECKS.fetch_add(1, Ordering::Relaxed);
    if !matched {
        PROTO_METHOD_SHADOW_MISMATCHES.fetch_add(1, Ordering::Relaxed);
        if let Ok(mut map) = proto_method_shadow_mismatch_by_key().lock() {
            *map.entry(detail()).or_insert(0) += 1;
        }
    }
}

// ADR-0024: mainline named subs resolving free variables through
// unit-lexical cells. `MAINLINE_LEXICAL_BOXES` counts every NEW `ContainerRef`
// cell created by `exec_register_sub_op`'s mainline capture (registration
// time, one-time per declaration; reusing an already-boxed cell does NOT
// bump this). `MAINLINE_LEXICAL_HITS` counts every successful resolution
// through the mainline candidate in `unit_lexical_slot` (reads and, via
// `unit_scope_lexical_write` sharing the same resolver, writes). The ADR's
// "measured cost basis" claims `MAINLINE_LEXICAL_BOXES == 0` across
// `benchmarks/*.raku` (no mainline sub there captures a `my` scalar) — verify
// with `MUTSU_VM_STATS=1` after any change here.
static MAINLINE_LEXICAL_BOXES: AtomicU64 = AtomicU64::new(0);
static MAINLINE_LEXICAL_HITS: AtomicU64 = AtomicU64::new(0);

/// Record one NEW mainline `my` scalar boxed into a shared cell at named-sub
/// registration time (ADR-0024 §2). Not incremented when an already-boxed
/// cell is merely reused by a sibling sub capturing the same name.
#[inline]
pub(crate) fn record_mainline_lexical_box() {
    if enabled() {
        MAINLINE_LEXICAL_BOXES.fetch_add(1, Ordering::Relaxed);
    }
}

/// Record one successful resolution through the mainline candidate in
/// `unit_lexical_slot` (ADR-0024 §3).
#[inline]
pub(crate) fn record_mainline_lexical_hit() {
    if enabled() {
        MAINLINE_LEXICAL_HITS.fetch_add(1, Ordering::Relaxed);
    }
}

// ADR-0019 Phase E box E5 (measurement slice, design decision 3 in
// `todo/deep/adr0019-e5-e7-entry-routing.md`): per-entry, per-outcome dispatch
// counters for the VM call entries (`CallMethod`, and in later slices
// `CallMethodMut`, `CallMethodDynamic`, the hyper entries, ...). Each entry
// records exactly one outcome per executed dispatch — `intercept` (a
// method-identity special-case arm fully handled the call before the general
// probes), `native` (the `try_native_method` cascade served it), `user` (the
// compiled/interpreted user-method fallthrough ran), `accessor` (the fast
// 0-arg public-attribute read served it), or `notfound` (an explicitly
// observed X::Method::NotFound completion; see the taxonomy table in the E5-E7
// doc for the one documented overlap with `user`). The by-key histogram is
// keyed `"<entry>:<outcome>"` so every future E5/E6 entry reuses this family
// instead of growing bespoke statics; the by-arm histogram is keyed
// `"<entry>:<arm-name>"` and is what lets the sweep identify dead/near-dead
// intercepts (design decision 3(ii): deletion candidates rather than porting
// targets). The sweep over full `t/` + whitelisted roast decides sub-slice
// order (3(i)) and the parity corpus for each cutover (3(iii)). Nothing reads
// these counters to make a dispatch decision: measurement-only, zero behavior
// change.
fn dispatch_entry_outcome_by_key() -> &'static Mutex<HashMap<String, u64>> {
    static BY_KEY: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();
    BY_KEY.get_or_init(|| Mutex::new(HashMap::new()))
}

fn dispatch_entry_intercept_by_arm() -> &'static Mutex<HashMap<String, u64>> {
    static BY_ARM: OnceLock<Mutex<HashMap<String, u64>>> = OnceLock::new();
    BY_ARM.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Record one dispatch outcome for VM call entry `entry` (e.g. `"callmethod"`)
/// with `outcome` one of `intercept`/`native`/`user`/`accessor`/`notfound`.
#[inline]
pub(crate) fn record_dispatch_entry_outcome(entry: &str, outcome: &str) {
    if !enabled() {
        return;
    }
    if let Ok(mut map) = dispatch_entry_outcome_by_key().lock() {
        *map.entry(format!("{entry}:{outcome}")).or_insert(0) += 1;
    }
}

/// Record one method-identity intercept at VM call entry `entry`: bumps the
/// `intercept` outcome via [`record_dispatch_entry_outcome`] AND the per-arm
/// histogram under `"<entry>:<arm>"` (short stable arm names, e.g.
/// `"callmethod:pair-freeze"`).
#[inline]
pub(crate) fn record_dispatch_entry_intercept(entry: &str, arm: &str) {
    if !enabled() {
        return;
    }
    record_dispatch_entry_outcome(entry, "intercept");
    if let Ok(mut map) = dispatch_entry_intercept_by_arm().lock() {
        *map.entry(format!("{entry}:{arm}")).or_insert(0) += 1;
    }
}

/// Whether instrumentation is active. Resolved once from the environment so the
/// hot path is a single cached boolean load when the feature is off.
#[inline]
pub(crate) fn enabled() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("MUTSU_VM_STATS").is_some())
}

/// Record one bytecode instruction dispatch (called from `exec_one` when stats
/// are on). Counts are exact and deterministic; the mutex + hash cost only
/// exists under `MUTSU_VM_STATS=1`, so use the counts (not wall-clock) from
/// instrumented runs.
#[inline]
pub(crate) fn record_opcode(op: &crate::opcode::OpCode) {
    if !enabled() {
        return;
    }
    let d = std::mem::discriminant(op);
    if let Ok(mut map) = opcode_histogram().lock() {
        match map.get_mut(&d) {
            Some(entry) => entry.1 += 1,
            None => {
                // First time this variant is seen: derive its bare name from the
                // Debug output (`ForLoop(..)` / `WhileLoop { .. }` -> `ForLoop`).
                let dbg = format!("{op:?}");
                let name: String = dbg
                    .chars()
                    .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
                    .collect();
                map.insert(d, (name, 1));
            }
        }
    }
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
/// Record one `add_constant` call; `deduped` = it reused an existing pool slot.
pub(crate) fn record_const_add(deduped: bool) {
    if enabled() {
        CONST_POOL_ADDS.fetch_add(1, Ordering::Relaxed);
        if deduped {
            CONST_POOL_DEDUP_HITS.fetch_add(1, Ordering::Relaxed);
        }
    }
}

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
    let const_adds = CONST_POOL_ADDS.load(Ordering::Relaxed);
    let const_hits = CONST_POOL_DEDUP_HITS.load(Ordering::Relaxed);
    let const_pct = if const_adds == 0 {
        0.0
    } else {
        const_hits as f64 * 100.0 / const_adds as f64
    };
    eprintln!(
        "[mutsu vm-stats] const-pool: add_constant={const_adds} dedup_hits={const_hits} ({const_pct:.1}% shared a slot)"
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
    let cap_makemut_total = REGEX_CAP_MAKEMUT_TOTAL.load(Ordering::Relaxed);
    let cap_makemut_shared = REGEX_CAP_MAKEMUT_SHARED.load(Ordering::Relaxed);
    let leaf_searches = REGEX_MATCH_LEAF_SEARCHES.load(Ordering::Relaxed);
    let leaf_spans = REGEX_MATCH_LEAF_SPANS.load(Ordering::Relaxed);
    let match_materializations = REGEX_MATCH_MATERIALIZATIONS.load(Ordering::Relaxed);
    eprintln!(
        "[mutsu vm-stats] regex-captures: cap_makemut={cap_makemut_total} shared_deep_copies={cap_makemut_shared} leaf_searches={leaf_searches} leaf_spans={leaf_spans} match_materializations={match_materializations}"
    );
    let code_parse_hits = REGEX_CODE_PARSE_HITS.load(Ordering::Relaxed);
    let code_parse_misses = REGEX_CODE_PARSE_MISSES.load(Ordering::Relaxed);
    eprintln!(
        "[mutsu vm-stats] regex-code-parse-cache: hits={code_parse_hits} misses={code_parse_misses}"
    );
    let registry_cow_clones = REGISTRY_COW_CLONES.load(Ordering::Relaxed);
    eprintln!("[mutsu vm-stats] registry-cow: clones={registry_cow_clones}");
    let mainline_lexical_boxes = MAINLINE_LEXICAL_BOXES.load(Ordering::Relaxed);
    let mainline_lexical_hits = MAINLINE_LEXICAL_HITS.load(Ordering::Relaxed);
    eprintln!(
        "[mutsu vm-stats] adr0024-mainline-lexicals: boxes={mainline_lexical_boxes} hits={mainline_lexical_hits}"
    );
    let owner_shadow_checks = OWNER_SHADOW_CHECKS.load(Ordering::Relaxed);
    let owner_shadow_mismatches = OWNER_SHADOW_MISMATCHES.load(Ordering::Relaxed);
    eprintln!(
        "[mutsu vm-stats] adr0019-e1a: owner_shadow_checks={owner_shadow_checks} owner_shadow_mismatches={owner_shadow_mismatches}"
    );
    if let Ok(map) = owner_shadow_mismatch_by_site().lock()
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
            "[mutsu vm-stats] adr0019-e1a owner-shadow mismatches by site (top {}): {}",
            top.len(),
            top.join(" ")
        );
    }
    let native_call_unmodeled = NATIVE_CALL_UNMODELED.load(Ordering::Relaxed);
    eprintln!("[mutsu vm-stats] adr0019-e2a: native_call_unmodeled={native_call_unmodeled}");
    if let Ok(map) = native_call_unmodeled_by_key().lock()
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
            "[mutsu vm-stats] adr0019-e2a native_call_unmodeled by (owner x name [site]) (top {}): {}",
            top.len(),
            top.join(" ")
        );
    }
    let resolver_shadow_checks = RESOLVER_SHADOW_CHECKS.load(Ordering::Relaxed);
    let resolver_shadow_mismatches = RESOLVER_SHADOW_MISMATCHES.load(Ordering::Relaxed);
    eprintln!(
        "[mutsu vm-stats] adr0019-e4a: resolver_shadow_checks={resolver_shadow_checks} resolver_shadow_mismatches={resolver_shadow_mismatches}"
    );
    if let Ok(map) = resolver_shadow_mismatch_by_site().lock()
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
            "[mutsu vm-stats] adr0019-e4a resolver-shadow mismatches by site (top {}): {}",
            top.len(),
            top.join(" ")
        );
    }
    let bypass_shadow_checks = BYPASS_SHADOW_CHECKS.load(Ordering::Relaxed);
    let bypass_shadow_mismatches = BYPASS_SHADOW_MISMATCHES.load(Ordering::Relaxed);
    eprintln!(
        "[mutsu vm-stats] adr0019-e4b: bypass_shadow_checks={bypass_shadow_checks} bypass_shadow_mismatches={bypass_shadow_mismatches}"
    );
    if let Ok(map) = bypass_shadow_mismatch_by_key().lock()
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
            "[mutsu vm-stats] adr0019-e4b bypass-shadow mismatches (top {}): {}",
            top.len(),
            top.join(" ")
        );
    }
    let native_row_shadow_checks = NATIVE_ROW_SHADOW_CHECKS.load(Ordering::Relaxed);
    let native_row_shadow_mismatches = NATIVE_ROW_SHADOW_MISMATCHES.load(Ordering::Relaxed);
    eprintln!(
        "[mutsu vm-stats] adr0019-e4b: native_row_shadow_checks={native_row_shadow_checks} native_row_shadow_mismatches={native_row_shadow_mismatches}"
    );
    if let Ok(map) = native_row_shadow_mismatch_by_key().lock()
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
            "[mutsu vm-stats] adr0019-e4b native-row-shadow mismatches (top {}): {}",
            top.len(),
            top.join(" ")
        );
    }
    let can_shadow_checks = CAN_SHADOW_CHECKS.load(Ordering::Relaxed);
    let can_shadow_mismatches = CAN_SHADOW_MISMATCHES.load(Ordering::Relaxed);
    eprintln!(
        "[mutsu vm-stats] adr0019-e7: can_shadow_checks={can_shadow_checks} can_shadow_mismatches={can_shadow_mismatches}"
    );
    if let Ok(map) = can_shadow_mismatch_by_key().lock()
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
            "[mutsu vm-stats] adr0019-e7 can-shadow mismatches (top {}): {}",
            top.len(),
            top.join(" ")
        );
    }
    let methods_shadow_checks = METHODS_SHADOW_CHECKS.load(Ordering::Relaxed);
    let methods_shadow_mismatches = METHODS_SHADOW_MISMATCHES.load(Ordering::Relaxed);
    eprintln!(
        "[mutsu vm-stats] adr0019-e7: methods_shadow_checks={methods_shadow_checks} methods_shadow_mismatches={methods_shadow_mismatches}"
    );
    if let Ok(map) = methods_shadow_mismatch_by_key().lock()
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
            "[mutsu vm-stats] adr0019-e7 methods-shadow mismatches (top {}): {}",
            top.len(),
            top.join(" ")
        );
    }
    let walk_shadow_checks = WALK_SHADOW_CHECKS.load(Ordering::Relaxed);
    let walk_shadow_mismatches = WALK_SHADOW_MISMATCHES.load(Ordering::Relaxed);
    eprintln!(
        "[mutsu vm-stats] adr0019-e7: walk_shadow_checks={walk_shadow_checks} walk_shadow_mismatches={walk_shadow_mismatches}"
    );
    if let Ok(map) = walk_shadow_mismatch_by_key().lock()
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
            "[mutsu vm-stats] adr0019-e7 walk-shadow mismatches (top {}): {}",
            top.len(),
            top.join(" ")
        );
    }
    let deferral_shadow_checks = DEFERRAL_SHADOW_CHECKS.load(Ordering::Relaxed);
    let deferral_shadow_mismatches = DEFERRAL_SHADOW_MISMATCHES.load(Ordering::Relaxed);
    eprintln!(
        "[mutsu vm-stats] adr0019-e8a: deferral_shadow_checks={deferral_shadow_checks} deferral_shadow_mismatches={deferral_shadow_mismatches}"
    );
    if let Ok(map) = deferral_shadow_mismatch_by_key().lock()
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
            "[mutsu vm-stats] adr0019-e8a deferral-shadow mismatches (top {}): {}",
            top.len(),
            top.join(" ")
        );
    }
    let proto_method_shadow_checks = PROTO_METHOD_SHADOW_CHECKS.load(Ordering::Relaxed);
    let proto_method_shadow_mismatches = PROTO_METHOD_SHADOW_MISMATCHES.load(Ordering::Relaxed);
    eprintln!(
        "[mutsu vm-stats] adr0019-e8b: proto_method_shadow_checks={proto_method_shadow_checks} proto_method_shadow_mismatches={proto_method_shadow_mismatches}"
    );
    if let Ok(map) = proto_method_shadow_mismatch_by_key().lock()
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
            "[mutsu vm-stats] adr0019-e8b proto-method-shadow mismatches (top {}): {}",
            top.len(),
            top.join(" ")
        );
    }
    if let Ok(map) = dispatch_entry_outcome_by_key().lock()
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
            "[mutsu vm-stats] adr0019-e5 dispatch-entry outcomes total={} (top {}): {}",
            total,
            top.len(),
            top.join(" ")
        );
    }
    if let Ok(map) = dispatch_entry_intercept_by_arm().lock()
        && !map.is_empty()
    {
        let total: u64 = map.values().sum();
        let mut entries: Vec<(&String, &u64)> = map.iter().collect();
        entries.sort_by(|a, b| b.1.cmp(a.1).then_with(|| a.0.cmp(b.0)));
        let top: Vec<String> = entries
            .iter()
            .take(40)
            .map(|(name, count)| format!("{name}={count}"))
            .collect();
        eprintln!(
            "[mutsu vm-stats] adr0019-e5 intercept arms total={} (top {}): {}",
            total,
            top.len(),
            top.join(" ")
        );
    }
    let method_body_runtime_compiles = METHOD_BODY_RUNTIME_COMPILES.load(Ordering::Relaxed);
    eprintln!(
        "[mutsu vm-stats] adr0019-d3-8: method_body_runtime_compiles={method_body_runtime_compiles} (registration-time compiles the main-pass compiler should make unnecessary)"
    );
    let spawn_seed_keys = SPAWN_SEED_KEYS.load(Ordering::Relaxed);
    let spawn_seed_inserts = SPAWN_SEED_INSERTS.load(Ordering::Relaxed);
    eprintln!(
        "[mutsu vm-stats] spawn-seeding: keys_walked={spawn_seed_keys} inserts={spawn_seed_inserts}"
    );
    let pool_tasks = POOL_TASKS.load(Ordering::Relaxed);
    let pool_spawns = POOL_SPAWNS.load(Ordering::Relaxed);
    eprintln!(
        "[mutsu vm-stats] worker-pool: tasks={pool_tasks} spawns={pool_spawns} warm_reuses={}",
        pool_tasks.saturating_sub(pool_spawns)
    );
    let jit_compiles = JIT_COMPILES.load(Ordering::Relaxed);
    let jit_entries = JIT_ENTRIES.load(Ordering::Relaxed);
    let jit_bailouts = JIT_BAILOUTS.load(Ordering::Relaxed);
    // Tier B GetLocal fast-path spoiler latches (J4d): nonzero means every
    // inline local read fell back to the shim for the rest of the run.
    let cells = crate::vm::vm_jit::CONTAINER_CELLS.load(Ordering::Relaxed);
    let caller_binds = crate::vm::vm_jit::CALLER_VAR_BINDS.load(Ordering::Relaxed);
    eprintln!(
        "[mutsu vm-stats] jit: compiles={jit_compiles} entries={jit_entries} bailouts={jit_bailouts} container_cells={cells} caller_binds={caller_binds}"
    );
    if let Ok(map) = jit_bailout_by_opcode().lock()
        && !map.is_empty()
    {
        let mut entries: Vec<(&String, &u64)> = map.iter().collect();
        entries.sort_by(|a, b| b.1.cmp(a.1).then_with(|| a.0.cmp(b.0)));
        let top: Vec<String> = entries
            .iter()
            .take(20)
            .map(|(name, count)| format!("{name}={count}"))
            .collect();
        eprintln!(
            "[mutsu vm-stats] jit bailout opcodes (top {}): {}",
            top.len(),
            top.join(" ")
        );
    }
    if let Ok(map) = opcode_histogram().lock()
        && !map.is_empty()
    {
        let total: u64 = map.values().map(|(_, c)| c).sum();
        let mut entries: Vec<(&String, &u64)> = map.values().map(|(n, c)| (n, c)).collect();
        entries.sort_by(|a, b| b.1.cmp(a.1).then_with(|| a.0.cmp(b.0)));
        let top: Vec<String> = entries
            .iter()
            .take(30)
            .map(|(name, count)| format!("{name}={count}"))
            .collect();
        eprintln!(
            "[mutsu vm-stats] opcodes executed total={} distinct={} (top {}): {}",
            total,
            map.len(),
            top.len(),
            top.join(" ")
        );
    }
    if let Ok(map) = function_full_resolve_by_name().lock()
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
            "[mutsu vm-stats] function-full-resolve total={} by name (top {}): {}",
            total,
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
