//! GC safepoint wiring (ADR-0001 §1.1, `docs/gc-level1-detailed-design.md`
//! §1.2 / §9.2 / §9.2a / §11 step 8 second half).
//!
//! The collector ([`super::collect::collect_cycles_at`]) may only run at a
//! *re-entry boundary* — a point holding no long borrow / lock / `gc_contents_mut`
//! (design doc §1.2). This module holds the trigger policy and the one hot-path
//! entry point ([`gc_safepoint`]) the VM calls at those boundaries.
//!
//! ## Triggers (design doc §9.2)
//! - `MUTSU_GC=off` disables everything: [`armed`] is `false`, so the VM's
//!   safepoint check is a single relaxed load that early-returns — execution
//!   pays essentially nothing and never collects. Unset means **on**
//!   (ADR-0003 §5) with the production size-threshold trigger below.
//! - `MUTSU_GC=on` + `MUTSU_GC_EVERY_SAFEPOINT=1` — collect at *every* safepoint
//!   (deterministic stress; §9.2).
//! - `MUTSU_GC=on` + `MUTSU_GC_EVERY_CANDIDATE=N` — arm a pending collect once
//!   every `N` candidate pushes; it runs at the next safepoint (never inline in
//!   `Gc::drop`, which may hold a borrow — §1.2).
//! - `MUTSU_GC=on` + `MUTSU_GC_AT=call,return,...` — collect at every safepoint
//!   of the listed kinds only (the [`SafepointKind`] names).
//! - `MUTSU_GC=on` + `MUTSU_GC_RANDOM_RATE=0.0..1.0` — collect at each safepoint
//!   with the given probability, from a seeded deterministic PRNG.
//!   `MUTSU_GC_RANDOM_SEED=<u64>` fixes the seed; unset derives it from the
//!   startup clock. The seed is always logged so a failing run can be replayed
//!   (§9.2 "seed を必ずログに出す").
//! - `MUTSU_GC=on` + `MUTSU_GC_COLLECT_NOW=1` — one collect right at program
//!   start ([`startup_collect_if_requested`], called from `Interpreter::run`).
//!
//! ## Production trigger (ADR-0003, on by default under `MUTSU_GC=on`)
//! - Candidate-buffer **size** threshold with adaptive backoff: a collect is
//!   armed when the buffer reaches the effective threshold, and after each
//!   collect `threshold = clamp(BASE, 2 × survivors, MAX)` — a scan that
//!   mostly re-proved liveness backs off (the fixed-period rescan of a large
//!   live suspect set is quadratic; measured on S17-lowlevel/cas-loop.t), a
//!   productive scan falls back toward BASE. `MUTSU_GC_THRESHOLD` sets BASE
//!   (default 16384; `0` disables the size trigger). Same production shape as
//!   PHP 7.3's Zend GC (root-buffer threshold that raises itself on
//!   unproductive collects).

use std::sync::OnceLock;
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};

use super::collect::collect_cycles_at;
use super::gc_ptr::gc_enabled;

/// A re-entry boundary at which a collect may run (design doc §9.2a). All
/// kinds are emitted: `Backedge` on both dispatch loops, the rest at their
/// §9.2a boundary (call-frame push/pop, promise/channel blocking receive,
/// react drive-loop poll, lazy-list force, nested-register entry, hyper/race
/// join merge). `Manual` is the explicit-collect reason (`MUTSU_GC_COLLECT_NOW`,
/// debug hooks).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum SafepointKind {
    /// Bytecode dispatch loop backward edge (between instructions).
    Backedge,
    /// Call-frame push boundary.
    Call,
    /// Call-frame pop / return-merge boundary.
    Return,
    /// Promise/Channel await poll/resume boundary.
    Await,
    /// react/supply drive-loop poll unit.
    ReactPoll,
    /// `force_lazy_list*` pull/resume boundary.
    LazyForce,
    /// `with_nested_registers` nested-VM entry/exit.
    NestedRun,
    /// `start`/`hyper`/`race` join/merge boundary.
    ThreadJoin,
    /// Explicit debug / manual collect.
    Manual,
}

impl SafepointKind {
    /// Short stable name, used as the collect `reason` in `MUTSU_GC_LOG` output
    /// and the string `MUTSU_GC_AT` matches.
    fn name(self) -> &'static str {
        match self {
            SafepointKind::Backedge => "backedge",
            SafepointKind::Call => "call",
            SafepointKind::Return => "return",
            SafepointKind::Await => "await",
            SafepointKind::ReactPoll => "react_poll",
            SafepointKind::LazyForce => "lazy_force",
            SafepointKind::NestedRun => "nested_run",
            SafepointKind::ThreadJoin => "thread_join",
            SafepointKind::Manual => "manual",
        }
    }

    /// Bit for the `MUTSU_GC_AT` kind mask.
    fn bit(self) -> u16 {
        1 << (self as u16)
    }

    fn from_name(name: &str) -> Option<SafepointKind> {
        Some(match name {
            "backedge" => SafepointKind::Backedge,
            "call" => SafepointKind::Call,
            "return" => SafepointKind::Return,
            "await" => SafepointKind::Await,
            "react_poll" => SafepointKind::ReactPoll,
            "lazy_force" => SafepointKind::LazyForce,
            "nested_run" => SafepointKind::NestedRun,
            "thread_join" => SafepointKind::ThreadJoin,
            "manual" => SafepointKind::Manual,
            _ => return None,
        })
    }
}

/// Resolved trigger policy, read once from the environment.
struct Triggers {
    /// GC on AND at least one automatic trigger configured. When `false`, the
    /// hot-path [`armed`] check early-returns.
    armed: bool,
    every_safepoint: bool,
    every_candidate: usize,
    /// `MUTSU_GC_AT` kind mask (0 = unset).
    at_mask: u16,
    /// `MUTSU_GC_RANDOM_RATE` as `f64` bits (0 = random stress off).
    random_rate_bits: u64,
    /// Production trigger (ADR-0003): BASE of the candidate-buffer size
    /// threshold. Defaults ON whenever `MUTSU_GC=on` (`MUTSU_GC_THRESHOLD`
    /// overrides; `0` disables). 0 here = size trigger off.
    threshold_base: usize,
}

/// Default BASE for the ADR-0003 size threshold (starting point pending the
/// acceptance measurements; tune via `MUTSU_GC_THRESHOLD`).
const THRESHOLD_BASE_DEFAULT: usize = 16384;
/// Upper clamp for the adaptive threshold: bounds worst-case buffer memory
/// while still backing far out of rescan churn on huge live suspect sets.
const THRESHOLD_MAX: usize = 1 << 20;

impl Triggers {
    fn from_env() -> Triggers {
        if !gc_enabled() {
            return Triggers {
                armed: false,
                every_safepoint: false,
                every_candidate: 0,
                at_mask: 0,
                random_rate_bits: 0,
                threshold_base: 0,
            };
        }
        let every_safepoint = parse_flag("MUTSU_GC_EVERY_SAFEPOINT");
        let every_candidate = parse_count("MUTSU_GC_EVERY_CANDIDATE");
        let at_mask = parse_at_mask("MUTSU_GC_AT");
        let random_rate_bits = init_random("MUTSU_GC_RANDOM_RATE", "MUTSU_GC_RANDOM_SEED");
        let threshold_base = match std::env::var("MUTSU_GC_THRESHOLD").ok().as_deref() {
            None => THRESHOLD_BASE_DEFAULT,
            Some(s) => s.parse::<usize>().unwrap_or_else(|_| {
                eprintln!(
                    "[mutsu gc] warning: unrecognized MUTSU_GC_THRESHOLD={s:?}, using default"
                );
                THRESHOLD_BASE_DEFAULT
            }),
        };
        Triggers {
            armed: every_safepoint
                || every_candidate > 0
                || at_mask != 0
                || random_rate_bits != 0
                || threshold_base > 0,
            every_safepoint,
            every_candidate,
            at_mask,
            random_rate_bits,
            threshold_base,
        }
    }
}

/// `1` = on; `0`/unset = off; anything else warns once and is off (design §9.1a).
fn parse_flag(var: &str) -> bool {
    match std::env::var(var).ok().as_deref() {
        Some("1") => true,
        None | Some("0") => false,
        Some(other) => {
            eprintln!("[mutsu gc] warning: unrecognized {var}={other:?}, treating as 0");
            false
        }
    }
}

/// `0`/unset = off; a positive integer = the every-N threshold; a bad value
/// warns once and is off.
fn parse_count(var: &str) -> usize {
    match std::env::var(var).ok().as_deref() {
        None => 0,
        Some(s) => s.parse::<usize>().unwrap_or_else(|_| {
            eprintln!("[mutsu gc] warning: unrecognized {var}={s:?}, treating as 0");
            0
        }),
    }
}

/// Comma-separated [`SafepointKind`] names -> kind bitmask. An unknown name
/// warns and is skipped (the rest of the list still applies).
fn parse_at_mask(var: &str) -> u16 {
    let Some(s) = std::env::var(var).ok() else {
        return 0;
    };
    let mut mask = 0u16;
    for name in s.split(',').map(str::trim).filter(|n| !n.is_empty()) {
        match SafepointKind::from_name(name) {
            Some(kind) => mask |= kind.bit(),
            None => {
                eprintln!("[mutsu gc] warning: unknown {var} safepoint kind {name:?}, skipping");
            }
        }
    }
    mask
}

/// Global PRNG state for the random stress trigger (splitmix64: `fetch_add` of
/// the golden gamma gives each draw a distinct counter; the finalizer mixes it).
static RNG_STATE: AtomicU64 = AtomicU64::new(0);

const SPLITMIX_GAMMA: u64 = 0x9E37_79B9_7F4A_7C15;

/// Parse the random-stress rate and seed. Returns the rate as `f64` bits
/// (0 = off). The seed — explicit or clock-derived — is always logged so a
/// failing stress run can be replayed with `MUTSU_GC_RANDOM_SEED` (§9.2).
fn init_random(rate_var: &str, seed_var: &str) -> u64 {
    let Some(rate_s) = std::env::var(rate_var).ok() else {
        return 0;
    };
    let rate = match rate_s.parse::<f64>() {
        Ok(r) if (0.0..=1.0).contains(&r) => r,
        _ => {
            eprintln!(
                "[mutsu gc] warning: unrecognized {rate_var}={rate_s:?} (want 0.0..=1.0), treating as 0"
            );
            return 0;
        }
    };
    if rate == 0.0 {
        return 0;
    }
    let seed = match std::env::var(seed_var).ok() {
        Some(s) => match s.parse::<u64>() {
            Ok(seed) => seed,
            Err(_) => {
                eprintln!("[mutsu gc] warning: unrecognized {seed_var}={s:?}, deriving from clock");
                clock_seed()
            }
        },
        None => clock_seed(),
    };
    RNG_STATE.store(seed, Ordering::Relaxed);
    eprintln!("[mutsu gc] random stress: rate={rate} seed={seed}");
    rate.to_bits()
}

fn clock_seed() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(0x5EED)
}

/// One seeded splitmix64 draw; `true` with probability `rate`.
fn random_fires(rate_bits: u64) -> bool {
    let rate = f64::from_bits(rate_bits);
    let mut x = RNG_STATE
        .fetch_add(SPLITMIX_GAMMA, Ordering::Relaxed)
        .wrapping_add(SPLITMIX_GAMMA);
    x = (x ^ (x >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    x = (x ^ (x >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    x ^= x >> 31;
    // 53 uniform mantissa bits -> [0, 1).
    ((x >> 11) as f64) * (1.0 / (1u64 << 53) as f64) < rate
}

fn triggers() -> &'static Triggers {
    static T: OnceLock<Triggers> = OnceLock::new();
    T.get_or_init(Triggers::from_env)
}

/// Whether any automatic collect trigger is configured. The VM gates its
/// per-safepoint work on this so a GC-off run pays a single cached load.
#[inline]
pub(crate) fn armed() -> bool {
    triggers().armed
}

/// Number of candidate pushes since the last `every_candidate` arming, and
/// whether a collect is pending (armed by the candidate threshold, run at the
/// next safepoint).
static CANDIDATE_PUSHES: AtomicUsize = AtomicUsize::new(0);
static PENDING: AtomicBool = AtomicBool::new(false);

/// ADR-0003 adaptive size threshold. `0` = not yet adapted (use the BASE from
/// `MUTSU_GC_THRESHOLD`); otherwise the clamped `2 × survivors` from the last
/// collect. A scan that mostly re-proves liveness raises it (backing out of
/// the fixed-period rescan pathology measured on cas-loop.t); a scan that
/// reclaims most of its input lets it fall back toward BASE.
static ADAPTIVE_THRESHOLD: AtomicUsize = AtomicUsize::new(0);

/// The size threshold currently in effect, or 0 when the size trigger is
/// disabled (`MUTSU_GC_THRESHOLD=0` or GC off). Also surfaced in the
/// `MUTSU_VM_STATS` gc line so tests/operators can observe adaptation.
pub(crate) fn current_size_threshold() -> usize {
    let base = triggers().threshold_base;
    if base == 0 {
        return 0;
    }
    match ADAPTIVE_THRESHOLD.load(Ordering::Relaxed) {
        0 => base,
        adapted => adapted,
    }
}

/// Record the candidate buffer's length after a push (called from
/// `gc_ptr::buffer_candidate`, outside the buffer lock). Arms a pending
/// collect when the buffer reaches the effective size threshold (ADR-0003).
#[inline]
pub(crate) fn note_buffer_len(len: usize) {
    let eff = current_size_threshold();
    if eff != 0 && len >= eff {
        PENDING.store(true, Ordering::Relaxed);
    }
}

/// Adapt the size threshold after a collect: `clamp(BASE, 2 × survivors, MAX)`
/// (ADR-0003 §2-2). `survivors` is the live portion of the scanned subgraph —
/// the nodes the next scan would re-trace for nothing. The deferred path (STW
/// timeout re-queues its suspects unscanned) reports the requeued count as
/// survivors for the same reason: they stay in the buffer, and without the
/// backoff every subsequent push over the threshold would re-arm a drain/
/// requeue round-trip against the same stuck set.
pub(crate) fn note_collect_survivors(survivors: usize) {
    let base = triggers().threshold_base;
    if base == 0 {
        return;
    }
    let next = survivors.saturating_mul(2).clamp(base, THRESHOLD_MAX);
    ADAPTIVE_THRESHOLD.store(next, Ordering::Relaxed);
}

/// Record a candidate-buffer push (called from `gc_ptr::buffer_candidate`).
/// Under `MUTSU_GC_EVERY_CANDIDATE=N`, arms a pending collect every `N` pushes.
/// Never collects inline — `Gc::drop` (the caller) may hold a borrow (§1.2).
#[inline]
pub(crate) fn note_candidate_push() {
    let n = triggers().every_candidate;
    if n == 0 {
        return;
    }
    let count = CANDIDATE_PUSHES.fetch_add(1, Ordering::Relaxed) + 1;
    if count.is_multiple_of(n) {
        PENDING.store(true, Ordering::Relaxed);
    }
}

/// A re-entry-boundary safepoint. Runs a collect iff a trigger fires. Cheap and
/// a no-op unless [`armed`]; safe to call anywhere the caller holds no borrow
/// into a `Gc`-managed container (design doc §1.2).
#[inline]
pub(crate) fn gc_safepoint(kind: SafepointKind) {
    if !armed() {
        return;
    }
    // Another thread may have stopped the world for its cycle scan: park here
    // until it releases (one load when no stop is requested — see gc::stw).
    super::stw::park_at_safepoint();
    let t = triggers();
    // `every_safepoint` / the `MUTSU_GC_AT` kind list / the random draw fire
    // directly; otherwise consume a pending arming from the candidate counter.
    // Test-and-test-and-set: the unconditional `swap` was a locked RMW on a
    // process-shared cache line executed once per call safepoint (~4% of a
    // recursion-heavy workload's hottest function); the plain load short-cuts
    // the common not-pending case. A pending flag set between the load and a
    // racing consumer's swap is simply consumed at the next safepoint — same
    // deferral the arming already tolerates.
    let fire = t.every_safepoint
        || t.at_mask & kind.bit() != 0
        || (t.random_rate_bits != 0 && random_fires(t.random_rate_bits))
        || (PENDING.load(Ordering::Relaxed) && PENDING.swap(false, Ordering::Relaxed));
    if fire {
        collect_cycles_at(kind.name());
    }
}

/// `MUTSU_GC_COLLECT_NOW=1`: one collect right at program start (design §9.2).
/// Called from `Interpreter::run`; a `Once` keeps re-entrant runs (`EVAL`,
/// REPL lines) from re-collecting.
pub(crate) fn startup_collect_if_requested() {
    static ONCE: std::sync::Once = std::sync::Once::new();
    ONCE.call_once(|| {
        if gc_enabled() && parse_flag("MUTSU_GC_COLLECT_NOW") {
            // An empty-heap collect is a silent no-op, so log the trigger
            // itself (under `MUTSU_GC_LOG`) to make the mode observable.
            if super::collect::log_mode() != super::collect::LogMode::Off {
                eprintln!("[mutsu gc] collect-now at startup");
            }
            collect_cycles_at(SafepointKind::Manual.name());
        }
    });
}
