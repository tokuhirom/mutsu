//! GC safepoint wiring (ADR-0001 §1.1, `docs/gc-level1-detailed-design.md`
//! §1.2 / §9.2 / §9.2a / §11 step 8 second half).
//!
//! The collector ([`super::collect::collect_cycles`]) may only run at a
//! *re-entry boundary* — a point holding no long borrow / lock / `gc_contents_mut`
//! (design doc §1.2). This module holds the trigger policy and the one hot-path
//! entry point ([`gc_safepoint`]) the VM calls at those boundaries.
//!
//! ## Triggers (design doc §9.2)
//! - `MUTSU_GC=off` (default / unset) disables everything: [`armed`] is `false`,
//!   so the VM's safepoint check is a single relaxed load that early-returns —
//!   normal execution pays essentially nothing and never collects.
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
}

impl Triggers {
    fn from_env() -> Triggers {
        if !gc_enabled() {
            return Triggers {
                armed: false,
                every_safepoint: false,
                every_candidate: 0,
                at_mask: 0,
                random_rate_bits: 0,
            };
        }
        let every_safepoint = parse_flag("MUTSU_GC_EVERY_SAFEPOINT");
        let every_candidate = parse_count("MUTSU_GC_EVERY_CANDIDATE");
        let at_mask = parse_at_mask("MUTSU_GC_AT");
        let random_rate_bits = init_random("MUTSU_GC_RANDOM_RATE", "MUTSU_GC_RANDOM_SEED");
        Triggers {
            armed: every_safepoint || every_candidate > 0 || at_mask != 0 || random_rate_bits != 0,
            every_safepoint,
            every_candidate,
            at_mask,
            random_rate_bits,
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
    let fire = t.every_safepoint
        || t.at_mask & kind.bit() != 0
        || (t.random_rate_bits != 0 && random_fires(t.random_rate_bits))
        || PENDING.swap(false, Ordering::Relaxed);
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
