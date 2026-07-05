//! GC safepoint wiring (ADR-0001 §1.1, `docs/gc-level1-detailed-design.md`
//! §1.2 / §9.2 / §9.2a / §11 step 8 second half).
//!
//! The collector ([`super::collect::collect_cycles`]) may only run at a
//! *re-entry boundary* — a point holding no long borrow / lock / `gc_contents_mut`
//! (design doc §1.2). This module holds the trigger policy and the one hot-path
//! entry point ([`gc_safepoint`]) the VM calls at those boundaries.
//!
//! ## Triggers (first cut, design doc §9.2)
//! - `MUTSU_GC=off` (default / unset) disables everything: [`armed`] is `false`,
//!   so the VM's safepoint check is a single relaxed load that early-returns —
//!   normal execution pays essentially nothing and never collects.
//! - `MUTSU_GC=on` + `MUTSU_GC_EVERY_SAFEPOINT=1` — collect at *every* safepoint
//!   (deterministic stress; §9.2).
//! - `MUTSU_GC=on` + `MUTSU_GC_EVERY_CANDIDATE=N` — arm a pending collect once
//!   every `N` candidate pushes; it runs at the next safepoint (never inline in
//!   `Gc::drop`, which may hold a borrow — §1.2).
//!
//! Deferred (design doc §9.2 "first cut では見送ってよい"): `MUTSU_GC_AT`,
//! `MUTSU_GC_COLLECT_NOW`, random stress. Only the `Backedge` safepoint is wired
//! for now (call/return/await/thread_join join the enum but are not yet emitted).

use std::sync::OnceLock;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

use super::collect::collect_cycles_at;
use super::gc_ptr::gc_enabled;

/// A re-entry boundary at which a collect may run (design doc §9.2a). Only
/// [`SafepointKind::Backedge`] is emitted so far; the rest are fixed now so the
/// enum (and a future `MUTSU_GC_AT`) is stable.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[allow(dead_code)]
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
    /// (and the string a future `MUTSU_GC_AT` would match).
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
}

/// Resolved trigger policy, read once from the environment.
struct Triggers {
    /// GC on AND at least one automatic trigger configured. When `false`, the
    /// hot-path [`armed`] check early-returns.
    armed: bool,
    every_safepoint: bool,
    every_candidate: usize,
}

impl Triggers {
    fn from_env() -> Triggers {
        if !gc_enabled() {
            return Triggers {
                armed: false,
                every_safepoint: false,
                every_candidate: 0,
            };
        }
        let every_safepoint = parse_flag("MUTSU_GC_EVERY_SAFEPOINT");
        let every_candidate = parse_count("MUTSU_GC_EVERY_CANDIDATE");
        Triggers {
            armed: every_safepoint || every_candidate > 0,
            every_safepoint,
            every_candidate,
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
    // `every_safepoint` fires unconditionally; otherwise consume a pending
    // arming from the candidate counter.
    let fire = t.every_safepoint || PENDING.swap(false, Ordering::Relaxed);
    if fire {
        collect_cycles_at(kind.name());
    }
}
