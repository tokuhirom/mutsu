//! `MUTSU_VM_STATS` counters for the cycle collector. They live beside the
//! collector they count so `gc` does not reach up into `vm::vm_stats`
//! (#10779); `vm_stats::dump` reads them through [`snapshot`].

use crate::stats_gate::enabled;
use std::sync::atomic::{AtomicU64, Ordering};

// GC Level 1a counters (ADR-0001/0002, docs/gc-level1-detailed-design.md
// §8/§9.4a). `candidate_pushes`/`dedup_hits` increment when `MUTSU_GC` is on
// and a `Gc` handle is dropped with survivors. Success criterion (§8):
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

/// Record a GC cycle-candidate buffer push: a mutation chokepoint flagged a
/// GC-managed node as a possible cycle member (design doc §4.2). Wired from
/// `gc::gc_ptr::buffer_candidate`.
// Cost: O(1).
#[inline]
pub(crate) fn record_gc_candidate_push() {
    if enabled() {
        GC_CANDIDATE_PUSHES.fetch_add(1, Ordering::Relaxed);
    }
}

/// Record that a candidate push deduplicated against an already-buffered node
/// instead of adding a new entry. Wired from `gc::gc_ptr::buffer_candidate`.
// Cost: O(1).
#[inline]
pub(crate) fn record_gc_candidate_dedup_hit() {
    if enabled() {
        GC_CANDIDATE_DEDUP_HITS.fetch_add(1, Ordering::Relaxed);
    }
}

/// Record one completed collect cycle: `roots_scanned` nodes visited from the
/// root set, `reclaimed_nodes`/`reclaimed_cycles` freed, taking `pause_ns`.
/// Wired from `gc::collect`.
// Cost: O(1).
#[inline]
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

/// The counters above, as `vm_stats::dump` prints them.
pub(crate) struct GcCounts {
    pub(crate) collections: u64,
    pub(crate) candidate_pushes: u64,
    pub(crate) dedup_hits: u64,
    pub(crate) reclaimed_nodes: u64,
    pub(crate) reclaimed_cycles: u64,
    pub(crate) pause_ns_total: u64,
    pub(crate) pause_ns_max: u64,
    pub(crate) roots_scanned: u64,
}

// Cost: O(1).
pub(crate) fn snapshot() -> GcCounts {
    GcCounts {
        collections: GC_COLLECTIONS.load(Ordering::Relaxed),
        candidate_pushes: GC_CANDIDATE_PUSHES.load(Ordering::Relaxed),
        dedup_hits: GC_CANDIDATE_DEDUP_HITS.load(Ordering::Relaxed),
        reclaimed_nodes: GC_RECLAIMED_NODES.load(Ordering::Relaxed),
        reclaimed_cycles: GC_RECLAIMED_CYCLES.load(Ordering::Relaxed),
        pause_ns_total: GC_PAUSE_NS_TOTAL.load(Ordering::Relaxed),
        pause_ns_max: GC_PAUSE_NS_MAX.load(Ordering::Relaxed),
        roots_scanned: GC_ROOTS_SCANNED.load(Ordering::Relaxed),
    }
}
