//! `MUTSU_VM_STATS` counters for env copy-on-write and by-name resolution. They
//! live beside `Env` so it does not reach up into `vm::vm_stats` (#10779);
//! `vm_stats::dump` reads them through [`snapshot`] and
//! [`name_resolution_snapshot`].

pub(crate) use crate::stats_gate::enabled;
use crate::symbol::Symbol;
use std::sync::atomic::{AtomicU64, Ordering};

static ENV_DEEP_COPY: AtomicU64 = AtomicU64::new(0);
/// Entries actually copied by those deep copies (the sum of the map's length at
/// each one). The *count* alone cannot tell a copy of a 900-entry frame env from
/// a copy of an empty scoped overlay, and only the former is a cost that grows
/// with the size of the program; this is the number that does.
static ENV_DEEP_COPY_ENTRIES: AtomicU64 = AtomicU64::new(0);

// ADR-12529 phase 0: what each later phase takes to zero. A call frame builds
// a scoped overlay over its caller's env (phase 3 removes it), a name that is
// not in the running frame's own overlay walks that caller chain (phases 1-3),
// and a closure capture copies and layers names out of it (phase 4).
static SCOPED_OVERLAYS: AtomicU64 = AtomicU64::new(0);
static CHAIN_WALKS: AtomicU64 = AtomicU64::new(0);
static CHAIN_HOPS: AtomicU64 = AtomicU64::new(0);
static CAPTURES: AtomicU64 = AtomicU64::new(0);
static CAPTURE_OWN_ENTRIES: AtomicU64 = AtomicU64::new(0);
static CAPTURE_LAYERS: AtomicU64 = AtomicU64::new(0);

/// Record an actual O(env_size) deep copy of the env HashMap, triggered when
/// `Arc::make_mut` clones a shared env on first mutation (e.g. the first env
/// write inside a method body whose frame holds a clone of the env). This is
/// the real cost the dual-store work targets, not `clone_env`.
// Cost: O(1).
#[inline]
pub(crate) fn record_env_deep_copy(entries: usize) {
    if enabled() {
        ENV_DEEP_COPY.fetch_add(1, Ordering::Relaxed);
        ENV_DEEP_COPY_ENTRIES.fetch_add(entries as u64, Ordering::Relaxed);
    }
}

/// `(deep copies, entries copied)`, as `vm_stats::dump` prints them.
// Cost: O(1).
pub(crate) fn snapshot() -> (u64, u64) {
    (
        ENV_DEEP_COPY.load(Ordering::Relaxed),
        ENV_DEEP_COPY_ENTRIES.load(Ordering::Relaxed),
    )
}

/// Record one `Env::scoped_child`: an overlay a frame chains over its caller.
// Cost: O(1).
#[inline]
pub(crate) fn record_scoped_overlay() {
    if enabled() {
        SCOPED_OVERLAYS.fetch_add(1, Ordering::Relaxed);
    }
}

/// Record one by-name lookup that missed the running frame's own overlay and
/// walks the parent chain, with the number of parent tiers it visits before it
/// answers. Only called when [`enabled`], and only in a debug build (see
/// `Env::get_sym`); the hop count re-walks the chain here so the lookup itself
/// carries no counter.
// Cost: O(d), d = chain depth; only with MUTSU_VM_STATS set.
#[cfg(debug_assertions)]
#[cold]
pub(crate) fn record_chain_walk(env: &super::Env, key: Symbol) {
    let mut hops = 0u64;
    let mut cur = env;
    while !cur.inner.contains_key(&key) && !cur.is_tombstoned(key) {
        match &cur.parent {
            Some(parent) => {
                cur = parent;
                hops += 1;
            }
            None => break,
        }
    }
    CHAIN_WALKS.fetch_add(1, Ordering::Relaxed);
    CHAIN_HOPS.fetch_add(hops, Ordering::Relaxed);
}

/// Record one closure capture built by `Env::layered_capture`: the entries it
/// copied into its own tier and the shared layers it stacked.
// Cost: O(1).
#[inline]
pub(crate) fn record_capture(own_entries: usize, layers: usize) {
    if enabled() {
        CAPTURES.fetch_add(1, Ordering::Relaxed);
        CAPTURE_OWN_ENTRIES.fetch_add(own_entries as u64, Ordering::Relaxed);
        CAPTURE_LAYERS.fetch_add(layers as u64, Ordering::Relaxed);
    }
}

/// The by-name resolution counters, as `vm_stats::dump` prints them.
pub(crate) struct NameResolutionCounts {
    pub(crate) scoped_overlays: u64,
    /// `None` in a release build, which does not count walks.
    pub(crate) chain_walks: Option<u64>,
    pub(crate) chain_hops: Option<u64>,
    pub(crate) captures: u64,
    pub(crate) capture_own_entries: u64,
    pub(crate) capture_layers: u64,
}

// Cost: O(1).
pub(crate) fn name_resolution_snapshot() -> NameResolutionCounts {
    NameResolutionCounts {
        scoped_overlays: SCOPED_OVERLAYS.load(Ordering::Relaxed),
        chain_walks: cfg!(debug_assertions).then(|| CHAIN_WALKS.load(Ordering::Relaxed)),
        chain_hops: cfg!(debug_assertions).then(|| CHAIN_HOPS.load(Ordering::Relaxed)),
        captures: CAPTURES.load(Ordering::Relaxed),
        capture_own_entries: CAPTURE_OWN_ENTRIES.load(Ordering::Relaxed),
        capture_layers: CAPTURE_LAYERS.load(Ordering::Relaxed),
    }
}
