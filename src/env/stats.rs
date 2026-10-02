//! `MUTSU_VM_STATS` counters for env copy-on-write. They live beside `Env` so
//! it does not reach up into `vm::vm_stats` (#10779); `vm_stats::dump` reads
//! them through [`snapshot`].

pub(crate) use crate::stats_gate::enabled;
use std::sync::atomic::{AtomicU64, Ordering};

static ENV_DEEP_COPY: AtomicU64 = AtomicU64::new(0);
/// Entries actually copied by those deep copies (the sum of the map's length at
/// each one). The *count* alone cannot tell a copy of a 900-entry frame env from
/// a copy of an empty scoped overlay, and only the former is a cost that grows
/// with the size of the program; this is the number that does.
static ENV_DEEP_COPY_ENTRIES: AtomicU64 = AtomicU64::new(0);

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
