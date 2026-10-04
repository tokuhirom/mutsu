//! How many `Lock`s each thread holds (`nqp::threadlockcount`).
//!
//! MoarVM keeps this count per thread and bumps it when a reentrant mutex is
//! first taken (re-entering a held lock does not count again), and Rakudo's
//! thread pool reads it. mutsu's `Lock` records only the owning OS thread,
//! so the count is kept here. It changes in the same places the owner
//! changes: `acquire_lock` / `release_lock` and the release/re-acquire around
//! `Condition.wait`.
//!
//! The owning thread is the only writer of its counter. Other threads read it
//! through a registry keyed by mutsu thread id, so `threadlockcount` works on
//! any `Thread`, as MoarVM's does.

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, LazyLock, Mutex, Weak};

/// Counters of threads that have taken at least one lock, by mutsu thread id.
/// A thread's entry dies with the thread (only the thread-local holds the
/// strong `Arc`).
static REGISTRY: LazyLock<Mutex<rustc_hash::FxHashMap<i64, Weak<AtomicU64>>>> =
    LazyLock::new(|| Mutex::new(rustc_hash::FxHashMap::default()));

thread_local! {
    static HELD: Arc<AtomicU64> = {
        let counter = Arc::new(AtomicU64::new(0));
        if let Ok(mut map) = REGISTRY.lock() {
            map.retain(|_, w| w.strong_count() > 0);
            map.insert(
                crate::runtime::current_mutsu_thread_id(),
                Arc::downgrade(&counter),
            );
        }
        counter
    };
}

/// The current thread took a lock it did not hold before.
// Cost: O(1) (O(t) once per thread, t = registered threads, on first use).
pub(crate) fn note_lock_taken() {
    HELD.with(|c| c.fetch_add(1, Ordering::Relaxed));
}

/// The current thread released its last hold on a lock.
// Cost: O(1).
pub(crate) fn note_lock_released() {
    HELD.with(|c| {
        let _ = c.try_update(Ordering::Relaxed, Ordering::Relaxed, |n| n.checked_sub(1));
    });
}

/// Locks held by the thread with mutsu id `tid`; 0 for a thread that never
/// took one or has ended.
// Cost: O(1).
pub(crate) fn lock_count_of(tid: i64) -> u64 {
    if tid == crate::runtime::current_mutsu_thread_id() {
        return HELD.with(|c| c.load(Ordering::Relaxed));
    }
    REGISTRY
        .lock()
        .ok()
        .and_then(|map| map.get(&tid).and_then(Weak::upgrade))
        .map_or(0, |c| c.load(Ordering::Relaxed))
}
