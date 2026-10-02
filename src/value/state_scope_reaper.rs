//! Lifetime of a closure clone's `state` variables (#9504).
//!
//! A `state` variable declared inside a closure is per *clone*: every time the
//! closure literal is evaluated it gets a fresh `SubData::id`, and the
//! interpreter's `state_vars` store (plus, once a thread has been spawned, the
//! cross-thread cell in `shared_vars`) keys the variable by that id. Nothing
//! used to remove those entries, so `map { ... ++$ ... }` run once per call of
//! an enclosing routine left one entry per call behind for the rest of the
//! program.
//!
//! A [`StateScopeGuard`] is attached to such a clone and shared by every
//! Rust-level copy of its `SubData`. When the last copy goes away the guard
//! records the id here, and the next interpreter that grows its state store
//! drops the dead ids' entries (`Interpreter::reap_dead_state_scopes`).
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

static DEAD_SCOPES: Mutex<Vec<u64>> = Mutex::new(Vec::new());
static DEAD_COUNT: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug)]
pub(crate) struct StateScopeGuard(u64);

impl StateScopeGuard {
    pub(crate) fn new(scope_id: u64) -> Arc<Self> {
        Arc::new(Self(scope_id))
    }
}

impl Drop for StateScopeGuard {
    fn drop(&mut self) {
        // The count is only touched under the lock, so it never runs ahead of
        // (or behind) the list it summarizes.
        let mut dead = DEAD_SCOPES.lock().unwrap_or_else(|e| e.into_inner());
        dead.push(self.0);
        DEAD_COUNT.store(dead.len(), Ordering::Relaxed);
    }
}

/// How many dead scope ids are waiting to be reaped.
pub(crate) fn dead_scope_count() -> usize {
    DEAD_COUNT.load(Ordering::Relaxed)
}

/// Take every dead scope id recorded so far.
pub(crate) fn take_dead_scopes() -> Vec<u64> {
    let mut dead = DEAD_SCOPES.lock().unwrap_or_else(|e| e.into_inner());
    DEAD_COUNT.store(0, Ordering::Relaxed);
    std::mem::take(&mut *dead)
}
