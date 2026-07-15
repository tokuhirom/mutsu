//! Cross-thread `once { ... }` result store.
//!
//! Raku's `once` fires exactly once per *clone* of the enclosing code object,
//! and a clone is shared across every thread it runs on — so a `once` block in a
//! sub called from `start { ... }` on several workers must fire only once in
//! total. mutsu keys each `once` site by `(routine-clone-id, op-position)` and
//! stores the fired result here. The store is wrapped in an `Arc` and shared by
//! handle (not by value) into every spawned thread's interpreter clone, so all
//! threads see one another's fires.
//!
//! To make "fires once" hold under a genuine race (two workers reaching the same
//! `once` before either records a result), a claim protocol is used: the first
//! thread to arrive installs a `Running` marker, runs the body with the lock
//! released, then records `Done`; a second thread that finds `Running` waits on
//! the condvar until the result appears. A thread that re-enters its own
//! in-progress `once` (a recursive `once` body) does not deadlock — it is served
//! `Nil` instead of blocking on itself.

use crate::value::Value;
use std::collections::HashMap;
use std::sync::{Condvar, Mutex};
use std::thread::ThreadId;

/// The state of one `once` site.
enum OnceSlot {
    /// A thread is currently evaluating the body; carries the claimer's id so a
    /// recursive re-entry from the same thread is not mistaken for a peer wait.
    Running(ThreadId),
    /// The body finished; carries its result value (reused by every later hit).
    Done(Value),
}

/// Shared, thread-safe store of `once` results, keyed by site string.
#[derive(Default)]
pub(crate) struct OnceStore {
    map: Mutex<HashMap<String, OnceSlot>>,
    cv: Condvar,
}

/// Outcome of trying to claim a `once` site.
pub(crate) enum OnceClaim {
    /// The site already fired; here is its cached value.
    Cached(Value),
    /// This thread now owns the claim and must run the body, then call
    /// [`OnceStore::fulfill`] (on success) or [`OnceStore::abandon`] (on error).
    Claimed,
}

impl OnceStore {
    /// Look up `key`; return its cached value, or claim it for this thread.
    ///
    /// If another thread holds the claim, block until it publishes a result (or
    /// abandons it, in which case this thread takes over the claim).
    pub(crate) fn claim(&self, key: &str) -> OnceClaim {
        let tid = std::thread::current().id();
        let mut map = self.map.lock().unwrap();
        loop {
            match map.get(key) {
                Some(OnceSlot::Done(v)) => return OnceClaim::Cached(v.clone()),
                // A recursive `once` body re-entering its own site: don't wait on
                // ourselves. Treat as not-yet-produced and yield `Nil` upstream.
                Some(OnceSlot::Running(owner)) if *owner == tid => {
                    return OnceClaim::Cached(Value::NIL);
                }
                Some(OnceSlot::Running(_)) => {
                    map = self.cv.wait(map).unwrap();
                    // Re-check the slot: the owner may have fulfilled or abandoned.
                }
                None => {
                    map.insert(key.to_string(), OnceSlot::Running(tid));
                    return OnceClaim::Claimed;
                }
            }
        }
    }

    /// Publish the result of a claimed site and wake any waiters.
    pub(crate) fn fulfill(&self, key: &str, value: Value) {
        let mut map = self.map.lock().unwrap();
        map.insert(key.to_string(), OnceSlot::Done(value));
        drop(map);
        self.cv.notify_all();
    }

    /// Release a claimed site without a result (the body threw); a waiter — or a
    /// later call — may retry it. Wakes waiters so one of them re-claims.
    pub(crate) fn abandon(&self, key: &str) {
        let mut map = self.map.lock().unwrap();
        map.remove(key);
        drop(map);
        self.cv.notify_all();
    }

    /// Visit every fired result value (GC root marking).
    pub(crate) fn visit_done_values(&self, mut f: impl FnMut(&Value)) {
        let map = self.map.lock().unwrap();
        for slot in map.values() {
            if let OnceSlot::Done(v) = slot {
                f(v);
            }
        }
    }
}
