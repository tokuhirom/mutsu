//! The program's IO handle table: one per process, shared live by every thread
//! (ADR-17216 §2.4, slice 1).
//!
//! Holds the program's *open IO handles* — files, sockets, listeners and their
//! buffering/encoding state ([`IoHandleState`]) — keyed by a small integer id
//! that `IO::Handle` / `IO::Socket::INET` values carry.
//!
//! Every interpreter of the process holds the same `Arc<IoHandleTable>`: a
//! spawned thread clones the `Arc`, never the handles. So a handle opened on a
//! worker is usable on every other thread as soon as its value reaches it (by
//! `await`, `.result`, a shared container, ...), and a close, seek or read on a
//! handle is seen by every thread holding it, as on rakudo. (Before ADR-17216 a
//! spawn deep-cloned the handles it could reach and a single-Promise `await`
//! merged the worker's new handles back; sibling workers allocated the same ids
//! from the copied counter and the merge dropped the second.)
//!
//! Synchronisation (ADR-0068, ADR-12814):
//!
//! - ids come from one [`AtomicUsize`], so two threads never allocate the same
//!   id;
//! - the id → handle map is behind its own `RwLock`, held only long enough to
//!   look an id up (cloning the handle's `Arc`) or to insert one;
//! - each handle's state is behind its own `RwLock`, so a blocking read on one
//!   handle does not block an operation on another. Two threads operating on
//!   the *same* handle are serialised by that lock, which is what MoarVM's
//!   per-handle mutex does too.
//!
//! Lock discipline (CRITICAL): never hold a handle's guard across a call that
//! re-enters a handle operation on the same handle. The accessors confine the
//! `&mut IoHandleState` borrow to a closure ([`IoHandleTable::with_mut`],
//! `Interpreter::with_handle_mut`), and the closure only ever receives the
//! state, never the interpreter. The debug re-entrancy guard (see
//! [`crate::runtime::lock_reentry`]) catches a same-thread re-acquire.

use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, RwLock};

use super::{IoHandleState, IoHandleTarget};

/// One open handle, shared by every thread that reaches its id.
pub(crate) type SharedIoHandle = Arc<RwLock<IoHandleState>>;

/// The program's open IO handles plus the id allocator. See module docs.
#[derive(Debug)]
pub(crate) struct IoHandleTable {
    /// Open handles keyed by id (the integer an `IO::Handle` value carries).
    map: RwLock<HashMap<usize, SharedIoHandle>>,
    /// Next handle id to allocate. Starts at 1; 0 is never a handle id.
    next_id: AtomicUsize,
    /// The first handle registered for each standard stream (stdin, stdout,
    /// stderr), 0 when none is: the startup handles `init_io_environment`
    /// creates. Output accounting (`emit_output`) and `nqp::getstdout` and kin
    /// reach them without scanning the table.
    std_ids: [AtomicUsize; 3],
}

impl Default for IoHandleTable {
    fn default() -> Self {
        Self {
            map: RwLock::new(HashMap::new()),
            next_id: AtomicUsize::new(1),
            std_ids: [AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0)],
        }
    }
}

fn std_slot(target: IoHandleTarget) -> Option<usize> {
    match target {
        IoHandleTarget::Stdin => Some(0),
        IoHandleTarget::Stdout => Some(1),
        IoHandleTarget::Stderr => Some(2),
        _ => None,
    }
}

type MapReadGuard<'a> =
    crate::runtime::lock_reentry::ReentrantReadGuard<'a, HashMap<usize, SharedIoHandle>>;
type MapWriteGuard<'a> =
    crate::runtime::lock_reentry::ReentrantWriteGuard<'a, HashMap<usize, SharedIoHandle>>;
/// A handle's state under its read lock.
pub(crate) type HandleReadGuard<'a> =
    crate::runtime::lock_reentry::ReentrantReadGuard<'a, IoHandleState>;
/// A handle's state under its write lock.
pub(crate) type HandleWriteGuard<'a> =
    crate::runtime::lock_reentry::ReentrantWriteGuard<'a, IoHandleState>;

impl IoHandleTable {
    /// The handle registered under `id`, if any.
    // Cost: O(1), one uncontended read lock and an `Arc` clone.
    pub(crate) fn get(&self, id: usize) -> Option<SharedIoHandle> {
        MapReadGuard::new(&self.map, "io_handles").get(&id).cloned()
    }

    /// Whether `id` names a registered (open or closed) handle.
    // Cost: O(1).
    pub(crate) fn contains(&self, id: usize) -> bool {
        MapReadGuard::new(&self.map, "io_handles").contains_key(&id)
    }

    /// Allocate a fresh id, register `state` under it and return the id.
    // Cost: O(1) amortised, one atomic increment and one write lock.
    pub(crate) fn insert(&self, state: IoHandleState) -> usize {
        let id = self.next_id.fetch_add(1, Ordering::Relaxed);
        let slot = std_slot(state.target);
        MapWriteGuard::new(&self.map, "io_handles").insert(id, Arc::new(RwLock::new(state)));
        if let Some(slot) = slot {
            // Only the first handle of a standard stream is recorded.
            let _ = self.std_ids[slot].compare_exchange(0, id, Ordering::AcqRel, Ordering::Relaxed);
        }
        id
    }

    /// The first handle registered for the standard stream `target`
    /// (`Stdin`/`Stdout`/`Stderr`), if one was.
    // Cost: O(1).
    pub(super) fn std_id(&self, target: IoHandleTarget) -> Option<usize> {
        let id = self.std_ids[std_slot(target)?].load(Ordering::Acquire);
        (id != 0).then_some(id)
    }

    /// Every registered handle, for a walk over the whole table (flushing at
    /// exit). The map lock is released before the caller locks any handle.
    // Cost: O(n), n = registered handles.
    pub(crate) fn snapshot(&self) -> Vec<SharedIoHandle> {
        MapReadGuard::new(&self.map, "io_handles")
            .values()
            .cloned()
            .collect()
    }

    /// Run `f` on the state of handle `id` under its read lock; `None` when no
    /// handle has that id.
    // Cost: O(1) plus `f`.
    pub(crate) fn with<R>(&self, id: usize, f: impl FnOnce(&IoHandleState) -> R) -> Option<R> {
        let handle = self.get(id)?;
        let guard = HandleReadGuard::new(&handle, "io_handle");
        Some(f(&guard))
    }

    /// Run `f` on the state of handle `id` under its write lock; `None` when no
    /// handle has that id. The lock is held across `f`, which may block on IO;
    /// only this one handle waits for it.
    // Cost: O(1) plus `f`.
    pub(crate) fn with_mut<R>(
        &self,
        id: usize,
        f: impl FnOnce(&mut IoHandleState) -> R,
    ) -> Option<R> {
        let handle = self.get(id)?;
        let mut guard = HandleWriteGuard::new(&handle, "io_handle");
        Some(f(&mut guard))
    }
}

/// Lock one handle (from [`IoHandleTable::get`] / [`IoHandleTable::snapshot`])
/// for reading. Never hold the guard across another handle operation.
// Cost: O(1).
pub(crate) fn read(handle: &SharedIoHandle) -> HandleReadGuard<'_> {
    HandleReadGuard::new(handle, "io_handle")
}

/// Lock one handle for writing. Never hold the guard across another handle
/// operation.
// Cost: O(1).
pub(crate) fn write(handle: &SharedIoHandle) -> HandleWriteGuard<'_> {
    HandleWriteGuard::new(handle, "io_handle")
}
