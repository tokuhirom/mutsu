//! Shared IO handle table for the VM/Interpreter decoupling (PLAN.md ③).
//!
//! Holds the program's *open IO handles* — files, sockets, listeners and their
//! buffering/encoding state ([`IoHandleState`](super::IoHandleState)) — keyed by
//! a small integer id that `IO::Handle` / `IO::Socket::INET` values carry. These
//! used to live as `Interpreter.handles: HashMap<usize, IoHandleState>` plus a
//! `next_handle_id: usize` counter, trapping native IO state inside the
//! tree-walking interpreter: VM-native code could only reach it through
//! `self.interpreter.<field>`. Phase ③ lifts it behind `Arc<RwLock<…>>` so the
//! VM and the Interpreter can reach it as *peers* (the same shared-handle
//! playbook ② used for the declaration [`Registry`](super::Registry)).
//!
//! Why a shared `Arc<RwLock>` is correct here, unlike `env` (which stays a plain
//! COW field): IO handles are **per-thread snapshots**, not live-shared.
//! `clone_for_thread` deep-clones the referenced handles into a fresh `Arc`
//! (`File::try_clone` etc.), and a child thread's newly opened handles are
//! merged back into the parent explicitly (`ThreadPromisePayload`). So the lock
//! never contends across threads; it exists only for the within-thread
//! VM<->Interpreter ping-pong, which is non-concurrent. `IoHandleState` is not
//! `Clone` and IO blocks, but because each thread owns its own snapshot, holding
//! a guard across a blocking read/write cannot deadlock another holder — the
//! only hazard is the *same* thread re-acquiring the lock, which the debug
//! re-entrancy guard (see [`crate::runtime::lock_reentry`]) catches.
//!
//! Lock discipline (CRITICAL): never hold a guard across a call that re-enters a
//! handle operation on the same table. The handle accessors confine the
//! `&mut IoHandleState` borrow inside a closure (`with_handle_mut`) precisely so
//! a caller cannot hold it across another `self.*` handle op. Once the
//! Interpreter execution path is removed (PLAN.md ④/⑤), this collapses to a
//! plain VM-owned field and the `Arc`/lock disappear.

use std::collections::HashMap;

use super::IoHandleState;

/// The program's open IO handles plus the id allocator. See module docs.
#[derive(Debug, Default)]
pub(crate) struct IoHandleTable {
    /// Open handles keyed by id (the integer an `IO::Handle` value carries).
    pub(crate) map: HashMap<usize, IoHandleState>,
    /// Next handle id to allocate. Starts at 1 (`Interpreter::new` seeds the
    /// standard handles at lower ids).
    pub(crate) next_id: usize,
}

/// Read guard for the shared [`IoHandleTable`]; a [`ReentrantReadGuard`] keyed by
/// the `"io_handles"` lock name. See [`crate::runtime::lock_reentry`].
///
/// [`ReentrantReadGuard`]: crate::runtime::lock_reentry::ReentrantReadGuard
pub(crate) type IoHandlesReadGuard<'a> =
    crate::runtime::lock_reentry::ReentrantReadGuard<'a, IoHandleTable>;

/// Write guard for the shared [`IoHandleTable`]; a [`ReentrantWriteGuard`] keyed
/// by the `"io_handles"` lock name. See [`crate::runtime::lock_reentry`].
///
/// [`ReentrantWriteGuard`]: crate::runtime::lock_reentry::ReentrantWriteGuard
pub(crate) type IoHandlesWriteGuard<'a> =
    crate::runtime::lock_reentry::ReentrantWriteGuard<'a, IoHandleTable>;
