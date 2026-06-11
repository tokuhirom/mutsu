//! Shared reentrancy-detecting `RwLock` guard infrastructure (debug-only).
//!
//! Several pieces of execution state that the VM and the tree-walking
//! `Interpreter` share during the decoupling transition live behind
//! `Arc<RwLock<…>>` scaffolding — the declaration [`Registry`](super::Registry)
//! (PLAN.md ②) and the IO handle table
//! ([`IoHandleTable`](super::io_handles::IoHandleTable), PLAN.md ③). Both are
//! per-thread snapshots (a fresh `Arc` is cloned in `clone_for_thread`), so the
//! lock never contends *across* threads; it exists only so the VM and the
//! Interpreter can reach the data as peers *within* one thread, ping-ponging.
//!
//! `RwLock` is not reentrant, so the discipline (see each owner's module docs)
//! is: never hold a read/write guard across a call that re-enters execution and
//! might re-acquire the same lock. A stray held guard deadlocks, and a silent
//! deadlock is only caught by `make roast`'s ~13-min timeout — a static text
//! scanner misses code shapes the borrow checker also misses (e.g. a
//! `write -> write` inside a `match self.lock_mut()…{}` arm).
//!
//! So instead we detect re-entry at runtime, in debug builds: each acquisition
//! checks a thread-local record of the guards this thread currently holds *on
//! that same lock* and panics with a located message before the would-be
//! deadlocking `.read()`/`.write()` call.
//!
//! The record is keyed by the lock's address, NOT just by thread: a single
//! thread legitimately holds guards on *different* locks at once (e.g.
//! `self.registry_mut().classes = nested.registry().classes.clone();` holds a
//! write guard on `self`'s registry and a read guard on a sub-interpreter's
//! simultaneously). Those are independent locks, so there is no deadlock; a
//! thread-global flag would false-positive. Only re-acquiring the *same* lock
//! deadlocks.
//!
//! The allowed/forbidden matrix (per lock) matches `std::sync::RwLock`'s actual
//! deadlock conditions:
//!   - acquiring a WRITE while ANY guard on the same lock is held -> deadlock
//!   - acquiring a READ while a WRITE on the same lock is held    -> deadlock
//!   - acquiring a READ while only READ guards are held           -> tolerated
//!     (nested reads are relied upon, e.g. `a().x && b().y` keeps both temporary
//!     read guards alive to end-of-statement).
//!
//! This is `#[cfg(debug_assertions)]` only: the per-access bookkeeping would
//! otherwise tax the hot read paths. CI's release `make roast` still backstops
//! via timeout.

#[cfg(debug_assertions)]
mod reentry_check {
    use std::cell::RefCell;
    use std::collections::HashMap;

    thread_local! {
        // lock address -> (outstanding read guards, write guard held?) on this thread.
        static HELD: RefCell<HashMap<usize, (u32, bool)>> = RefCell::new(HashMap::new());
    }

    /// Called before `RwLock::read()`. Panics if this thread already holds a write
    /// guard on the same lock (read-while-write deadlocks).
    pub(super) fn enter_read(lock: usize, name: &str) {
        HELD.with(|h| {
            let mut h = h.borrow_mut();
            let entry = h.entry(lock).or_insert((0, false));
            assert!(
                !entry.1,
                "{name}(): a read guard was acquired while this thread already \
                 holds a write guard on the same lock (read-while-write). \
                 RwLock is not reentrant; drop the write guard before re-entering. \
                 See the lock discipline in src/runtime/lock_reentry.rs.",
            );
            entry.0 += 1;
        });
    }

    pub(super) fn exit_read(lock: usize) {
        HELD.with(|h| {
            let mut h = h.borrow_mut();
            if let Some(entry) = h.get_mut(&lock) {
                entry.0 = entry.0.saturating_sub(1);
                if entry.0 == 0 && !entry.1 {
                    h.remove(&lock);
                }
            }
        });
    }

    /// Called before `RwLock::write()`. Panics if this thread already holds any
    /// guard on the same lock (write-while-anything deadlocks).
    pub(super) fn enter_write(lock: usize, name: &str) {
        HELD.with(|h| {
            let mut h = h.borrow_mut();
            let entry = h.entry(lock).or_insert((0, false));
            assert!(
                !entry.1,
                "{name}_mut(): a write guard was acquired while this thread \
                 already holds a write guard on the same lock (write -> write). \
                 RwLock is not reentrant; consolidate into a single write guard. \
                 See src/runtime/lock_reentry.rs.",
            );
            assert!(
                entry.0 == 0,
                "{name}_mut(): a write guard was acquired while this thread holds \
                 a read guard on the same lock (read -> write upgrade). \
                 RwLock is not reentrant; drop the read guard first. See \
                 src/runtime/lock_reentry.rs.",
            );
            entry.1 = true;
        });
    }

    pub(super) fn exit_write(lock: usize) {
        HELD.with(|h| {
            let mut h = h.borrow_mut();
            if let Some(entry) = h.get_mut(&lock) {
                entry.1 = false;
                if entry.0 == 0 {
                    h.remove(&lock);
                }
            }
        });
    }
}

/// Reentrancy-checked read guard over an `Arc<RwLock<T>>`. Derefs to `T`; in
/// debug builds it records the acquisition in a thread-local so a re-entrant
/// lock attempt panics with a located message instead of silently deadlocking
/// (see the `reentry_check` module above). `name` identifies the lock in panic
/// messages (e.g. `"registry"`, `"io_handles"`).
pub(crate) struct ReentrantReadGuard<'a, T> {
    inner: std::sync::RwLockReadGuard<'a, T>,
    #[cfg(debug_assertions)]
    lock_id: usize,
}

impl<'a, T> ReentrantReadGuard<'a, T> {
    pub(crate) fn new(lock: &'a std::sync::RwLock<T>, name: &'static str) -> Self {
        #[cfg(debug_assertions)]
        let lock_id = lock as *const _ as usize;
        #[cfg(debug_assertions)]
        reentry_check::enter_read(lock_id, name);
        // Acquire only after the reentry check, so a would-be deadlock panics
        // instead of hanging on the blocking `.read()`.
        let inner = lock.read().unwrap();
        Self {
            inner,
            #[cfg(debug_assertions)]
            lock_id,
        }
    }
}

impl<T> std::ops::Deref for ReentrantReadGuard<'_, T> {
    type Target = T;
    #[inline]
    fn deref(&self) -> &T {
        &self.inner
    }
}

#[cfg(debug_assertions)]
impl<T> Drop for ReentrantReadGuard<'_, T> {
    fn drop(&mut self) {
        reentry_check::exit_read(self.lock_id);
    }
}

/// Reentrancy-checked write guard over an `Arc<RwLock<T>>`. Same instrumentation
/// as [`ReentrantReadGuard`].
pub(crate) struct ReentrantWriteGuard<'a, T> {
    inner: std::sync::RwLockWriteGuard<'a, T>,
    #[cfg(debug_assertions)]
    lock_id: usize,
}

impl<'a, T> ReentrantWriteGuard<'a, T> {
    pub(crate) fn new(lock: &'a std::sync::RwLock<T>, name: &'static str) -> Self {
        #[cfg(debug_assertions)]
        let lock_id = lock as *const _ as usize;
        #[cfg(debug_assertions)]
        reentry_check::enter_write(lock_id, name);
        let inner = lock.write().unwrap();
        Self {
            inner,
            #[cfg(debug_assertions)]
            lock_id,
        }
    }
}

impl<T> std::ops::Deref for ReentrantWriteGuard<'_, T> {
    type Target = T;
    #[inline]
    fn deref(&self) -> &T {
        &self.inner
    }
}

impl<T> std::ops::DerefMut for ReentrantWriteGuard<'_, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut T {
        &mut self.inner
    }
}

#[cfg(debug_assertions)]
impl<T> Drop for ReentrantWriteGuard<'_, T> {
    fn drop(&mut self) {
        reentry_check::exit_write(self.lock_id);
    }
}
