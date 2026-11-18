//! [`WorldCell`](crate::runtime::world_cell::WorldCell): one world table of the program, shared live by every thread
//! (ADR-17216 §2.1-2.3).
//!
//! The world (the declarations, wrappers, module state a program builds) exists
//! once per process. A table of it lives in a [`WorldCell`](crate::runtime::world_cell::WorldCell): every thread's
//! interpreter holds the same shared slot, and a spawn shares the slot instead
//! of copying the table.
//!
//! - **Reads take a snapshot.** The slot holds an `Arc<T>`; a thread keeps the
//!   last version it saw in a private cache, keyed by the slot's generation. A
//!   read loads the generation (one atomic load, no read-modify-write, so
//!   readers on many threads do not bounce a cache line) and, only when it has
//!   moved, takes the read lock long enough to clone the new `Arc`. A reader
//!   therefore works on one consistent version of the table even while another
//!   thread writes it.
//! - **Writes publish (read-copy-update).** A write takes the write lock,
//!   applies `Arc::make_mut` to the current version (copying the table if any
//!   thread still holds a snapshot of it), stores it, and moves the generation
//!   to a fresh value, still under the lock. A later read on any thread sees
//!   the new version: the generation store is `Release` and the read's load
//!   `Acquire`, so a write that happened-before a read on another thread (a
//!   `Promise` kept after it, a lock, a channel) is visible to that read.
//! - **Read-your-writes.** The writer's own cache is refreshed by the write.
//!
//! Synchronisation follows ADR-0068 / ADR-12814: every write is synchronised at
//! the table; no `&mut T` ever escapes the write lock; a program racing two
//! writes may see either order but never a torn table. A nested read on the
//! same thread while an outer snapshot is borrowed keeps that snapshot (the
//! cache cannot be swapped under a live borrow): one operation sees one
//! version.

use std::cell::{Cell, Ref, RefCell};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, RwLock};

/// The process-wide part of a [`WorldCell`]: the current version and its
/// generation.
#[derive(Debug)]
struct WorldSlot<T> {
    current: RwLock<Arc<T>>,
    generation: AtomicU64,
}

/// One world table, shared by every thread. See the module docs.
#[derive(Debug)]
pub(crate) struct WorldCell<T> {
    slot: Arc<WorldSlot<T>>,
    /// This thread's snapshot and the generation it was taken at. `None`
    /// only between a write dropping it and the write storing the new one.
    cache: RefCell<(u64, Option<Arc<T>>)>,
    /// `cache.0`, readable without borrowing the cache: the read fast path
    /// is one atomic load and one compare against it.
    cached_gen: Cell<u64>,
}

impl<T: Default> Default for WorldCell<T> {
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T> WorldCell<T> {
    /// A new world table holding `value`, shared with no other thread yet.
    // Cost: O(1).
    pub(crate) fn new(value: T) -> Self {
        Self::from_arc(Arc::new(value))
    }

    /// A new world table starting from a shared template version (the
    /// built-in registry every program starts from); the first write copies.
    // Cost: O(1).
    pub(crate) fn from_arc(value: Arc<T>) -> Self {
        let generation = crate::runtime::registry_gen::next_generation();
        Self {
            slot: Arc::new(WorldSlot {
                current: RwLock::new(Arc::clone(&value)),
                generation: AtomicU64::new(generation),
            }),
            cache: RefCell::new((generation, Some(value))),
            cached_gen: Cell::new(generation),
        }
    }

    /// The same table for a spawned thread: the slot is shared, never copied.
    // Cost: O(1), two reference-count increments.
    pub(crate) fn share(&self) -> Self {
        let cache = match self.cache.try_borrow() {
            Ok(cache) if cache.1.is_some() => (cache.0, cache.1.clone()),
            _ => {
                let (generation, current) = self.load();
                (generation, Some(current))
            }
        };
        Self {
            slot: Arc::clone(&self.slot),
            cached_gen: Cell::new(cache.0),
            cache: RefCell::new(cache),
        }
    }

    /// The table's current generation: a value no other version of any world
    /// table has had. A per-thread memo keyed by it is invalidated by a write
    /// on any thread.
    // Cost: O(1), one atomic load.
    #[inline]
    pub(crate) fn generation(&self) -> u64 {
        self.slot.generation.load(Ordering::Acquire)
    }

    // Cost: O(1), one read lock and an `Arc` clone.
    fn load(&self) -> (u64, Arc<T>) {
        let current =
            crate::runtime::lock_reentry::ReentrantReadGuard::new(&self.slot.current, "world_cell");
        // The generation is read under the lock, so it names `current`: a
        // write stores both under the write lock.
        (self.generation(), Arc::clone(&current))
    }

    /// A snapshot of the table, current as of this call -- unless an outer
    /// snapshot of the same table is still borrowed on this thread, which is
    /// then returned, so one operation sees one version. Never hold it across
    /// a [`Self::write`] of the same table on this thread.
    // Cost: O(1): one atomic load; a read lock and an `Arc` clone only after
    // another thread wrote.
    #[inline]
    pub(crate) fn read(&self) -> Ref<'_, T> {
        if self.cached_gen.get() != self.generation() {
            self.refresh();
        }
        Ref::map(self.cache.borrow(), |cache| match cache.1.as_ref() {
            Some(current) => &**current,
            None => unreachable!("world cell snapshot present outside a write"),
        })
    }

    /// Take the current version into this thread's cache, unless an outer
    /// snapshot of it is still borrowed here (that one is kept).
    // Cost: O(1), one read lock and an `Arc` clone.
    #[cold]
    #[inline(never)]
    fn refresh(&self) {
        if let Ok(mut cache) = self.cache.try_borrow_mut() {
            let (generation, current) = self.load();
            *cache = (generation, Some(current));
            self.cached_gen.set(generation);
        }
    }

    /// An owned snapshot of the table's current version (this thread's).
    // Cost: O(1), one reference-count increment.
    pub(crate) fn snapshot(&self) -> Arc<T> {
        drop(self.read());
        match self.cache.try_borrow() {
            Ok(cache) => match &cache.1 {
                Some(current) => Arc::clone(current),
                None => self.load().1,
            },
            Err(_) => self.load().1,
        }
    }
}

impl<T: Clone> WorldCell<T> {
    /// Apply `f` to the table and publish the result to every thread. `f`
    /// runs under the table's write lock, so it must not touch the same table.
    // Cost: O(1) plus `f` when no other thread holds a snapshot of the
    // current version; otherwise O(size of T) for the copy-on-write clone.
    pub(crate) fn write<R>(&self, f: impl FnOnce(&mut T) -> R) -> R {
        // Drop this thread's own snapshot first, so it does not force the
        // copy-on-write clone by itself. (A snapshot still borrowed by an
        // outer read stays; the write then copies.)
        let mut cache = self.cache.try_borrow_mut().ok();
        if let Some(cache) = cache.as_mut() {
            drop(cache.1.take());
        }
        // Until the new version is stored below, no fast-path read may trust
        // the cache.
        self.cached_gen.set(u64::MAX);
        let mut current = crate::runtime::lock_reentry::ReentrantWriteGuard::new(
            &self.slot.current,
            "world_cell",
        );
        let result = f(Arc::make_mut(&mut current));
        let generation = crate::runtime::registry_gen::next_generation();
        self.slot.generation.store(generation, Ordering::Release);
        if let Some(cache) = cache.as_mut() {
            **cache = (generation, Some(Arc::clone(&current)));
            self.cached_gen.set(generation);
        }
        result
    }
}

impl<T: Clone> WorldCell<T> {
    /// A write guard over the table: derefs to the current version, and its
    /// first mutable deref copies it (`Arc::make_mut`) when any thread still
    /// holds a snapshot. Dropping a guard that was mutably dereferenced
    /// publishes the result under a fresh generation. Never read the same
    /// table on this thread while the guard lives.
    // Cost: O(1) to take; the copy-on-write clone as for [`Self::write`].
    pub(crate) fn write_guard(&self) -> WorldWriteGuard<'_, T> {
        let mut cache = self.cache.try_borrow_mut().ok();
        // Drop this thread's own snapshot so it does not force the copy by
        // itself; restored on drop.
        // (Released before the lock is taken: when it is the last reference
        // to an old version, that version is freed outside the lock.)
        drop(cache.as_mut().and_then(|cache| cache.1.take()));
        self.cached_gen.set(u64::MAX);
        let current = crate::runtime::lock_reentry::ReentrantWriteGuard::new(
            &self.slot.current,
            "world_cell",
        );
        WorldWriteGuard {
            slot: &self.slot,
            current,
            cache,
            cached_gen: &self.cached_gen,
            dirty: false,
        }
    }
}

/// See [`WorldCell::write_guard`].
pub(crate) struct WorldWriteGuard<'a, T> {
    slot: &'a WorldSlot<T>,
    current: crate::runtime::lock_reentry::ReentrantWriteGuard<'a, Arc<T>>,
    cache: Option<std::cell::RefMut<'a, (u64, Option<Arc<T>>)>>,
    cached_gen: &'a Cell<u64>,
    dirty: bool,
}

impl<T: Clone> WorldWriteGuard<'_, T> {
    /// Whether another reference to the current version is alive (a mutable
    /// deref would then copy it).
    // Cost: O(1).
    pub(crate) fn is_shared(&self) -> bool {
        Arc::strong_count(&self.current) > 1
    }
}

impl<T> std::ops::Deref for WorldWriteGuard<'_, T> {
    type Target = T;
    #[inline]
    fn deref(&self) -> &T {
        &self.current
    }
}

impl<T: Clone> std::ops::DerefMut for WorldWriteGuard<'_, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut T {
        self.dirty = true;
        Arc::make_mut(&mut self.current)
    }
}

impl<T> Drop for WorldWriteGuard<'_, T> {
    fn drop(&mut self) {
        let generation = if self.dirty {
            let generation = crate::runtime::registry_gen::next_generation();
            self.slot.generation.store(generation, Ordering::Release);
            generation
        } else {
            self.slot.generation.load(Ordering::Acquire)
        };
        if let Some(cache) = self.cache.as_mut() {
            **cache = (generation, Some(Arc::clone(&self.current)));
            self.cached_gen.set(generation);
        }
    }
}
