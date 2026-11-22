//! A loom model of the lock-free shared-array snapshot protocol of
//! ADR-18637 (`src/value/cell_snapshot.rs` in the interpreter).
//!
//! The model keeps the protocol's shape and drops everything else:
//!
//! - `Cell::lock` stands for the cell's mutex plus ADR-0068's stripes, which
//!   every writer and every snapshot builder holds;
//! - `Node` is the live array node: a writer bumps its version (the
//!   interpreter's `Trace::note_mutable_access`) and then writes its items in
//!   place;
//! - `Cell::published` is the cell's published word, holding an immutable
//!   `Snap` that `crossbeam_epoch` reclaims after its unlink.
//!
//! Loom explores every interleaving of a writer and readers, and checks that
//! no access to a `loom::cell::UnsafeCell` (the node's items, a snapshot's
//! items) races with a write, which is exactly the §6.1 claim: a fast-path
//! reader reads only fully published, never-written snapshot memory and never
//! the live node. The assertions add the value-level claims: a read returns a
//! value that was stored, and a read ordered after a write (by a join) sees it.

#[cfg(test)]
mod model {
    use crossbeam_epoch::{Atomic, Collector, LocalHandle, Owned};
    use loom::cell::UnsafeCell;
    use loom::sync::Mutex;
    use loom::sync::atomic::{AtomicU64, Ordering};
    use std::sync::Arc;

    struct Node {
        version: Arc<AtomicU64>,
        items: UnsafeCell<[u64; 2]>,
    }

    struct Snap {
        items: UnsafeCell<[u64; 2]>,
        version: Arc<AtomicU64>,
        seen: u64,
    }

    struct Cell {
        lock: Mutex<()>,
        node: Node,
        published: Atomic<Snap>,
    }

    impl Cell {
        fn new() -> Self {
            Self {
                lock: Mutex::new(()),
                node: Node {
                    version: Arc::new(AtomicU64::new(0)),
                    items: UnsafeCell::new([0, 0]),
                },
                published: Atomic::null(),
            }
        }

        /// `ContainerCell::snapshot_element`.
        fn fast_read(&self, handle: &LocalHandle) -> Option<u64> {
            let guard = handle.pin();
            let shared = self.published.load(Ordering::Acquire, &guard);
            // SAFETY: pinned before the load; a snapshot is destroyed only
            // through `defer_destroy` after its unlink.
            let snap = unsafe { shared.as_ref() }?;
            if snap.version.load(Ordering::Acquire) != snap.seen {
                return None;
            }
            // SAFETY: a published snapshot is never written again.
            Some(snap.items.with(|items| unsafe { (*items)[0] }))
        }

        /// The locked read plus `try_publish_snapshot`.
        fn slow_read(&self, handle: &LocalHandle) -> u64 {
            let _exclusion = self.lock.lock().unwrap();
            // SAFETY: the node's items are touched only under `lock`.
            let items = self.node.items.with(|items| unsafe { *items });
            let seen = self.node.version.load(Ordering::Acquire);
            let snap = Owned::new(Snap {
                items: UnsafeCell::new(items),
                version: Arc::clone(&self.node.version),
                seen,
            });
            let guard = handle.pin();
            let unlinked = self.published.swap(snap, Ordering::AcqRel, &guard);
            if !unlinked.is_null() {
                // SAFETY: unlinked under `lock`, so no later pin can load it.
                unsafe { guard.defer_destroy(unlinked) };
            }
            items[0]
        }

        fn read(&self, handle: &LocalHandle) -> u64 {
            self.fast_read(handle)
                .unwrap_or_else(|| self.slow_read(handle))
        }

        /// An in-place element write: version first, then the items.
        fn write(&self, value: u64) {
            let _exclusion = self.lock.lock().unwrap();
            self.node.version.fetch_add(1, Ordering::AcqRel);
            // SAFETY: the node's items are touched only under `lock`.
            self.node
                .items
                .with_mut(|items| unsafe { *items = [value, value] });
        }

        /// A whole-slot replacement: the cell's word is unlinked.
        fn unlink(&self, handle: &LocalHandle) {
            let _exclusion = self.lock.lock().unwrap();
            let guard = handle.pin();
            let unlinked = self
                .published
                .swap(crossbeam_epoch::Shared::null(), Ordering::AcqRel, &guard);
            if !unlinked.is_null() {
                // SAFETY: unlinked under `lock`.
                unsafe { guard.defer_destroy(unlinked) };
            }
        }
    }

    const PREEMPTIONS: usize = 2;

    #[test]
    fn readers_never_race_a_writer_and_see_its_write_after_join() {
        // crossbeam-epoch's own bookkeeping multiplies the state space, so the
        // exploration is bounded the way loom recommends for such models: every
        // interleaving with at most `PREEMPTIONS` forced context switches.
        let mut builder = loom::model::Builder::new();
        builder.preemption_bound = Some(PREEMPTIONS);
        builder.check(|| {
            let collector = Collector::new();
            let cell = Arc::new(Cell::new());
            // Publish a snapshot of the initial state.
            cell.slow_read(&collector.register());

            let reader = {
                let (cell, collector) = (Arc::clone(&cell), collector.clone());
                loom::thread::spawn(move || {
                    let handle = collector.register();
                    for _ in 0..2 {
                        let v = cell.read(&handle);
                        assert!(v == 0 || v == 1, "read a value never stored: {v}");
                    }
                })
            };
            let writer = {
                let (cell, collector) = (Arc::clone(&cell), collector.clone());
                loom::thread::spawn(move || {
                    cell.write(1);
                    cell.unlink(&collector.register());
                })
            };
            reader.join().unwrap();
            writer.join().unwrap();
            assert_eq!(cell.read(&collector.register()), 1, "stale after join");
        });
    }
}
