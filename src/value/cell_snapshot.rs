//! A shared array's element read that writes no shared cache line
//! (ADR-18637, the amendment of ADR-8069 D3).
//!
//! An array held by a `ContainerCell` is mutated in place by every writer
//! (`ArrayData::items_mut()`), so a reader cannot look at the live node
//! without exclusion. Instead the cell can publish an immutable **snapshot**:
//! a private copy of the node, validated against a mutation version the node
//! carries (`ArrayData::snapshot_version`, bumped by
//! `Trace::note_mutable_access` before every in-place write). A reader pins
//! the epoch, loads the snapshot, checks the version, and reads the element
//! out of the copy; anything else takes ADR-0068's locked path.
//!
//! # Why the read cannot race (ADR-18637 §6.1)
//!
//! 1. The cell's `published` word is an `AtomicU64`; only a holder of the
//!    cell's mutex stores to it (`cell_publish`'s protocol). A snapshot word
//!    is the snapshot's address tagged with [`SNAP_TAG`] in page 0, which no
//!    value word uses.
//! 2. A snapshot is built completely by the rebuilding reader under the
//!    cell's mutex and ADR-0068 stripes, then published with a release swap.
//!    A reader's acquire load synchronizes with it. Nothing writes a snapshot
//!    after that: it is reachable only through the word and the epoch's
//!    retired list, neither of which hands out `&mut`.
//! 3. A snapshot leaves the word only through a swap under the cell's mutex
//!    (or through `&mut ContainerCell`, when no reader can exist), and the
//!    remover hands it to `crossbeam_epoch`'s deferred destruction, which
//!    runs only after every thread pinned at the unlink has unpinned. A
//!    reader pins before it loads, so the snapshot outlives its read.
//! 4. The version counter is an `Arc<AtomicU64>` the snapshot keeps alive;
//!    the reader never dereferences the live node.
//! 5. The element is cloned out of the snapshot's own items, which hold a
//!    reference to it, so the clone's refcount increment lands on a live node.

use std::sync::Arc;
use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};

use super::cell_publish::{PUB_EMPTY, PUB_ESCAPED};
use super::container_lock::{ContainerStructGuard, multi_mutator_threads_live};
use super::{ArrayData, ArrayKind, ContainerCell, Value, ValueView};

/// The tag a snapshot word carries in its low bits. The word's page (top 16
/// bits) is 0, which no value uses; `cell_publish`'s sentinels are 0 and 1.
const SNAP_TAG: u64 = 2;
const TAG_MASK: u64 = 7;

/// An immutable copy of a cell's array node, valid while `version` still
/// reads `seen`.
pub(crate) struct CellSnapshot {
    array: ArrayData,
    kind: ArrayKind,
    version: Arc<AtomicU64>,
    seen: u64,
}

/// The snapshot a published word names, if it names one.
// Cost: O(1).
#[inline]
fn snapshot_ptr(word: u64) -> Option<*const CellSnapshot> {
    (word >> 48 == 0 && word & TAG_MASK == SNAP_TAG)
        .then(|| std::ptr::with_exposed_provenance::<CellSnapshot>((word & !TAG_MASK) as usize))
}

/// Whether `word` names a snapshot.
// Cost: O(1).
#[inline]
pub(crate) fn is_snapshot_word(word: u64) -> bool {
    snapshot_ptr(word).is_some()
}

/// Hand a word that was just unlinked from a cell (under its mutex) to the
/// epoch, so the snapshot it names is freed once no pinned reader can still
/// be reading it. A word that names no snapshot needs nothing.
// Cost: O(1) amortized (the deferred free runs on a later epoch advance).
pub(crate) fn retire_word(word: u64) {
    if snapshot_ptr(word).is_none() {
        return;
    }
    let addr = (word & !TAG_MASK) as usize;
    let guard = crossbeam_epoch::pin();
    // SAFETY: `word` was unlinked from its cell by the caller (a swap under
    // the cell's mutex), so no reader that pins from now on can load it; the
    // epoch runs this closure only after every thread pinned before the
    // unlink has unpinned, so no reader is still inside the snapshot. The box
    // was created by `Box::into_raw` in `publish_snapshot` and is freed once,
    // here: a word is unlinked at most once. `CellSnapshot` holds only
    // `Send` data (`Value`, `ArrayData`, `Arc<AtomicU64>`), so dropping it on
    // whichever thread advances the epoch is sound.
    unsafe {
        guard.defer_unchecked(move || {
            drop(Box::from_raw(std::ptr::with_exposed_provenance_mut::<
                CellSnapshot,
            >(addr)));
        });
    }
}

/// Free the snapshot `word` names right away. For a caller holding
/// `&mut ContainerCell`: no other reference to the cell exists, so no reader
/// can be inside the snapshot.
// Cost: O(n), n = the snapshot's elements.
pub(crate) fn free_word_now(word: u64) {
    if let Some(ptr) = snapshot_ptr(word) {
        // SAFETY: the caller holds the only reference to the cell that owned
        // the word, so no reader can have loaded it and still be reading; the
        // box came from `Box::into_raw` in `publish_snapshot` and the word is
        // being dropped from the cell, so this is its single free.
        drop(unsafe { Box::from_raw(ptr.cast_mut()) });
    }
}

/// Whether the snapshot `word` names is still a snapshot of `value`, the
/// slot's content: the same node (by its version counter) seen as the same
/// kind. The caller holds the cell's mutex.
// Cost: O(1).
pub(crate) fn snapshot_matches_slot(word: u64, value: &Value) -> bool {
    let Some(ptr) = snapshot_ptr(word) else {
        return false;
    };
    let ValueView::Array(node, kind) = value.view() else {
        return false;
    };
    // SAFETY: `word` is still the cell's published word and the caller holds
    // the cell's mutex, which every unlink of the word requires, so the
    // snapshot has not been retired and is alive.
    let snapshot = unsafe { &*ptr };
    snapshot.kind == kind
        && node
            .snapshot_version
            .get()
            .is_some_and(|version| Arc::ptr_eq(version, &snapshot.version))
}

/// Slow reads a snapshot rebuild has to be preceded by: the copy then costs at
/// most about sixteen element clones per read (ADR-18637 S3).
// Cost: O(1).
fn rebuild_threshold(len: usize) -> u32 {
    u32::try_from(len / 16 + 1).unwrap_or(u32::MAX)
}

/// The two counters a cell keeps for its snapshot (ADR-18637 S3).
#[derive(Debug, Default)]
pub(crate) struct SnapshotCounters {
    /// Indexed reads that took the locked path since the last publish.
    slow_reads: AtomicU32,
    /// The slow-read count at which a rebuild is next attempted.
    next_attempt: AtomicU32,
}

impl ContainerCell {
    /// `self[i]` read lock-free from the published snapshot, when the cell
    /// has one and its node has not been written since. `None` sends the
    /// caller to the locked path.
    // Cost: O(1): one pin, two atomic loads and the element clone.
    #[inline]
    pub(crate) fn snapshot_element(&self, i: i64) -> Option<Value> {
        // Unpinned pre-check: a cell with no snapshot pays one load.
        if !is_snapshot_word(self.published.load(Ordering::Relaxed)) {
            return None;
        }
        let _pin = crossbeam_epoch::pin();
        let ptr = snapshot_ptr(self.published.load(Ordering::Acquire))?;
        // SAFETY: this thread pinned the epoch before loading the word, and a
        // snapshot is freed only through `retire_word` after its unlink, which
        // waits for every such pin to end (module docs, point 3); the publish
        // was a release swap this acquire load synchronizes with, so the
        // snapshot is fully built, and nothing writes it afterwards (point 2).
        let snapshot = unsafe { &*ptr };
        if snapshot.version.load(Ordering::Acquire) != snapshot.seen {
            return None;
        }
        snapshot.array.plain_positional_read(snapshot.kind, i)
    }

    /// Count an indexed read of this cell that took the locked path, and
    /// publish a snapshot once enough of them have paid for its copy.
    // Cost: O(1), or O(n) on the read that rebuilds, n = the array's length.
    pub(crate) fn note_slow_indexed_read(&self) {
        if !multi_mutator_threads_live() {
            return;
        }
        let counters = &self.snapshot_counters;
        let reads = counters.slow_reads.fetch_add(1, Ordering::Relaxed) + 1;
        if reads < counters.next_attempt.load(Ordering::Relaxed) {
            return;
        }
        self.try_publish_snapshot(reads);
    }

    /// Build and publish a snapshot of the array this cell holds, if it is
    /// eligible and `reads` slow reads have paid for the copy.
    // Cost: O(n), n = the array's length.
    fn try_publish_snapshot(&self, reads: u32) {
        // The stripes exclude every writer of the cell and of its node
        // (ADR-0068). A thread that already holds a stripe gets `None` and
        // must not build: it would copy without exclusion.
        let Some(_structure) = ContainerStructGuard::acquire_for_celled(self) else {
            return;
        };
        let slot = self
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let current = self.published.load(Ordering::Relaxed);
        // An escaped slot can change without the mutex (`cell_publish`), and
        // the first implementation publishes nothing for it (ADR-18637 S4).
        if current == PUB_ESCAPED || is_snapshot_word(current) {
            return;
        }
        let ValueView::Array(node, kind) = slot.view() else {
            return;
        };
        if !matches!(
            kind,
            ArrayKind::List | ArrayKind::Array | ArrayKind::ItemArray
        ) || node.has_native_backing()
        {
            return;
        }
        let threshold = rebuild_threshold(node.len());
        if reads < threshold {
            self.snapshot_counters
                .next_attempt
                .store(threshold, Ordering::Relaxed);
            return;
        }
        let version = Arc::clone(
            node.snapshot_version
                .get_or_init(|| Arc::new(AtomicU64::new(0))),
        );
        let seen = version.load(Ordering::Acquire);
        let snapshot = Box::new(CellSnapshot {
            array: ArrayData::clone(&node),
            kind,
            version,
            seen,
        });
        let ptr = Box::into_raw(snapshot);
        let addr = ptr.expose_provenance() as u64;
        if addr >> 48 != 0 || addr & TAG_MASK != 0 {
            // Not representable in page 0 on this target: publish nothing.
            // SAFETY: `ptr` came from `Box::into_raw` just above and was
            // never published.
            drop(unsafe { Box::from_raw(ptr) });
            return;
        }
        debug_assert_eq!(current, PUB_EMPTY);
        let unlinked = self.published.swap(addr | SNAP_TAG, Ordering::AcqRel);
        retire_word(unlinked);
        self.snapshot_counters
            .slow_reads
            .store(0, Ordering::Relaxed);
        self.snapshot_counters
            .next_attempt
            .store(0, Ordering::Relaxed);
        // Dropping `slot` re-syncs the word: the slot still holds this node,
        // so the snapshot stays (`snapshot_matches_slot`).
        drop(slot);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc::Gc;

    fn array_value(n: i64) -> Value {
        Value::Array(
            Gc::new(ArrayData::new((0..n).map(Value::Int).collect())),
            ArrayKind::Array,
        )
    }

    #[test]
    fn snapshot_words_are_not_values_or_sentinels() {
        assert!(!is_snapshot_word(PUB_EMPTY));
        assert!(!is_snapshot_word(PUB_ESCAPED));
        assert!(!is_snapshot_word(Value::Int(7).nanbox_bits()));
        assert!(is_snapshot_word(0x1000 | SNAP_TAG));
    }

    #[test]
    fn threshold_amortizes_the_copy() {
        assert_eq!(rebuild_threshold(0), 1);
        assert_eq!(rebuild_threshold(256), 17);
    }

    #[test]
    fn a_write_to_the_node_invalidates_the_snapshot() {
        crate::value::container_lock::note_mutator_thread_spawned();
        let cell = ContainerCell::new(array_value(4));
        for _ in 0..2 {
            cell.note_slow_indexed_read();
        }
        assert!(is_snapshot_word(cell.published.load(Ordering::Relaxed)));
        assert!(matches!(cell.snapshot_element(2), Some(v) if v == Value::Int(2)));
        {
            let slot = cell.lock().unwrap();
            let ValueView::Array(node, _) = slot.view() else {
                unreachable!()
            };
            // SAFETY: the test thread is the only one touching the node.
            let data = unsafe { crate::gc::gc_contents_mut(&node) };
            data.items_mut()[2] = Value::Int(20);
        }
        assert!(cell.snapshot_element(2).is_none(), "stale snapshot served");
    }

    #[test]
    fn replacing_the_slot_unlinks_the_snapshot() {
        crate::value::container_lock::note_mutator_thread_spawned();
        let cell = ContainerCell::new(array_value(2));
        cell.note_slow_indexed_read();
        assert!(is_snapshot_word(cell.published.load(Ordering::Relaxed)));
        *cell.lock().unwrap() = array_value(3);
        assert!(!is_snapshot_word(cell.published.load(Ordering::Relaxed)));
        assert!(cell.snapshot_element(0).is_none());
    }

    /// Readers on several threads race a writer that mutates the node and
    /// replaces the slot; Miri's race detector checks the protocol
    /// (ADR-18637 §6.2). Kept small so it interprets in seconds.
    #[test]
    fn threaded_readers_race_writes_and_republishes() {
        crate::value::container_lock::note_mutator_thread_spawned();
        let rounds = if cfg!(miri) { 20 } else { 2_000 };
        let cell = Gc::new(ContainerCell::new(array_value(8)));
        std::thread::scope(|scope| {
            for _ in 0..2 {
                let cell = cell.clone();
                scope.spawn(move || {
                    for i in 0..rounds {
                        if let Some(v) = cell.snapshot_element((i % 8) as i64) {
                            assert!(matches!(v.view(), ValueView::Int(_)));
                        } else {
                            cell.note_slow_indexed_read();
                        }
                    }
                });
            }
            let writer = cell.clone();
            scope.spawn(move || {
                for i in 0..rounds {
                    let _structure = ContainerStructGuard::acquire_for_celled(&writer);
                    let mut slot = writer.lock().unwrap();
                    if i % 7 == 0 {
                        *slot = array_value(8);
                    } else if let ValueView::Array(node, _) = slot.view() {
                        // SAFETY: the stripes above exclude every other
                        // writer and the snapshot builders of this cell.
                        let data = unsafe { crate::gc::gc_contents_mut(&node) };
                        data.items_mut()[(i % 8) as usize] = Value::Int(i as i64);
                    }
                }
            });
        });
    }
}
