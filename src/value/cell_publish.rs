//! A cross-thread read of a `ContainerCell` that writes no shared cache line
//! (ADR-8069, slice 1).
//!
//! A `ContainerCell`'s value lives in `slot: Mutex<Value>`, and ADR-0068 has
//! every reader take the cell's stripe and that mutex. Both are atomic
//! read-modify-writes on lines every reading thread shares, so four threads
//! reading one bound `Int` ran *slower* than one (ADR-8069 §1: 0.47x scaling
//! for `$s += $sref`).
//!
//! This module gives the cell a second word, `published`, that a reader can
//! load without writing anything. Slice 1 publishes only a **payload-free**
//! value — a small `Int`, a `Num`, a `Bool`, `Nil`, `Whatever`, a type object:
//! a word that owns no heap reference. Its bits are the whole value, so a
//! reader that copies them out holds a complete, independent `Value` and
//! dereferences nothing, and a stale or concurrently replaced word can at worst
//! be an older value. That is why this slice needs no `unsafe` code and no
//! deferred reclamation: the epoch (ADR-8069 D2, `crossbeam-epoch`) arrives
//! with the slices that publish heap values (container nodes, strings).
//!
//! # The protocol
//!
//! - `published` holds either the bits of `slot`'s current value (when that is
//!   payload-free), [`PUB_EMPTY`] (it is not, or nobody has published it yet),
//!   or [`PUB_ESCAPED`] (see below). Page 0 of a NaN-box word is never a
//!   value, so neither sentinel can be mistaken for one.
//! - **Only a holder of `slot`'s mutex writes `published`** (or a holder of
//!   `&mut ContainerCell`). [`ContainerCell::lock`] returns a [`CellGuard`]
//!   whose drop re-syncs `published` from the slot it is about to release, so
//!   every write made through the mutex is published before the mutex is
//!   released. A reader that loaded the previous word before that store saw a
//!   value the cell held until the writer's store: the read is ordered before
//!   the write, which is a legal outcome of an unsynchronized race
//!   (ADR-12814) and the same one a reader blocked on the mutex would allow.
//! - A writer that mutates the slot **after releasing the mutex** — the raw
//!   pointer `descend_container_ref` derives for an element store — cannot be
//!   followed by a re-sync. [`CellGuard::escape_slot`] therefore marks the
//!   cell [`PUB_ESCAPED`] under the mutex before handing the pointer out, and
//!   an escaped cell is never published again: its readers keep ADR-0068's
//!   stripe-and-mutex path, exactly as before this module existed.
//! - Nothing is published until a second VM mutator thread exists
//!   (`multi_mutator_threads_live`, sticky), so a single-threaded program's
//!   guards do one relaxed load more and its readers find [`PUB_EMPTY`] and
//!   take the path they always took. Once the latch is set, a cell's first
//!   locked access (a reader's slow path included) publishes it.

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{LockResult, MutexGuard, PoisonError};

use super::container_lock::multi_mutator_threads_live;
use super::nanbox::payload_free_word;
use super::{ContainerCell, Value};

/// `published` holds no value: the slot's value is not payload-free, or no
/// locked access has published it since the second mutator thread appeared.
pub(crate) const PUB_EMPTY: u64 = 0;

/// The slot was handed out as a raw pointer and may change with the mutex
/// released, so `published` can never be trusted again for this cell.
pub(crate) const PUB_ESCAPED: u64 = 1;

/// Exclusive access to a cell's slot. Derefs to the held `Value`; dropping it
/// publishes what the slot holds then (see the module docs).
pub struct CellGuard<'a> {
    guard: MutexGuard<'a, Value>,
    published: &'a AtomicU64,
}

impl std::ops::Deref for CellGuard<'_> {
    type Target = Value;
    #[inline]
    fn deref(&self) -> &Value {
        &self.guard
    }
}

impl std::ops::DerefMut for CellGuard<'_> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Value {
        &mut self.guard
    }
}

impl std::fmt::Debug for CellGuard<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("CellGuard").field(&*self.guard).finish()
    }
}

impl Drop for CellGuard<'_> {
    // Cost: O(1).
    fn drop(&mut self) {
        // Runs before the `MutexGuard` field is dropped, so the slot is still
        // held: no other writer can change it between this read and the store.
        if multi_mutator_threads_live() {
            sync_published(self.published, &self.guard);
        }
    }
}

impl CellGuard<'_> {
    /// Hand out a raw pointer to the slot that its caller will write through
    /// after this guard (and the mutex) is released. The cell is marked
    /// [`PUB_ESCAPED`] first, so no reader trusts `published` for it again.
    // Cost: O(1).
    pub(crate) fn escape_slot(mut self) -> *mut Value {
        self.published.store(PUB_ESCAPED, Ordering::Release);
        std::ptr::from_mut::<Value>(&mut self.guard)
    }
}

/// Bring `published` in line with `value`, the slot's content. The caller
/// holds the slot's mutex.
// Cost: O(1).
#[inline]
fn sync_published(published: &AtomicU64, value: &Value) {
    let current = published.load(Ordering::Relaxed);
    if current == PUB_ESCAPED {
        return;
    }
    let bits = value.nanbox_bits();
    let next = if payload_free_word(bits).is_some() {
        bits
    } else {
        PUB_EMPTY
    };
    // Skipping an unchanged word keeps a reader's slow path (and a cell that
    // holds a container, which is never published) from writing the line.
    if next != current {
        published.store(next, Ordering::Release);
    }
}

impl ContainerCell {
    /// Lock the cell's slot. The returned guard publishes the slot's value
    /// when it is dropped (ADR-8069; see `cell_publish`).
    // Cost: O(1), one mutex acquisition.
    pub fn lock(&self) -> LockResult<CellGuard<'_>> {
        let published = &self.published;
        match self.slot.lock() {
            Ok(guard) => Ok(CellGuard { guard, published }),
            Err(poisoned) => Err(PoisonError::new(CellGuard {
                guard: poisoned.into_inner(),
                published,
            })),
        }
    }

    /// The slot, through exclusive access to the whole cell. No reader can
    /// exist meanwhile, but every later one must not see the word published
    /// before this write, so it is cleared (an escaped cell stays escaped).
    // Cost: O(1).
    pub fn get_mut(&mut self) -> LockResult<&mut Value> {
        let published = self.published.get_mut();
        if *published != PUB_ESCAPED {
            *published = PUB_EMPTY;
        }
        self.slot.get_mut()
    }

    /// The cell's value without taking its mutex or its ADR-0068 stripe, when
    /// it is a published payload-free word; `None` sends the caller to the
    /// locked read. One atomic load and no store, so readers on different
    /// cores do not contend (ADR-8069 §2 D1).
    // Cost: O(1), one atomic load.
    #[inline]
    pub(crate) fn published_value(&self) -> Option<Value> {
        payload_free_word(self.published.load(Ordering::Acquire)).map(Value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn published_bits(cell: &ContainerCell) -> u64 {
        cell.published.load(Ordering::Relaxed)
    }

    #[test]
    fn sync_publishes_only_payload_free_words() {
        let word = AtomicU64::new(PUB_EMPTY);
        sync_published(&word, &Value::Int(7));
        assert_eq!(word.load(Ordering::Relaxed), Value::Int(7).nanbox_bits());
        sync_published(&word, &Value::str_from("heap"));
        assert_eq!(word.load(Ordering::Relaxed), PUB_EMPTY);
    }

    #[test]
    fn escaped_word_is_never_republished() {
        let word = AtomicU64::new(PUB_ESCAPED);
        sync_published(&word, &Value::Int(7));
        assert_eq!(word.load(Ordering::Relaxed), PUB_ESCAPED);
    }

    #[test]
    fn sentinels_are_not_values() {
        assert!(payload_free_word(PUB_EMPTY).is_none());
        assert!(payload_free_word(PUB_ESCAPED).is_none());
    }

    #[test]
    fn published_value_round_trips_and_get_mut_clears() {
        let mut cell = ContainerCell::new(Value::Int(3));
        // Nothing is published before a guard syncs it.
        assert!(cell.published_value().is_none());
        sync_published(&cell.published, &Value::Int(3));
        assert!(matches!(cell.published_value(), Some(v) if v == Value::Int(3)));
        *cell.get_mut().unwrap() = Value::Int(4);
        assert_eq!(published_bits(&cell), PUB_EMPTY);
        assert!(cell.published_value().is_none());
    }

    #[test]
    fn escape_marks_the_cell_for_good() {
        let cell = ContainerCell::new(Value::Int(1));
        let guard = cell.lock().unwrap();
        let _ = guard.escape_slot();
        assert_eq!(published_bits(&cell), PUB_ESCAPED);
        drop(cell.lock().unwrap());
        assert_eq!(published_bits(&cell), PUB_ESCAPED);
    }
}
