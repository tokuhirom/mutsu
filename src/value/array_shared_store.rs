//! ADR-18752 D2: an in-range element store into a shared array node, made
//! while the node's stripes are held on the SHARED side (D1), so that two such
//! stores of one array run concurrently.
//!
//! The store never creates a `&mut ArrayData`. It swaps the slot's NaN-box word
//! atomically through a pointer derived from the node's buffer. This slice
//! narrows D2 further: both the new word and the word it replaces must be
//! refcount-free (a small Int, a Num, a Bool, a type object). Then no
//! ownership moves through the swap, and nothing has to be dropped or retired.
//! A store that would displace or install an owning word takes the ordinary,
//! exclusive path.

use std::sync::atomic::{AtomicU64, Ordering};

use super::nanbox::word_is_refcount_free;
use super::{ArrayData, ArrayKind, Value};
use crate::gc::{Gc, Trace};

impl ArrayData {
    /// Whether an element store into this node can be a plain word swap: a
    /// plain `Array` with no native backing, no element type or default to
    /// check, no shape, no hole bookkeeping and no dead front prefix. Anything
    /// else has a check, a decay or a side table to update, which the
    /// ordinary path does.
    // Cost: O(1).
    fn takes_word_swap_store(&self) -> bool {
        self.native.is_none()
            && self.value_type.is_none()
            && self.declared_type.is_none()
            && self.default.is_none()
            && self.shape.is_none()
            && self.initialized.is_none()
            && self.head == 0
    }
}

/// Store `value` at `idx` of `node` by swapping the slot's word, when both
/// that word and `value` are refcount-free and `idx` is in range. Returns
/// whether it stored; on `false` nothing was written and the caller takes the
/// ordinary path.
///
/// The caller must hold `node`'s stripes on the shared side
/// ([`ContainerStructGuard::acquire_shared_pair`]): that is what excludes every
/// structural write (which could reallocate or free the buffer) and every
/// live-node reader (which reads the slots non-atomically) for the duration.
///
/// [`ContainerStructGuard::acquire_shared_pair`]:
///     crate::value::container_lock::ContainerStructGuard::acquire_shared_pair
// Cost: O(1).
pub(crate) fn store_word_in_range(
    node: &Gc<ArrayData>,
    kind: ArrayKind,
    idx: usize,
    value: &Value,
) -> bool {
    let new_bits = value.nanbox_bits();
    if kind != ArrayKind::Array || value.is_nil() || !word_is_refcount_free(new_bits) {
        return false;
    }
    let data: &ArrayData = node;
    if !data.takes_word_swap_store() || idx >= data.items.len() {
        return false;
    }
    // `Vec::as_ptr` materializes no reference to the elements, so the other
    // stores' swaps through their own element pointers do not alias a shared
    // reference to the slot.
    let base = data.items.as_ptr();
    // SAFETY: `idx < len`, so the element pointer is in bounds of the live
    // buffer. The buffer cannot be reallocated or freed meanwhile: either would
    // be a structural write, which takes the node's stripe on the exclusive
    // side, and the caller holds it on the shared side. A `Value` is one
    // `NonZeroU64` word, so the element is a valid, 8-aligned `u64` for
    // `AtomicU64::from_ptr`. Every other access to the slot while the shared
    // side is held is another store's atomic operation on the same word:
    // non-atomic readers (iteration, `.elems`, the locked element read) hold
    // the exclusive side, and ADR-18637 snapshot readers read their own copy.
    let slot = unsafe { AtomicU64::from_ptr(base.add(idx).cast_mut().cast::<u64>()) };
    let current = slot.load(Ordering::Acquire);
    // A bound element (a `ContainerRef` cell), an element-share word or any
    // other owning word is not ours to replace here: the ordinary path writes
    // through it or drops it.
    if !word_is_refcount_free(current) {
        return false;
    }
    // A concurrent store to the same slot won the race: let the ordinary path
    // decide, rather than retry under the shared side.
    if slot
        .compare_exchange(current, new_bits, Ordering::AcqRel, Ordering::Acquire)
        .is_err()
    {
        return false;
    }
    // D3: invalidate an ADR-18637 snapshot of this node, only if one was
    // ever published (a write-heavy array never earns one).
    data.note_mutable_access();
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::container_lock::{ContainerStructGuard, note_mutator_thread_spawned};

    fn node_of(value: &Value) -> (Gc<ArrayData>, ArrayKind) {
        match value.view() {
            crate::value::ValueView::Array(node, kind) => (node.clone(), kind),
            _ => unreachable!("an Array"),
        }
    }

    #[test]
    fn swaps_only_refcount_free_words_in_range() {
        let arr = Value::real_array(vec![Value::int(1), Value::str_from("s")]);
        let (node, kind) = node_of(&arr);
        assert!(store_word_in_range(&node, kind, 0, &Value::int(9)));
        assert_eq!(node.items()[0], Value::int(9));
        // An owning word in the slot, an owning new word, out of range, Nil.
        assert!(!store_word_in_range(&node, kind, 1, &Value::int(9)));
        assert!(!store_word_in_range(&node, kind, 0, &Value::str_from("t")));
        assert!(!store_word_in_range(&node, kind, 2, &Value::int(9)));
        assert!(!store_word_in_range(&node, kind, 0, &Value::NIL));
    }

    /// Two threads storing into disjoint and identical slots of one node
    /// under the shared side, racing a third that iterates under the
    /// exclusive side (ADR-18752 §4: Miri's race detector checks D1 and D2).
    #[test]
    fn concurrent_shared_stores_and_an_exclusive_reader() {
        note_mutator_thread_spawned();
        let arr = Value::real_array((0..4).map(Value::int).collect());
        let (node, kind) = node_of(&arr);
        let addr = Gc::as_ptr(&node) as usize;
        let rounds = if cfg!(miri) { 4 } else { 200 };
        std::thread::scope(|s| {
            for t in 0..2i64 {
                let node = node.clone();
                s.spawn(move || {
                    for r in 0..rounds {
                        let _guard = ContainerStructGuard::acquire_shared_pair(addr, addr);
                        store_word_in_range(&node, kind, t as usize, &Value::int(r));
                        store_word_in_range(&node, kind, 3, &Value::int(t));
                    }
                });
            }
            let node = node.clone();
            s.spawn(move || {
                for _ in 0..rounds {
                    let _guard = ContainerStructGuard::acquire(addr);
                    let sum: i64 = node
                        .items()
                        .iter()
                        .map(|v| match v.view() {
                            crate::value::ValueView::Int(i) => i,
                            _ => 0,
                        })
                        .sum();
                    assert!(sum >= 0);
                }
            });
        });
        assert!(matches!(
            node.items()[3].view(),
            crate::value::ValueView::Int(0 | 1)
        ));
    }
}
