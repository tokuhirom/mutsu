//! `ArrayData`'s front head offset (#9121), serving both `shift` and `unshift`, and the `Vec` mutators that
//! have to be forwarded explicitly now that `Deref` targets the live slice.

use super::{ArrayData, Value};

impl ArrayData {
    /// The live elements as a slice (what `Vec::as_slice` answered before
    /// `Deref` moved to `[Value]`).
    pub(crate) fn as_slice(&self) -> &[Value] {
        self.items()
    }

    /// The live elements straight from the element vector, bypassing the
    /// native-backing sync (for the NaN-box peek path, which never sees a
    /// native-backed array's decode cache).
    pub(crate) fn live_slice_raw(&self) -> &[Value] {
        &self.items[self.head..]
    }

    /// Drop the dead prefix [`ArrayData::shift_front`] left behind, so
    /// `items` is exactly the live elements again.
    pub(super) fn compact_head(&mut self) {
        if self.head > 0 {
            self.items.drain(..self.head);
            self.head = 0;
        }
    }

    /// Remove and return the first element in amortized O(1) (#9121).
    ///
    /// `Vec::remove(0)` moves every remaining element, which made consuming a
    /// list from the front (`nqp::shift` loops, `Array.shift` loops)
    /// quadratic. Instead the slot is overwritten with `Value::NIL` and the
    /// live range's start advances. The dead prefix is dropped once it
    /// outgrows the live part, so it never costs more than one memmove per
    /// element shifted and never holds more slots than the array has live.
    // Cost: O(1) amortized; O(e), e = elements, for an array with a
    // `NativeBacking` (ADR-0030), which still takes `Vec::remove(0)` (a plain
    // `my int @a` measures O(1): scripts/array-complexity-check.sh).
    // Rakudo: O(1) amortized -- see #9156.
    pub(crate) fn shift_front(&mut self) -> Option<Value> {
        let value = if self.native.is_some() {
            let items = self.items_mut();
            if items.is_empty() {
                None
            } else {
                Some(items.remove(0))
            }
        } else if self.head >= self.items.len() {
            None
        } else {
            let value = std::mem::replace(&mut self.items[self.head], Value::NIL);
            self.head += 1;
            let live = self.items.len() - self.head;
            if live == 0 {
                self.items.clear();
                self.head = 0;
            } else if self.head > live {
                self.compact_head();
            }
            Some(value)
        };
        if value.is_some() {
            self.shift_initialized_after_front();
        }
        value
    }

    // `Vec` mutators, forwarded through [`ArrayData::items_mut`]: `Deref`
    // targets the live slice (`[Value]`) since #9121, so they no longer come
    // for free.

    pub(crate) fn push(&mut self, value: Value) {
        if self.native.is_none() {
            // Appending never disturbs the dead prefix; no compaction needed.
            self.items.push(value);
        } else {
            self.items_mut().push(value);
        }
    }

    pub(crate) fn pop(&mut self) -> Option<Value> {
        if self.native.is_some() {
            return self.items_mut().pop();
        }
        if self.head >= self.items.len() {
            return None;
        }
        let value = self.items.pop();
        if self.head == self.items.len() {
            self.items.clear();
            self.head = 0;
        }
        value
    }

    pub(crate) fn extend<I: IntoIterator<Item = Value>>(&mut self, iter: I) {
        if self.native.is_none() {
            self.items.extend(iter);
        } else {
            self.items_mut().extend(iter);
        }
    }

    // Cost: O(1) amortized at index 0 of a boxed array (`unshift_front`); otherwise
    // O(e), e = elements (compaction via `items_mut` plus `Vec::insert`), including
    // index 0 of a `NativeBacking` array. Rakudo: O(1) amortized at the front -- see #9156.
    pub(crate) fn insert(&mut self, index: usize, value: Value) {
        if index == 0 && self.native.is_none() {
            self.unshift_front(value);
        } else {
            self.items_mut().insert(index, value);
        }
    }

    /// Prepend one element in amortized O(1) (#9121): the dead prefix doubles
    /// as front slack. With none left, it is regrown to the live length, so a
    /// run of n unshifts moves each element O(1) times rather than O(n).
    fn unshift_front(&mut self, value: Value) {
        if self.head == 0 {
            let gap = self.items.len().max(4);
            let mut grown = Vec::with_capacity(gap + self.items.len());
            grown.resize(gap, Value::NIL);
            grown.append(&mut self.items);
            self.items = grown;
            self.head = gap;
        }
        self.head -= 1;
        self.items[self.head] = value;
        self.note_front_inserted(1);
    }

    /// Shift the embedded explicit-assignment bitmap along with a front
    /// removal.  The bitmap uses live-array indices, while the optimized
    /// storage keeps a dead prefix behind `head`.
    fn shift_initialized_after_front(&mut self) {
        if let Some(initialized) = self.initialized.as_mut() {
            let old = std::mem::take(initialized);
            *initialized = old.into_iter().filter_map(|i| i.checked_sub(1)).collect();
        }
    }

    /// Record an explicit front insertion in the hole bitmap.  A `None`
    /// bitmap means every existing slot is present, and remains sufficient
    /// after inserting another present slot.
    pub(crate) fn note_front_inserted(&mut self, count: usize) {
        if let Some(initialized) = self.initialized.as_mut() {
            let old = std::mem::take(initialized);
            *initialized = old.into_iter().map(|i| i + count).collect();
            initialized.extend(0..count);
        }
    }

    pub(crate) fn remove(&mut self, index: usize) -> Value {
        if index == 0
            && let Some(value) = self.shift_front()
        {
            return value;
        }
        self.items_mut().remove(index)
    }

    pub(crate) fn resize(&mut self, new_len: usize, value: Value) {
        self.items_mut().resize(new_len, value);
    }

    pub(crate) fn truncate(&mut self, len: usize) {
        self.items_mut().truncate(len);
    }

    pub(crate) fn split_off(&mut self, at: usize) -> Vec<Value> {
        self.items_mut().split_off(at)
    }

    pub(crate) fn drain<R: std::ops::RangeBounds<usize>>(
        &mut self,
        range: R,
    ) -> std::vec::Drain<'_, Value> {
        self.items_mut().drain(range)
    }
}
