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
    // O(e - i), e = elements, i = index (`Vec::insert` on the live range moves the
    // tail only); index 0 of a `NativeBacking` array is O(e).
    // Rakudo: O(1) amortized at the front -- see #9156.
    pub(crate) fn insert(&mut self, index: usize, value: Value) {
        if self.native.is_some() {
            self.items_mut().insert(index, value);
        } else if index == 0 {
            self.unshift_front(value);
        } else {
            self.items.insert(self.head + index, value);
        }
    }

    /// Prepend one element in amortized O(1) (#9121): the dead prefix doubles
    /// as front slack. With none left, it is regrown (see
    /// [`ArrayData::reserve_front`]), so a run of n unshifts moves each element
    /// O(1) times rather than O(n).
    fn unshift_front(&mut self, value: Value) {
        self.reserve_front(1);
        self.head -= 1;
        self.items[self.head] = value;
        self.note_front_inserted(1);
    }

    /// Make at least `k` dead slots available in front of the live range.
    ///
    /// When the dead prefix is too short, the live elements are copied once
    /// into a fresh vector that leaves `k + max(live, 4)` slots of slack in
    /// front, so after the caller consumes `k` of them at least `live` remain:
    /// the next regrow only happens after as many front insertions as the
    /// array held, which keeps a run of unshifts/prepends amortized O(1) per
    /// element. Never called on a `NativeBacking` array (its `items` is only a
    /// seed; the head offset stays 0 there).
    // Cost: O(1) when the slack suffices, else O(e + k), e = live elements.
    fn reserve_front(&mut self, k: usize) {
        if self.head >= k {
            return;
        }
        let live = self.items.len() - self.head;
        let gap = k + live.max(4);
        let mut grown = Vec::with_capacity(gap + live);
        grown.resize(gap, Value::NIL);
        grown.extend(self.items.drain(self.head..));
        self.items = grown;
        self.head = gap;
    }

    /// Insert `values` in front of the live range, in order, without touching
    /// the hole bitmap (the callers adjust it).
    // Cost: O(k) amortized, k = inserted elements (see `reserve_front`).
    fn prepend_live(&mut self, values: Vec<Value>) {
        let k = values.len();
        if k == 0 {
            return;
        }
        self.reserve_front(k);
        let start = self.head - k;
        for (slot, value) in self.items[start..self.head].iter_mut().zip(values) {
            *slot = value;
        }
        self.head = start;
    }

    /// `@a.unshift(|@v)` / `@a.prepend(@v)`: insert all of `values` at the
    /// front, keeping their order, in one operation. Inserting them one by one
    /// at increasing indices moved the whole tail once per element.
    // Cost: O(k) amortized, k = inserted elements, for a boxed array (plus
    // O(h), h = explicitly-assigned indices, when the array has holes); O(e + k),
    // e = elements, for a `NativeBacking` array.
    pub(crate) fn prepend_values(&mut self, values: Vec<Value>) {
        let k = values.len();
        if k == 0 {
            return;
        }
        if self.native.is_some() {
            self.items_mut().splice(0..0, values);
        } else {
            self.prepend_live(values);
        }
        self.note_front_inserted(k);
    }

    /// `@a.splice(start, end - start, |replacement)` on the live range:
    /// remove elements `start..end`, put `replacement` in their place and
    /// return the removed elements. `start <= end <= len` is the caller's
    /// contract.
    ///
    /// At the front the removal only advances the head offset and the
    /// replacement goes into the dead prefix, so no surviving element moves;
    /// elsewhere it is one `Vec::splice`, which moves the tail at most once.
    /// The hole bitmap is shifted along with the elements.
    // Cost: O(n + r) amortized at the front, n = removed, r = replacement
    // elements; otherwise O(n + r + (e - end)), e = elements; plus O(h), h =
    // explicitly-assigned indices, when the array has holes. A `NativeBacking`
    // array is O(e + r).
    pub(crate) fn splice_live(
        &mut self,
        start: usize,
        end: usize,
        replacement: Vec<Value>,
    ) -> Vec<Value> {
        let inserted = replacement.len();
        let removed: Vec<Value> = if self.native.is_some() {
            self.items_mut().splice(start..end, replacement).collect()
        } else if start == 0 {
            let head = self.head;
            let removed = self.items[head..head + end]
                .iter_mut()
                .map(|slot| std::mem::replace(slot, Value::NIL))
                .collect();
            self.head += end;
            self.prepend_live(replacement);
            let live = self.items.len() - self.head;
            if live == 0 {
                self.items.clear();
                self.head = 0;
            } else if self.head > live + inserted.max(4) {
                // A long run of front removals: drop the dead prefix once it
                // outgrows the live part (the `shift_front` rule, with the
                // slack a following prepend would want left in place).
                self.compact_head();
            }
            removed
        } else {
            let head = self.head;
            self.items
                .splice(head + start..head + end, replacement)
                .collect()
        };
        if let Some(initialized) = self.initialized.as_mut() {
            let old = std::mem::take(initialized);
            *initialized = old
                .into_iter()
                .filter_map(|i| {
                    if i < start {
                        Some(i)
                    } else if i < end {
                        None
                    } else {
                        Some(i - end + start + inserted)
                    }
                })
                .chain(start..start + inserted)
                .collect();
        }
        removed
    }

    /// Mutably borrow the live elements for in-place overwrites (`@a[$i] =
    /// $v`, element-wise rewrites). Unlike [`ArrayData::items_mut`] it never
    /// compacts the dead prefix a `shift`/`unshift` left behind, because an
    /// overwrite neither grows nor shrinks the array.
    // Cost: O(1) for a boxed array; a `NativeBacking` array first syncs its
    // decode cache (see `items_mut`).
    pub(crate) fn live_mut(&mut self) -> &mut [Value] {
        if let Some(nb) = &mut self.native {
            debug_assert_eq!(self.head, 0, "a native-backed array has no head offset");
            nb.sync_into_seed_mut(&mut self.items);
        }
        &mut self.items[self.head..]
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

    // The mutators below work on the live range `items[head..]` directly for a
    // boxed array, so none of them pays `items_mut`'s head compaction.

    // Cost: O(1) amortized at index 0; otherwise O(e - i), e = elements, i = index.
    pub(crate) fn remove(&mut self, index: usize) -> Value {
        if index == 0
            && let Some(value) = self.shift_front()
        {
            return value;
        }
        if self.native.is_some() {
            return self.items_mut().remove(index);
        }
        self.items.remove(self.head + index)
    }

    // Cost: O(|new_len - e| + 1), e = elements.
    pub(crate) fn resize(&mut self, new_len: usize, value: Value) {
        if self.native.is_some() {
            self.items_mut().resize(new_len, value);
        } else {
            self.items.resize(self.head + new_len, value);
            self.reset_head_if_empty();
        }
    }

    // Cost: O(e - len), e = elements (the dropped elements).
    pub(crate) fn truncate(&mut self, len: usize) {
        if self.native.is_some() {
            self.items_mut().truncate(len);
        } else {
            self.items.truncate(self.head + len);
            self.reset_head_if_empty();
        }
    }

    // Cost: O(e - at), e = elements (the split-off tail).
    pub(crate) fn split_off(&mut self, at: usize) -> Vec<Value> {
        if self.native.is_some() {
            return self.items_mut().split_off(at);
        }
        let tail = self.items.split_off(self.head + at);
        self.reset_head_if_empty();
        tail
    }

    // Cost: O(n + (e - end)), n = drained elements, e = elements.
    pub(crate) fn drain<R: std::ops::RangeBounds<usize>>(
        &mut self,
        range: R,
    ) -> std::vec::Drain<'_, Value> {
        use std::ops::Bound;
        if self.native.is_some() {
            return self.items_mut().drain(range);
        }
        let head = self.head;
        let start = match range.start_bound() {
            Bound::Included(&s) => s,
            Bound::Excluded(&s) => s + 1,
            Bound::Unbounded => 0,
        };
        let end = match range.end_bound() {
            Bound::Included(&e) => e + 1,
            Bound::Excluded(&e) => e,
            Bound::Unbounded => self.items.len() - head,
        };
        self.items.drain(head + start..head + end)
    }

    /// Drop an all-dead prefix once nothing live is left behind it, so an
    /// emptied array does not keep its slack around.
    fn reset_head_if_empty(&mut self) {
        if self.head >= self.items.len() {
            self.items.clear();
            self.head = 0;
        }
    }
}
