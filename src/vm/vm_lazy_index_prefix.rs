//! The index list of a slice whose subscript is an infinite sequence
//! (`@a[0, 2 ... *]`, `@a[1..*]` as a sequence LazyList).
//!
//! A lazy index stops at the first index past the end of the array, so a
//! slice only needs the indices up to that one. An infinite sequence cannot be
//! strictly forced (#10846), so it is pulled in growing chunks until the
//! out-of-range index has been seen.

use super::*;

impl Interpreter {
    /// The leading indices of the infinite sequence `index` that a slice of an
    /// array of `len` elements reads: everything up to and including the first
    /// index `>= len` (the slice loop stops there). `None` when `index` is not
    /// an infinite sequence.
    ///
    /// Cost: O(p), p = indices up to the first out-of-range one. An index
    /// sequence that never leaves the array (`0 xx *`-shaped) does not
    /// terminate, as in Rakudo.
    pub(super) fn lazy_index_prefix_within(
        &mut self,
        index: &LazyList,
        len: usize,
    ) -> Option<Result<Vec<Value>, RuntimeError>> {
        index.sequence_spec.as_ref()?;
        let mut want = len.saturating_add(1).max(16);
        let mut seen = 0;
        loop {
            let items = match self.force_lazy_list_vm_n(index, want) {
                Ok(items) => items,
                Err(e) => return Some(Err(e)),
            };
            if let Some(p) = items[seen..]
                .iter()
                .position(|v| Self::index_to_usize(v).is_none_or(|i| i >= len))
            {
                let mut items = items;
                items.truncate(seen + p + 1);
                return Some(Ok(items));
            }
            seen = items.len();
            want = want.saturating_mul(2);
        }
    }
}
