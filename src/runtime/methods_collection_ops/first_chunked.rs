//! `.first` over an Array/List/Seq receiver without decomposing all of it (#9162).

use super::*;

impl Interpreter {
    /// Whether `.first` can read `target`'s items in place (a List or a
    /// reified Seq; a mutable Array goes through its element cells instead).
    pub(crate) fn first_borrows(target: &Value) -> bool {
        matches!(
            target.descalarize().view(),
            ValueView::Array(..)
                | ValueView::Seq(_)
                | ValueView::HyperSeq(_)
                | ValueView::RaceSeq(_)
        )
    }

    /// `.first` over an Array/List/Seq receiver without decomposing all of it:
    /// the items are handed to the matcher in chunks of doubling size (from
    /// the end for `:end`), so a hit at index i decomposes O(i) elements and
    /// pays O(log i) matcher setups. A mutable array hands out its element
    /// CONTAINERS, so the matcher's topic aliases the element (`@a.first({
    /// $_ = 5 })` writes `@a`, as `.grep`/`.map` do); a List/Seq its items.
    /// Each chunk is copied out before the matcher runs, and the length is
    /// re-read per chunk, so a matcher that mutates the receiver is safe.
    // Cost: O(i + log i), i = elements scanned before the hit.
    pub(super) fn find_first_match_chunked(
        &mut self,
        target: &Value,
        func: Option<Value>,
        from_end: bool,
    ) -> Result<Option<(usize, Value)>, RuntimeError> {
        self.find_first_match_chunked_with(target, from_end, |interp, items, from_end| {
            interp.find_first_match_over_items(func.clone(), items, from_end)
        })
    }

    /// [`Interpreter::find_first_match_chunked`] with the per-chunk scan
    /// supplied by the caller (the VM's native `.first` uses its own matcher).
    /// `scan` answers the index of the hit within the chunk it is handed.
    // Cost: O(i + log i) plus the scans, i = elements scanned before the hit.
    pub(crate) fn find_first_match_chunked_with(
        &mut self,
        target: &Value,
        from_end: bool,
        mut scan: impl FnMut(&mut Self, &[Value], bool) -> Result<Option<(usize, Value)>, RuntimeError>,
    ) -> Result<Option<(usize, Value)>, RuntimeError> {
        let as_cells = Self::promotable_array_len(target).is_some();
        let current_len = |t: &Value| {
            if as_cells {
                Self::promotable_array_len(t).unwrap_or(0)
            } else {
                crate::runtime::utils::with_receiver_items(t, <[Value]>::len)
            }
        };
        let fetch = |t: &Value, lo: usize, hi: usize| -> Vec<Value> {
            if as_cells {
                (lo..hi).filter_map(|i| t.array_slot_ref(i, true)).collect()
            } else {
                crate::runtime::utils::with_receiver_items(t, |items| {
                    let n = items.len();
                    items[lo.min(n)..hi.min(n)].to_vec()
                })
            }
        };
        const FIRST_CHUNK: usize = 16;
        let mut chunk = FIRST_CHUNK;
        if from_end {
            let mut hi = current_len(target);
            while hi > 0 {
                let lo = hi.saturating_sub(chunk);
                let items = fetch(target, lo, hi);
                if let Some((i, v)) = scan(self, &items, true)? {
                    return Ok(Some((lo + i, v)));
                }
                hi = lo.min(current_len(target));
                chunk = chunk.saturating_mul(2);
            }
        } else {
            let mut lo = 0;
            loop {
                let len = current_len(target);
                if lo >= len {
                    break;
                }
                let hi = lo.saturating_add(chunk).min(len);
                let items = fetch(target, lo, hi);
                if let Some((i, v)) = scan(self, &items, false)? {
                    return Ok(Some((lo + i, v)));
                }
                lo = hi;
                chunk = chunk.saturating_mul(2);
            }
        }
        Ok(None)
    }
}
