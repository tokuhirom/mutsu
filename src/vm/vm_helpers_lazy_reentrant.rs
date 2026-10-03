//! A pull that re-enters a `gather` from inside its own body.
//!
//! `my \S := gather { my $it = S.iterator; take 1; take 2 * $it.pull-one }`
//! reads the sequence it is producing (Smooth::Numbers builds Hamming-style
//! numbers this way). While the body runs, the elements it has taken so far
//! live in its take collector on the gather-items stack, not in the list's
//! cache, and the coroutine state still says "not started" on the first run;
//! the inner pull therefore restarted the body from scratch, which re-entered
//! it again, until the native stack overflowed. The running body records its
//! collector index (`GatherCoroutineState::running_collector`), and a
//! re-entrant pull answers from that collector.
use super::*;

impl Interpreter {
    /// Mark `list`'s gather body as running with its take collector at
    /// `collector` on the gather-items stack (or as no longer running).
    // Cost: O(1).
    pub(super) fn set_gather_running_collector(list: &LazyList, collector: Option<usize>) {
        if let Some(ref coro_mutex) = list.coroutine {
            coro_mutex.lock().unwrap().running_collector = collector;
        }
    }

    /// When `list`'s gather body is running further up the call stack, the
    /// first `needed` elements it has taken so far. `None` when the body is
    /// not running (the ordinary pull path applies).
    ///
    /// Asking for an element the body has not taken yet cannot be answered:
    /// producing it means running the very body that is waiting for it.
    /// Rakudo hangs there; this reports the cycle instead.
    // Cost: O(needed) for the copy.
    pub(super) fn reentrant_gather_pull(
        &self,
        list: &LazyList,
        needed: usize,
    ) -> Option<Result<Vec<Value>, RuntimeError>> {
        let depth = list.coroutine.as_ref()?.lock().unwrap().running_collector?;
        let taken = self.gather_items_at(depth)?;
        if needed <= taken.len() {
            return Some(Ok(taken[..needed].to_vec()));
        }
        let wanted = if needed == usize::MAX {
            "all of its elements".to_string()
        } else {
            format!("element {}", needed - 1)
        };
        Some(Err(RuntimeError::new(format!(
            "A gather read {wanted} of its own sequence from inside its body, \
             but has only taken {} so far",
            taken.len()
        ))))
    }
}
