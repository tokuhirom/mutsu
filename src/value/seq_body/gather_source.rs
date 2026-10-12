//! A plain `gather` as a `Seq` (docs/adr/11678-gather-is-a-deferred-seq-source.md).
//!
//! [`SeqSource::Gather`] is an adapter: it holds the gather's existing
//! `Gc<LazyList>` coroutine as one `Value` edge, and the coroutine, its GC
//! tracing and its resume machinery stay in `LazyList`. The body owns what Raku
//! attaches to a `Seq` — identity, aliasing, consumption, `.cache` and `$`
//! itemization — while the interpreter's gather drivers (`force_lazy_list_vm`,
//! `force_lazy_list_vm_n`) produce the elements.
//!
//! The inner list keeps its own cache of everything the coroutine has taken so
//! far, so a prefix pull never has to append: it asks the driver for the first
//! `needed` elements and stores that prefix as the body's new generation, and a
//! full pull stores the whole cache. The source stays in place across a
//! non-consuming pull (ADR-11678 §2.2 invariant 3): a body that names its own
//! gather (`my \S := gather { ... S.iterator ... }`) re-enters it mid-pull.

use super::{SeqBody, SeqSource};
use crate::value::{RuntimeError, Value};

impl SeqBody {
    /// The inner gather list while this body's source is still a
    /// [`SeqSource::Gather`]; `None` once it is reified, taken, or any other
    /// source. Does not change anything.
    // Cost: O(1), one lock read.
    pub(crate) fn gather_source(&self) -> Option<Value> {
        match &self.core.state.lock().unwrap().source {
            SeqSource::Gather(list) => Some(list.clone()),
            _ => None,
        }
    }

    /// Whether this body is a not-yet-finished gather.
    // Cost: O(1), one lock read.
    pub(crate) fn is_gather_source(&self) -> bool {
        matches!(
            self.core.state.lock().unwrap().source,
            SeqSource::Gather(_)
        )
    }

    /// Steal the inner gather list for a method that Raku also lets consume
    /// the Seq and that streams the coroutine itself: `.lazy`, `.map`,
    /// `.grep`, `.iterator` (ADR-11678 §2.3). Leaves the body `Taken`. `None`
    /// (and nothing changes) for every other source, and once `.cache` was
    /// requested or a reifying touch retained the body — those serve the
    /// stored elements instead. A prefix an earlier subscript stored is not
    /// lost: the inner list's own cache holds it too.
    // Cost: O(1).
    pub(crate) fn take_gather_source(&self) -> Option<Value> {
        let mut state = self.core.state.lock().unwrap();
        if state.cache_requested || state.retained || !matches!(state.source, SeqSource::Gather(_))
        {
            return None;
        }
        match std::mem::replace(&mut state.source, SeqSource::Taken) {
            SeqSource::Gather(list) => Some(list),
            _ => unreachable!("matched Gather above"),
        }
    }

    /// A non-consuming prefix pull of a not-yet-finished gather (`$g[i]`,
    /// `?$g`): `pull` drives the coroutine until it has taken `needed`
    /// elements (or finished) and answers the first `needed` of everything it
    /// has taken. That prefix becomes the body's live generation; the source
    /// stays in place unless the coroutine ran out. A no-op for every other
    /// source or when the body already holds `needed` elements.
    ///
    /// The state lock is not held across `pull`, which runs the gather body
    /// (invariant 3).
    // Cost: `pull`'s cost, plus a copy of the prefix.
    pub(crate) fn extend_gather_prefix(
        &self,
        needed: usize,
        pull: impl FnOnce(&Value, usize) -> Result<Vec<Value>, RuntimeError>,
    ) -> Result<(), RuntimeError> {
        if self.live_generation().len() >= needed {
            return Ok(());
        }
        let Some(list) = self.gather_source() else {
            return Ok(());
        };
        let prefix = pull(&list, needed)?;
        let exhausted = prefix.len() < needed;
        // A re-entrant pull may have finished (or stolen) the source while
        // `pull` ran; only a body still on this gather takes the prefix.
        let mut state = self.core.state.lock().unwrap();
        if !matches!(&state.source, SeqSource::Gather(current) if same_list(current, &list)) {
            return Ok(());
        }
        if prefix.len() > self.live_generation().len() {
            // SAFETY: same reasoning as `pull_and_store` — no reference into
            // `gens` is held across this push, and earlier generations are
            // never rewritten, only superseded by a longer one.
            unsafe { (*self.core.gens.get()).push(Box::new(prefix)) };
        }
        if exhausted {
            state.source = SeqSource::Reified;
        }
        Ok(())
    }
}

/// Whether two gather source edges are the same coroutine node.
// Cost: O(1).
fn same_list(a: &Value, b: &Value) -> bool {
    use crate::value::ValueView;
    match (a.view(), b.view()) {
        (ValueView::LazyList(a), ValueView::LazyList(b)) => crate::gc::Gc::ptr_eq(&a, &b),
        _ => false,
    }
}
