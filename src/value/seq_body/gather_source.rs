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
        match &self.lock_state().source {
            SeqSource::Gather(list) => Some(list.clone()),
            _ => None,
        }
    }

    /// Whether this body is a not-yet-finished gather.
    // Cost: O(1), one lock read.
    pub(crate) fn is_gather_source(&self) -> bool {
        matches!(self.lock_state().source, SeqSource::Gather(_))
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
        let mut state = self.lock_state();
        if state.cache_requested || state.retained || !matches!(state.source, SeqSource::Gather(_))
        {
            return None;
        }
        match std::mem::replace(&mut state.source, SeqSource::Taken) {
            SeqSource::Gather(list) => Some(list),
            other => {
                state.source = other;
                None
            }
        }
    }

    /// `.List` / `.list` on an uncached gather: Rakudo's `Seq.List` consumes
    /// the Seq and returns a List that reifies from its iterator lazily, so a
    /// `for` over it still interleaves with the gather body and `[2]` on an
    /// infinite gather answers. Steals the inner list into a fresh cached body
    /// and returns that body's List view. `None` for every other source and
    /// for a cached body, which serves its elements as usual.
    // Cost: O(1).
    pub(crate) fn take_gather_as_list(&self) -> Option<std::sync::Arc<SeqBody>> {
        let list = self.take_gather_source()?;
        let fresh = SeqBody::deferred(SeqSource::Gather(list));
        fresh.mark_cache_requested();
        Some(fresh.as_list_view())
    }

    /// The inner gather list for `.iterator`, `.map` or `.grep` on a gather
    /// whose body was cached (`.cache`, or a subscript or `?` read, which
    /// cache a Seq in Rakudo): such a Seq is re-readable, so the new iterator
    /// or pipe shares the coroutine list without consuming the body. A body
    /// that names its own gather mid-pull (`my \S := gather { ... S.iterator
    /// ... }`, read by `S[^3]`) gets its iterator this way. `None` for an
    /// uncached body ([`SeqBody::take_gather_source`] steals that) and for
    /// every other source.
    // Cost: O(1), one lock read.
    pub(crate) fn shared_gather_source(&self) -> Option<Value> {
        let state = self.lock_state();
        match &state.source {
            SeqSource::Gather(list) if state.cache_requested => Some(list.clone()),
            _ => None,
        }
    }

    /// A non-consuming prefix pull of a not-yet-finished gather (`$g[i]`,
    /// `?$g`): `pull(list, from, needed)` drives the coroutine until it has
    /// taken `needed` elements (or finished) and answers elements
    /// `from..needed` of everything it has taken, where `from` is how many the
    /// body already holds. Those are appended to the body's live generation;
    /// the source stays in place unless the coroutine ran out, and the body is
    /// cached (Rakudo's `[i]` and `Bool` cache a Seq). A no-op for every other
    /// source or when the body already holds `needed` elements.
    ///
    /// Only the new elements are copied, so a consumer reading a gather one
    /// element at a time (`pull_source_element`) is linear overall, not
    /// quadratic (ADR-11678 §8.5).
    ///
    /// The state lock is not held across `pull`, which runs the gather body
    /// (invariant 3).
    // Cost: `pull`'s cost, plus O(needed - from) amortized for the append.
    pub(crate) fn extend_gather_prefix(
        &self,
        needed: usize,
        pull: impl FnOnce(&Value, usize, usize) -> Result<Vec<Value>, RuntimeError>,
    ) -> Result<(), RuntimeError> {
        let from = self.live_generation().len();
        if from >= needed {
            return Ok(());
        }
        let list = {
            let mut state = self.lock_state();
            let SeqSource::Gather(list) = &state.source else {
                return Ok(());
            };
            let list = list.clone();
            // Rakudo's positional read and `Bool` cache the Seq: it stays
            // re-readable, and a later `.List` or `.iterator` serves the
            // cached elements instead of consuming it.
            state.cache_requested = true;
            list
        };
        let window = pull(&list, from, needed)?;
        let exhausted = from + window.len() < needed;
        // A re-entrant pull may have finished (or stolen) the source while
        // `pull` ran; only a body still on this gather takes the window.
        let mut state = self.lock_state();
        if !matches!(&state.source, SeqSource::Gather(current) if same_list(current, &list)) {
            return Ok(());
        }
        // A re-entrant pull of the same body may already have appended part
        // of the window (both read the same coroutine cache, so the elements
        // agree); append only what the body does not hold yet.
        let have = self.live_generation().len();
        if have >= from && have < from + window.len() {
            self.core
                .gens
                .append(window.into_iter().skip(have - from).collect());
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

#[cfg(test)]
mod tests {
    use super::*;

    /// A gather read one element at a time asks its coroutine only for the
    /// new element and appends it, so n reads copy O(n) elements, not O(n^2).
    #[test]
    fn a_gather_prefix_pull_appends_only_the_new_window() {
        let pipe = crate::value::LazyList::new_index_pipe(
            Value::int(0),
            crate::value::IndexTransform::Flat,
        );
        let list = Value::lazy_list(crate::gc::Gc::new(pipe));
        let body = SeqBody::deferred(SeqSource::Gather(list));
        let mut requested = 0;
        for needed in 1..=1000 {
            body.extend_gather_prefix(needed, |_, from, needed| {
                requested += needed - from;
                Ok((from..needed).map(|i| Value::int(i as i64)).collect())
            })
            .unwrap();
        }
        assert_eq!(requested, 1000, "each read pulls only its new element");
        let live = body.live_generation();
        assert_eq!(live.len(), 1000);
        assert!(
            live.iter()
                .enumerate()
                .all(|(i, v)| *v == Value::int(i as i64))
        );
        assert!(body.is_gather_source(), "the gather is not exhausted yet");

        // A short window means the coroutine finished.
        body.extend_gather_prefix(1002, |_, from, _| Ok(vec![Value::int(from as i64)]))
            .unwrap();
        assert_eq!(body.live_generation().len(), 1001);
        assert!(!body.is_gather_source());
    }
}
