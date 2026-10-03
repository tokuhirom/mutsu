//! A deferred `.map`/`.grep` source handed to an `Iterator` instance as a
//! stream it advances in place (#10186).
//!
//! `.iterator` on a not-yet-run `.map`/`.grep` Seq used to take the whole
//! body — running the callback over every source element — and wrap the
//! materialized list. Rakudo's map iterator runs the callback once per
//! `pull-one`. [`SeqBody::take_map_grep_stream_source`] steals the
//! [`SeqSource::MapGrep`] out of the consumed body instead, and the iterator
//! keeps it in a private body of its own whose source
//! [`SeqBody::advance_map_grep_source`] advances a chunk at a time, returning
//! only the elements that chunk produced (never the prefix already handed
//! out), so draining the iterator one element at a time is O(n) overall.
//!
//! A `for` loop likewise borrows the source and pulls it one iteration at a
//! time (#9936, docs/adr/0058 §3.3). Rakudo's `for` pulls its iterable's iterator once per iteration, so a
//! `.map` callback and the loop body interleave (`m1 b1 m2 b2 ...`) and a
//! `last` stops the callback from running over the rest of the source. The
//! loop borrows the body's [`SeqSource::MapGrep`] for its whole run and hands
//! back what it pulled when it is done.

use super::{SeqBody, SeqSource};
use crate::value::{RuntimeError, Value};

impl SeqBody {
    /// `.iterator`'s steal of a not-yet-finished `.map`/`.grep` source:
    /// returns the elements a prefix pull (boolification, a subscript)
    /// already produced plus the source itself, and leaves this body `Taken`
    /// exactly as [`SeqBody::take`] would. `None` (and nothing changes) for
    /// every other source, and once `.cache` was requested or an earlier
    /// reifying touch retained the body — those iterate the retained
    /// elements.
    // Cost: O(p), p = elements a prefix pull already produced.
    pub(crate) fn take_map_grep_stream_source(&self) -> Option<(Vec<Value>, SeqSource)> {
        let mut state = self.core.state.lock().unwrap();
        if state.cache_requested
            || state.retained
            || !matches!(state.source, SeqSource::MapGrep { .. })
        {
            return None;
        }
        let source = std::mem::replace(&mut state.source, SeqSource::Taken);
        Some((self.live_generation().clone(), source))
    }

    /// Whether [`SeqBody::take_map_grep_stream_source`] would hand this
    /// body's source out — a `.map`/`.grep` called on this Seq chains onto
    /// it (`MapGrepItems::Chain`) instead of reifying it first (#11176).
    // Cost: O(1).
    pub(crate) fn has_map_grep_stream_source(&self) -> bool {
        let state = self.core.state.lock().unwrap();
        !state.cache_requested
            && !state.retained
            && matches!(state.source, SeqSource::MapGrep { .. })
    }

    /// Advance the [`SeqSource::MapGrep`] of a stream body built from
    /// [`SeqBody::take_map_grep_stream_source`]: hand it to `pull`, which
    /// advances its `pos` and returns the elements it produced plus whether
    /// the source is now exhausted, and return those elements. Nothing is
    /// stored in the body — the iterator owns what it pulled. `None` once the
    /// source is exhausted (or while a re-entrant pull holds it). A failed
    /// pull leaves the source `Taken`, as a failed full pull does.
    // Cost: `pull`'s cost; no copy of anything pulled before.
    pub(crate) fn advance_map_grep_source(
        &self,
        pull: impl FnOnce(&mut SeqSource) -> Result<(Vec<Value>, bool), RuntimeError>,
    ) -> Result<Option<Vec<Value>>, RuntimeError> {
        let mut source = {
            let mut state = self.core.state.lock().unwrap();
            if !matches!(state.source, SeqSource::MapGrep { .. }) {
                return Ok(None);
            }
            std::mem::replace(&mut state.source, SeqSource::Taken)
        };
        let (items, exhausted) = pull(&mut source)?;
        self.core.state.lock().unwrap().source = if exhausted {
            SeqSource::Reified
        } else {
            source
        };
        Ok(Some(items))
    }

    /// Claim a not-yet-finished `.map`/`.grep` source for a `for` loop to
    /// stream. Returns the elements an earlier prefix pull already stored
    /// (the loop iterates those first) and the source itself, leaving the body
    /// `Taken` until [`SeqBody::finish_map_grep_streaming`] puts it back:
    /// while the loop runs nothing else may pull the same source, as Rakudo
    /// refuses a second iterator over a Seq whose iterator is in use.
    ///
    /// `None` (and nothing changes) for every other source, and once `.cache`
    /// was requested — the ordinary reify then serves the loop.
    // Cost: O(1), or O(p) for p elements an earlier prefix pull stored.
    pub(crate) fn claim_map_grep_for_streaming(&self) -> Option<(Vec<Value>, SeqSource)> {
        let mut state = self
            .core
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if state.cache_requested || !matches!(state.source, SeqSource::MapGrep { .. }) {
            return None;
        }
        let source = std::mem::replace(&mut state.source, SeqSource::Taken);
        Some((self.live_generation().clone(), source))
    }

    /// Hand a streamed source back after the loop: `items` is every element
    /// the body holds now (the stored prefix plus whatever the loop pulled),
    /// and `source` is what is left to pull — [`SeqSource::Reified`] once the
    /// loop ran it dry, [`SeqSource::Taken`] after a failed pull. The body
    /// stays readable afterward, as it is after an eagerly reified `for`
    /// (`reify` marks that touch `retained`; so does this).
    // Cost: O(1).
    pub(crate) fn finish_map_grep_streaming(&self, items: Vec<Value>, source: SeqSource) {
        if items.len() != self.live_generation().len() {
            // SAFETY: same reasoning as `pull_and_store` — no reference into
            // `gens` is held across this push, and earlier generations are
            // never rewritten, only superseded by a longer one.
            unsafe { (*self.core.gens.get()).push(Box::new(items)) };
        }
        let mut state = self
            .core
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        state.source = source;
        state.retained = true;
    }
}
