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
}
