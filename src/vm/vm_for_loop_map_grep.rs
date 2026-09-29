//! `for` over a not-yet-run `.map`/`.grep` Seq, pulled one iteration at a
//! time (#9936, docs/adr/0058 §3.3).
//!
//! The loop claims the Seq's [`SeqSource::MapGrep`]
//! ([`crate::value::SeqBody::claim_map_grep_for_streaming`]) and pulls just
//! the elements each iteration binds through
//! [`Interpreter::pull_map_grep_prefix`], so the callback and the loop body
//! interleave as in Rakudo and a `last` leaves the rest of the source
//! unmapped. A callback `pull_map_grep_prefix` cannot run a chunk at a time
//! (a multi-parameter block, a `FIRST`/`LAST` phaser, a shaped source) is
//! pulled whole on the first iteration, which is what the loop did before.

use super::*;
use crate::value::{SeqBody, SeqSource};
use std::sync::Arc;

/// A `.map`/`.grep` source a `for` loop is streaming, plus every element it
/// produced so far.
pub(super) struct MapGrepStream {
    body: Arc<SeqBody>,
    /// Every element produced so far, the body's stored prefix first.
    buf: Vec<Value>,
    /// What is left to pull; `Reified` once the source ran dry.
    source: SeqSource,
}

impl MapGrepStream {
    /// Claim `iterable`'s source when it is a not-yet-finished `.map`/`.grep`
    /// Seq (see `SeqBody::claim_map_grep_for_streaming`).
    // Cost: O(1), or O(p) for p elements an earlier prefix pull stored.
    pub(super) fn claim(iterable: &Value) -> Option<Self> {
        let ValueView::Seq(body) = iterable.view() else {
            return None;
        };
        let (buf, source) = body.claim_map_grep_for_streaming()?;
        Some(Self {
            body: Arc::clone(&body),
            buf,
            source,
        })
    }

    fn exhausted(&self) -> bool {
        !matches!(self.source, SeqSource::MapGrep { .. })
    }

    /// Give the body back what the loop pulled and what is left.
    // Cost: O(1).
    pub(super) fn finish(self) {
        self.body.finish_map_grep_streaming(self.buf, self.source);
    }
}

impl Interpreter {
    /// The `n` elements from `start` on (fewer at the end of the source),
    /// pulling only as many source elements as that needs.
    // Cost: one callback call per source element up to the `start + n`-th
    // element produced, plus O(n) to copy the chunk out.
    pub(super) fn map_grep_stream_chunk(
        &mut self,
        stream: &mut MapGrepStream,
        start: usize,
        n: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        let want = start.saturating_add(n);
        while stream.buf.len() < want && !stream.exhausted() {
            let needed = want - stream.buf.len();
            match self.pull_map_grep_prefix(&mut stream.source, needed) {
                Ok((items, exhausted)) => {
                    stream.buf.extend(items);
                    if exhausted {
                        stream.source = SeqSource::Reified;
                    }
                }
                Err(e) => {
                    // A failed pull is not retried, as a failed full pull
                    // leaves the body `Taken`.
                    stream.source = SeqSource::Taken;
                    return Err(e);
                }
            }
        }
        let end = want.min(stream.buf.len());
        Ok(stream.buf.get(start..end).unwrap_or_default().to_vec())
    }

    /// Pull the rest of the source, for a loop that has to snapshot every
    /// element (a `gather` suspending inside the loop body saves a list
    /// continuation). Returns every element produced.
    // Cost: one callback call per source element not yet pulled, plus O(e)
    // to copy the elements out, e = elements produced.
    pub(super) fn map_grep_stream_drain(
        &mut self,
        stream: &mut MapGrepStream,
    ) -> Result<Vec<Value>, RuntimeError> {
        if !stream.exhausted() {
            match self.pull_map_grep_rest(&stream.source) {
                Ok(items) => {
                    stream.buf.extend(items);
                    stream.source = SeqSource::Reified;
                }
                Err(e) => {
                    stream.source = SeqSource::Taken;
                    return Err(e);
                }
            }
        }
        Ok(stream.buf.clone())
    }
}
