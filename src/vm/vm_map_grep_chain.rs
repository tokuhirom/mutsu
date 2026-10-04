//! Chaining deferred `.map` and `.grep` Seqs while preserving callback order.

use super::vm_map_grep_pull::map_grep_pullable_by_prefix;
use super::*;
use crate::value::{MapGrepItems, SeqSource};

impl Interpreter {
    /// The receiver of a `.map`/`.grep` that is itself a not-yet-run
    /// `.map`/`.grep` Seq (`reify_or_consume_seq_target` leaves those
    /// untouched). With `chain`, steal its source as the new stage's
    /// [`MapGrepItems::Chain`] (#11176), which consumes the receiver as any
    /// `.map` does; without, take it whole so the eager path reads its
    /// elements. `(None, target)` for every other receiver.
    // Cost: O(1), or O(p) for p elements a prefix pull already produced;
    // without `chain`, a full pull of the receiver.
    pub(crate) fn map_grep_receiver_chain(
        &mut self,
        target: Value,
        chain: bool,
    ) -> Result<(Option<MapGrepItems>, Value), RuntimeError> {
        let ValueView::Seq(body) = target.view() else {
            return Ok((None, target));
        };
        if !body.has_map_grep_stream_source() {
            return Ok((None, target));
        }
        if chain && let Some((prefix, source)) = body.take_map_grep_stream_source() {
            let items = MapGrepItems::Chain(crate::value::MapGrepChain::new(prefix, source));
            return Ok((Some(items), target));
        }
        let (items, outcome) = self.take_seq_body(&body)?;
        Ok((
            None,
            if matches!(outcome, crate::value::SeqTaken::Taken) {
                Value::seq(items)
            } else {
                target
            },
        ))
    }

    /// [`Self::pull_map_grep_prefix`] for a `.map`/`.grep` chained onto
    /// another one (`MapGrepItems::Chain`, #11176): pull the upstream one
    /// element at a time and run this callback over each element as it
    /// arrives, so the two callbacks interleave per element as Rakudo's pull
    /// pipeline does. A callback that cannot be run a chunk at a time (see
    /// `pull_map_grep_prefix`) drains the upstream first and runs once.
    // Cost: one upstream pull plus one callback call per upstream element up
    // to the `needed`-th element produced.
    pub(super) fn pull_map_grep_chain(
        &mut self,
        source: &mut SeqSource,
        needed: usize,
    ) -> Result<(Vec<Value>, bool), RuntimeError> {
        let SeqSource::MapGrep {
            items,
            pos,
            func,
            fatal,
            mode,
            plan,
        } = source
        else {
            return Ok((Vec::new(), true));
        };
        let MapGrepItems::Chain(chain) = items else {
            return Ok((Vec::new(), true));
        };
        let chain = chain.clone();
        let fatal = *fatal;
        if needed == usize::MAX && self.can_batch_pure_int_chain(&chain, func.as_ref(), mode) {
            while chain.extend(|source| self.pull_map_grep_prefix(source, usize::MAX))? > 0 {}
            let (out, _) =
                self.run_map_grep_chunk(func, fatal, mode, plan, items, *pos, items.len(), None)?;
            *pos = items.len();
            return Ok((out, true));
        }
        if !plan.prefix_pullable(|| map_grep_pullable_by_prefix(func.as_ref(), mode)) {
            while chain.extend(|source| self.pull_map_grep_prefix(source, usize::MAX))? > 0 {}
            let (out, _) =
                self.run_map_grep_chunk(func, fatal, mode, plan, items, *pos, items.len(), None)?;
            *pos = items.len();
            return Ok((out, true));
        }
        let mut out = Vec::new();
        while out.len() < needed {
            if *pos >= items.len() {
                if chain.exhausted() {
                    break;
                }
                chain.extend(|source| self.pull_map_grep_prefix(source, 1))?;
                continue;
            }
            let end = items.len();
            let max_matches = mode.is_grep().then_some(needed - out.len());
            let depth = crate::runtime::loop_handler_depth::loop_handler_depth();
            self.async_state.map_grep_last_depth = None;
            let (chunk, end) =
                self.run_map_grep_chunk(func, fatal, mode, plan, items, *pos, end, max_matches)?;
            *pos = end;
            out.extend(chunk);
            if self.async_state.map_grep_last_depth.take() == Some(depth + 1) {
                return Ok((out, true));
            }
        }
        let exhausted = *pos >= items.len() && chain.exhausted();
        Ok((out, exhausted))
    }
}
