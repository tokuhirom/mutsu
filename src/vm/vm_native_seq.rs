//! Resuming a finite `...` sequence with a built-in term generator past its
//! eager prefix (`1 ... 20000`, `"a" ... "zzz"`): the lazy list keeps the
//! sequence's [`SeqGen`], and each pull runs it for the terms still needed.

use super::vm_helpers_junction::publish_generated;
use super::*;
use crate::value::seq_gen::SeqGen;

/// Generation steps per run of the generator while a strict consumer drives
/// the sequence to its end: bounds the steps taken past what a lazy consumer
/// asked for, and publishes the terms in batches.
const NATIVE_SEQ_CHUNK: usize = 4096;

impl Interpreter {
    /// Extend `history` (the sequence's own terms) until it holds `target`
    /// terms or the sequence ends, publishing the terms past `generated_from`
    /// to `cache`. On the end, the values after a list endpoint follow.
    // Cost: O(k) term computations, k = `target` minus the history's length
    // (plus one chunk's overshoot at the end).
    pub(super) fn run_native_sequence(
        &mut self,
        g: &SeqGen,
        state: &mut crate::value::ClosureSeqState,
        cache: &std::sync::Mutex<Option<Vec<Value>>>,
        history: &mut Vec<Value>,
        target: usize,
        generated_from: usize,
    ) -> Result<(), RuntimeError> {
        let mut published = generated_from;
        while history.len() < target && !state.finished {
            let budget = (target - history.len()).min(NATIVE_SEQ_CHUNK);
            let before = history.len();
            let outcome = self.run_seq_gen(g, history, budget)?;
            if outcome.ended || history.len() == before {
                history.extend(state.post_endpoint.iter().cloned());
                state.finished = true;
            }
            publish_generated(cache, history, &mut published);
        }
        publish_generated(cache, history, &mut published);
        Ok(())
    }
}
