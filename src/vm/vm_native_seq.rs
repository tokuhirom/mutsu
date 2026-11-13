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
    /// Extend `history` (the sequence's own terms) until the cache holds
    /// `needed` elements or the sequence ends, publishing the new terms to
    /// `cache`. On the end, the values after a list endpoint follow.
    // Cost: O(k) term computations, k = the new terms (at most `needed`
    // minus what the cache holds, plus one chunk's overshoot at the end).
    #[allow(clippy::too_many_arguments)]
    pub(super) fn run_native_sequence(
        &mut self,
        g: &SeqGen,
        state: &mut crate::value::ClosureSeqState,
        cache: &std::sync::Mutex<Option<Vec<Value>>>,
        history: &mut Vec<Value>,
        needed: usize,
        generated_from: usize,
        cached_len: Option<usize>,
    ) -> Result<(), RuntimeError> {
        let mut published = generated_from;
        let have = |history: &Vec<Value>| match cached_len {
            Some(c) => c + (history.len() - generated_from),
            None => history.len(),
        };
        while have(history) < needed && !state.finished {
            let budget = (needed - have(history)).min(NATIVE_SEQ_CHUNK);
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
