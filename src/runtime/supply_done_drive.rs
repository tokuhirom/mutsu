//! Driving a live supplier's `done` through every hop of a combinator chain.
//!
//! This is the terminal-event twin of `supply_emit_drive`. A live `Supply`
//! combinator (`map`, `grep`, `head`, `batch`, `reduce`, `zip`, ...) owns a
//! derived supplier fed by a tap on its source, so a `done` on the head of a
//! chain owes every derived supplier a `done` of its own -- and each of
//! *those* owes its own derived suppliers the same, with whatever a stage
//! does at done (a `batch` flushes its partial batch, a `reduce` emits its
//! folded value).
//!
//! The source's `done` used to run that whole set, but finished each derived
//! supplier with only its done callbacks. A stage tapped on a derived supplier
//! therefore never saw the end: `$s.Supply.map(...).reduce(...)` never
//! delivered its result and `.map(...).batch(...)` lost its last batch
//! (#11647). [`Interpreter::propagate_supplier_done`] is the single terminal
//! driver now, and [`Interpreter::finish_derived_supplier`] recurses into it
//! for as many stages as the chain has.

use super::*;
use crate::runtime::native_methods::{
    ZipAction, close_supplier_channel_taps, flush_supplier_batch_taps, flush_supplier_line_taps,
    flush_supplier_words_taps, get_classify_sub_supplier_ids, get_start_output_supplier_ids,
    get_supplier_merge_state_ids, get_supplier_zip_latest_state_ids, get_supplier_zip_state_ids,
    get_transform_output_supplier_ids, merge_source_done, supplier_done,
    take_supplier_done_callbacks, take_supplier_reduce_results, take_supplier_tail_results,
    zip_latest_source_done, zip_source_done,
};

impl Interpreter {
    /// Run everything a supplier that has just been marked done owes: flush
    /// its buffering taps, fire its done callbacks, and finish every supplier
    /// derived from it. The caller has already called `supplier_done(sid)`;
    /// closing and resetting the supplier itself stays with the caller.
    ///
    /// Cost: O(t + d), t = taps on `sid`, d = suppliers derived from it
    /// (transitively, each finished once).
    pub(in crate::runtime) fn propagate_supplier_done(
        &mut self,
        sid: u64,
    ) -> Result<(), RuntimeError> {
        close_supplier_channel_taps(sid, None);
        // Flush batch buffers before done; this finishes the batch supplier.
        for (dsid, batch) in flush_supplier_batch_taps(sid) {
            self.forward_and_finish_supply(dsid, Value::array(batch))?;
        }
        for sub_sid in get_classify_sub_supplier_ids(sid) {
            self.finish_derived_supplier(sub_sid);
        }
        // `lines`/`words` own their derived supplier (issue #8474): the
        // flushed trailing partial line/word forwards into it like any other
        // emission, and the transform-output loop below then finishes it (it
        // is included in `get_transform_output_supplier_ids`).
        for (dsid, emitted) in flush_supplier_line_taps(sid) {
            self.handle_supply_forward(dsid, emitted)?;
        }
        for (dsid, emitted) in flush_supplier_words_taps(sid) {
            self.handle_supply_forward(dsid, emitted)?;
        }
        // `tail` can only name its values once the source is done: release
        // the held-back ones into its derived supplier now, and the
        // transform-output loop below then finishes it (#11839).
        for (dsid, values) in take_supplier_tail_results(sid) {
            for value in values {
                self.handle_supply_forward(dsid, value)?;
            }
        }
        for done_cb in take_supplier_done_callbacks(sid) {
            if self.invoke_done_callback_or_quit(done_cb, sid)? {
                break;
            }
        }
        for out_sid in get_start_output_supplier_ids(sid) {
            self.finish_derived_supplier(out_sid);
        }
        // grep/map/do/lines/words/head/... transform output suppliers.
        for out_sid in get_transform_output_supplier_ids(sid) {
            self.finish_derived_supplier(out_sid);
        }
        for zid in get_supplier_zip_state_ids(sid) {
            let (action, output_sid) = zip_source_done(zid);
            if matches!(action, ZipAction::AllDone) {
                self.finish_derived_supplier(output_sid);
            }
        }
        for zid in get_supplier_zip_latest_state_ids(sid) {
            let (action, output_sid) = zip_latest_source_done(zid);
            if matches!(action, ZipAction::AllDone) {
                self.finish_derived_supplier(output_sid);
            }
        }
        // `Supply.reduce` over a live source emits its single folded value
        // now, at done, then finishes downstream.
        for (dsid, acc) in take_supplier_reduce_results(sid) {
            let _ = self.forward_and_finish_supply(dsid, acc);
        }
        // A merged Supply is done only once *every* source is.
        for mid in get_supplier_merge_state_ids(sid) {
            if let Some(output_sid) = merge_source_done(mid) {
                self.finish_derived_supplier(output_sid);
            }
        }
        Ok(())
    }

    /// Finish a supplier derived from one that is done: mark it done and
    /// drive its own terminal actions through [`Self::propagate_supplier_done`].
    /// A supplier that is already done (a `head` that reached its limit) only
    /// has its late done callbacks drained, so no stage is finished twice and
    /// the recursion ends at every supplier it has already visited.
    ///
    /// Cost: O(t + d), as [`Self::propagate_supplier_done`].
    pub(in crate::runtime) fn finish_derived_supplier(&mut self, dsid: u64) {
        if supplier_done(dsid) {
            let _ = self.propagate_supplier_done(dsid);
        } else {
            for done_cb in take_supplier_done_callbacks(dsid) {
                let _ = self.invoke_done_callback(done_cb);
            }
        }
    }
}
