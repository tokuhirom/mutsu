//! Propagating a live supplier's quit through derived supply stages.
//!
//! `done` already walks transform outputs one stage at a time. A source quit
//! needs the same walk so each derived supplier can run its own QUIT phasers
//! and callbacks before the next stage is reached. If a stage handles the
//! quit, its done callbacks own completion and its downstream stages are left
//! to that existing done path.

use super::*;
use crate::runtime::native_methods::{
    close_all_supplier_taps, close_supplier_channel_taps,
    flush_supplier_line_taps, flush_supplier_words_taps,
    get_direct_transform_output_supplier_ids, supplier_reset,
    supplier_snapshot, supplier_quit, take_supplier_done_callbacks,
    take_supplier_quit_callbacks_via_group, take_supplier_tail_results,
    take_supplier_whenever_quit_callbacks,
};
use crate::runtime::native_supply_methods::QuitOutcome;

impl Interpreter {
    /// Propagate an already-published source quit to each derived transform.
    /// The source itself was marked quit by its `Supplier.quit` caller.
    // Cost: O(t + d), t = taps on each visited supplier, d = derived stages.
    pub(in crate::runtime) fn propagate_supplier_quit(
        &mut self,
        supplier_id: u64,
        reason: Value,
    ) -> Result<(), RuntimeError> {
        let mut visited = Vec::new();
        self.propagate_supplier_quit_stage(supplier_id, reason, true, &mut visited)
    }

    /// Finish one stage's quit protocol, then visit its immediate transform
    /// outputs if no QUIT phaser handled the reason.
    // Cost: O(t + d), t = taps on this supplier, d = downstream transform stages.
    fn propagate_supplier_quit_stage(
        &mut self,
        supplier_id: u64,
        reason: Value,
        already_marked: bool,
        visited: &mut Vec<u64>,
    ) -> Result<(), RuntimeError> {
        if visited.contains(&supplier_id) {
            return Ok(());
        }
        visited.push(supplier_id);
        if !already_marked {
            supplier_quit(supplier_id, reason.clone());
        }
        let (_, done, quit_reason) = supplier_snapshot(supplier_id);
        let Some(reason) = quit_reason else {
            return Ok(());
        };
        if done {
            return Ok(());
        }
        let children = get_direct_transform_output_supplier_ids(supplier_id);
        close_supplier_channel_taps(supplier_id, Some(reason.clone()));
        for (downstream, emitted) in flush_supplier_line_taps(supplier_id) {
            self.handle_supply_forward(downstream, emitted)?;
        }
        for (downstream, emitted) in flush_supplier_words_taps(supplier_id) {
            self.handle_supply_forward(downstream, emitted)?;
        }
        // `tail` releases its buffer only on done. A quit discards those values
        // and still reaches the tail stage's own downstream supplier below.
        let _discarded_tail_values = take_supplier_tail_results(supplier_id);

        let mut handled = false;
        let mut via_done = false;
        for phaser in take_supplier_whenever_quit_callbacks(supplier_id) {
            match self.run_whenever_quit_phaser(phaser, reason.clone()) {
                QuitOutcome::HandledViaDone => {
                    handled = true;
                    via_done = true;
                }
                QuitOutcome::Handled => handled = true,
                QuitOutcome::Unhandled => {}
            }
        }
        if handled {
            if via_done {
                let _ = take_supplier_done_callbacks(supplier_id);
            } else {
                for callback in take_supplier_done_callbacks(supplier_id) {
                    let _ = self.invoke_done_callback(callback);
                }
            }
        } else {
            for callback in take_supplier_quit_callbacks_via_group(supplier_id) {
                self.call_supply_quit_handler(callback, reason.clone())?;
            }
            let _ = take_supplier_done_callbacks(supplier_id);
        }
        close_all_supplier_taps(supplier_id);
        if !already_marked {
            supplier_reset(supplier_id);
        }

        if !handled {
            for child in children {
                self.propagate_supplier_quit_stage(child, reason.clone(), false, visited)?;
            }
        }
        Ok(())
    }
}
