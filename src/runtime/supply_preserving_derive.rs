//! A `.map`/`.grep`/`.do` derived from a `Supplier::Preserving` supply keeps the
//! source's backlog.
//!
//! rakudo's derived supply taps its source only when it is tapped itself, so a
//! value emitted before that tap (before the `.map` call, or between it and
//! the tap) still reaches the derived supply's first tap. mutsu registers the
//! transform tap eagerly, at `.map` time (`make_live_transform_supply`), so it
//! hands the source's backlog through the transform at that point and makes
//! the derived supply preserving in turn: whatever reaches it while nothing
//! taps it is replayed to its first tap (`supplier_take_preserved_backlog`,
//! #8825).

use super::*;
use crate::runtime::native_methods::{
    TransformMode, register_supplier_transform_tap_if_drained, supplier_done, supplier_snapshot,
    supplier_take_preserved_backlog, supplier_take_preserved_terminal,
};

impl Interpreter {
    /// Register the transform tap of the derived supply `downstream_sid` on the
    /// preserving `source_sid`, after running the source's un-replayed backlog
    /// through the transform into it. A source that already finished never
    /// signals the transform tap, so its `done` is carried over too; the
    /// derived supply's first tap then replays it after the values.
    ///
    /// The tap is registered only once the backlog is drained (under the emit
    /// gate): values emitted while the backlog is being carried pile up in the
    /// source and are carried in the next round, so nothing is delivered twice
    /// or out of order against a concurrent `emit`.
    // Cost: O(b), b = the values buffered over the rounds (one transform call each).
    pub(in crate::runtime) fn tap_preserved_source_into_derived(
        &mut self,
        source_sid: u64,
        downstream_sid: u64,
        callable: &Value,
        mode: TransformMode,
    ) {
        loop {
            for value in supplier_take_preserved_backlog(source_sid) {
                let _ = self.handle_supply_transform_emit(
                    downstream_sid,
                    callable.clone(),
                    mode,
                    value,
                );
            }
            if register_supplier_transform_tap_if_drained(source_sid, downstream_sid, callable, mode)
            {
                break;
            }
        }
        let (_, source_done, _) = supplier_snapshot(source_sid);
        if source_done && supplier_take_preserved_terminal(source_sid) {
            supplier_done(downstream_sid);
        }
    }
}
