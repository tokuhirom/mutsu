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
    TransformMode, supplier_done, supplier_snapshot, supplier_take_preserved_backlog,
    supplier_take_preserved_terminal,
};

impl Interpreter {
    /// Run the un-replayed backlog of preserving `source_sid` through the
    /// transform into `downstream_sid`. A source that already finished never
    /// signals the transform tap, so its `done` is carried over too; the
    /// derived supply's first tap then replays it after the values.
    // Cost: O(b), b = the backlog length (plus one transform call per value).
    pub(in crate::runtime) fn carry_preserved_backlog_into_derived(
        &mut self,
        source_sid: u64,
        downstream_sid: u64,
        callable: &Value,
        mode: TransformMode,
    ) {
        for value in supplier_take_preserved_backlog(source_sid) {
            let _ =
                self.handle_supply_transform_emit(downstream_sid, callable.clone(), mode, value);
        }
        let (_, source_done, _) = supplier_snapshot(source_sid);
        if source_done && supplier_take_preserved_terminal(source_sid) {
            supplier_done(downstream_sid);
        }
    }
}
