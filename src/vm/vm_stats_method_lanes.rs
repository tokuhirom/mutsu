//! `MUTSU_VM_STATS` counters for the method-call lanes that skip the method
//! frame, kept out of `vm_stats.rs` so that file does not grow. Reported as
//! one line:
//!
//! `method-lanes: straight_line=N`
//!
//! `straight_line` counts role-method calls on a mixin that ran through
//! `run_straight_line_method` (#16215) -- `CArray`'s `ASSIGN-POS` / `AT-POS`.
//! A compiler change that adds an op the lane's gate
//! (`CompiledCode::ignores_role_context`) does not know silently sends every
//! such call back to the full method frame, ~5x the cost; this counter is what
//! a test pins.

use std::sync::atomic::{AtomicU64, Ordering};

use super::vm_stats::enabled;

static STRAIGHT_LINE: AtomicU64 = AtomicU64::new(0);

/// One role-method call answered by the straight-line lane.
#[inline]
pub(crate) fn record_straight_line_method() {
    if enabled() {
        STRAIGHT_LINE.fetch_add(1, Ordering::Relaxed);
    }
}

pub(crate) fn dump() {
    let straight_line = STRAIGHT_LINE.load(Ordering::Relaxed);
    eprintln!("[mutsu vm-stats] method-lanes: straight_line={straight_line}");
}
