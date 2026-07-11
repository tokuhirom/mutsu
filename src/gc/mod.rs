//! GC Level 1a: the Bacon-Rajan cycle collector on `Arc` (ADR-0001 / ADR-0002 /
//! ADR-0003, `docs/gc-level1-detailed-design.md`). **Default on** since
//! 2026-07-05 (ADR-0003 §5); `MUTSU_GC=off` disarms it.
//!
//! - [`gc_ptr`] — [`Gc<T>`], its Bacon-Rajan node header, the [`Trace`] trait,
//!   and the process-global cycle-candidate buffer. Every container-kind
//!   `Value` variant is a `Gc<_>` node (layer 3a migration, complete).
//! - [`collect`] — the synchronous trial-deletion collector that reclaims
//!   cycles from the candidate buffer.
//! - [`safepoint`] — the trigger policy (ADR-0003 adaptive size threshold, or
//!   the `MUTSU_GC_EVERY_CANDIDATE` stress period) and the `gc_safepoint`
//!   entry point the VM calls at re-entry boundaries (dispatch backedge).
//! - [`stw`] — the cooperative stop-the-world (design §6.1): a collect waits
//!   until every other mutator thread is parked at a safepoint or inside a
//!   registered blocking wait, then scans.
//! - [`root_visitor`] — the [`RootVisitor`] trait and `visit_*` helpers used by
//!   `Interpreter::visit_roots` / `Env::visit_values` to enumerate every
//!   `Value` reachable from execution state (§11 steps 1-2; verification /
//!   future tracing infra — the candidate-buffer collector does not consume it).

mod collect;
mod gc_ptr;
mod root_visitor;
mod safepoint;
mod stw;

pub(crate) use collect::collect_at_program_end;
#[cfg(test)]
pub(crate) use collect::collect_cycles;
#[cfg(test)]
pub(crate) use gc_ptr::drain_candidates;
pub(crate) use gc_ptr::{
    ContainerMakeMut, ErasedGc, Gc, Trace, WeakGc, enter_mutator_worker, exit_mutator_worker,
    gc_contents_mut,
};
pub(crate) use root_visitor::{RootVisitor, visit_map_values, visit_opt, visit_slice};
pub(crate) use safepoint::{
    SafepointKind, armed as gc_safepoints_armed,
    current_size_threshold as gc_current_size_threshold, gc_safepoint,
    startup_collect_if_requested,
};
pub(crate) use stw::{
    block_quiescent, mark_thread_registered, park_at_safepoint as gc_park_point,
    preregister_worker_quiescent, stw_aware_wait, worker_started,
};

/// Test-only serialization for every unit test that touches the process-global
/// GC statics (candidate buffer, worker count, STW flags, cooldown). The
/// modules' tests run in parallel threads within one process; without a single
/// shared lock they interfere through those statics.
///
/// NOTE: this lock only serializes the GC modules' own tests. With `MUTSU_GC=on`
/// (the CI gc-stress job), *interpreter* unit tests in other modules also drive
/// the collector through VM safepoints and can steal buffered candidates or
/// react to a test's fake registered workers — so the stress job runs
/// `cargo test -- --test-threads=1`. A parallel `MUTSU_GC=on cargo test` is
/// expected to flake; that is test isolation, not a GC bug.
#[cfg(test)]
pub(crate) mod test_support {
    use std::sync::{Mutex, MutexGuard, OnceLock};

    pub(crate) fn serial_lock() -> MutexGuard<'static, ()> {
        static L: OnceLock<Mutex<()>> = OnceLock::new();
        let g = L
            .get_or_init(|| Mutex::new(()))
            .lock()
            .unwrap_or_else(|e| e.into_inner());
        super::stw::test_reset();
        g
    }
}
