//! GC Level 1a infrastructure (ADR-0001 / ADR-0002,
//! `docs/gc-level1-detailed-design.md`).
//!
//! Two concerns live here, both "compiled in, default off" (design doc §9.1)
//! ahead of a running collector:
//!
//! - [`root_visitor`] — the [`RootVisitor`] trait and `visit_*` helpers used by
//!   `Interpreter::visit_roots` / `Env::visit_values` to enumerate every
//!   `Value` reachable from execution state (§11 steps 1-2).
//! - [`gc_ptr`] — [`Gc<T>`], its Bacon-Rajan node header, the [`Trace`] trait,
//!   and the process-global cycle-candidate buffer (§11 step 4). The first wave
//!   is migrated to `Gc<_>`: `Value::Hash` (5b), `Value::Array` (5c),
//!   `Value::ContainerRef` (5d).
//! - [`collect`] — the synchronous Bacon-Rajan trial-deletion collector (§11
//!   step 8) that reclaims cycles from the candidate buffer.
//! - [`safepoint`] — the trigger policy and the `gc_safepoint` entry point the
//!   VM calls at re-entry boundaries (dispatch backedge) to run a collect under
//!   a `MUTSU_GC` stress mode. With `MUTSU_GC` unset, safepoints are disarmed
//!   (one cached load) and a collect is a no-op — normal execution is unaffected.

mod collect;
mod gc_ptr;
mod root_visitor;
mod safepoint;
mod stw;

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

#[allow(unused_imports)]
pub(crate) use collect::{
    CollectStats, LogMode, collect_cycles, collect_cycles_at, collect_if_enabled,
    gc_debug_collect_now, log_mode, verify_enabled,
};
#[allow(unused_imports)]
pub(crate) use gc_ptr::{
    Color, ContainerMakeMut, ErasedGc, Gc, Trace, WeakGc, drain_candidates, enter_mutator_worker,
    exit_mutator_worker, gc_contents_mut, gc_enabled, mutator_workers_active,
};
pub(crate) use root_visitor::{RootVisitor, visit_map_values, visit_opt, visit_slice};
#[allow(unused_imports)]
pub(crate) use safepoint::{
    SafepointKind, armed as gc_safepoints_armed, gc_safepoint, startup_collect_if_requested,
};
#[allow(unused_imports)]
pub(crate) use stw::{
    block_quiescent, mark_thread_registered, park_at_safepoint as gc_park_point,
    preregister_worker_quiescent, stw_aware_wait, stw_requested, worker_started,
};
