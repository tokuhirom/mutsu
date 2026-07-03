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
//!   step 8) that reclaims cycles from the candidate buffer. Manual/opt-in for
//!   now (`gc_debug_collect_now`); with `MUTSU_GC` unset the buffer is empty so
//!   a collect is a no-op.

mod collect;
mod gc_ptr;
mod root_visitor;

#[allow(unused_imports)]
pub(crate) use collect::{CollectStats, collect_cycles, gc_debug_collect_now};
#[allow(unused_imports)]
pub(crate) use gc_ptr::{
    Color, ContainerMakeMut, ErasedGc, Gc, Trace, drain_candidates, gc_contents_mut, gc_enabled,
};
pub(crate) use root_visitor::{RootVisitor, visit_map_values, visit_opt, visit_slice};
