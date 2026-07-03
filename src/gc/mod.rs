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

#[allow(unused_imports)]
pub(crate) use collect::{CollectStats, collect_cycles, collect_if_enabled, gc_debug_collect_now};
#[allow(unused_imports)]
pub(crate) use gc_ptr::{
    Color, ContainerMakeMut, ErasedGc, Gc, Trace, WeakGc, drain_candidates, gc_contents_mut,
    gc_enabled,
};
pub(crate) use root_visitor::{RootVisitor, visit_map_values, visit_opt, visit_slice};
#[allow(unused_imports)]
pub(crate) use safepoint::{SafepointKind, armed as gc_safepoints_armed, gc_safepoint};
