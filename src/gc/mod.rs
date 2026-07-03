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
//!   and the process-global cycle-candidate buffer (§11 step 4). This is the
//!   managed-pointer *type machinery* a synchronous cycle collector needs; no
//!   `Value` variant is migrated to `Gc<T>` yet (first wave, §11 step 5+) and
//!   no trial-deletion reclaim runs yet (§11 step 8).

mod gc_ptr;
mod root_visitor;

#[allow(unused_imports)]
pub(crate) use gc_ptr::{
    Color, ErasedGc, Gc, Trace, drain_candidates, gc_contents_mut, gc_enabled,
};
pub(crate) use root_visitor::{RootVisitor, visit_map_values, visit_opt, visit_slice};
