//! The I/O and concurrency classes' rows (ADR-11276 §10, slice 3E).
//!
//! Owners: `IO::Path`, `IO::Handle`, `IO::Spec`, the socket classes,
//! `Proc::Async`, `Promise`, `Channel`, `Supply`, the schedulers, `Lock`. A
//! slice adds a family module here and lists it in [`FAMILIES`]; no other file
//! names it.

use super::MethodRow;

/// Every family of this group.
pub(super) static FAMILIES: &[&[MethodRow]] = &[];
