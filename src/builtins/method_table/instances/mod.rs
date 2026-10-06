//! The built-in instance classes' rows (ADR-11276 §10, slice 3D).
//!
//! Owners: `Date`, `DateTime`, `Instant`, `Duration`, `Match`, `Mu`, `Code`,
//! `Backtrace`, `Exception`, `Failure`, `Signature`, `RakuAST::*`. A slice adds
//! a family module here and lists it in [`FAMILIES`]; no other file names it.

use super::MethodRow;

/// Every family of this group.
pub(super) static FAMILIES: &[&[MethodRow]] = &[];
