//! The constructors' and the metaobject protocol's rows (ADR-11276 §10,
//! slice 3G).
//!
//! `new` per built-in type, `Metamodel::*`, the subscript protocol and the
//! internal `__mutsu_*` names. A slice adds a family module here and lists it
//! in [`FAMILIES`]; no other file names it.

use super::MethodRow;

/// Every family of this group.
pub(super) static FAMILIES: &[&[MethodRow]] = &[];
