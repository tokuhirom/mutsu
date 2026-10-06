//! The collections' rows (ADR-11276 §10, slice 3C).
//!
//! Owners: `Any`, `List`, `Array`, `Hash`, `Map`, `Range`, `Seq`, `Pair`,
//! `Capture`, `Set`, `SetHash`, `Bag`, `BagHash`, `Mix`, `MixHash`. A slice
//! adds a family module here and lists it in [`FAMILIES`]; no other file names
//! it.

use super::{Handler, MethodRow};

pub(crate) mod any_collection;
pub(crate) mod list;
pub(crate) mod list_aggregate;
pub(crate) mod list_transform;
pub(crate) mod map;
pub(crate) mod positional;

/// Every family of this group.
pub(super) static FAMILIES: &[&[MethodRow]] = &[
    any_collection::ROWS,
    list::ROWS,
    list_aggregate::ANY_ROWS,
    list_aggregate::LIST_ROWS,
    list_transform::ANY_ROWS,
    list_transform::LIST_ROWS,
    list_transform::ARRAY_ROWS,
    map::ROWS,
    positional::ROWS,
];
