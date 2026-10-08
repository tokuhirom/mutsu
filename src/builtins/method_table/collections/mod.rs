//! The collections' rows (ADR-11276 §10, slice 3C).
//!
//! Owners: `Any`, `List`, `Array`, `Hash`, `Map`, `Range`, `Seq`, `Pair`,
//! `Capture`, `Set`, `SetHash`, `Bag`, `BagHash`, `Mix`, `MixHash`. A slice
//! adds a family module here and lists it in [`FAMILIES`]; no other file names
//! it.

use super::{Handler, MethodRow, RowFlags};

pub(crate) mod any_collection;
mod any_interp;
pub(crate) mod capture;
pub(crate) mod lazy;
pub(crate) mod list;
pub(crate) mod list_aggregate;
pub(crate) mod list_transform;
pub(crate) mod map;
pub(crate) mod pair;
pub(crate) mod positional;
pub(crate) mod quanthash;
pub(crate) mod range;
pub(crate) mod render;
mod sampling;
pub(crate) mod subscript;
mod truth;

/// Every family of this group.
pub(super) static FAMILIES: &[&[MethodRow]] = &[
    any_collection::ROWS,
    render::WHICH_ROWS,
    render::QUANT_GIST_ROWS,
    render::QUANT_RAKU_ROWS,
    render::RANGE_ROWS,
    capture::ROWS,
    any_interp::ROWS,
    lazy::ROWS,
    list::ROWS,
    list_aggregate::ANY_ROWS,
    list_aggregate::LIST_ROWS,
    list_aggregate::LIST_COMBINATIONS_OF,
    list_transform::ANY_ROWS,
    list_transform::LIST_ROWS,
    list_transform::FLAT_ROWS,
    map::ROWS,
    positional::ROWS,
    pair::ROWS,
    quanthash::ROWS,
    subscript::ROWS,
    range::ROWS,
    sampling::PICK_ROWS,
    sampling::ROLL_ROWS,
    sampling::PICKPAIRS_ROWS,
    truth::ROWS,
];
