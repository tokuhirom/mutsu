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
mod fmt;
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
pub(crate) mod render_names;
mod sampling;
pub(crate) mod seq;
pub(crate) mod subscript;
mod truth;

/// Every family of this group.
pub(super) static FAMILIES: &[&[MethodRow]] = &[
    any_collection::ROWS,
    render::WHICH_ROWS,
    render::LIST_GIST_ROWS,
    render::QUANT_GIST_ROWS,
    render::QUANT_RAKU_ROWS,
    render::RANGE_ROWS,
    render_names::RAKU_ROWS,
    render_names::STR_ROWS,
    render_names::CAPTURE_ROWS,
    render_names::STRINGY_ROWS,
    fmt::ROWS,
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
    seq::ROWS,
    truth::ROWS,
];
