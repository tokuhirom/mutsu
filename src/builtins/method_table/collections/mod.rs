//! The collections' rows (ADR-11276 §10, slice 3C).
//!
//! Owners: `Any`, `List`, `Array`, `Hash`, `Map`, `Range`, `Seq`, `Pair`,
//! `Capture`, `Set`, `SetHash`, `Bag`, `BagHash`, `Mix`, `MixHash`. A slice
//! adds a family module here and lists it in [`FAMILIES`]; no other file names
//! it.

use super::{Handler, MethodRow, RowFlags};

mod accepts;
pub(crate) mod accepts_open;
pub(crate) mod any_collection;
mod any_interp;
pub(crate) mod any_list_hash;
pub(crate) mod any_misc;
pub(crate) mod any_subscript;
pub(crate) mod any_views;
pub(crate) mod capture;
pub(crate) mod clone;
mod descriptor;
mod fmt;
pub(crate) mod join;
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
pub(crate) mod rotate;
mod sampling;
pub(crate) mod seq;
pub(crate) mod subscript;
pub(crate) mod terminals;
mod truth;
pub(crate) mod type_params;

/// Every family of this group.
pub(super) static FAMILIES: &[&[MethodRow]] = &[
    any_collection::ROWS,
    any_views::ROWS,
    any_views::TYPE_OBJECT_ROWS,
    render::WHICH_ROWS,
    render::PAIR_WHICH_ROWS,
    render::LIST_GIST_ROWS,
    render::QUANT_GIST_ROWS,
    render::QUANT_STR_ROWS,
    render::QUANT_RAKU_ROWS,
    render::RANGE_ROWS,
    render_names::RAKU_ROWS,
    render_names::STR_ROWS,
    render_names::CAPTURE_ROWS,
    render_names::STRINGY_ROWS,
    fmt::ROWS,
    capture::ROWS,
    join::ROWS,
    clone::ROWS,
    descriptor::ROWS,
    any_interp::ROWS,
    any_list_hash::ROWS,
    any_misc::ROWS,
    lazy::ROWS,
    list::ROWS,
    list_aggregate::ANY_ROWS,
    list_aggregate::LIST_ROWS,
    list_aggregate::LIST_COMBINATIONS_OF,
    list_transform::ANY_ROWS,
    list_transform::LIST_ROWS,
    list_transform::FLAT_ROWS,
    list_transform::FLAT_LEVELS_ROWS,
    map::ROWS,
    positional::ROWS,
    rotate::ROWS,
    pair::ROWS,
    quanthash::ROWS,
    subscript::ROWS,
    accepts::ROWS,
    accepts_open::ROWS,
    any_subscript::ROWS,
    range::ROWS,
    sampling::BOOL_ROWS,
    sampling::PICK_ROWS,
    sampling::ROLL_ROWS,
    sampling::ANY_PICK_ROWS,
    sampling::ANY_ROLL_ROWS,
    sampling::PICKPAIRS_ROWS,
    seq::ROWS,
    terminals::ROWS,
    type_params::ROWS,
    truth::ROWS,
];
