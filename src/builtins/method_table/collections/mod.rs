//! The collections' rows (ADR-11276 §10, slice 3C).
//!
//! Owners: `Any`, `List`, `Array`, `Hash`, `Map`, `Range`, `Seq`, `Pair`,
//! `Capture`, `Set`, `SetHash`, `Bag`, `BagHash`, `Mix`, `MixHash`. A slice
//! adds a family module here and lists it in [`FAMILIES`]; no other file names
//! it.

use super::{Handler, MethodRow, RowFlags};

mod accepts;
pub(crate) mod accepts_open;
pub(crate) mod any_coerce;
mod any_coerce_late;
pub(crate) mod any_collection;
pub(crate) mod any_enum_views;
pub(crate) mod any_interp;
mod any_interp_twins;
pub(crate) mod any_list_hash;
mod any_list_late;
pub(crate) mod any_misc;
mod any_pos_late;
pub(crate) mod any_subscript;
pub(crate) mod any_views;
pub(crate) mod capture;
pub(crate) mod clone;
mod composed_dateish;
pub(crate) mod composed_real;
pub(crate) mod composed_roles;
mod declared_scalars;
mod descriptor;
mod fmt;
pub(crate) mod from_iterator;
pub(crate) mod iteration_buffer;
pub(crate) mod join;
mod junction;
pub(crate) mod lazy;
pub(crate) mod list;
pub(crate) mod list_aggregate;
mod list_bounds;
pub(crate) mod list_transform;
pub(crate) mod map;
mod match1_late;
mod match3_late;
mod mu_late;
mod mu_render_late;
pub(crate) mod pair;
pub(crate) mod positional;
pub(crate) mod quanthash;
pub(crate) mod range;
pub(crate) mod render;
pub(crate) mod render_names;
mod revision_e_late;
pub(crate) mod rotate;
pub(crate) mod sampling;
pub(crate) mod seq;
pub(crate) mod subscript;
pub(crate) mod terminals;
mod truth;
pub(crate) mod type_params;

/// Every family of this group.
pub(super) static FAMILIES: &[&[MethodRow]] = &[
    any_collection::ROWS,
    any_views::ROWS,
    any_coerce::ROWS,
    any_coerce::MU_CAPTURE_ROWS,
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
    from_iterator::ROWS,
    capture::ROWS,
    list_bounds::ROWS,
    join::ROWS,
    junction::ROWS,
    clone::ROWS,
    descriptor::ROWS,
    any_interp::ROWS,
    declared_scalars::ROWS,
    any_interp::ITERATOR_ROWS,
    any_interp::DECLARED_ELSEWHERE_ROWS,
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
    iteration_buffer::ROWS,
    positional::ROWS,
    rotate::ROWS,
    pair::ROWS,
    pair::PAIR_ROWS,
    quanthash::ROWS,
    subscript::ROWS,
    accepts::ROWS,
    accepts_open::ROWS,
    any_subscript::ROWS,
    any_subscript::NIL_ROWS,
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

/// The interpreter twins and late rows (ADR-11276 §9.135, §9.136). They are a
/// group of their own, registered after every other group, so each stands
/// behind any pure row of the same owner, name and arity wherever that row
/// lives.
pub(super) static LATE_FAMILIES: &[&[MethodRow]] = &[
    any_interp_twins::ROWS,
    composed_roles::ROWS,
    composed_real::BRIDGED_ROWS,
    composed_real::OWN_ROWS,
    composed_dateish::ROWS,
    any_coerce_late::ROWS,
    any_enum_views::ROWS,
    any_list_late::ROWS,
    mu_late::ROWS,
    mu_render_late::ROWS,
    any_pos_late::ROWS,
    match3_late::ROWS,
    match1_late::ROWS,
    revision_e_late::ROWS,
];
