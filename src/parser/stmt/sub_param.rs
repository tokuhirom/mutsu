//! Sub/method parameter parsing: facade re-exporting themed submodules.
//!
//! Split from the original monolithic `sub_param.rs` (1915 lines, over the
//! 500-line limit). Each submodule owns a cohesive group of free functions;
//! this facade preserves every function's original visibility through explicit
//! named re-exports so external callers keep working unchanged.
//!
//! Submodules:
//! - `helpers` (85 lines): `make_param` and small parsing helpers
//! - `type_constraint` (193 lines): type constraint and smiley parsing
//! - `where_constraint` (238 lines): where-clause and placeholder validation
//! - `param_inner` (563 lines): `parse_single_param` and its inner helper
//! - `method_decl` (170 lines): method/submethod declaration parsing
//!
//! `param_inner` exceeds 500 lines because `parse_single_param_inner` is a
//! single indivisible function; it cannot be split at a function boundary.

mod helpers;
mod method_decl;
mod param_inner;
mod type_constraint;
mod where_constraint;

// --- Re-exports preserving original visibilities ---

// make_param is pub(crate): used from crate::parser::primary::misc::lambda
pub(crate) use helpers::make_param;

// These were pub(super) in the original file (visible within `stmt`).
pub(super) use helpers::{is_anonymous_sigil_param, starts_with_sigil_param};
pub(super) use method_decl::{
    mark_params_as_invocant, method_decl, method_decl_body, method_decl_body_my, submethod_decl,
};
pub(super) use param_inner::parse_single_param;
pub(super) use type_constraint::{
    check_invalid_type_smiley, parse_implicit_invocant_marker, parse_of_type_constraint_chain,
    parse_type_constraint_expr,
};
