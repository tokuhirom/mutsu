//! The built-in instance classes' rows (ADR-11276 §10, slice 3D).
//!
//! Owners: `Date`, `DateTime`, `Instant`, `Duration`, `Match`, `Mu`, `Code`,
//! `Backtrace`, `Exception`, `Failure`, `Signature`, `RakuAST::*`, `Unicode`, `Format`,
//! `Formatter`. A slice adds
//! a family module here and lists it in [`FAMILIES`]; no other file names it.

use super::{Handler, MethodRow, RowFlags};

/// A [`Handler::Narrow`] row of `owner` for `name` with `arity` positional
/// arguments, flagged nothing.
macro_rules! narrow_row {
    ($owner:literal, $name:literal, $arity:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}
use narrow_row;

pub(crate) mod backtrace;
pub(crate) mod code;
pub(crate) mod code_accepts;
pub(crate) mod code_class;
pub(crate) mod code_methods;
pub(crate) mod code_regex;
pub(crate) mod collation;
pub(crate) mod compunit;
pub(crate) mod compunit_repo;
pub(crate) mod date;
pub(crate) mod dateish;
pub(crate) mod dateish_io;
pub(crate) mod datetime;
pub(crate) mod depspec;
pub(crate) mod distribution;
pub(crate) mod enumeration;
pub(crate) mod exception;
mod exception_control;
pub(crate) mod failure;
mod format;
pub(crate) mod hyper_seq;
pub(crate) mod instant;
pub(crate) mod iterator;
pub(crate) mod label;
pub(crate) mod parameter;
pub(crate) mod rakuast_accessors;
mod rakuast_apply_queries;
mod rakuast_arg_list_queries;
mod rakuast_block_queries;
mod rakuast_doc_edit;
mod rakuast_literal_queries;
mod rakuast_locate;
mod rakuast_name_queries;
pub(crate) mod rakuast_node;
mod rakuast_parameter_queries;
mod rakuast_rakudoc;
mod rakuast_visit;
mod rakuast_walk;
mod rakuast_why;
pub(crate) mod rakudo_internals;
pub(crate) mod regex_match;
pub(crate) mod routine;
pub(crate) mod runtime_objects;
pub(crate) mod signature;
pub(crate) mod stash;
pub(crate) mod supply;
pub(crate) mod temporal;
pub(crate) mod temporal_class;
pub(crate) mod temporal_edit;
pub(crate) mod temporal_shift;
pub(crate) mod unicode_data;
mod walk_list;
pub(crate) mod wrapper_role;

/// Every family of this group.
pub(super) static FAMILIES: &[&[MethodRow]] = &[
    hyper_seq::HYPER_ROWS,
    hyper_seq::RACE_ROWS,
    walk_list::ROWS,
    runtime_objects::ROWS,
    temporal::DATE_ROWS,
    temporal::DATETIME_ROWS,
    dateish::DATE_ROWS,
    dateish::DATETIME_ROWS,
    dateish_io::ROWS,
    date::ROWS,
    datetime::ROWS,
    instant::INSTANT_ROWS,
    instant::DURATION_ROWS,
    instant::INSTANT_OWN_ROWS,
    instant::DURATION_OWN_ROWS,
    regex_match::ROWS,
    backtrace::ROWS,
    exception::ROWS,
    exception_control::ROWS,
    enumeration::ROWS,
    iterator::ROWS,
    collation::ROWS,
    label::ROWS,
    rakudo_internals::ROWS,
    code::ROWS,
    code::BLOCK_ROWS,
    code_accepts::ROWS,
    code_methods::ROWS,
    routine::ROWS,
    failure::ROWS,
    signature::ROWS,
    parameter::ROWS,
    stash::ROWS,
    wrapper_role::ROWS,
    supply::ROWS,
    rakuast_node::ROWS,
    &rakuast_accessors::ROWS,
    rakuast_visit::ROWS,
    rakuast_locate::ROWS,
    rakuast_rakudoc::ROWS,
    rakuast_walk::ROWS,
    rakuast_doc_edit::ROWS,
    rakuast_literal_queries::ROWS,
    rakuast_name_queries::ROWS,
    rakuast_name_queries::ONE_ARG_ROWS,
    rakuast_apply_queries::ROWS,
    rakuast_arg_list_queries::ROWS,
    rakuast_block_queries::ROWS,
    rakuast_why::ROWS,
    rakuast_parameter_queries::ROWS,
    compunit::ROWS,
    temporal_shift::ROWS,
    temporal_edit::ROWS,
    temporal_class::ROWS,
    unicode_data::ROWS,
    depspec::ROWS,
    compunit_repo::ROWS,
    distribution::ROWS,
    format::FORMAT_ROWS,
    format::FORMATTER_ROWS,
];
