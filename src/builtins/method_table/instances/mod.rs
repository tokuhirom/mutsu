//! The built-in instance classes' rows (ADR-11276 §10, slice 3D).
//!
//! Owners: `Date`, `DateTime`, `Instant`, `Duration`, `Match`, `Mu`, `Code`,
//! `Backtrace`, `Exception`, `Failure`, `Signature`, `RakuAST::*`. A slice adds
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

pub(crate) mod date;
pub(crate) mod dateish;
pub(crate) mod datetime;
pub(crate) mod instant;
pub(crate) mod regex_match;
pub(crate) mod temporal;
pub(crate) mod temporal_edit;
pub(crate) mod temporal_shift;

/// Every family of this group.
pub(super) static FAMILIES: &[&[MethodRow]] = &[
    temporal::DATE_ROWS,
    temporal::DATETIME_ROWS,
    dateish::DATE_ROWS,
    dateish::DATETIME_ROWS,
    date::ROWS,
    datetime::ROWS,
    instant::INSTANT_ROWS,
    instant::DURATION_ROWS,
    instant::INSTANT_OWN_ROWS,
    instant::DURATION_OWN_ROWS,
    regex_match::ROWS,
    temporal_shift::ROWS,
    temporal_edit::ROWS,
];
