//! The scalar value types' rows: numbers and text (ADR-11276 §10, slice 3B).
//!
//! Owners: `Int`, `Num`, `Rat`, `FatRat`, `Complex`, `Bool`, `Str`, `Cool`,
//! `Uni`, `Blob`, `Version`. A slice adds a family module here and lists it in
//! [`FAMILIES`]; no other file names it.

use super::{Handler, MethodRow};

pub(crate) mod coerce;
pub(crate) mod complex;
mod int;
mod num;
mod rational;
pub(crate) mod real;
pub(crate) mod str;
pub(crate) mod str_iter;
pub(crate) mod str_search;
mod stringify;
pub(crate) mod succ_pred;

/// Every family of this group.
pub(super) static FAMILIES: &[&[MethodRow]] = &[
    str::ROWS,
    str::STR_TEXT_ROWS,
    str::COOL_TEXT_ROWS,
    str_iter::STR_ROWS,
    str_iter::COOL_ROWS,
    str_search::STR_ROWS,
    str_search::COOL_ROWS,
    str_search::STR_SUBSTR_2,
    str_search::COOL_SUBSTR_2,
    stringify::ROWS,
    int::ROWS,
    num::ROWS,
    rational::RAT_ROWS,
    rational::FAT_RAT_ROWS,
    complex::ROWS,
    real::INT_ROWS,
    real::NUM_ROWS,
    real::RAT_ROWS,
    real::FAT_RAT_ROWS,
    real::COMPLEX_ROWS,
    coerce::INT_ROWS,
    coerce::NUM_ROWS,
    coerce::RAT_ROWS,
    coerce::FAT_RAT_ROWS,
    coerce::COMPLEX_ROWS,
    succ_pred::STR_ROWS,
    succ_pred::INT_ROWS,
    succ_pred::NUM_ROWS,
    succ_pred::RAT_ROWS,
    succ_pred::FAT_RAT_ROWS,
    succ_pred::COMPLEX_ROWS,
];
