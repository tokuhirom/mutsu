//! The scalar value types' rows: numbers and text (ADR-11276 §10, slice 3B).
//!
//! Owners: `Int`, `Num`, `Rat`, `FatRat`, `Complex`, `Bool`, `Str`, `Cool`,
//! `Uni`, `Blob`, `Version`. A slice adds a family module here and lists it in
//! [`FAMILIES`]; no other file names it.

use super::{Handler, MethodRow, RowFlags};

pub(crate) mod coerce;
pub(crate) mod complex;
pub(crate) mod complex_math;
pub(crate) mod cool_real;
mod int;
pub(crate) mod math;
mod num;
pub(crate) mod numify;
mod rational;
pub(crate) mod real;
pub(crate) mod real_misc;
pub(crate) mod str;
pub(crate) mod str_iter;
pub(crate) mod str_search;
mod stringify;
pub(crate) mod succ_pred;
pub(crate) mod truth;
pub(crate) mod uni;
pub(crate) mod unicode;
pub(crate) mod version;

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
    coerce::INT_BOOL,
    coerce::NUM_BOOL,
    coerce::RAT_BOOL,
    coerce::FAT_RAT_BOOL,
    coerce::COMPLEX_BOOL,
    succ_pred::STR_ROWS,
    succ_pred::INT_ROWS,
    succ_pred::NUM_ROWS,
    succ_pred::RAT_ROWS,
    succ_pred::FAT_RAT_ROWS,
    succ_pred::COMPLEX_ROWS,
    truth::BOOL_ROWS,
    truth::UNI_ROWS,
    version::ROWS,
    math::INT_ROWS,
    math::NUM_ROWS,
    math::RAT_ROWS,
    math::COMPLEX_ROWS,
    math::COOL_ROWS,
    real_misc::INT_ROWS,
    real_misc::NUM_ROWS,
    real_misc::RAT_ROWS,
    real_misc::FAT_RAT_ROWS,
    real_misc::COMPLEX_ROWS,
    real_misc::COOL_ROWS,
    real_misc::COOL_NATIVE_INT_ROWS,
    real_misc::INT_NATIVE_INT_ROWS,
    cool_real::COOL_ROWS,
    cool_real::BOOL_ROWS,
    unicode::STR_ROWS,
    unicode::COOL_ROWS,
    unicode::INT_ROWS,
    unicode::UNI_ROWS,
    uni::ROWS,
];
