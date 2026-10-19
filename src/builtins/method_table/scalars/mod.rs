//! The scalar value types' rows: numbers and text (ADR-11276 §10, slice 3B).
//!
//! Owners: `Int`, `Num`, `Rat`, `FatRat`, `Complex`, `Bool`, `Str`, `Cool`,
//! `Uni`, `Blob`, `Version`. A slice adds a family module here and lists it in
//! [`FAMILIES`]; no other file names it.

use super::{Handler, MethodRow, Named, RowFlags};

pub(crate) mod blob;
mod blob_class;
pub(crate) mod blob_decode;
pub(crate) mod blob_pos;
pub(crate) mod blob_read;
mod capture_none;
pub(crate) mod coerce;
pub(crate) mod complex;
pub(crate) mod complex_math;
pub(crate) mod complex_real;
mod cool_coerce;
pub(crate) mod cool_real;
mod int;
pub(crate) mod math;
mod num;
pub(crate) mod numeric_base;
pub(crate) mod numeric_coerce;
pub(crate) mod numeric_polymod;
mod numeric_range;
pub(crate) mod numify;
mod rational;
pub(crate) mod real;
pub(crate) mod real_misc;
pub(crate) mod render;
mod revision_e;
pub(crate) mod str;
pub(crate) mod str_args;
pub(crate) mod str_complex_real;
pub(crate) mod str_interp;
pub(crate) mod str_iter;
pub(crate) mod str_numeric;
pub(crate) mod str_search;
mod str_search_interp;
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
    str_search_interp::STR_ROWS,
    str_search_interp::COOL_ROWS,
    stringify::ROWS,
    stringify::INT_ROWS,
    str_numeric::STR_ROWS,
    str_numeric::COOL_ROWS,
    str_args::COOL_ROWS,
    str_args::COOL_FMT_ROWS,
    str_args::STR_SHARED_ROWS,
    str_args::STR_ROWS,
    str_args::UNI_ROWS,
    str_interp::STR_ROWS,
    str_interp::STR_MATCH_ROWS,
    str_interp::COOL_ROWS,
    str_interp::COOL_FORMAT_ROWS,
    int::ROWS,
    num::ROWS,
    rational::RAT_ROWS,
    rational::FAT_RAT_ROWS,
    complex::ROWS,
    revision_e::ROWS,
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
    version::COOL_ROWS,
    capture_none::ROWS,
    blob::ROWS,
    blob::BYTES_ROWS,
    blob::COERCE_ROWS,
    blob::OF_ROWS,
    blob_class::ROWS,
    blob_decode::ROWS,
    blob_read::READ_ROWS,
    blob_read::SUBBUF_ROWS,
    blob_read::BITS_ROWS,
    blob_pos::ROWS,
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
    cool_coerce::COOL_ROWS,
    cool_real::BOOL_ROWS,
    numeric_coerce::INT_ROWS,
    numeric_coerce::NUM_ROWS,
    numeric_coerce::RAT_ROWS,
    numeric_coerce::COMPLEX_ROWS,
    numeric_base::INT_ROWS,
    numeric_base::NUM_ROWS,
    numeric_base::RAT_ROWS,
    numeric_base::FAT_RAT_ROWS,
    numeric_base::FAT_RAT_REPEATING_ROWS,
    numeric_polymod::ROWS,
    numeric_range::ROWS,
    complex_real::ROWS,
    unicode::STR_ROWS,
    unicode::COOL_ROWS,
    unicode::INT_ROWS,
    unicode::UNI_ROWS,
    uni::ROWS,
    render::ROWS,
    // Late twins of the `Str`/`Cool` real conversions (after their pure rows).
    str_complex_real::ROWS,
];
