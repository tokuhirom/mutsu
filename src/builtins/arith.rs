#![allow(clippy::result_large_err)]
//! Arithmetic operator implementations: facade re-exporting themed submodules.
//!
//! Split from the original monolithic `arith.rs`. Each submodule owns a
//! cohesive group of free functions; this facade preserves every public
//! function's original visibility through explicit named re-exports so
//! external callers (`arith::<name>`) keep working unchanged.

mod add_sub;
mod mul_div_mod;
mod pow_negate;
mod range;
mod rat;
mod temporal;

// Core arithmetic operators — re-exported pub(crate) to match original visibility.
pub(crate) use add_sub::{arith_add, arith_sub};
pub(crate) use mul_div_mod::{arith_div, arith_mod, arith_mul};
pub(crate) use pow_negate::{arith_negate, arith_pow};

// Helpers used by external callers (vm/, runtime/).
pub(crate) use rat::real_to_rat;
pub(crate) use temporal::{is_temporal_operand, make_duration_value};
