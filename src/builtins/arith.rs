//! Arithmetic operator implementations: facade re-exporting themed submodules.
//!
//! Split from the original monolithic `arith.rs`. Each submodule owns a
//! cohesive group of free functions; this facade preserves every public
//! function's original visibility through explicit named re-exports so
//! external callers (`arith::<name>`) keep working unchanged.

mod add_sub;
mod int_ops;
mod mul_div_mod;
mod pow_negate;
pub(crate) mod range;
mod rat;
mod succ;
mod temporal;

// Core arithmetic operators — re-exported pub(crate) to match original visibility.
pub(crate) use add_sub::{arith_add, arith_sub};
pub(crate) use mul_div_mod::{arith_div, arith_mod, arith_mul};
pub(crate) use pow_negate::{arith_negate, arith_pow};

// The one implementation of each integer operator and of succ/pred
// (ADR-0118), shared by the VM opcodes, the reduction fold and the methods.
pub(crate) use int_ops::{
    BitOp, int_abs, int_abs_value, int_bitneg, int_bitop, int_cmp, int_div, int_gcd, int_lcm,
    int_mod_i64, int_negate, int_operand, int_shift_left, int_shift_right,
};
pub(crate) use succ::{value_pred, value_succ};

// Helpers used by external callers (vm/, runtime/).
pub(crate) use rat::{
    big_int_add, big_int_mul, big_int_sub, bigint_ratio_to_f64, exact_round_scaled, real_to_rat,
};
pub(crate) use temporal::{instance_instant_value, is_temporal_operand, make_duration_value};
