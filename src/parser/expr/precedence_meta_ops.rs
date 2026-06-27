//! Precedence-level expression parsers and meta-operator helpers: facade
//! re-exporting themed submodules.
//!
//! The original monolithic `precedence_meta_ops.rs` has been split into:
//! - `meta_bracket`: bracket/meta-op utilities (parse_meta_op, BracketInfix, etc.)
//! - `set_ops`:      set operator parsing + structural_expr
//! - `hyper_concat`: hyper operator parsing + concat_expr / replication_expr
//! - `arith`:        additive, multiplicative, power expression parsers

mod arith;
mod hyper_concat;
mod meta_bracket;
mod set_ops;

// Re-exports preserving original pub(super) visibility
pub(super) use arith::{additive_expr, multiplicative_expr, power_expr, power_expr_tight};
pub(super) use hyper_concat::concat_expr;
pub(super) use meta_bracket::{
    BracketInfix, op_str_to_token_kind, parse_bracket_infix_op, parse_infix_func_op, parse_meta_op,
    strip_sequence_op,
};
pub(super) use set_ops::structural_expr;
