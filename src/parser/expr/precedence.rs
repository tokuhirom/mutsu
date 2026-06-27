use super::super::helpers::{is_ident_char, ws};
use super::super::parse_result::{PError, PResult, merge_expected_messages, parse_char, parse_tag};
use super::super::stmt::assign::{
    build_compound_assign_expr, compound_assign_op_from_name, compound_assigned_value_expr,
    parse_assign_expr_or_comma, parse_compound_assign_op, parse_meta_compound_assign_op,
};

use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

use super::operators::*;
use super::precedence_meta_ops::{
    BracketInfix, additive_expr, concat_expr, multiplicative_expr, op_str_to_token_kind,
    parse_bracket_infix_op, parse_infix_func_op, parse_meta_op, power_expr, strip_sequence_op,
    structural_expr,
};
use super::{contains_whatever, wrap_whatevercode};

static CHAIN_CMP_TMP_COUNTER: AtomicUsize = AtomicUsize::new(0);

mod assign;
mod chain_cmp;
mod comparison;
mod custom_infix;
mod errors;
mod feed;
mod list_infix;
mod list_infix_loop;
mod logic;
mod logic2;
mod operands;
mod ternary;

// ---- Re-exports preserving each public function's original visibility ----
pub(super) use chain_cmp::range_expr;
pub(super) use custom_infix::parse_custom_infix_word;
pub(super) use operands::{loose_prefix_operand, multiplicative_operand, power_operand};
pub(super) use ternary::{ternary, ternary_mode, ternary_no_assign};
// These take `pub(super)` types (ComparisonOp/FeedOp), so re-export at the same
// visibility to avoid leaking a more-private type (private_interfaces).
pub(super) use chain_cmp::{parse_comparison_op, parse_negated_meta_comparison_op};
pub(super) use feed::make_feed_node;
pub(super) use ternary::is_structural_comparison_op;

pub(in crate::parser) use ternary::call_arg_expr;

pub(crate) use feed::{feed_leftmost_operand_mut, lower_feed_node};

// ---- Internal cross-submodule helper re-exports (so `use super::*` reaches them) ----
pub(crate) use assign::{
    assign_to_target_expr, build_compound_assign_target_expr, list_lvalue_assign_expr,
    parse_assignment_rhs_mode, unwrap_grouped_lvalue,
};
pub(crate) use chain_cmp::{
    build_chain_cmp_expr, build_chain_cmp_expr_with_repeated_middle, make_chain_cmp,
    wrap_smartmatch_rhs,
};
pub(crate) use comparison::comparison_expr_mode;
pub(crate) use custom_infix::{
    parse_comma_list_of_range, parse_comma_list_of_range_raw, parse_flipflop_infix,
};
pub(crate) use errors::{
    check_range_precedence_worry, conditional_precedence_too_loose_error, non_associative_error,
    non_associative_pair_error, non_list_associative_error, syntax_exception,
};
pub(crate) use list_infix::{list_infix_expr, sequence_expr};
pub(crate) use list_infix_loop::parse_list_infix_loop;
pub(crate) use logic::{assign_not_expr_mode, or_expr_mode, or_expr_no_assign_mode};
pub(crate) use logic2::{junctive_expr_mode, not_expr_mode, or_or_expr_mode};
pub(crate) use ternary::{comparison_nonassoc_key, structural_comparison_expr_mode};

#[cfg(test)]
mod tests {
    use crate::parser::expr::expression;

    #[test]
    fn mixed_cross_and_sequence_are_non_list_associative() {
        let err = expression("4 X+> 1...2").unwrap_err();
        assert!(err.messages.iter().any(|msg| {
            msg.as_str()
                .contains("Only identical operators may be list associative")
        }));
    }
}
