pub(in crate::parser) mod anon_sub;
pub(crate) mod circumfix;
mod identifier_call;
mod listop;
mod loop_control;
pub(crate) mod predicates;
mod supply;
pub(crate) use anon_sub::{anon_method_expr, anon_method_expr_declared, folded_invocant};
pub(crate) use supply::supply_block;
mod term_literals;

pub(super) use circumfix::declared_circumfix_op;
pub(super) use identifier_call::identifier_or_call;
pub(in crate::parser) use identifier_call::slipped_control_stmt;
pub(crate) use listop::{TEST_CALLSITE_LINE_KEY, callsite_line_arg, stamp_call_site_markers};
pub(in crate::parser) use listop::{
    colon_starts_colonpair, export_term_or_call, expr_is_colonpair, make_call_expr,
    parse_expr_listop_args, try_adjacent_colonpair_arg, try_parse_no_paren_invocant_colon_call,
};
pub(in crate::parser) use loop_control::{control_flow_slip_args, loop_control_listop_arg_start};
pub(in crate::parser) use predicates::{is_infix_word_op, is_keyword};
pub(super) use term_literals::{class_literal, declared_term_symbol, keyword_literal, whatever};
