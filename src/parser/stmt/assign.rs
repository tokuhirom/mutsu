use std::sync::atomic::{AtomicUsize, Ordering};

use super::super::expr::{
    QuotedMethodName, expression, expression_no_sequence, parse_quoted_method_name,
};
use super::super::helpers::ws;
use super::super::parse_result::{
    PError, PResult, merge_expected_messages, parse_char, take_while1,
};
use super::super::primary::parse_call_arg_list;

use crate::ast::{AssignOp, Expr, Stmt};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::{ident, parse_statement_modifier, var_name};

static TMP_INDEX_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CompoundAssignOp {
    Comma,
    DefinedOr,
    LogicalOr,
    LogicalAnd,
    Add,
    Sub,
    Concat,
    Mul,
    Div,
    Mod,
    Power,
    Repeat,
    ListRepeat,
    BitOr,
    BitAnd,
    BitXor,
    BitShiftLeft,
    BitShiftRight,
    Min,
    Max,
    KeywordOr,
    KeywordAnd,
    Orelse,
    Andthen,
    IntDiv,
    Lcm,
    Gcd,
    StrBitAnd,
    StrBitOr,
    StrBitXor,
    StrShiftLeft,
    StrShiftRight,
    BoolBitAnd,
    BoolBitOr,
    BoolBitXor,
    XorXor,
    JuncAny,
    JuncAll,
    JuncOne,
}

mod assign_stmt;
mod bracket;
mod comma;
mod compound_expr;
mod lvalue;
mod op;
mod paren;
mod sink;
mod try_assign;

// ---- Re-exports preserving each public function's original visibility ----
pub(in crate::parser) use assign_stmt::assign_stmt;
pub(in crate::parser) use comma::parse_comma_or_expr;
pub(in crate::parser) use try_assign::try_parse_assign_expr;

pub(crate) use bracket::parse_bracket_meta_assign_op;
pub(crate) use compound_expr::{
    build_compound_assign_expr, build_custom_compound_assign_expr, build_meta_assign_expr,
};
pub(crate) use lvalue::{
    callable_lvalue_assign_expr, list_lvalue_assign_expr, method_lvalue_assign_expr,
    named_sub_lvalue_assign_expr, subscript_adverb_lvalue_assign_expr,
};
pub(crate) use op::{
    compound_assign_op_from_name, compound_assigned_value_expr, parse_compound_assign_op,
    parse_custom_compound_assign_op, parse_meta_compound_assign_op, parse_set_compound_assign_op,
};
pub(crate) use paren::{looks_like_parenthesized_assignment, parenthesized_assign_expr};
pub(crate) use sink::{
    parse_assign_expr_or_comma, rewrite_scalar_assignment_rhs_as_sink,
    rewrite_scalar_assignment_stmt_as_sink,
};
pub(crate) use try_assign::parse_colon_method_arg;
