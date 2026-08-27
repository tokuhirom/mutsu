mod anon_sub;
mod circumfix;
mod identifier_call;
mod listop;
mod predicates;
mod supply;
mod supply_emit_expr;
mod term_literals;

pub(super) use circumfix::declared_circumfix_op;
pub(super) use identifier_call::identifier_or_call;
pub(in crate::parser) use listop::{
    colon_starts_colonpair, expr_is_colonpair, parse_expr_listop_args, try_adjacent_colonpair_arg,
};
pub(in crate::parser) use predicates::is_keyword;
pub(super) use term_literals::{class_literal, declared_term_symbol, keyword_literal, whatever};
