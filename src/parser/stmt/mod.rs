mod args;
pub(crate) mod assign;
pub(super) mod class;
pub(in crate::parser) mod control;
pub(crate) mod decl;
mod idents;
pub(super) mod modifier;
mod pub_shims;
pub(super) mod simple;
mod simple_expr_stmt;
mod stmtlist;
mod sub;
pub(crate) mod sub_param;

use super::memo::{MemoEntry, MemoStats, ParseMemo};
use super::parse_result::{PError, PResult, parse_char, update_best_error};
use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::Stmt;

use super::helpers::{
    is_ident_char, is_raku_identifier_continue, is_raku_identifier_start,
    normalize_raku_identifier, ws, ws_bol, ws1,
};

// Re-export submodule items used across submodules
use args::{parse_stmt_call_args, parse_stmt_call_args_no_paren};
pub(in crate::parser) use assign::assign_stmt;
use assign::{parse_assign_expr_or_comma, parse_comma_or_expr, try_parse_assign_expr};
use class::class_decl_body;
use modifier::{is_stmt_modifier_keyword, parse_statement_modifier};
use sub::{
    method_decl_body, method_decl_body_my, parse_param_list, parse_sub_traits, sub_decl_body,
};

// Re-export the identifier/keyword/variable-name parsers moved to `idents`.
// `keyword`/`parse_raku_ident` are referenced from sibling parser modules
// (`primary`, `expr`) and must keep their original `pub(super)` reach; the
// others are only used within the `stmt` descendant tree.
use idents::{ident, qualified_ident, starts_unit_class_role_grammar, var_name};
pub(super) use idents::{keyword, parse_raku_ident};

// Re-export the block/statement-list parsers moved to `stmtlist`.
use stmtlist::block;
pub(in crate::parser) use stmtlist::stmt_list_pub;
pub(super) use stmtlist::{block_inner, stmt_list_partial};

// Re-export the thin public-accessor shims moved to `pub_shims`, all at their
// original `pub(super)` visibility (consumed by `primary`/`expr`).
pub(super) use pub_shims::{
    constant_decl_pub, for_stmt_pub, ident_pub, if_stmt_pub, labeled_loop_stmt_pub,
    lazy_for_stmt_pub, let_stmt_pub, loop_stmt_pub, my_decl_expr_pub, parse_param_list_pub,
    parse_param_list_with_return_pub, parse_pointy_param_pub, parse_return_type_annotation_pub,
    parse_sub_name_pub, parse_sub_traits_pub, statement_pub, sub_decl_with_semicolon_mode_pub,
    temp_stmt_pub, until_stmt_pub, while_stmt_pub,
};

thread_local! {
    static STMT_MEMO_TLS: RefCell<HashMap<(usize, usize), MemoEntry<Stmt>>> = RefCell::new(HashMap::new());
    static STMT_MEMO_STATS_TLS: RefCell<MemoStats> = RefCell::new(MemoStats::default());
}

static STMT_MEMO: ParseMemo<Stmt> = ParseMemo::new(&STMT_MEMO_TLS, &STMT_MEMO_STATS_TLS);

pub(super) fn reset_statement_memo() {
    STMT_MEMO.reset();
}

pub(super) fn reset_user_subs() {
    simple::reset_user_subs();
}

pub(super) fn set_eval_operator_preseed(names: Vec<String>) {
    simple::set_eval_operator_preseed(names);
}

pub(super) fn set_eval_operator_assoc_preseed(assoc: std::collections::HashMap<String, String>) {
    simple::set_eval_operator_assoc_preseed(assoc);
}

pub(super) fn set_eval_imported_function_preseed(names: Vec<String>) {
    simple::set_eval_imported_function_preseed(names);
}

pub(super) fn set_eval_user_sub_preseed(names: Vec<String>) {
    simple::set_eval_user_sub_preseed(names);
}

pub(super) fn statement_memo_stats() -> (usize, usize, usize) {
    STMT_MEMO.stats()
}

/// Statement parser function type.
type StmtParser = fn(&str) -> PResult<'_, Stmt>;

/// Dispatch table for statement parsers.
/// Each parser is tried in order until one succeeds.
/// Order is critical — do not reorder without careful consideration.
const STMT_PARSERS: &[StmtParser] = &[
    decl::use_stmt,
    decl::import_stmt,
    decl::no_stmt,
    decl::need_stmt,
    class::unit_module_stmt,
    decl::my_decl,
    decl::constant_decl,
    class::augment_class_decl,
    class::anon_class_decl,
    class::class_decl,
    class::role_decl,
    class::grammar_decl,
    class::module_decl,
    decl::subset_decl,
    decl::anon_enum_decl,
    decl::enum_decl,
    decl::has_decl,
    class::does_decl,
    class::trusts_decl,
    class::proto_decl,
    sub::anon_multi_check,
    sub::sub_decl,
    sub::method_decl,
    sub::submethod_decl,
    class::token_decl,
    simple::say_stmt,
    simple::put_stmt,
    simple::print_stmt,
    simple::note_stmt,
    control::if_stmt,
    control::unless_stmt,
    control::with_stmt,
    control::labeled_loop_stmt,
    control::foreach_stmt,
    control::race_for_stmt,
    control::hyper_for_stmt,
    control::for_stmt,
    control::while_stmt,
    control::until_stmt,
    control::loop_stmt,
    control::repeat_stmt,
    control::given_stmt,
    control::when_stmt,
    control::default_stmt,
    simple::return_stmt,
    simple::goto_stmt,
    simple::last_stmt,
    simple::next_stmt,
    simple::redo_stmt,
    simple::die_stmt,
    simple::take_stmt,
    simple::catch_stmt,
    simple::control_stmt,
    simple::phaser_stmt,
    simple::subtest_stmt,
    control::react_stmt,
    control::whenever_stmt,
    class::package_decl,
    class::also_trait_stmt,
    simple::let_stmt,
    simple::temp_stmt,
    simple::known_call_stmt,
    assign_stmt,
    simple::block_stmt,
];

fn statement(input: &str) -> PResult<'_, Stmt> {
    let (input, _) = ws(input)?;
    if let Some(cached) = STMT_MEMO.get(input) {
        return cached;
    }
    if (input.starts_with("qx") || input.starts_with("qqx"))
        && let Ok((rest, expr)) = crate::parser::primary::string::qx_string(input)
    {
        let result = parse_statement_modifier(rest, Stmt::Expr(expr));
        STMT_MEMO.store(input, &result);
        return result;
    }
    let input_len = input.len();
    let mut best_error: Option<(usize, PError)> = None;
    let mut early_success: Option<(&str, Stmt)> = None;

    // Try each statement parser in order
    for parser in STMT_PARSERS {
        match parser(input) {
            Ok(r) => {
                early_success = Some(r);
                break;
            }
            Err(err) => {
                if err.is_fatal() {
                    let result = Err(err);
                    STMT_MEMO.store(input, &result);
                    return result;
                }
                update_best_error(&mut best_error, err, input_len);
            }
        }
    }

    let result = if let Some(r) = early_success {
        Ok(r)
    } else {
        // Fall back to expression statement
        match simple::expr_stmt(input) {
            Ok(r) => Ok(r),
            Err(err) => {
                if err.is_fatal() {
                    Err(err)
                } else {
                    update_best_error(&mut best_error, err, input_len);
                    Err(best_error
                        .map(|(_, err)| err)
                        .unwrap_or_else(|| PError::expected_at("statement", input)))
                }
            }
        }
    };
    STMT_MEMO.store(input, &result);
    result
}

/// Parse a full program (sequence of statements).
pub(super) fn program(input: &str) -> PResult<'_, Vec<Stmt>> {
    // Strip BOM if present
    let input = if let Some(stripped) = input.strip_prefix('\u{FEFF}') {
        stripped
    } else {
        input
    };
    // Enable SetLine emission so that source line tracking works for
    // backtraces and $?LINE at the top level.
    stmtlist::stmt_list_with_mode(input, true, true)
}

#[cfg(test)]
mod tests;
#[cfg(test)]
mod tests_2;
#[cfg(test)]
mod tests_3;
#[cfg(test)]
mod tests_4;
