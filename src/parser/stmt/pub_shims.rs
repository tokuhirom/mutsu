//! Thin public-accessor delegation shims.
//!
//! Moved out of `stmt/mod.rs` (§7-8 cohesive split). Each function here is a
//! one-line wrapper that exposes a parser entry point (living in a sibling
//! submodule) to external callers — primarily `primary.rs` / `expr.rs`, which
//! need to parse statement/declaration fragments in expression context. They
//! are re-exported from `mod.rs` at their original `pub(super)` visibility.

use super::*;

/// Public accessor for ident (used by primary.rs for hash literal detection).
pub(crate) fn ident_pub(input: &str) -> PResult<'_, String> {
    ident(input)
}

/// Public accessor for `temp_stmt` (used by `container.rs` for `(temp @a)` parsing).
pub(crate) fn temp_stmt_pub(input: &str) -> PResult<'_, crate::ast::Stmt> {
    simple_expr_stmt::temp_stmt(input)
}

/// Public accessor for `let_stmt` (used by `container.rs` for `(let $x)` parsing).
pub(crate) fn let_stmt_pub(input: &str) -> PResult<'_, crate::ast::Stmt> {
    simple_expr_stmt::let_stmt(input)
}

/// Public accessor for var_name (used by primary.rs for anon sub params).
#[allow(dead_code)]
pub(crate) fn var_name_pub(input: &str) -> PResult<'_, String> {
    var_name(input)
}
/// Public accessor for parse_param_list (used by primary.rs for arrow lambda sub-signatures).
pub(crate) fn parse_param_list_pub(input: &str) -> PResult<'_, Vec<crate::ast::ParamDef>> {
    parse_param_list(input)
}

/// Public accessor for parse_param_list_with_return (used by signature literals).
pub(crate) fn parse_param_list_with_return_pub(
    input: &str,
) -> PResult<'_, (Vec<crate::ast::ParamDef>, Option<String>)> {
    sub::parse_param_list_with_return(input)
}

pub(crate) fn parse_return_type_annotation_pub(input: &str) -> PResult<'_, String> {
    sub::parse_return_type_annotation(input)
}

pub(crate) fn parse_sub_traits_pub(input: &str) -> PResult<'_, sub::SubTraits> {
    sub::parse_sub_traits(input)
}

pub(crate) fn parse_sub_name_pub(input: &str) -> PResult<'_, String> {
    sub::parse_sub_name(input)
}

/// Public accessor for sub_decl_with_semicolon_mode (used by primary/ident.rs for multi sub exprs).
pub(crate) fn sub_decl_with_semicolon_mode_pub(
    input: &str,
    allow_main_semicolon_decl: bool,
) -> PResult<'_, Stmt> {
    sub::sub_decl_with_semicolon_mode(input, allow_main_semicolon_decl)
}

/// Public accessor for constant declaration parser (used by primary.rs in expression context).
pub(crate) fn constant_decl_pub(input: &str) -> PResult<'_, Stmt> {
    decl::constant_decl(input)
}

/// Public accessor for statement (used by primary.rs for do-statement expressions).
pub(crate) fn statement_pub(input: &str) -> PResult<'_, Stmt> {
    statement(input)
}

/// Parse `my`/`our`/`state` in expression context — does not consume
/// semicolons or apply statement modifiers.
pub(crate) fn my_decl_expr_pub(input: &str) -> PResult<'_, Stmt> {
    decl::my_decl_expr(input)
}

/// Public accessor for `for` statement parser (used by primary.rs for `for` expressions).
pub(crate) fn for_stmt_pub(input: &str) -> PResult<'_, Stmt> {
    control::for_stmt(input)
}

/// Public accessor for `lazy for` statement parser (used by primary.rs for `lazy for` expressions).
/// Input should start at `for` (not `lazy`).
pub(crate) fn lazy_for_stmt_pub(input: &str) -> PResult<'_, Stmt> {
    control::lazy_for_body(input)
}

/// Public accessor for `if` statement parser (used by primary.rs for `if` expressions).
pub(crate) fn if_stmt_pub(input: &str) -> PResult<'_, Stmt> {
    control::if_stmt(input)
}

/// Public accessor for `while` statement parser (used by primary.rs for `while` expressions).
pub(crate) fn while_stmt_pub(input: &str) -> PResult<'_, Stmt> {
    control::while_stmt(input)
}

/// Public accessor for `until` statement parser (used by primary.rs for `until` expressions).
pub(crate) fn until_stmt_pub(input: &str) -> PResult<'_, Stmt> {
    control::until_stmt(input)
}

/// Public accessor for `loop` statement parser (used by primary.rs for `loop` expressions).
pub(crate) fn loop_stmt_pub(input: &str) -> PResult<'_, Stmt> {
    control::loop_stmt(input)
}

pub(crate) fn labeled_loop_stmt_pub(input: &str) -> PResult<'_, Stmt> {
    control::labeled_loop_stmt(input)
}

pub(crate) fn parse_pointy_param_pub(input: &str) -> PResult<'_, crate::ast::ParamDef> {
    control::parse_pointy_param(input)
}
