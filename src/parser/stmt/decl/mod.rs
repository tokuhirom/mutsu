mod constant_subset;
mod destructure;
mod enum_decl;
mod handles;
mod has_decl;
mod helpers;
mod my_decl;
mod my_decl_assign;
mod my_decl_dispatch;
mod my_decl_helpers;
mod use_decl;

// Re-export public items that were previously accessible from the flat `decl` module.
pub(in crate::parser::stmt) use constant_subset::{constant_decl, subset_decl};
pub(crate) use enum_decl::{anon_enum_decl, enum_decl};
pub(in crate::parser::stmt) use handles::parse_handle_specs;
pub(in crate::parser::stmt) use has_decl::has_decl;
pub(in crate::parser::stmt) use helpers::parse_array_shape_suffix;
pub(in crate::parser::stmt) use my_decl::{my_decl, my_decl_expr};
pub(in crate::parser::stmt) use use_decl::{import_stmt, need_stmt, no_stmt, use_stmt};

use super::super::expr::expression;
use super::super::helpers::{ws, ws1};
use super::super::parse_result::{PResult, parse_char, take_while1};

use crate::ast::{Expr, PhaserKind, Stmt};
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::sub::parse_type_constraint_expr;
use super::{
    class::{module_decl, package_decl, proto_decl, role_decl},
    keyword, parse_assign_expr_or_comma, parse_statement_modifier,
};

use super::super::parse_result::take_while_opt;
use super::{
    class_decl_body, method_decl_body, method_decl_body_my, parse_comma_or_expr, sub_decl_body,
};

/// Parse a single argument in colon method-call syntax (.method: arg1, arg2).
/// Tries colonpair first (:name, :$var, :!flag, :0port), then expression.
fn parse_colon_method_arg(input: &str) -> PResult<'_, Expr> {
    if input.starts_with(':')
        && !input.starts_with("::")
        && let Ok(result) = crate::parser::primary::misc::colonpair_expr(input)
    {
        return Ok(result);
    }
    expression(input)
}

fn parse_decl_type_constraint(input: &str) -> Option<(&str, String)> {
    let (mut rest, mut type_name) = parse_type_constraint_expr(input)?;
    loop {
        let (after_ws, _) = ws(rest).ok()?;
        let Some(after_of) = keyword("of", after_ws) else {
            break;
        };
        let (after_of, _) = ws1(after_of).ok()?;
        let (after_inner, inner_type) = parse_decl_type_constraint(after_of)?;
        type_name = format!("{}[{}]", type_name, inner_type);
        rest = after_inner;
    }
    Some((rest, type_name))
}

/// Wrap a statement with a LEAVE phaser for `will leave { ... }`.
/// The phaser body sets `$_` to the variable and executes the block.
fn wrap_with_will_leave(stmt: Stmt, var_name: &str, leave_body: Option<Vec<Stmt>>) -> Stmt {
    match leave_body {
        None => stmt,
        Some(body) => {
            // Build: LEAVE { my $_ = $var; <body> }
            let mut phaser_body = vec![Stmt::VarDecl {
                name: "$_".to_string(),
                expr: Expr::Var(var_name.to_string()),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            }];
            phaser_body.extend(body);
            let phaser = Stmt::Phaser {
                kind: PhaserKind::Leave,
                body: phaser_body,
            };
            Stmt::SyntheticBlock(vec![stmt, phaser])
        }
    }
}

fn strip_type_smiley_suffix(type_name: &str) -> &str {
    type_name
        .strip_suffix(":U")
        .or_else(|| type_name.strip_suffix(":D"))
        .or_else(|| type_name.strip_suffix(":_"))
        .unwrap_or(type_name)
}

fn typed_default_expr(type_name: &str) -> Expr {
    let base = strip_type_smiley_suffix(type_name);
    if base == "Mu" {
        Expr::BareWord("Mu".to_string())
    } else if base == "int"
        || base == "int8"
        || base == "int16"
        || base == "int32"
        || base == "int64"
        || base == "uint"
        || base == "uint8"
        || base == "uint16"
        || base == "uint32"
        || base == "uint64"
    {
        Expr::Literal(Value::Int(0))
    } else if base == "num" || base == "num32" || base == "num64" {
        Expr::Literal(Value::Num(0.0))
    } else if base == "str" {
        Expr::Literal(Value::str("".to_string()))
    } else {
        Expr::Literal(Value::Nil)
    }
}

fn default_decl_expr(
    is_array: bool,
    is_hash: bool,
    shape_dims: Option<&[Expr]>,
    type_constraint: Option<&str>,
) -> Expr {
    if is_array {
        if let Some(dims) = shape_dims {
            helpers::shaped_array_new_expr(dims.to_vec())
        } else {
            Expr::Literal(Value::real_array(Vec::new()))
        }
    } else if is_hash {
        Expr::Hash(Vec::new())
    } else if let Some(tc) = type_constraint {
        typed_default_expr(tc)
    } else {
        Expr::Literal(Value::Nil)
    }
}

fn scalar_binding_rhs_is_readonly(expr: &Expr) -> bool {
    matches!(expr, Expr::Literal(_))
}

fn parse_comma_chained_decls<'a>(input: &'a str, first: Stmt) -> PResult<'a, Stmt> {
    let (r, _) = ws(input)?;
    if !r.starts_with(',') {
        return Ok((input, first));
    }
    let after_comma = &r[1..];
    let (after_ws, _) = ws(after_comma)?;
    // Check if the next token is my/our/state (another declaration)
    if keyword("my", after_ws).is_none()
        && keyword("our", after_ws).is_none()
        && keyword("state", after_ws).is_none()
    {
        return Ok((input, first));
    }
    // Parse the next declaration
    let (rest, next) = my_decl::my_decl_inner(after_ws, false)?;
    let mut stmts = match first {
        Stmt::SyntheticBlock(v) => v,
        other => vec![other],
    };
    match next {
        Stmt::SyntheticBlock(v) => stmts.extend(v),
        other => stmts.push(other),
    }
    Ok((rest, Stmt::SyntheticBlock(stmts)))
}

/// Consume trailing comma-separated sink expressions after a scalar declaration.
/// In Raku, `my $c = 1, 2, 3;` is valid: `$c` gets `1`, and `2, 3` are in sink context.
fn consume_scalar_decl_trailing_comma<'a>(input: &'a str, first: Stmt) -> PResult<'a, Stmt> {
    let (r, _) = ws(input)?;
    if !r.starts_with(',') || r.starts_with(",,") {
        return Ok((input, first));
    }
    // Don't consume if followed by another declaration — that's handled by parse_comma_chained_decls
    let after_comma = &r[1..];
    let (after_ws, _) = ws(after_comma)?;
    if keyword("my", after_ws).is_some()
        || keyword("our", after_ws).is_some()
        || keyword("state", after_ws).is_some()
    {
        return Ok((input, first));
    }
    // Consume trailing sink expressions
    let mut stmts = match first {
        Stmt::SyntheticBlock(v) => v,
        other => vec![other],
    };
    let mut r_inner = after_ws;
    if r_inner.starts_with(';') || r_inner.is_empty() || r_inner.starts_with('}') {
        // Trailing comma only, no expressions
        return Ok((r_inner, Stmt::SyntheticBlock(stmts)));
    }
    let (r2, sink_expr) = expression(r_inner)?;
    stmts.push(Stmt::Expr(sink_expr));
    r_inner = r2;
    loop {
        let (r2, _) = ws(r_inner)?;
        if !r2.starts_with(',') || r2.starts_with(",,") {
            r_inner = r2;
            break;
        }
        let r2 = &r2[1..];
        let (r2, _) = ws(r2)?;
        if r2.starts_with(';') || r2.is_empty() || r2.starts_with('}') {
            r_inner = r2;
            break;
        }
        let (r2, sink_expr) = expression(r2)?;
        stmts.push(Stmt::Expr(sink_expr));
        r_inner = r2;
    }
    Ok((r_inner, Stmt::SyntheticBlock(stmts)))
}

fn is_decl_trailing_or_chain_op(op: &TokenKind) -> bool {
    matches!(op, TokenKind::OrWord | TokenKind::OrElse)
}

fn rewrite_decl_assignment_or_chain(expr: Expr, mut decl_stmt: Stmt) -> Option<Stmt> {
    let mut init = expr;
    let mut tails: Vec<(TokenKind, Expr)> = Vec::new();
    while let Expr::Binary { left, op, right } = init {
        if !is_decl_trailing_or_chain_op(&op) {
            init = Expr::Binary { left, op, right };
            break;
        }
        tails.push((op, *right));
        init = *left;
    }
    if tails.is_empty() {
        return None;
    }

    if let Stmt::VarDecl { expr, .. } = &mut decl_stmt {
        *expr = init;
    } else {
        return None;
    }

    let mut chain = Expr::DoStmt(Box::new(decl_stmt));
    for (op, right) in tails.into_iter().rev() {
        chain = Expr::Binary {
            left: Box::new(chain),
            op,
            right: Box::new(right),
        };
    }
    Some(Stmt::Expr(chain))
}
