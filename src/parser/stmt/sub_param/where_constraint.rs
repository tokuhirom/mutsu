use crate::ast::{Expr, Stmt};
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PError, PResult};
use crate::symbol::Symbol;
use crate::value::Value;
use std::collections::HashMap;

pub(crate) fn parse_where_constraint_expr(input: &str) -> PResult<'_, Expr> {
    let (r, _) = ws(input)?;
    if r.starts_with('{') {
        let (r, body) = crate::parser::primary::parse_block_body(r)?;
        if stmts_contain_whatever(&body) {
            return Err(malformed_double_closure_error());
        }
        return Ok((
            r,
            Expr::AnonSub {
                body,
                is_rw: false,
                is_block: true,
            },
        ));
    }
    // Parse the constraint without consuming a trailing `=` default value: in a
    // signature `$x where <constraint> = <default>`, the `=` introduces the
    // parameter default, not an assignment to the constraint expression.
    crate::parser::expr::expression_no_assign(input)
}

pub(crate) fn malformed_double_closure_error() -> PError {
    let msg = "Malformed double closure; WhateverCode is already a closure without curlies, so either remove the curlies or use valid parameter syntax instead of *".to_string();
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert("what".to_string(), Value::str("closure".to_string()));
    let ex = Value::make_instance(Symbol::intern("X::Syntax::Malformed"), attrs);
    PError::fatal_with_exception(msg, Box::new(ex))
}

/// Reject placeholder (`$:x`, `$^x`) and twigil (`$?x`, `$=x`, `$~x`) variables
/// when they appear as a signature parameter. `input` is the remaining param
/// text, positioned at the sigil. Returns `Ok(())` when the param is not one of
/// these forms (the caller proceeds with normal parsing).
pub(crate) fn reject_placeholder_or_twigil_param(input: &str) -> Result<(), PError> {
    let bytes = input.as_bytes();
    if bytes.len() < 3 || !matches!(bytes[0], b'$' | b'@' | b'%' | b'&') {
        return Ok(());
    }
    let sigil = bytes[0] as char;
    let twigil = bytes[1] as char;
    if !matches!(twigil, ':' | '^' | '?' | '=' | '~') {
        return Ok(());
    }
    // `$::x` is a package-qualified variable, not a `:` placeholder.
    if twigil == ':' && bytes[2] == b':' {
        return Ok(());
    }
    // Must be followed by an identifier character to be a placeholder/twigil var.
    let after = &input[2..];
    let next = after.chars().next();
    if !next.is_some_and(|c| c.is_alphanumeric() || c == '_') {
        return Ok(());
    }
    let end = after
        .find(|c: char| !c.is_alphanumeric() && c != '_')
        .unwrap_or(after.len());
    let name = &after[..end];
    let parameter = format!("{sigil}{twigil}{name}");

    let mut attrs = HashMap::new();
    attrs.insert("parameter".to_string(), Value::str(parameter.clone()));

    let (class, msg) = match twigil {
        ':' => {
            let right = format!(":{sigil}{name}");
            attrs.insert("right".to_string(), Value::str(right.clone()));
            (
                "X::Parameter::Placeholder",
                format!(
                    "Named placeholder variables like '{parameter}' are not allowed in signatures. \nDid you mean: '{right}' ?"
                ),
            )
        }
        '^' => {
            let right = format!("{sigil}{name}");
            attrs.insert("right".to_string(), Value::str(right.clone()));
            (
                "X::Parameter::Placeholder",
                format!(
                    "Positional placeholder variables like '{parameter}' are not allowed in signatures.  Did you mean: '{right}' ?"
                ),
            )
        }
        _ => {
            attrs.insert("twigil".to_string(), Value::str(twigil.to_string()));
            (
                "X::Parameter::Twigil",
                format!(
                    "Parameters with a '{twigil}' twigil, like '{parameter}', are not allowed in signatures."
                ),
            )
        }
    };
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern(class), attrs);
    Err(PError::fatal_with_exception(msg, Box::new(ex)))
}

pub(crate) fn stmts_contain_whatever(stmts: &[Stmt]) -> bool {
    stmts.iter().any(stmt_contains_whatever)
}

fn stmt_contains_whatever(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Expr(expr)
        | Stmt::Return(expr)
        | Stmt::Take(expr, _)
        | Stmt::Die(expr)
        | Stmt::Fail(expr) => expr_contains_whatever(expr),
        Stmt::VarDecl {
            expr,
            where_constraint,
            ..
        } => {
            expr_contains_whatever(expr)
                || where_constraint
                    .as_ref()
                    .is_some_and(|wc| expr_contains_whatever(wc))
        }
        Stmt::Assign { expr, .. } => expr_contains_whatever(expr),
        Stmt::Block(body) | Stmt::SyntheticBlock(body) | Stmt::Package { body, .. } => {
            stmts_contain_whatever(body)
        }
        Stmt::SubDecl { body, .. } | Stmt::TokenDecl { body, .. } | Stmt::RuleDecl { body, .. } => {
            stmts_contain_whatever(body)
        }
        Stmt::Label { stmt, .. } => stmt_contains_whatever(stmt),
        _ => false,
    }
}

fn expr_contains_whatever(expr: &Expr) -> bool {
    match expr {
        Expr::Whatever | Expr::HyperWhatever => true,
        Expr::StringInterpolation(parts)
        | Expr::ArrayLiteral(parts)
        | Expr::BracketArray(parts, _)
        | Expr::CaptureLiteral(parts) => parts.iter().any(expr_contains_whatever),
        Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
            expr_contains_whatever(target) || args.iter().any(expr_contains_whatever)
        }
        Expr::DynamicMethodCall {
            target,
            name_expr,
            args,
            ..
        }
        | Expr::HyperMethodCallDynamic {
            target,
            name_expr,
            args,
            ..
        } => {
            expr_contains_whatever(target)
                || expr_contains_whatever(name_expr)
                || args.iter().any(expr_contains_whatever)
        }
        Expr::Exists { target, arg, .. } => {
            expr_contains_whatever(target)
                || arg
                    .as_ref()
                    .is_some_and(|arg_expr| expr_contains_whatever(arg_expr))
        }
        Expr::ZenSlice(inner)
        | Expr::PositionalPair(inner)
        | Expr::Eager(inner)
        | Expr::Itemize(inner)
        | Expr::Reduction { expr: inner, .. }
        | Expr::IndirectTypeLookup(inner)
        | Expr::SymbolicDeref { expr: inner, .. } => expr_contains_whatever(inner),
        Expr::Lambda { param, body, .. } => param == "_" || stmts_contain_whatever(body),
        Expr::AnonSubParams { params, body, .. } => {
            params.iter().any(|p| p.starts_with("__wc_")) || stmts_contain_whatever(body)
        }
        Expr::Block(body) | Expr::AnonSub { body, .. } | Expr::Gather(body) => {
            stmts_contain_whatever(body)
        }
        Expr::CallOn { target, args } => {
            expr_contains_whatever(target) || args.iter().any(expr_contains_whatever)
        }
        Expr::Index { target, index, .. } => {
            expr_contains_whatever(target) || expr_contains_whatever(index)
        }
        Expr::MultiDimIndex { target, dimensions } => {
            expr_contains_whatever(target) || dimensions.iter().any(expr_contains_whatever)
        }
        Expr::MultiDimIndexAssign {
            target,
            dimensions,
            value,
        } => {
            expr_contains_whatever(target)
                || dimensions.iter().any(expr_contains_whatever)
                || expr_contains_whatever(value)
        }
        Expr::IndexAssign {
            target,
            index,
            value,
            ..
        } => {
            expr_contains_whatever(target)
                || expr_contains_whatever(index)
                || expr_contains_whatever(value)
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            expr_contains_whatever(cond)
                || expr_contains_whatever(then_expr)
                || expr_contains_whatever(else_expr)
        }
        Expr::AssignExpr { expr, .. } => expr_contains_whatever(expr),
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => expr_contains_whatever(expr),
        Expr::Binary { left, right, .. }
        | Expr::HyperOp { left, right, .. }
        | Expr::MetaOp { left, right, .. } => {
            expr_contains_whatever(left) || expr_contains_whatever(right)
        }
        Expr::Call { args, .. } => args.iter().any(expr_contains_whatever),
        Expr::Try { body, catch } => {
            stmts_contain_whatever(body)
                || catch
                    .as_ref()
                    .is_some_and(|branch| stmts_contain_whatever(branch))
        }
        Expr::InfixFunc { left, right, .. } => {
            expr_contains_whatever(left) || right.iter().any(expr_contains_whatever)
        }
        Expr::DoBlock { body, .. } => stmts_contain_whatever(body),
        Expr::DoStmt(stmt) => stmt_contains_whatever(stmt),
        Expr::IndirectCodeLookup { package, .. } => expr_contains_whatever(package),
        Expr::Hash(pairs) => pairs
            .iter()
            .any(|(_, value)| value.as_ref().is_some_and(expr_contains_whatever)),
        Expr::HyperSlice { target, .. } => expr_contains_whatever(target),
        Expr::HyperIndex { target, keys } => {
            expr_contains_whatever(target) || expr_contains_whatever(keys)
        }
        _ => false,
    }
}
