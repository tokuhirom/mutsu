use super::super::expr::expression;
use super::super::helpers::{ws, ws1};
use super::super::parse_result::{PError, PResult, parse_char};

use crate::ast::{Expr, ParamDef, Stmt, collect_placeholders_shallow};
use crate::symbol::Symbol;
use crate::value::Value;
use std::collections::HashMap;

/// Check for invalid type smileys (e.g. Int:foo) in a type constraint string.
/// Valid smileys are :D, :U, and :_. Anything else raises X::InvalidTypeSmiley.
pub(super) fn check_invalid_type_smiley(type_constraint: &Option<String>) -> Result<(), PError> {
    if let Some(tc) = type_constraint
        && let Some(colon_pos) = tc.rfind(':')
        && (colon_pos == 0 || tc.as_bytes()[colon_pos - 1] != b':')
    {
        let after_colon = &tc[colon_pos + 1..];
        // Extract the smiley portion: only alphanumeric and underscore/hyphen chars
        let smiley_end = after_colon
            .find(|c: char| !c.is_ascii_alphanumeric() && c != '_' && c != '-')
            .unwrap_or(after_colon.len());
        let smiley = &after_colon[..smiley_end];
        if !smiley.is_empty()
            && smiley != "D"
            && smiley != "U"
            && smiley != "_"
            && smiley.chars().next().is_some_and(|c| c.is_alphabetic())
        {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("name".to_string(), Value::str(smiley.to_string()));
            attrs.insert(
                "message".to_string(),
                Value::str(format!(
                    "Invalid type smiley ':{}' used, only ':D', ':U' and ':_' are allowed",
                    smiley
                )),
            );
            let ex = Value::make_instance(Symbol::intern("X::InvalidTypeSmiley"), attrs);
            return Err(PError::fatal_with_exception(
                format!(
                    "Invalid type smiley ':{}' used, only ':D', ':U' and ':_' are allowed",
                    smiley
                ),
                Box::new(ex),
            ));
        }
    }
    Ok(())
}

use super::decl::parse_array_shape_suffix;
use super::sub::{
    literal_value_from_expr, parse_indirect_decl_name, parse_param_list,
    parse_param_list_with_return, parse_sub_name, parse_sub_traits, validate_param_trait,
    validate_signature_params,
};
use super::{block, ident, keyword, qualified_ident, var_name};

pub(super) fn mark_params_as_invocant(params: &mut [ParamDef]) {
    for param in params {
        if !param.traits.iter().any(|t| t == "invocant") {
            param.traits.push("invocant".to_string());
        }
    }
}

/// Helper to construct a default ParamDef with only required fields.
fn make_param(name: String) -> ParamDef {
    ParamDef {
        name,
        default: None,
        multi_invocant: true,
        required: false,
        named: false,
        slurpy: false,
        double_slurpy: false,
        onearg: false,
        sigilless: false,
        type_constraint: None,
        literal_value: None,
        sub_signature: None,
        where_constraint: None,
        traits: Vec::new(),
        optional_marker: false,
        outer_sub_signature: None,
        code_signature: None,
        is_invocant: false,
        shape_constraints: None,
    }
}

fn parse_param_default_expr(input: &str) -> PResult<'_, Expr> {
    if let Ok((rest, expr)) = expression(input) {
        return Ok((rest, expr));
    }
    if let Some(after_my) = input.strip_prefix("my ")
        && let Ok((rest, expr)) = expression(after_my)
    {
        return Ok((rest, expr));
    }
    Err(PError::expected("parameter default expression"))
}

pub(super) fn is_anonymous_sigil_param(param: &ParamDef) -> bool {
    matches!(
        param.name.as_str(),
        "__ANON_STATE__" | "__ANON_ARRAY__" | "__ANON_HASH__" | "__ANON_CODE__"
    )
}

pub(super) fn starts_with_sigil_param(input: &str) -> bool {
    matches!(input.as_bytes().first(), Some(b'$' | b'@' | b'%' | b'&'))
}

pub(super) fn parse_implicit_invocant_marker(input: &str) -> Option<(&str, String)> {
    if input.starts_with('$')
        || input.starts_with('@')
        || input.starts_with('%')
        || input.starts_with('&')
        || input.starts_with('*')
        || input.starts_with(':')
    {
        return None;
    }
    let (mut rest, mut type_name) = qualified_ident(input).ok()?;
    while rest.starts_with('[') {
        let (r2, suffix) = parse_generic_suffix(rest).ok()?;
        type_name.push_str(&suffix);
        rest = r2;
    }
    if rest.starts_with(":D") || rest.starts_with(":U") || rest.starts_with(":_") {
        type_name.push_str(&rest[..2]);
        rest = &rest[2..];
    }
    let after_colon = rest.strip_prefix(':')?;
    if after_colon.starts_with(':') {
        return None;
    }
    // If the character after ':' is alphanumeric or '_', this is likely a type
    // smiley (e.g., Int:foo) rather than an invocant marker (e.g., Int: $self).
    // Invocant markers must be followed by whitespace, sigil, or ')'.
    if after_colon
        .chars()
        .next()
        .is_some_and(|c| c.is_alphanumeric() || c == '_')
    {
        return None;
    }
    let (after_colon, _) = ws(after_colon).ok()?;
    Some((after_colon, type_name))
}

/// Returns (rest, required, optional_marker).
/// `!` → required=true, optional_marker=false
/// `?` → required=false, optional_marker=true
/// neither → required=false, optional_marker=false
fn parse_required_suffix(input: &str) -> (&str, bool, bool) {
    if let Some(rest) = input.strip_prefix('!') {
        (rest, true, false)
    } else if let Some(rest) = input.strip_prefix('?') {
        (rest, false, true)
    } else {
        (input, false, false)
    }
}

pub(super) fn parse_type_constraint_expr(input: &str) -> Option<(&str, String)> {
    // Handle ::?CLASS and ::?ROLE pseudo-types
    let (mut rest, mut type_name) = if let Some(r) = input.strip_prefix("::?CLASS") {
        (r, "::?CLASS".to_string())
    } else if let Some(r) = input.strip_prefix("::?ROLE") {
        (r, "::?ROLE".to_string())
    // Handle type capture variables like `::a` (e.g., `my ::a $a`)
    } else if input.starts_with("::")
        && input[2..]
            .chars()
            .next()
            .is_some_and(|c| c.is_alphabetic() || c == '_')
    {
        let r = &input[2..];
        let end = r
            .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
            .unwrap_or(r.len());
        (&r[end..], format!("::{}", &r[..end]))
    } else {
        qualified_ident(input).ok()?
    };
    while rest.starts_with('[') {
        let (r2, suffix) = parse_generic_suffix(rest).ok()?;
        type_name.push_str(&suffix);
        rest = r2;
    }
    if rest.starts_with(":D") || rest.starts_with(":U") || rest.starts_with(":_") {
        type_name.push_str(&rest[..2]);
        rest = &rest[2..];
    } else if rest.starts_with(':')
        && rest[1..]
            .chars()
            .next()
            .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
    {
        // Invalid type smiley like :foo — include it in the type name
        // so the caller can detect and report it as X::InvalidTypeSmiley
        let smiley_rest = &rest[1..];
        let end = smiley_rest
            .find(|c: char| !c.is_ascii_alphanumeric() && c != '_' && c != '-')
            .unwrap_or(smiley_rest.len());
        type_name.push_str(&rest[..end + 1]);
        rest = &rest[end + 1..];
    }

    let (rest, type_name) = parse_of_type_constraint_chain(rest, type_name)?;

    let (r2, _) = ws(rest).ok()?;
    if let Some(after_open) = r2.strip_prefix('(') {
        let (after_ws, _) = ws(after_open).ok()?;
        if let Some(r3) = after_ws.strip_prefix(')') {
            let (r3, _) = ws(r3).ok()?;
            return Some((r3, format!("{}()", type_name)));
        }
        if let Some((inner_r, source_type)) = parse_type_constraint_expr(after_ws) {
            let (inner_r, _) = ws(inner_r).ok()?;
            if let Some(r3) = inner_r.strip_prefix(')') {
                let (r3, _) = ws(r3).ok()?;
                return Some((r3, format!("{}({})", type_name, source_type)));
            }
        }
    }
    Some((rest, type_name))
}

pub(super) fn parse_of_type_constraint_chain(
    input: &str,
    base_type: String,
) -> Option<(&str, String)> {
    let mut rest = input;
    let mut type_name = base_type;
    loop {
        let (r_ws, _) = ws(rest).ok()?;
        let Some(after_of) = keyword("of", r_ws) else {
            break;
        };
        let (after_of, _) = ws(after_of).ok()?;
        let Some((r_elem, elem_type)) = parse_type_constraint_expr(after_of) else {
            break;
        };
        type_name = format!("{}[{}]", type_name, elem_type);
        rest = r_elem;
    }
    Some((rest, type_name))
}

fn parse_where_constraint_expr(input: &str) -> PResult<'_, Expr> {
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
    expression(input)
}

fn malformed_double_closure_error() -> PError {
    let msg = "Malformed double closure; WhateverCode is already a closure without curlies, so either remove the curlies or use valid parameter syntax instead of *".to_string();
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert("what".to_string(), Value::str("closure".to_string()));
    let ex = Value::make_instance(Symbol::intern("X::Syntax::Malformed"), attrs);
    PError::fatal_with_exception(msg, Box::new(ex))
}

fn stmts_contain_whatever(stmts: &[Stmt]) -> bool {
    stmts.iter().any(stmt_contains_whatever)
}

fn stmt_contains_whatever(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Expr(expr)
        | Stmt::Return(expr)
        | Stmt::Take(expr)
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

fn parse_generic_suffix(input: &str) -> PResult<'_, String> {
    if !input.starts_with('[') {
        return Ok((input, String::new()));
    }
    let mut depth = 0usize;
    let mut end = 0usize;
    for (idx, ch) in input.char_indices() {
        match ch {
            '[' => depth += 1,
            ']' => {
                if depth == 0 {
                    return Err(PError::expected("matching ']'"));
                }
                depth -= 1;
                if depth == 0 {
                    end = idx + ch.len_utf8();
                    break;
                }
            }
            _ => {}
        }
    }
    if end == 0 {
        return Err(PError::expected("matching ']'"));
    }
    Ok((&input[end..], input[..end].to_string()))
}

pub(super) fn parse_single_param(input: &str) -> PResult<'_, ParamDef> {
    let mut rest = input;
    let mut named = false;
    let mut slurpy = false;
    let mut type_constraint = None;

    // Array sub-signature: [Type1, Type2, ...] → anonymous @ with sub-sig
    if rest.starts_with('[') {
        let (r, _) = parse_char(rest, '[')?;
        let (r, _) = ws(r)?;
        let (r, sub_params) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ']')?;
        let mut p = make_param("@".to_string());
        p.sub_signature = Some(sub_params);
        return Ok((r, p));
    }

    // Capture-all: (|), (|$c), (|c), or capture sub-signature forms like | ($x)
    if let Some(stripped) = rest.strip_prefix('|') {
        let (r, _) = ws(stripped)?;
        if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            let (r, sub_params) = parse_param_list(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            let (r, _) = ws(r)?;
            if sub_params.len() == 1 {
                return Ok((r, sub_params[0].clone()));
            }
            let mut p = make_param("__subsig__".to_string());
            p.sub_signature = Some(sub_params);
            return Ok((r, p));
        }
        let (r, _) = ws(r)?;
        // Optional capture variable name with sigil, optionally followed by
        // a capture sub-signature: |$c ($a, $b?)
        if r.starts_with('$') || r.starts_with('@') || r.starts_with('%') {
            let (r, name) = var_name(r)?;
            let mut p = make_param(name);
            p.slurpy = true;
            let (r, _) = ws(r)?;
            if r.starts_with('(') {
                let (r, _) = parse_char(r, '(')?;
                let (r, _) = ws(r)?;
                let (r, sub_params) = parse_param_list(r)?;
                let (r, _) = ws(r)?;
                let (r, _) = parse_char(r, ')')?;
                p.sub_signature = Some(sub_params);
                return Ok((r, p));
            }
            return Ok((r, p));
        }
        // Sigilless capture variable name: |c, |args, optionally followed by
        // a capture sub-signature: |c ($a, $b?)
        // or a where constraint: |c where { ... }
        if let Ok((r_ident, name)) = ident(r)
            && !matches!(name.as_str(), "where" | "is")
        {
            let mut p = make_param(name);
            p.slurpy = true;
            p.sigilless = true;
            let (r_ident, _) = ws(r_ident)?;
            if r_ident.starts_with('(') {
                let (r_ident, _) = parse_char(r_ident, '(')?;
                let (r_ident, _) = ws(r_ident)?;
                let (r_ident, sub_params) = parse_param_list(r_ident)?;
                let (r_ident, _) = ws(r_ident)?;
                let (r_ident, _) = parse_char(r_ident, ')')?;
                p.sub_signature = Some(sub_params);
                return Ok((r_ident, p));
            }
            // Check for where constraint on named capture: |c where { ... }
            let (r_ident, where_constraint) = if let Some(r2) = keyword("where", r_ident) {
                let (r2, _) = ws1(r2)?;
                let (r2, constraint) = parse_where_constraint_expr(r2)?;
                (r2, Some(Box::new(constraint)))
            } else {
                (r_ident, None)
            };
            p.where_constraint = where_constraint;
            return Ok((r_ident, p));
        }
        // Bare |, possibly followed by traits/where
        let mut p = make_param("_capture".to_string());
        p.slurpy = true;
        let (r, _) = ws(r)?;
        let mut param_traits = Vec::new();
        let (mut r, _) = ws(r)?;
        while let Some(r2) = keyword("is", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, trait_name) = ident(r2)?;
            validate_param_trait(&trait_name, &param_traits, r2)?;
            param_traits.push(trait_name);
            let (r2, _) = ws(r2)?;
            r = r2;
        }
        let (r, where_constraint) = if let Some(r2) = keyword("where", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, constraint) = parse_where_constraint_expr(r2)?;
            (r2, Some(Box::new(constraint)))
        } else {
            (r, None)
        };
        p.traits = param_traits;
        p.where_constraint = where_constraint;
        return Ok((r, p));
    }

    // Slurpy: *@arr or *%hash or *$scalar or *[...] (slurpy unpack)
    let mut slurpy_sigil = None;
    let mut double_slurpy = false;
    let mut onearg = false;
    // Single-argument rule slurpy marker (+@a, +%h, +$x, +&f, or sigilless +foo).
    // Treat as regular slurpy for now.
    if rest.starts_with('+')
        && rest.len() > 1
        && (rest.as_bytes()[1] == b'@'
            || rest.as_bytes()[1] == b'%'
            || rest.as_bytes()[1] == b'$'
            || rest.as_bytes()[1] == b'&')
    {
        onearg = true;
        rest = &rest[1..];
    }
    // Sigilless single-argument rule slurpy: +foo
    if rest.starts_with('+') && rest.len() > 1 && rest.as_bytes()[1].is_ascii_alphabetic() {
        let r = &rest[1..];
        if let Ok((r, name)) = ident(r) {
            let (r, _) = ws(r)?;
            // Handle traits (is copy, is rw, etc.)
            let mut param_traits = Vec::new();
            let (mut r, _) = ws(r)?;
            while let Some(rt) = keyword("is", r) {
                let (rt, _) = ws1(rt)?;
                let (rt, trait_name) = ident(rt)?;
                validate_param_trait(&trait_name, &param_traits, rt)?;
                param_traits.push(trait_name);
                let (rt, _) = ws(rt)?;
                r = rt;
            }
            // Default value
            let (r, default) = if r.starts_with('=') && !r.starts_with("==") {
                let r = &r[1..];
                let (r, _) = ws(r)?;
                let (r, expr) = parse_param_default_expr(r)?;
                (r, Some(expr))
            } else {
                (r, None)
            };
            let mut p = make_param(name);
            p.slurpy = true;
            p.onearg = true;
            p.sigilless = true;
            p.named = named;
            p.default = default;
            p.type_constraint = type_constraint;
            p.traits = param_traits;
            return Ok((r, p));
        }
    }
    if rest.starts_with('*')
        && rest.len() > 1
        && (rest.as_bytes()[1] == b'@'
            || rest.as_bytes()[1] == b'%'
            || rest.as_bytes()[1] == b'$'
            || rest.as_bytes()[1] == b'&')
    {
        slurpy = true;
        slurpy_sigil = Some(rest.as_bytes()[1] as char);
        rest = &rest[1..];
    }

    // Slurpy unpack: *[$a, $b, ...] — gather remaining args then unpack
    if rest.starts_with("*[") {
        let r = &rest[1..]; // skip '*', keep '['
        let (r, _) = parse_char(r, '[')?;
        let (r, _) = ws(r)?;
        let (r, sub_params) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ']')?;
        let mut p = make_param("@".to_string());
        p.slurpy = true;
        p.sub_signature = Some(sub_params);
        return Ok((r, p));
    }

    // Handle ::?CLASS and ::?ROLE pseudo-types in signatures (must come before named check)
    if rest.starts_with("::?CLASS") || rest.starts_with("::?ROLE") {
        let end = if rest.starts_with("::?CLASS") { 8 } else { 7 };
        let pseudo_type = &rest[..end];
        let r = &rest[end..];
        let (r, tc) = if r.starts_with(":D") || r.starts_with(":U") || r.starts_with(":_") {
            let smiley = &r[..2];
            (&r[2..], format!("{}{}", pseudo_type, smiley))
        } else {
            (r, pseudo_type.to_string())
        };
        if r.starts_with(':')
            && !r.starts_with(":D")
            && !r.starts_with(":U")
            && !r.starts_with(":_")
        {
            // The `:` here is the invocant marker. Return the invocant param
            // with rest positioned AT the `:` so parse_param_list_inner can
            // detect it and mark the param as invocant before parsing more params.
            let mut p = make_param("self".to_string());
            p.type_constraint = Some(tc);
            p.is_invocant = true;
            p.traits.push("invocant".to_string());
            return Ok((r, p));
        }
        let (r, _) = ws(r)?;
        if r.starts_with('$') || r.starts_with('@') || r.starts_with('%') {
            type_constraint = Some(tc);
            rest = r;
        } else {
            let mut p = make_param("self".to_string());
            p.type_constraint = Some(tc);
            p.is_invocant = true;
            p.traits.push("invocant".to_string());
            return Ok((r, p));
        }
    }

    // Named param marker: :$name (but not :: which is a parametric type prefix)
    if rest.starts_with(':') && !rest.starts_with("::") {
        named = true;
        rest = &rest[1..];
    }

    // Type-capture parameter: ::T $x  or bare ::T
    if let Some(after_capture) = rest.strip_prefix("::")
        && let Ok((r, capture_name)) = ident(after_capture)
    {
        type_constraint = Some(format!("::{}", capture_name));
        let (r, _) = ws(r)?;
        rest = r;
        if rest.starts_with('=') && !rest.starts_with("==") {
            let r = &rest[1..];
            let (r, _) = ws(r)?;
            let (r, default) = parse_param_default_expr(r)?;
            let mut p = make_param(format!("__type_capture__{}", capture_name));
            p.type_constraint = type_constraint;
            p.named = named;
            p.slurpy = slurpy;
            p.default = Some(default);
            return Ok((r, p));
        }
        if rest.starts_with(')')
            || rest.starts_with(']')
            || rest.starts_with(',')
            || rest.starts_with(';')
        {
            let mut p = make_param(format!("__type_capture__{}", capture_name));
            p.type_constraint = type_constraint;
            p.named = named;
            p.slurpy = slurpy;
            return Ok((rest, p));
        }
        if rest.starts_with(':') && !rest.starts_with("::") {
            // Check if this `:` is an invocant marker (followed by whitespace/sigil/paren)
            // rather than a named-parameter marker.
            // In `(::T: $ where Foo[T])`, the `:` after `::T` is the invocant separator.
            let after_colon = &rest[1..];
            let is_invocant_marker = after_colon.is_empty()
                || after_colon.starts_with(' ')
                || after_colon.starts_with('\t')
                || after_colon.starts_with('\n')
                || after_colon.starts_with('\r')
                || after_colon.starts_with(')')
                || after_colon.starts_with(',');
            if is_invocant_marker {
                // Return the bare ::T as a standalone param; leave `:` for the
                // outer param-list loop to handle as the invocant marker.
                let mut p = make_param(format!("__type_capture__{}", capture_name));
                p.type_constraint = type_constraint;
                p.named = named;
                p.slurpy = slurpy;
                return Ok((rest, p));
            }
            named = true;
            rest = &rest[1..];
        }
    }

    // Type constraint (may be qualified: IO::Path)
    // Skip type constraint parsing for named params with lowercase identifiers followed by '('
    // Parametric type constraint: ::T
    if let Some(after_colon) = rest.strip_prefix("::")
        && let Ok((r, tc_name)) = ident(after_colon)
    {
        let tc = format!("::{}", tc_name);
        let (r2, _) = ws(r)?;
        if r2.starts_with('$') || r2.starts_with('@') || r2.starts_with('%') {
            type_constraint = Some(tc);
            rest = r2;
        }
    }

    // — those are named aliases like :x($r), not type constraints.
    let skip_type_for_named_alias = named
        && rest
            .as_bytes()
            .first()
            .is_some_and(|b| b.is_ascii_lowercase())
        && rest.contains('(');
    if type_constraint.is_none()
        && !skip_type_for_named_alias
        && let Some((r, tc)) = parse_type_constraint_expr(rest)
    {
        // Validate type smiley before proceeding
        check_invalid_type_smiley(&Some(tc.clone()))?;
        let (r2, _) = ws(r)?;

        // Check for coercion type: Int() or Int(Rat)
        // Also handles sub-signature: Type (inner-params)
        if let Some(after_open) = r2.strip_prefix('(') {
            let (after_ws, _) = ws(after_open)?;
            // Coercion type: Int() — empty parens
            if let Some(r3) = after_ws.strip_prefix(')') {
                let (r3, _) = ws(r3)?;
                // This is a coercion type like Int()
                type_constraint = Some(format!("{}()", tc));
                rest = r3;
                // Re-check named after type
                if rest.starts_with(':') {
                    named = true;
                    rest = &rest[1..];
                }
            } else if let Some((inner_r, source_type)) = parse_type_constraint_expr(after_ws) {
                let (inner_r, _) = ws(inner_r)?;
                if let Some(r3) = inner_r.strip_prefix(')') {
                    let (r3, _) = ws(r3)?;
                    // This is a coercion type like Int(Rat)
                    type_constraint = Some(format!("{}({})", tc, source_type));
                    rest = r3;
                    if rest.starts_with(':') {
                        named = true;
                        rest = &rest[1..];
                    }
                } else {
                    // Fall through to sub-signature parsing
                    let (r3, _) = parse_char(r2, '(')?;
                    let (r3, _) = ws(r3)?;
                    let (r3, sub_params) = parse_param_list(r3)?;
                    let (r3, _) = ws(r3)?;
                    let (r3, _) = parse_char(r3, ')')?;
                    let (r3, _) = ws(r3)?;
                    let mut p = make_param("__subsig__".to_string());
                    p.type_constraint = Some(tc);
                    p.sub_signature = Some(sub_params);
                    p.named = named;
                    p.slurpy = slurpy;
                    return Ok((r3, p));
                }
            } else {
                // Parse sub-signature (destructuring)
                let (r3, _) = parse_char(r2, '(')?;
                let (r3, _) = ws(r3)?;
                let (r3, sub_params) = parse_param_list(r3)?;
                let (r3, _) = ws(r3)?;
                let (r3, _) = parse_char(r3, ')')?;
                let (r3, _) = ws(r3)?;
                let mut p = make_param("__subsig__".to_string());
                p.type_constraint = Some(tc);
                p.sub_signature = Some(sub_params);
                p.named = named;
                p.slurpy = slurpy;
                // Handle optional (?) / required (!) suffix after sub-signature
                let (rest, required, opt_marker) = parse_required_suffix(r3);
                // Skip whitespace
                let (rest, _) = ws(rest)?;
                // Handle is copy, is rw, is readonly, is raw traits
                let mut param_traits = Vec::new();
                let (mut rest, _) = ws(rest)?;
                while let Some(r) = keyword("is", rest) {
                    let (r, _) = ws1(r)?;
                    let (r, trait_name) = ident(r)?;
                    validate_param_trait(&trait_name, &param_traits, r)?;
                    param_traits.push(trait_name);
                    let (r, _) = ws(r)?;
                    rest = r;
                }
                p.traits = param_traits;
                p.required = required;
                p.optional_marker = opt_marker;
                // Handle where constraint
                let (rest, where_constraint) = if let Some(r) = keyword("where", rest) {
                    let (r, _) = ws1(r)?;
                    let (r, constraint) = parse_where_constraint_expr(r)?;
                    (r, Some(Box::new(constraint)))
                } else {
                    (rest, None)
                };
                p.where_constraint = where_constraint;
                return Ok((rest, p));
            }
        }

        if r2.starts_with('$')
            || r2.starts_with('@')
            || r2.starts_with('%')
            || r2.starts_with('&')
            || (r2.starts_with('*')
                && r2.len() > 1
                && (r2.as_bytes()[1] == b'$'
                    || r2.as_bytes()[1] == b'@'
                    || r2.as_bytes()[1] == b'%'
                    || r2.as_bytes()[1] == b'&'))
            || r2.starts_with(':')
            || r2.starts_with('|')
            || r2.starts_with('\\')
        {
            type_constraint = Some(tc);
            rest = r2;
            // Re-check named after type
            if rest.starts_with(':') {
                named = true;
                rest = &rest[1..];
            }
        } else if r2.starts_with(')')
            || r2.starts_with(',')
            || r2.starts_with(']')
            || r2.starts_with("-->")
        {
            // True/False in signature position are literal Bool values, not type names.
            // In Raku, `sub f(True)` means "type Bool, smartmatched against True".
            // Smartmatch against True always succeeds, so the literal check is just
            // a Bool type constraint. Smartmatch against False always fails, so
            // sub f(False) would reject all calls (equivalent to an impossible constraint).
            if tc == "True" || tc == "False" {
                let mut p = make_param("__type_only__".to_string());
                p.type_constraint = Some("Bool".to_string());
                p.named = named;
                p.slurpy = slurpy;
                return Ok((r2, p));
            }
            // Bare identifier as type-only parameter (e.g., enum values in multi dispatch)
            // multi infix:<->(e1, e2) { ... }
            let mut p = make_param("__type_only__".to_string());
            p.type_constraint = Some(tc);
            p.named = named;
            p.slurpy = slurpy;
            return Ok((r2, p));
        } else {
            // Check for multiple prefix type constraints (e.g. `Int Str $x`)
            if let Some((r3, _second_tc)) = parse_type_constraint_expr(r2) {
                let (r4, _) = ws(r3).unwrap_or((r3, ()));
                if r4.starts_with('$')
                    || r4.starts_with('@')
                    || r4.starts_with('%')
                    || r4.starts_with('&')
                    || r4.starts_with('\\')
                {
                    return Err(PError::raw(
                        "FATAL:X::Parameter::MultipleTypeConstraints: Multiple prefix type constraints are not supported. You may use a subset type instead."
                            .to_string(),
                        Some(r2.len()),
                    ));
                }
            }
        }
    }

    if let Some(tc) = type_constraint.take() {
        let (r, tc) =
            parse_of_type_constraint_chain(rest, tc).ok_or_else(|| PError::expected("type"))?;
        type_constraint = Some(tc);
        rest = r;
    }

    // Typed capture parameter, e.g. `Capture |cap`
    if type_constraint.is_some() && rest.starts_with('|') {
        let (r, mut p) = parse_single_param(rest)?;
        if p.type_constraint.is_none() {
            p.type_constraint = type_constraint.clone();
        }
        p.named = named;
        if slurpy {
            p.slurpy = true;
        }
        return Ok((r, p));
    }

    // Double slurpy marker may appear after a type constraint:
    // e.g. `Array **@AoA`
    if rest.starts_with("**")
        && rest.len() > 2
        && (rest.as_bytes()[2] == b'@' || rest.as_bytes()[2] == b'%')
    {
        slurpy = true;
        double_slurpy = true;
        slurpy_sigil = Some(rest.as_bytes()[2] as char);
        rest = &rest[2..];
    }
    // Slurpy marker may appear after a type constraint:
    // e.g. `Code *$block`, `Int *@xs`, `Hash *%h`.
    else if rest.starts_with('*')
        && rest.len() > 1
        && (rest.as_bytes()[1] == b'@'
            || rest.as_bytes()[1] == b'%'
            || rest.as_bytes()[1] == b'$'
            || rest.as_bytes()[1] == b'&')
    {
        slurpy = true;
        slurpy_sigil = Some(rest.as_bytes()[1] as char);
        rest = &rest[1..];
    }

    // Handle literal value parameters: multi sub foo(0), foo(-١), foo(Inf), foo("x")
    if let Ok((lit_rest, lit_expr)) = expression(rest)
        && let Some(v) = literal_value_from_expr(&lit_expr)
        && let Ok((after_lit, _)) = ws(lit_rest)
        && (after_lit.starts_with(')')
            || after_lit.starts_with(',')
            || after_lit.starts_with(';')
            || after_lit.starts_with(']')
            || after_lit.starts_with("-->"))
    {
        let mut p = make_param("__literal__".to_string());
        // If no explicit type constraint, infer from the literal value type
        p.type_constraint = type_constraint.or_else(|| {
            Some(
                match &v {
                    crate::value::Value::Int(_) | crate::value::Value::BigInt(_) => "Int",
                    crate::value::Value::Num(_) => "Num",
                    crate::value::Value::Rat(..)
                    | crate::value::Value::FatRat(..)
                    | crate::value::Value::BigRat(..) => "Rat",
                    crate::value::Value::Str(_) => "Str",
                    crate::value::Value::Bool(_) => "Bool",
                    crate::value::Value::Complex(..) => "Complex",
                    _ => return None,
                }
                .to_string(),
            )
        });
        p.literal_value = Some(v);
        return Ok((after_lit, p));
    }

    // Sigilless parameter: \name or anonymous \
    if let Some(r) = rest.strip_prefix('\\') {
        let (r, name) = match ident(r) {
            Ok((r, name)) => (r, name),
            Err(_) => (r, String::new()),
        };
        let (r, _) = ws(r)?;
        // `is copy`, `is rw`, `is readonly`, `is raw` traits
        let (mut r, _) = ws(r)?;
        let mut sigilless_traits = Vec::new();
        while let Some(rt) = keyword("is", r) {
            let (rt, _) = ws1(rt)?;
            let (rt, trait_name) = ident(rt)?;
            validate_param_trait(&trait_name, &sigilless_traits, rt)?;
            sigilless_traits.push(trait_name);
            let (rt, _) = ws(rt)?;
            r = rt;
        }
        // Default value
        let (r, default) = if r.starts_with('=') && !r.starts_with("==") {
            let r = &r[1..];
            let (r, _) = ws(r)?;
            let (r, expr) = parse_param_default_expr(r)?;
            (r, Some(expr))
        } else {
            (r, None)
        };
        let mut p = make_param(name);
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.onearg = onearg;
        p.sigilless = true;
        p.default = default;
        p.type_constraint = type_constraint;
        p.traits = sigilless_traits;
        return Ok((r, p));
    }

    // Sub-signature without type: just (inner-params)
    if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, sub_params) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let (r, _) = ws(r)?;
        let (r, required, opt_marker) = parse_required_suffix(r);
        let mut p = make_param("__subsig__".to_string());
        p.sub_signature = Some(sub_params);
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.onearg = onearg;
        p.type_constraint = type_constraint;
        p.required = required;
        p.optional_marker = opt_marker;
        return Ok((r, p));
    }

    // Named parameter with alias: :key($var) or :value(&callback)
    if named && let Ok((r, alias_name)) = ident(rest) {
        let (r2, _) = ws(r)?;
        if r2.starts_with('(') {
            let (r3, _) = parse_char(r2, '(')?;
            let (r3, _) = ws(r3)?;
            // Inside could be a sub-signature or a single variable
            // Try to parse as a sub-signature first (handles nested params)
            let (r3, sub_params) = parse_param_list(r3)?;
            let (r3, _) = ws(r3)?;
            let (r3, _) = parse_char(r3, ')')?;
            // If it's a single simple variable param, use its name; otherwise sub-signature
            if sub_params.len() == 1
                && sub_params[0].sub_signature.is_none()
                && !sub_params[0].name.starts_with("__")
            {
                let mut p = make_param(alias_name.clone());
                p.named = true;
                p.slurpy = slurpy;
                p.double_slurpy = double_slurpy;
                p.onearg = onearg;
                p.type_constraint = type_constraint;
                p.sub_signature = Some(sub_params.clone());
                // Check for a sub-signature after the alias: :x($r) (Str $g, Any $i)
                let (r3, _) = ws(r3)?;
                if r3.starts_with('(') {
                    let (r4, _) = parse_char(r3, '(')?;
                    let (r4, _) = ws(r4)?;
                    let (r4, outer_sub) = parse_param_list(r4)?;
                    let (r4, _) = ws(r4)?;
                    let (r3_new, _) = parse_char(r4, ')')?;
                    p.outer_sub_signature = Some(outer_sub);
                    let (rest, alias_required, alias_opt_marker) = parse_required_suffix(r3_new);
                    p.required = alias_required;
                    p.optional_marker = alias_opt_marker;
                    let (rest, _) = ws(rest)?;
                    let mut param_traits = Vec::new();
                    let (mut rest, _) = ws(rest)?;
                    while let Some(r) = keyword("is", rest) {
                        let (r, _) = ws1(r)?;
                        let (r, trait_name) = ident(r)?;
                        validate_param_trait(&trait_name, &param_traits, r)?;
                        param_traits.push(trait_name);
                        let (r, _) = ws(r)?;
                        rest = r;
                    }
                    p.traits = param_traits;
                    let (rest, where_constraint) = if let Some(r) = keyword("where", rest) {
                        let (r, _) = ws1(r)?;
                        let (r, constraint) = parse_where_constraint_expr(r)?;
                        (r, Some(Box::new(constraint)))
                    } else {
                        (rest, None)
                    };
                    p.where_constraint = where_constraint;
                    let (rest_ws, _) = ws(rest)?;
                    let (rest, default) = if rest_ws.starts_with('=') && !rest_ws.starts_with("==")
                    {
                        let rest = &rest_ws[1..];
                        let (rest, _) = ws(rest)?;
                        let (rest, expr) = expression(rest)?;
                        (rest, Some(expr))
                    } else {
                        (rest_ws, None)
                    };
                    p.default = default;
                    return Ok((rest, p));
                }
                // Handle optional (?) / required (!) suffix after alias
                let (rest, alias_required, alias_opt_marker) = parse_required_suffix(r3);
                p.required = alias_required;
                p.optional_marker = alias_opt_marker;
                // Skip whitespace
                let (rest, _) = ws(rest)?;
                // Handle is copy, is rw, is readonly, is raw traits
                let mut param_traits = Vec::new();
                let (mut rest, _) = ws(rest)?;
                while let Some(r) = keyword("is", rest) {
                    let (r, _) = ws1(r)?;
                    let (r, trait_name) = ident(r)?;
                    validate_param_trait(&trait_name, &param_traits, r)?;
                    param_traits.push(trait_name);
                    let (r, _) = ws(r)?;
                    rest = r;
                }
                p.traits = param_traits;
                // Handle where constraint
                let (rest, where_constraint) = if let Some(r) = keyword("where", rest) {
                    let (r, _) = ws1(r)?;
                    let (r, constraint) = parse_where_constraint_expr(r)?;
                    (r, Some(Box::new(constraint)))
                } else {
                    (rest, None)
                };
                p.where_constraint = where_constraint;
                let (rest_ws, _) = ws(rest)?;
                let (rest, default) = if rest_ws.starts_with('=') && !rest_ws.starts_with("==") {
                    let rest = &rest_ws[1..];
                    let (rest, _) = ws(rest)?;
                    let (rest, expr) = expression(rest)?;
                    (rest, Some(expr))
                } else {
                    (rest_ws, None)
                };
                p.default = default;
                return Ok((rest, p));
            }
            // Multiple params or complex: treat as sub-signature
            let mut p = make_param(alias_name);
            p.named = true;
            p.slurpy = slurpy;
            p.double_slurpy = double_slurpy;
            p.onearg = onearg;
            p.type_constraint = type_constraint;
            p.sub_signature = Some(sub_params);
            // Handle optional (?) / required (!) suffix after sub-signature
            let (rest, required, opt_marker) = parse_required_suffix(r3);
            // Skip whitespace
            let (rest, _) = ws(rest)?;
            // Handle is copy, is rw, is readonly, is raw traits
            let mut param_traits = Vec::new();
            let (mut rest, _) = ws(rest)?;
            while let Some(r) = keyword("is", rest) {
                let (r, _) = ws1(r)?;
                let (r, trait_name) = ident(r)?;
                validate_param_trait(&trait_name, &param_traits, r)?;
                param_traits.push(trait_name);
                let (r, _) = ws(r)?;
                rest = r;
            }
            p.traits = param_traits;
            p.required = required;
            p.optional_marker = opt_marker;
            // Handle where constraint
            let (rest, where_constraint) = if let Some(r) = keyword("where", rest) {
                let (r, _) = ws1(r)?;
                let (r, constraint) = parse_where_constraint_expr(r)?;
                (r, Some(Box::new(constraint)))
            } else {
                (rest, None)
            };
            p.where_constraint = where_constraint;
            let (rest_ws, _) = ws(rest)?;
            let (rest, default) = if rest_ws.starts_with('=') && !rest_ws.starts_with("==") {
                let rest = &rest_ws[1..];
                let (rest, _) = ws(rest)?;
                let (rest, expr) = expression(rest)?;
                (rest, Some(expr))
            } else {
                (rest_ws, None)
            };
            p.default = default;
            return Ok((rest, p));
        }
    }

    // Anonymous optional sigil parameters: $?, @?, %?
    for (prefix, anon_name) in [
        ("$?", "__ANON_OPTIONAL__"),
        ("@?", "@__ANON_ARRAY__"),
        ("%?", "%__ANON_HASH__"),
    ] {
        if let Some(after_q) = rest.strip_prefix(prefix)
            && (after_q.is_empty()
                || after_q.starts_with(',')
                || after_q.starts_with(')')
                || after_q.starts_with(' ')
                || after_q.starts_with('\t')
                || after_q.starts_with('\n'))
        {
            let mut p = make_param(anon_name.to_string());
            p.named = named;
            p.slurpy = slurpy;
            p.double_slurpy = double_slurpy;
            p.onearg = onearg;
            p.type_constraint = type_constraint;
            p.optional_marker = true;
            return Ok((after_q, p));
        }
    }

    // Anonymous callable parameter with code signature: &:(Str --> Bool)
    if rest.starts_with("&:(") {
        let r = &rest[1..]; // skip '&'
        let (r, _) = parse_char(r, ':')?;
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws(r)?;
        let (r, (sig_params, sig_ret)) = parse_param_list_with_return(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let (r, required, opt_marker) = parse_required_suffix(r);
        let (r, _) = ws(r)?;

        let mut p = make_param("&".to_string());
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.onearg = onearg;
        p.type_constraint = type_constraint;
        p.code_signature = Some((sig_params, sig_ret));
        p.required = required;
        p.optional_marker = opt_marker;

        let mut param_traits = Vec::new();
        let (mut r, _) = ws(r)?;
        while let Some(r2) = keyword("is", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, trait_name) = ident(r2)?;
            validate_param_trait(&trait_name, &param_traits, r2)?;
            param_traits.push(trait_name);
            let (r2, _) = ws(r2)?;
            r = r2;
        }
        p.traits = param_traits;

        let (r, where_constraint) = if let Some(r2) = keyword("where", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, constraint) = parse_where_constraint_expr(r2)?;
            (r2, Some(Box::new(constraint)))
        } else {
            (r, None)
        };
        p.where_constraint = where_constraint;
        return Ok((r, p));
    }

    // Bare & (anonymous callable parameter)
    if rest.starts_with('&')
        && (rest.len() == 1
            || rest.as_bytes()[1] == b','
            || rest.as_bytes()[1] == b')'
            || rest.as_bytes()[1] == b' '
            || rest.as_bytes()[1] == b'\t'
            || rest.as_bytes()[1] == b'\n'
            || rest.as_bytes()[1] == b'?'
            || rest.as_bytes()[1] == b'!')
    {
        let rest = &rest[1..];
        let (rest, required, opt_marker) = parse_required_suffix(rest);
        let mut p = make_param("&".to_string());
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.onearg = onearg;
        p.type_constraint = type_constraint;
        p.required = required;
        p.optional_marker = opt_marker;
        return Ok((rest, p));
    }

    // Capture the original sigil before var_name strips it
    let original_sigil = rest.as_bytes().first().copied().unwrap_or(b'$');
    let param_sigil = rest.as_bytes().first().copied();
    // Handle $.x / @.x / %.x (public accessor twigil) in parameter context.
    // The '.' twigil is only valid in signatures, not in general expressions,
    // so we handle it here rather than in var_name.
    let (rest, name) =
        if (rest.starts_with("$.") || rest.starts_with("@.") || rest.starts_with("%."))
            && rest.len() > 2
            && rest.as_bytes()[2].is_ascii_alphabetic()
        {
            let sigil_prefix = if rest.starts_with('$') {
                ""
            } else if rest.starts_with('@') {
                "@"
            } else {
                "%"
            };
            let after_dot = &rest[2..]; // skip sigil + "."
            let end = after_dot
                .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                .unwrap_or(after_dot.len());
            let ident = &after_dot[..end];
            (&after_dot[end..], format!("{}.{}", sigil_prefix, ident))
        } else {
            var_name(rest)?
        };

    // Shape constraint for array parameters: @a[3], @a[4,4], @a[*], @a[$n]
    let mut shape_constraints = None;
    let rest = if original_sigil == b'@' && rest.starts_with('[') {
        let (r, dims) = parse_array_shape_suffix(rest)?;
        shape_constraints = Some(dims);
        r
    } else {
        rest
    };

    // Code signature constraint: &foo:(Str --> Bool)
    let mut code_sig = None;
    let rest = if original_sigil == b'&' && rest.starts_with(":(") {
        let r = &rest[1..]; // skip ':'
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws(r)?;
        let (r, (sig_params, sig_ret)) = parse_param_list_with_return(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        code_sig = Some((sig_params, sig_ret));
        r
    } else {
        rest
    };

    // Optional (?) / required (!) suffix
    let (rest, required, opt_marker) = parse_required_suffix(rest);
    let (rest, _) = ws(rest)?;

    // Sub-signature after variable: $x ($a, $b) or $ ($a, $b)
    if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, sub_params) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let (r, _) = ws(r)?;
        // Handle optional (?) / required (!) suffix after sub-signature
        let (r, post_required, post_opt_marker) = parse_required_suffix(r);
        let (r, _) = ws(r)?;
        let mut param_traits = Vec::new();
        let (mut r, _) = ws(r)?;
        while let Some(r2) = keyword("is", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, trait_name) = ident(r2)?;
            validate_param_trait(&trait_name, &param_traits, r2)?;
            param_traits.push(trait_name);
            let (r2, _) = ws(r2)?;
            r = r2;
        }
        let param_name = if slurpy {
            match slurpy_sigil {
                Some('%') => format!("%{}", name),
                Some('@') => format!("@{}", name),
                _ => name,
            }
        } else if named && original_sigil == b'&' {
            name
        } else {
            match original_sigil {
                b'@' => format!("@{}", name),
                b'%' => format!("%{}", name),
                b'&' => format!("&{}", name),
                _ => name,
            }
        };
        let mut p = make_param(param_name);
        p.required = required || post_required;
        p.optional_marker = opt_marker || post_opt_marker;
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.onearg = onearg;
        p.type_constraint = type_constraint;
        p.sub_signature = Some(sub_params);
        p.traits = param_traits;
        return Ok((r, p));
    }

    // Sub-signature with brackets after variable: @a [$x, $y] or $a [$x, $y]
    if rest.starts_with('[') {
        let (r, _) = parse_char(rest, '[')?;
        let (r, _) = ws(r)?;
        let (r, sub_params) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ']')?;
        let (r, _) = ws(r)?;
        // Handle optional (?) / required (!) suffix after sub-signature
        let (r, post_required, post_opt_marker) = parse_required_suffix(r);
        let (r, _) = ws(r)?;
        let mut param_traits = Vec::new();
        let (mut r, _) = ws(r)?;
        while let Some(r2) = keyword("is", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, trait_name) = ident(r2)?;
            validate_param_trait(&trait_name, &param_traits, r2)?;
            param_traits.push(trait_name);
            let (r2, _) = ws(r2)?;
            r = r2;
        }
        let param_name = if slurpy {
            match slurpy_sigil {
                Some('%') => format!("%{}", name),
                Some('@') => format!("@{}", name),
                _ => name,
            }
        } else if named && original_sigil == b'&' {
            name
        } else {
            match original_sigil {
                b'@' => format!("@{}", name),
                b'%' => format!("%{}", name),
                b'&' => format!("&{}", name),
                _ => name,
            }
        };
        let mut p = make_param(param_name);
        p.required = required || post_required;
        p.optional_marker = opt_marker || post_opt_marker;
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.onearg = onearg;
        p.type_constraint = type_constraint;
        p.sub_signature = Some(sub_params);
        p.traits = param_traits;
        return Ok((r, p));
    }

    // Default value
    let (rest, mut default) = if rest.starts_with('=') && !rest.starts_with("==") {
        let rest = &rest[1..];
        let (rest, _) = ws(rest)?;
        let (rest, expr) = expression(rest)?;
        (rest, Some(expr))
    } else {
        (rest, None)
    };

    // `is copy`, `is rw`, `is readonly`, `is raw` traits (may have multiple)
    let (mut rest, _) = ws(rest)?;
    let mut param_traits = Vec::new();
    while let Some(r) = keyword("is", rest) {
        let (r, _) = ws1(r)?;
        let (r, trait_name) = ident(r)?;
        validate_param_trait(&trait_name, &param_traits, r)?;
        param_traits.push(trait_name);
        let (r, _) = ws(r)?;
        rest = r;
    }

    // `where` constraint
    let (rest, where_constraint) = if let Some(r) = keyword("where", rest) {
        let (r, _) = ws1(r)?;
        let (r, constraint) = parse_where_constraint_expr(r)?;
        (r, Some(Box::new(constraint)))
    } else {
        (rest, None)
    };
    let (rest_ws, _) = ws(rest)?;
    let (rest, late_default) =
        if default.is_none() && rest_ws.starts_with('=') && !rest_ws.starts_with("==") {
            let rest = &rest_ws[1..];
            let (rest, _) = ws(rest)?;
            let (rest, expr) = expression(rest)?;
            (rest, Some(expr))
        } else {
            (rest_ws, None)
        };
    if late_default.is_some() {
        default = late_default;
    }

    // Prefix the name with the sigil so runtime can distinguish types.
    // For slurpy params, use the slurpy_sigil; for non-slurpy, use original_sigil.
    let param_name = if slurpy {
        match slurpy_sigil {
            Some('%') => format!("%{}", name),
            Some('@') => format!("@{}", name),
            _ => name,
        }
    } else if named && original_sigil == b'&' {
        name
    } else if param_sigil == Some(b'@') {
        format!("@{}", name)
    } else if param_sigil == Some(b'%') {
        format!("%{}", name)
    } else {
        match original_sigil {
            b'@' => format!("@{}", name),
            b'%' => format!("%{}", name),
            b'&' => format!("&{}", name),
            _ => name,
        }
    };
    let mut p = make_param(param_name);
    p.default = default;
    p.required = required;
    p.optional_marker = opt_marker;
    p.named = named;
    p.slurpy = slurpy;
    p.double_slurpy = double_slurpy;
    p.onearg = onearg;
    p.type_constraint = type_constraint;
    p.where_constraint = where_constraint;
    p.traits = param_traits;
    p.code_signature = code_sig;
    p.shape_constraints = shape_constraints;
    Ok((rest, p))
}

/// Parse `method` declaration.
pub(super) fn method_decl(input: &str) -> PResult<'_, Stmt> {
    let (rest, multi) = if let Some(r) = keyword("multi", input) {
        let (r, _) = ws1(r)?;
        let r = keyword("method", r).ok_or_else(|| PError::expected("method declaration"))?;
        let (r, _) = ws1(r)?;
        (r, true)
    } else {
        let r = keyword("method", input).ok_or_else(|| PError::expected("method declaration"))?;
        let (r, _) = ws1(r)?;
        (r, false)
    };
    method_decl_body(rest, multi, false)
}

/// Parse `submethod` declaration (treated like method, not inherited by subclasses).
pub(super) fn submethod_decl(input: &str) -> PResult<'_, Stmt> {
    let r = keyword("submethod", input).ok_or_else(|| PError::expected("submethod declaration"))?;
    let (r, _) = ws1(r)?;
    method_decl_body_with_my(r, false, false, true, true)
}

pub(super) fn method_decl_body(input: &str, multi: bool, is_our: bool) -> PResult<'_, Stmt> {
    method_decl_body_with_my(input, multi, is_our, false, false)
}

pub(super) fn method_decl_body_my(input: &str, multi: bool, is_our: bool) -> PResult<'_, Stmt> {
    method_decl_body_with_my(input, multi, is_our, true, false)
}

fn method_decl_body_with_my(
    input: &str,
    multi: bool,
    is_our: bool,
    is_my: bool,
    is_submethod: bool,
) -> PResult<'_, Stmt> {
    let (rest, is_private) = if let Some(rest) = input.strip_prefix('!') {
        (rest, true)
    } else {
        (input, false)
    };
    let (rest, name, name_expr) = if rest.starts_with("::") {
        let (rest, (name, expr)) = parse_indirect_decl_name(rest)?;
        (rest, name, Some(expr))
    } else {
        let (rest, name) = parse_sub_name(rest)?;
        (rest, name, None)
    };
    let (rest, _) = ws(rest)?;

    let (rest, (params, param_defs, param_return_type)) = if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, (pd, rt)) = parse_param_list_with_return(r)?;
        validate_signature_params(&pd)?;
        // In BUILD/TWEAK submethods, reject $.x (public accessor twigil) parameters
        // as X::Syntax::VirtualCall — the object is not fully constructed yet.
        if name == "BUILD" || name == "TWEAK" {
            for p in &pd {
                if p.name.starts_with('.') {
                    let call = format!("$.{}", &p.name[1..]);
                    let msg = format!(
                        "X::Syntax::VirtualCall: Virtual method call {} may not be used on partially constructed object",
                        call
                    );
                    let mut attrs = HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.clone()));
                    attrs.insert("call".to_string(), Value::str(call));
                    let ex = Value::make_instance(Symbol::intern("X::Syntax::VirtualCall"), attrs);
                    return Err(PError::fatal_with_exception(msg, Box::new(ex)));
                }
            }
        }
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let names: Vec<String> = pd.iter().map(|p| p.name.clone()).collect();
        (r, (names, pd, rt))
    } else {
        (rest, (Vec::new(), Vec::new(), None))
    };

    let (rest, traits) = parse_sub_traits(rest)?;
    let return_type = traits.return_type.or(param_return_type);
    let (rest, body) = block(rest)?;
    // When no explicit signature is given, collect placeholder variables
    // (@_, $^a, $^b, etc.) from the body as implicit parameters.
    let (params, param_defs) = if params.is_empty() && param_defs.is_empty() {
        let placeholders = collect_placeholders_shallow(&body);
        if placeholders.is_empty() {
            (params, param_defs)
        } else {
            (placeholders, Vec::new())
        }
    } else {
        (params, param_defs)
    };
    Ok((
        rest,
        Stmt::MethodDecl {
            name: Symbol::intern(&name),
            name_expr,
            params,
            param_defs,
            body,
            multi,
            is_rw: traits.is_rw,
            is_private,
            is_our,
            is_my,
            is_submethod,
            our_variable_form: false,
            return_type,
            is_default_candidate: traits.custom_traits.contains(&"default".to_string()),
            deprecated_message: traits.custom_traits.iter().find_map(|t| {
                if t == "DEPRECATED" {
                    Some(String::new())
                } else {
                    t.strip_prefix("DEPRECATED:").map(|msg| msg.to_string())
                }
            }),
            handles: traits.handles.clone(),
        },
    ))
}
