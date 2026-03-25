use super::super::super::expr::expression;
use super::super::super::helpers::ws;
use super::super::super::parse_result::{PError, PResult, take_while1};
use super::super::keyword;
use super::constant_subset::constant_decl;
use super::helpers::{parse_sigilless_decl_name, register_term_symbol_from_decl_name};
use super::{parse_decl_type_constraint, parse_statement_modifier, typed_default_expr};
use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

use super::parse_assign_expr_or_comma;

/// Parse a sigilless variable declaration: `my \name = expr`
pub(super) fn parse_sigilless_decl(
    input: &str,
    is_state: bool,
    is_our: bool,
    type_constraint: Option<String>,
    apply_modifier: bool,
) -> PResult<'_, Stmt> {
    let (r, name) = parse_sigilless_decl_name(input)?;
    register_term_symbol_from_decl_name(&name);
    let (r, _) = ws(r)?;
    if let Some(r) = r.strip_prefix("::=").or_else(|| r.strip_prefix(":=")) {
        let (r, _) = ws(r)?;
        let (r, expr) = parse_assign_expr_or_comma(r)?;
        let decl = Stmt::VarDecl {
            name: name.clone(),
            expr,
            type_constraint: type_constraint.clone(),
            is_state,
            is_our,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
            where_constraint: None,
        };
        // Sigilless variables without type constraint are always readonly.
        // With a type constraint (e.g. `my Mu \a := $a`), the variable
        // binds to the container, preserving mutability.
        let stmt = if type_constraint.is_none() {
            Stmt::SyntheticBlock(vec![decl, Stmt::MarkSigillessReadonly(name)])
        } else {
            decl
        };
        if apply_modifier {
            return parse_statement_modifier(r, stmt);
        }
        return Ok((r, stmt));
    }
    if r.starts_with('=') && !r.starts_with("==") && !r.starts_with("=>") {
        let r = &r[1..];
        let (r, _) = ws(r)?;
        let (r, expr) = parse_assign_expr_or_comma(r)?;
        let decl = Stmt::VarDecl {
            name: name.clone(),
            expr,
            type_constraint: type_constraint.clone(),
            is_state,
            is_our,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
            where_constraint: None,
        };
        let stmt = if type_constraint.is_none() {
            Stmt::SyntheticBlock(vec![decl, Stmt::MarkSigillessReadonly(name)])
        } else {
            decl
        };
        if apply_modifier {
            return parse_statement_modifier(r, stmt);
        }
        return Ok((r, stmt));
    }
    Ok((
        r,
        Stmt::VarDecl {
            name,
            expr: type_constraint
                .as_deref()
                .map_or(Expr::Literal(Value::Nil), typed_default_expr),
            type_constraint,
            is_state,
            is_our,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
            where_constraint: None,
        },
    ))
}

/// Parse optional type constraint before a variable sigil.
pub(super) fn parse_optional_type_constraint(rest: &str) -> PResult<'_, Option<String>> {
    let saved = rest;
    if let Some((r, tc)) = parse_decl_type_constraint(rest) {
        let (r2, _) = ws(r)?;
        if r2.starts_with('$')
            || r2.starts_with('@')
            || r2.starts_with('%')
            || r2.starts_with('&')
            || r2.starts_with('\\')
            || r2.starts_with('(')
            || keyword("constant", r2).is_some()
        {
            Ok((r2, Some(tc)))
        } else {
            // Check for multiple prefix type constraints (e.g. `my Int Str $x`)
            if let Some((r3, _second_tc)) = parse_decl_type_constraint(r2) {
                let (r4, _) = ws(r3).unwrap_or((r3, ()));
                if r4.starts_with('$')
                    || r4.starts_with('@')
                    || r4.starts_with('%')
                    || r4.starts_with('&')
                    || r4.starts_with('\\')
                {
                    return Err(PError::raw(
                        "FATAL:X::Comp::NYI: Multiple prefix constraints not yet implemented. Sorry."
                            .to_string(),
                        Some(r2.len()),
                    ));
                }
            }
            Ok((saved, None))
        }
    } else {
        Ok((saved, None))
    }
}

/// Check for invalid type smileys (e.g. Int:foo).
pub(super) fn check_invalid_type_smiley(type_constraint: &Option<String>) -> Result<(), PError> {
    if let Some(tc) = type_constraint
        && let Some(colon_pos) = tc.rfind(':')
        && (colon_pos == 0 || tc.as_bytes()[colon_pos - 1] != b':')
    {
        let smiley = &tc[colon_pos + 1..];
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

/// Parse `my <Type> constant <name> = <expr>`.
pub(super) fn parse_typed_constant(
    rest: &str,
    type_constraint: Option<String>,
    is_our: bool,
    apply_modifier: bool,
) -> PResult<'_, Stmt> {
    let (r, mut stmt) = constant_decl(rest)?;
    if let Stmt::VarDecl {
        type_constraint: ref mut tc,
        is_our: ref mut our_flag,
        ref mut expr,
        ..
    } = stmt
    {
        if let Some(ref type_name) = type_constraint
            && let Expr::MethodCall { target, .. } = expr
            && matches!(target.as_ref(), Expr::BareWord(w) if w == "Mu")
        {
            **target = Expr::BareWord(type_name.clone());
        }
        *tc = type_constraint;
        *our_flag = is_our;
    }
    if apply_modifier {
        return parse_statement_modifier(r, stmt);
    }
    Ok((r, stmt))
}

/// Try to parse `our $.name` / `my $.name` — class-level (shared) attributes.
#[allow(clippy::too_many_arguments)]
pub(super) fn try_dot_twigil_attr<'a>(
    rest: &'a str,
    sigil: u8,
    is_array: bool,
    is_hash: bool,
    is_our: bool,
    is_state: bool,
    type_constraint: &Option<String>,
    apply_modifier: bool,
) -> Result<Option<(&'a str, Stmt)>, PError> {
    if (sigil == b'$' || is_array || is_hash)
        && rest.len() > 2
        && rest.as_bytes()[1] == b'.'
        && rest[2..]
            .chars()
            .next()
            .is_some_and(|c| c.is_alphabetic() || c == '_')
    {
        let after_dot = &rest[2..];
        let (after_name, attr_name) = take_while1(after_dot, |c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        })?;
        let attr_name = attr_name.to_string();
        let (after_name, _) = ws(after_name)?;
        let (after_name, default) = if after_name.starts_with('=')
            && !after_name.starts_with("==")
            && !after_name.starts_with("=>")
        {
            let r = &after_name[1..];
            let (r, _) = ws(r)?;
            let (r, expr) = expression(r)?;
            (r, Some(expr))
        } else {
            (after_name, None)
        };
        let stmt = Stmt::HasDecl {
            name: Symbol::intern(&attr_name),
            is_public: true,
            default,
            handles: Vec::new(),
            is_rw: true,
            is_readonly: false,
            type_constraint: type_constraint.clone(),
            type_smiley: None,
            is_required: None,
            sigil: sigil as char,
            where_constraint: None,
            is_alias: false,
            is_our,
            is_my: !is_our && !is_state,
            is_default: None,
            is_type: None,
        };
        if apply_modifier {
            return parse_statement_modifier(after_name, stmt).map(Some);
        }
        return Ok(Some((after_name, stmt)));
    }
    Ok(None)
}
