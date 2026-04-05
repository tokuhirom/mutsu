use super::super::super::expr::expression;
use super::super::super::helpers::{ws, ws1};
use super::super::super::parse_result::{PError, PResult, opt_char, parse_char, take_while1};
use super::super::{ident, keyword};
use super::handles::parse_handle_specs;
use super::helpers::{
    parse_array_shape_suffix, shaped_array_new_expr, shaped_array_new_with_data_expr,
};
use super::parse_colon_method_arg;
use super::parse_decl_type_constraint;
use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

use super::super::sub::parse_type_constraint_expr;

/// Parse parenthesized list form of `has`: `has ($a, $.b, $!c)`
/// Desugars into a SyntheticBlock containing multiple HasDecl statements.
fn has_decl_list(input: &str) -> PResult<'_, Stmt> {
    let (mut rest, _) = parse_char(input, '(')?;
    let mut stmts = Vec::new();
    loop {
        let (r, _) = ws(rest)?;
        rest = r;
        if rest.starts_with(')') {
            rest = &rest[1..];
            break;
        }
        // Parse sigil
        let sigil = rest.as_bytes().first().copied().unwrap_or(0);
        if sigil != b'$' && sigil != b'@' && sigil != b'%' {
            return Err(PError::expected("sigil ($, @, %) in has list"));
        }
        rest = &rest[1..];
        // Parse optional twigil (. or !)
        let (r, is_public, is_alias) = if let Some(stripped) = rest.strip_prefix('.') {
            (stripped, true, false)
        } else if let Some(stripped) = rest.strip_prefix('!') {
            (stripped, false, false)
        } else {
            (rest, false, true)
        };
        rest = r;
        // Parse name
        let (r, name) = take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        let name = name.to_string();
        rest = r;
        stmts.push(Stmt::HasDecl {
            name: Symbol::intern(&name),
            is_public,
            default: None,
            handles: Vec::new(),
            is_rw: false,
            is_readonly: false,
            type_constraint: None,
            type_smiley: None,
            is_required: None,
            sigil: sigil as char,
            where_constraint: None,
            is_alias,
            is_our: false,
            is_my: false,
            is_default: None,
            is_type: None,
        });
        let (r, _) = ws(rest)?;
        rest = r;
        // Expect comma or closing paren
        if rest.starts_with(',') {
            rest = &rest[1..];
        }
    }
    let (mut rest, _) = ws(rest)?;

    // Parse optional `is default(expr)` trait on grouped has declaration
    if let Some(r) = keyword("is", rest)
        && let Ok((r, _)) = ws1(r)
        && let Some(r) = keyword("default", r)
    {
        let (r, _) = ws(r)?;
        if let Some(inner) = r.strip_prefix('(') {
            let (inner, _) = ws(inner)?;
            let (inner, default_expr) = expression(inner)?;
            let (inner, _) = ws(inner)?;
            let inner = inner
                .strip_prefix(')')
                .ok_or_else(|| PError::expected("closing paren in is default"))?;
            // Apply the default to all attributes in the list
            for stmt in &mut stmts {
                if let Stmt::HasDecl {
                    default,
                    is_default,
                    ..
                } = stmt
                {
                    *default = Some(default_expr.clone());
                    *is_default = Some(default_expr.clone());
                }
            }
            let (r2, _) = ws(inner)?;
            rest = r2;
        }
    }

    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::SyntheticBlock(stmts)))
}

/// Parse `has` attribute declaration.
pub(in crate::parser::stmt) fn has_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("has", input).ok_or_else(|| PError::expected("has declaration"))?;
    let (rest, _) = ws1(rest)?;

    // Handle parenthesized list form: has ($a, $.b, $!c)
    // This desugars into a Block containing multiple HasDecl statements.
    if rest.starts_with('(') {
        return has_decl_list(rest);
    }

    // Optional type constraint.
    let (rest, mut type_constraint, type_smiley) = {
        let saved = rest;
        if let Some((r, tc)) = parse_decl_type_constraint(rest) {
            let (r2, _) = ws(r)?;
            if r2.starts_with('$') || r2.starts_with('@') || r2.starts_with('%') {
                // Extract smiley suffix and strip it for the type_constraint name
                let (base, smiley) = if let Some(b) = tc.strip_suffix(":D") {
                    (b.to_string(), Some("D".to_string()))
                } else if let Some(b) = tc.strip_suffix(":U") {
                    (b.to_string(), Some("U".to_string()))
                } else if let Some(b) = tc.strip_suffix(":_") {
                    (b.to_string(), Some("_".to_string()))
                } else {
                    (tc.to_string(), None)
                };
                (r2, Some(base), smiley)
            } else {
                (saved, None, None)
            }
        } else {
            (saved, None, None)
        }
    };

    let sigil = rest.as_bytes().first().copied().unwrap_or(0);
    let (rest, _) = if sigil == b'$' || sigil == b'@' || sigil == b'%' || sigil == b'&' {
        (&rest[1..], ())
    } else {
        return Err(PError::expected("sigil ($, @, %, &)"));
    };

    // Check for public accessor marker '.'
    let (rest, is_public, is_alias) = if let Some(stripped) = rest.strip_prefix('.') {
        (stripped, true, false)
    } else if let Some(stripped) = rest.strip_prefix('!') {
        (stripped, false, false)
    } else {
        // No twigil: `has $x` creates a private attribute with an alias
        (rest, false, true)
    };

    let (rest, name) = take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
    let name = name.to_string();

    // Optional shaped-array declaration suffix: has @.a[3, 3]
    let (rest, shape_dims) = if sigil == b'@' && rest.starts_with('[') {
        let (r, dims) = parse_array_shape_suffix(rest)?;
        (r, Some(dims))
    } else {
        (rest, None)
    };

    let (mut rest, _) = ws(rest)?;

    // `is` traits (may have multiple: `is rw is required`)
    // Traits come before default value: `has $.x is rw = 42`
    let mut is_rw = false;
    let mut is_readonly = false;
    let mut is_required: Option<Option<String>> = None;
    let mut is_default_trait: Option<Expr> = None;
    let mut is_type: Option<String> = None;
    while let Some(r) = keyword("is", rest) {
        let (r, _) = ws1(r)?;
        let (r, trait_name) = ident(r)?;
        if trait_name == "rw" {
            is_rw = true;
        } else if trait_name == "readonly" {
            is_readonly = true;
        } else if trait_name == "default" {
            // `is default(expr)` — set the attribute's default value
            let (r_ws, _) = ws(r)?;
            if let Some(inner) = r_ws.strip_prefix('(') {
                let (inner, _) = ws(inner)?;
                let (inner, default_expr) = expression(inner)?;
                let (inner, _) = ws(inner)?;
                let inner = inner
                    .strip_prefix(')')
                    .ok_or_else(|| PError::expected("closing paren in is default"))?;
                is_default_trait = Some(default_expr);
                let (r2, _) = ws(inner)?;
                rest = r2;
                continue;
            }
            // `is default` without parens — just ignore (no-op)
        } else if trait_name == "required" {
            // Check for optional reason: `is required("reason")`
            let (r_ws, _) = ws(r)?;
            if let Some(inner) = r_ws.strip_prefix('(') {
                let (inner, _) = ws(inner)?;
                // Parse string literal for the reason
                if let Some(double_quoted) = inner.strip_prefix('"') {
                    let end = double_quoted
                        .find('"')
                        .ok_or_else(|| PError::expected("closing quote in is required reason"))?;
                    let reason = double_quoted[..end].to_string();
                    let after = &double_quoted[end + 1..];
                    let (after, _) = ws(after)?;
                    let after = after
                        .strip_prefix(')')
                        .ok_or_else(|| PError::expected("closing paren in is required"))?;
                    is_required = Some(Some(reason));
                    rest = after;
                    let (r2, _) = ws(rest)?;
                    rest = r2;
                    continue;
                } else if let Some(single_quoted) = inner.strip_prefix('\'') {
                    let end = single_quoted
                        .find('\'')
                        .ok_or_else(|| PError::expected("closing quote in is required reason"))?;
                    let reason = single_quoted[..end].to_string();
                    let after = &single_quoted[end + 1..];
                    let (after, _) = ws(after)?;
                    let after = after
                        .strip_prefix(')')
                        .ok_or_else(|| PError::expected("closing paren in is required"))?;
                    is_required = Some(Some(reason));
                    rest = after;
                    let (r2, _) = ws(rest)?;
                    rest = r2;
                    continue;
                }
            }
            is_required = Some(None);
        } else if trait_name
            .chars()
            .next()
            .is_some_and(|c| c.is_ascii_uppercase())
        {
            // Uppercase-starting trait name: `is Buf`, `is BagHash`, etc.
            // This is a container type trait for `@`/`%` attributes.
            is_type = Some(trait_name.to_string());
        }
        let (r, _) = ws(r)?;
        rest = r;
    }

    // `handles` trait, e.g. `has $.x handles <a b>`
    let mut handles = Vec::new();
    while let Some(r) = keyword("handles", rest) {
        let (r, _) = ws1(r)?;
        parse_handle_specs(r, &mut handles, &mut rest)?;
        let (r, _) = ws(rest)?;
        rest = r;
    }

    // Postfix container typing: has @.a of Int; has %.h of Str;
    if (sigil == b'@' || sigil == b'%')
        && type_constraint.is_none()
        && let Some(r) = keyword("of", rest)
    {
        let (r, _) = ws1(r)?;
        let (r, tc) = parse_type_constraint_expr(r).ok_or_else(|| PError::expected("type"))?;
        let (r, _) = ws(r)?;
        type_constraint = Some(tc);
        rest = r;
    }

    // Optional `where` constraint
    let (rest, where_constraint) = if let Some(r) = keyword("where", rest) {
        let (r, _) = ws1(r)?;
        let (r, pred) = expression(r)?;
        let (r, _) = ws(r)?;
        (r, Some(Box::new(pred)))
    } else {
        (rest, None)
    };

    // Default value
    let (rest, mut default) = if let Some(stripped) = rest.strip_prefix(".=") {
        let (rest, _) = ws(stripped)?;
        let (rest, method_name) =
            take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        let method_name = method_name.to_string();
        let (rest, args) = if rest.starts_with('(') {
            let (r, _) = parse_char(rest, '(')?;
            let (r, _) = ws(r)?;
            let (r, args) = super::super::super::primary::parse_call_arg_list(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            (r, args)
        } else if rest.starts_with(':') && !rest.starts_with("::") {
            let r = &rest[1..];
            let (r, _) = ws(r)?;
            let (r, first_arg) = parse_colon_method_arg(r)?;
            let mut args = vec![first_arg];
            let mut r_inner = r;
            loop {
                let (r2, _) = ws(r_inner)?;
                if r2.starts_with(':')
                    && !r2.starts_with("::")
                    && let Ok((r3, arg)) = crate::parser::primary::misc::colonpair_expr(r2)
                {
                    args.push(arg);
                    r_inner = r3;
                    continue;
                }
                if !r2.starts_with(',') {
                    break;
                }
                let r2 = &r2[1..];
                let (r2, _) = ws(r2)?;
                // Handle trailing comma before ';' or '}'
                if r2.starts_with(';') || r2.starts_with('}') || r2.is_empty() {
                    r_inner = r2;
                    break;
                }
                let (r2, next) = parse_colon_method_arg(r2)?;
                args.push(next);
                r_inner = r2;
            }
            (r_inner, args)
        } else {
            (rest, Vec::new())
        };
        let target_name = type_constraint.clone().unwrap_or_else(|| name.clone());
        let target_expr = if target_name == "::?CLASS" {
            Expr::Var("?CLASS".to_string())
        } else if target_name == "::?ROLE" {
            Expr::Var("?ROLE".to_string())
        } else {
            Expr::BareWord(target_name)
        };
        (
            rest,
            Some(Expr::MethodCall {
                target: Box::new(target_expr),
                name: Symbol::intern(&method_name),
                args,
                modifier: None,
                quoted: false,
            }),
        )
    } else if rest.starts_with('=') && !rest.starts_with("==") {
        let rest = &rest[1..];
        let (rest, _) = ws(rest)?;
        let (rest, expr) = expression(rest)?;
        // For @ and % sigils, parse comma-separated list of expressions
        if (sigil == b'@' || sigil == b'%') && rest.starts_with(',') {
            let mut items = vec![expr];
            let mut r = rest;
            while r.starts_with(',') {
                r = &r[1..];
                let (r2, _) = ws(r)?;
                let (r2, next_expr) = expression(r2)?;
                items.push(next_expr);
                r = r2;
            }
            (r, Some(Expr::ArrayLiteral(items)))
        } else {
            (rest, Some(expr))
        }
    } else if let Some(default_expr) = is_default_trait.clone() {
        // `is default(expr)` was used — apply it as the default value
        (rest, Some(default_expr))
    } else {
        (rest, None)
    };

    // Track whether user provided an explicit default (before auto-default)
    let has_explicit_default = default.is_some();

    // Apply `use attributes :D/:U/:_` pragma if no explicit smiley on the type
    let smiley_from_pragma = type_smiley.is_none() && type_constraint.is_some() && {
        let pragma = super::super::simple::current_attributes_pragma();
        matches!(pragma.as_str(), ":D" | ":U")
    };
    let type_smiley = if type_smiley.is_none() && type_constraint.is_some() {
        let pragma = super::super::simple::current_attributes_pragma();
        match pragma.as_str() {
            ":D" => Some("D".to_string()),
            ":U" => Some("U".to_string()),
            _ => type_smiley, // ":_" or empty - no change
        }
    } else {
        type_smiley
    };

    // Enforce type smiley constraints at parse time
    let effective_smiley = type_smiley.as_deref().unwrap_or("_");
    if effective_smiley == "D" && !has_explicit_default && is_required.is_none() {
        // :D attribute without a default or `is required` → X::Syntax::Variable::MissingInitializer
        let twigil = if is_public { "." } else { "!" };
        let tc_display = if let Some(ref tc) = type_constraint {
            format!("{}:D", tc)
        } else {
            "Any:D".to_string()
        };
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("type".to_string(), Value::str(tc_display));
        attrs.insert("name".to_string(), Value::str(format!("$!{}", name)));
        if smiley_from_pragma {
            attrs.insert(
                "implicit".to_string(),
                Value::str(":D by pragma".to_string()),
            );
        }
        let ex = Value::make_instance(
            Symbol::intern("X::Syntax::Variable::MissingInitializer"),
            attrs,
        );
        return Err(PError::fatal_with_exception(
            format!(
                "Variable '{}{}{}' of type '{}:D' must be initialized",
                sigil as char,
                twigil,
                name,
                type_constraint.as_deref().unwrap_or("Any")
            ),
            Box::new(ex),
        ));
    }

    // Auto-default: typed scalar attribute with no explicit default → use type object
    // Native types (int, atomicint, num, str, etc.) get zero/empty defaults.
    if !has_explicit_default
        && is_required.is_none()
        && sigil == b'$'
        && let Some(ref tc) = type_constraint
    {
        if tc == "::?CLASS" {
            default = Some(Expr::Var("?CLASS".to_string()));
        } else if tc == "::?ROLE" {
            default = Some(Expr::Var("?ROLE".to_string()));
        } else {
            default = Some(match tc.as_str() {
                "int" | "int8" | "int16" | "int32" | "int64" | "uint" | "uint8" | "uint16"
                | "uint32" | "uint64" | "byte" | "atomicint" => Expr::Literal(Value::Int(0)),
                "num" | "num32" | "num64" => Expr::Literal(Value::Num(0.0)),
                "str" => Expr::Literal(Value::str("".to_string())),
                _ => Expr::BareWord(tc.clone()),
            });
        }
    }

    if sigil == b'@' {
        if let Some(dims) = shape_dims {
            // Shaped array attribute: has @.a[3, 3]
            if let Some(data_expr) = default.take() {
                default = Some(shaped_array_new_with_data_expr(dims, data_expr));
            } else {
                default = Some(shaped_array_new_expr(dims));
            }
        } else if let Some(expr) = default.take() {
            default = Some(match expr {
                Expr::ArrayLiteral(_)
                | Expr::BracketArray(..)
                | Expr::ArrayVar(_)
                | Expr::Var(_)
                | Expr::Index { .. } => expr,
                other => Expr::ArrayLiteral(vec![other]),
            });
        }
    }
    let (rest, _) = ws(rest)?;

    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::HasDecl {
            name: Symbol::intern(&name),
            is_public,
            default,
            handles,
            is_rw,
            is_readonly,
            type_constraint,
            type_smiley,
            is_required,
            sigil: sigil as char,
            where_constraint,
            is_alias,
            is_our: false,
            is_my: false,
            is_default: is_default_trait,
            is_type,
        },
    ))
}
