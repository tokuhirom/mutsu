use super::super::super::expr::expression;
use super::super::super::helpers::{skip_balanced_parens, ws, ws1};
use super::super::super::parse_result::{PError, PResult, opt_char};
use super::super::{ident, keyword, qualified_ident, var_name};
use super::helpers::{
    parse_export_trait_tags, parse_sigilless_decl_name, register_term_symbol_from_decl_name,
};
use super::parse_comma_or_expr;
use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

use super::super::sub::parse_type_constraint_expr;

/// Parse `constant` declaration.
pub(in crate::parser::stmt) fn constant_decl(input: &str) -> PResult<'_, Stmt> {
    let rest =
        keyword("constant", input).ok_or_else(|| PError::expected("constant declaration"))?;
    let (rest, _) = ws1(rest)?;
    // The name can be $var, @var, %var, &var, or bare identifier.
    let sigil = rest.as_bytes().first().copied().unwrap_or(0);
    let (rest, name) = if let Some(r) = rest.strip_prefix('\\') {
        let (r, n) = parse_sigilless_decl_name(r)?;
        register_term_symbol_from_decl_name(&n);
        (r, n)
    } else if matches!(sigil, b'$' | b'@' | b'%' | b'&') {
        let prefix = match sigil {
            b'@' => "@",
            b'%' => "%",
            b'&' => "&",
            _ => "",
        };
        let (r, n) = var_name(rest)?;
        let name = format!("{prefix}{n}");
        register_term_symbol_from_decl_name(&name);
        (r, name)
    } else {
        let (r, n) = ident(rest)?;
        register_term_symbol_from_decl_name(&n);
        (r, n)
    };
    let (mut rest, _) = ws(rest)?;
    let mut is_export = false;
    let mut export_tags: Vec<String> = Vec::new();
    while let Some(after_is) = keyword("is", rest) {
        let (r2, _) = ws1(after_is)?;
        let (r2, trait_name) = ident(r2)?;
        if trait_name != "export" {
            return Err(PError::fatal(format!(
                "X::Comp::Trait::Unknown: Unknown variable trait 'is {}'",
                trait_name
            )));
        }
        is_export = true;
        let (r3, tags) = parse_export_trait_tags(r2)?;
        if tags.is_empty() {
            if !export_tags.iter().any(|t| t == "DEFAULT") {
                export_tags.push("DEFAULT".to_string());
            }
        } else {
            for tag in tags {
                if !export_tags.iter().any(|t| t == &tag) {
                    export_tags.push(tag);
                }
            }
        }
        rest = r3;
    }
    if rest.starts_with('=') || rest.starts_with("::=") || rest.starts_with(":=") {
        let rest = if let Some(stripped) = rest.strip_prefix("::=") {
            stripped
        } else if let Some(stripped) = rest.strip_prefix(":=") {
            stripped
        } else {
            &rest[1..]
        };
        let (rest, _) = ws(rest)?;
        let (rest, expr) = parse_comma_or_expr(rest)?;
        // Track compile-time string constants for operator name resolution
        if let crate::ast::Expr::Literal(crate::value::Value::Str(ref s)) = expr {
            super::super::simple::register_compile_time_constant(&name, s.to_string());
        }
        let (rest, _) = ws(rest)?;
        let (rest, _) = opt_char(rest, ';');
        return Ok((
            rest,
            Stmt::VarDecl {
                name,
                expr,
                type_constraint: None,
                is_state: false,
                is_our: true,
                is_dynamic: false,
                is_export,
                export_tags: export_tags.clone(),
                custom_traits: vec![("__constant".to_string(), None)],
                where_constraint: None,
            },
        ));
    }
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::VarDecl {
            name,
            expr: Expr::Literal(Value::Nil),
            type_constraint: None,
            is_state: false,
            is_our: true,
            is_dynamic: false,
            is_export,
            export_tags,
            custom_traits: vec![("__constant".to_string(), None)],
            where_constraint: None,
        },
    ))
}

/// Parse `subset` declaration.
pub(in crate::parser::stmt) fn subset_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("subset", input).ok_or_else(|| PError::expected("subset declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (mut rest, _) = ws(rest)?;
    while let Some(r) = keyword("is", rest) {
        let (r, _) = ws1(r)?;
        let (r, trait_name) = ident(r)?;
        let (r, _) = ws(r)?;
        if trait_name == "export" {
            rest = skip_balanced_parens(r);
            let (r2, _) = ws(rest)?;
            rest = r2;
        } else {
            rest = r;
        }
    }
    let (rest, base) = if let Some(r) = keyword("of", rest) {
        let (r, _) = ws1(r)?;
        let (r, base) = parse_type_constraint_expr(r).ok_or_else(|| PError::expected("type"))?;
        let (r, _) = ws(r)?;
        (r, base)
    } else {
        (rest, "Any".to_string())
    };
    let (rest, predicate) = if let Some(r) = keyword("where", rest) {
        let (r, _) = ws1(r)?;
        let (r, pred) = expression(r)?;
        (r, Some(pred))
    } else {
        (rest, None)
    };
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::SubsetDecl {
            name: Symbol::intern(&name),
            base,
            predicate,
            version: super::super::simple::current_language_version(),
        },
    ))
}
