use super::super::super::expr::expression;
use super::super::super::helpers::{ws, ws1};
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
    // `constant ($a, $b) = ...` is a syntax error (X::Syntax::Missing)
    if rest.starts_with('(') {
        return Err(PError::fatal(
            "X::Syntax::Missing: Missing initializer on constant declaration".to_string(),
        ));
    }
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
    // Record the source sigil so the compiler can distinguish a scalar
    // `constant $x` from a sigilless `constant x` for X::Redeclaration purposes
    // (the VarDecl `name` strips the `$`, so both would otherwise collide).
    let sigil_marker = match sigil {
        b'$' => "$",
        b'@' => "@",
        b'%' => "%",
        b'&' => "&",
        _ => "",
    };
    let constant_traits = vec![
        ("__constant".to_string(), None),
        (
            "__constant_sigil".to_string(),
            Some(Expr::Literal(Value::str(sigil_marker.to_string()))),
        ),
    ];
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
    // .= mutating method call: constant foo .= new => constant foo = Mu.new
    if let Some(stripped) = rest.strip_prefix(".=") {
        let (r, _) = ws(stripped)?;
        let (r, method_name) = crate::parser::parse_result::take_while1(r, |c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        })?;
        let method_name = method_name.to_string();
        let r_before_ws = r;
        let (r, _) = ws(r)?;
        // Parse optional args (parenthesized, colon-form, or fake-infix adverbs)
        let (r, args) = if let Some(inner) = r.strip_prefix('(') {
            let r = inner;
            let (r, _) = ws(r)?;
            let (r, args) = crate::parser::primary::parse_call_arg_list(r)?;
            let (r, _) = ws(r)?;
            let r = r.strip_prefix(')').ok_or_else(|| PError::expected(")"))?;
            (r, args)
        } else if r_before_ws.starts_with(':') && !r_before_ws.starts_with("::") {
            super::my_decl_assign::parse_colon_args(r_before_ws)?
        } else if r.starts_with(':') && !r.starts_with("::") {
            super::my_decl_assign::parse_fake_infix_adverbs(r)?
        } else {
            (r, Vec::new())
        };
        let expr = crate::ast::Expr::MethodCall {
            target: Box::new(crate::ast::Expr::BareWord("Mu".to_string())),
            name: crate::symbol::Symbol::intern(&method_name),
            args,
            modifier: None,
            quoted: false,
        };
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::VarDecl {
                name,
                expr,
                type_constraint: None,
                is_state: false,
                is_our: true,
                is_dynamic: false,
                is_export,
                export_tags: export_tags.clone(),
                custom_traits: constant_traits.clone(),
                where_constraint: None,
            },
        ));
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
                custom_traits: constant_traits.clone(),
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
            custom_traits: constant_traits.clone(),
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
    let mut is_export = false;
    let mut export_tags: Vec<String> = Vec::new();
    while let Some(r) = keyword("is", rest) {
        let (r, _) = ws1(r)?;
        let (r, trait_name) = ident(r)?;
        if trait_name == "export" {
            is_export = true;
            let (r2, tags) = parse_export_trait_tags(r)?;
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
            let (r2, _) = ws(r2)?;
            rest = r2;
        } else {
            let (r, _) = ws(r)?;
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
            is_export,
            export_tags,
        },
    ))
}
