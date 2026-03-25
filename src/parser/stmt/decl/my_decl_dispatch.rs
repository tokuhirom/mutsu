use super::super::super::expr::expression;
use super::super::super::helpers::{skip_balanced_parens, ws, ws1};
use super::super::super::parse_result::{PError, opt_char};
use super::super::{ident, keyword, qualified_ident};
use super::constant_subset::{constant_decl, subset_decl};
use super::{
    class_decl_body, method_decl_body, method_decl_body_my, module_decl, package_decl,
    parse_statement_modifier, role_decl, sub_decl_body,
};
use crate::ast::Stmt;
use crate::symbol::Symbol;

use super::super::sub::parse_type_constraint_expr;

/// Try to dispatch to keyword-based declarations (typed routines, multi, sub, method, etc.)
/// Returns `None` if no keyword matched (fall through to variable parsing).
pub(super) fn try_keyword_dispatch(
    rest: &str,
    _is_state: bool,
    is_our: bool,
    apply_modifier: bool,
) -> Result<Option<(&str, Stmt)>, PError> {
    // Typed routine declarations, e.g. `my Bool sub f(...) { ... }`.
    if let Some((after_type, routine_type)) = parse_type_constraint_expr(rest)
        && routine_type
            .chars()
            .next()
            .is_some_and(|c| c.is_ascii_uppercase())
    {
        let (after_type, _) = ws(after_type)?;
        if let Some(r) = keyword("multi", after_type) {
            let (r, _) = ws1(r)?;
            let r = keyword("sub", r)
                .map(|r2| ws(r2).map(|(r3, _)| r3).unwrap_or(r2))
                .unwrap_or(r);
            let (r, mut stmt) = sub_decl_body(r, true, false, false)?;
            if let Stmt::SubDecl {
                return_type,
                custom_traits,
                ..
            } = &mut stmt
            {
                if return_type.is_none() {
                    *return_type = Some(routine_type.clone());
                }
                if is_our {
                    custom_traits.push("__our_scoped".to_string());
                }
            }
            return Ok(Some((r, stmt)));
        }
        if let Some(r) = keyword("sub", after_type) {
            let (r, _) = ws1(r)?;
            let (r, mut stmt) = sub_decl_body(r, false, false, false)?;
            if let Stmt::SubDecl {
                return_type,
                custom_traits,
                ..
            } = &mut stmt
            {
                if return_type.is_none() {
                    *return_type = Some(routine_type.clone());
                }
                if is_our {
                    custom_traits.push("__our_scoped".to_string());
                }
            }
            return Ok(Some((r, stmt)));
        }
        // `my Str subset MyStr [where ...]` — shorthand subset with prefix base type
        if let Some(r) = keyword("subset", after_type) {
            let (r, _) = ws1(r)?;
            let (r, name) = qualified_ident(r)?;
            let (mut r, _) = ws(r)?;
            while let Some(r2) = keyword("is", r) {
                let (r2, _) = ws1(r2)?;
                let (r2, trait_name) = ident(r2)?;
                let (r2, _) = ws(r2)?;
                if trait_name == "export" {
                    r = skip_balanced_parens(r2);
                    let (r2, _) = ws(r)?;
                    r = r2;
                } else {
                    r = r2;
                }
            }
            let (r, predicate) = if let Some(r2) = keyword("where", r) {
                let (r2, _) = ws1(r2)?;
                let (r2, pred) = expression(r2)?;
                (r2, Some(pred))
            } else {
                (r, None)
            };
            let (r, _) = ws(r)?;
            let (r, _) = opt_char(r, ';');
            return Ok(Some((
                r,
                Stmt::SubsetDecl {
                    name: Symbol::intern(&name),
                    base: routine_type,
                    predicate,
                    version: super::super::simple::current_language_version(),
                },
            )));
        }
        // Check for multiple prefix constraints on routine
        if let Some((after_second_type, _second_tc)) = parse_type_constraint_expr(after_type) {
            let (after_second_type, _) = ws(after_second_type)?;
            if keyword("sub", after_second_type).is_some()
                || keyword("multi", after_second_type).is_some()
            {
                return Err(PError::raw(
                    "FATAL:X::Comp::NYI: Multiple prefix constraints not yet implemented. Sorry."
                        .to_string(),
                    Some(after_type.len()),
                ));
            }
        }
    }

    // my multi [sub] name(...) { ... }
    if let Some(r) = keyword("multi", rest) {
        let (r, _) = ws1(r)?;
        let r = keyword("sub", r)
            .map(|r2| ws(r2).map(|(r3, _)| r3).unwrap_or(r2))
            .unwrap_or(r);
        let (r, mut stmt) = sub_decl_body(r, true, false, false)?;
        if is_our && let Stmt::SubDecl { custom_traits, .. } = &mut stmt {
            custom_traits.push("__our_scoped".to_string());
        }
        return Ok(Some((r, stmt)));
    }

    // my sub name(...) { ... }
    if let Some(r) = keyword("sub", rest) {
        let (r, _) = ws1(r)?;
        let (r, mut stmt) = sub_decl_body(r, false, false, false)?;
        if is_our && let Stmt::SubDecl { custom_traits, .. } = &mut stmt {
            custom_traits.push("__our_scoped".to_string());
        }
        return Ok(Some((r, stmt)));
    }

    // my/our method name(...) { ... }
    if let Some(r) = keyword("method", rest) {
        let (r, _) = ws1(r)?;
        if is_our {
            return method_decl_body(r, false, true).map(Some);
        } else {
            return method_decl_body_my(r, false, false).map(Some);
        }
    }

    // my/our submethod name(...) { ... }
    if let Some(r) = keyword("submethod", rest) {
        let (r, _) = ws1(r)?;
        if is_our {
            return method_decl_body(r, false, true).map(Some);
        } else {
            return method_decl_body_my(r, false, false).map(Some);
        }
    }

    // my class Name is Parent { ... }
    if let Some(r) = keyword("class", rest) {
        let (r, _) = ws1(r)?;
        return class_decl_body(r, !is_our).map(Some);
    }
    // my grammar Name { ... }
    if keyword("grammar", rest).is_some() {
        return super::super::class::grammar_decl(rest).map(Some);
    }
    // my role Name[...] { ... }
    if keyword("role", rest).is_some() {
        return role_decl(rest).map(Some);
    }
    // my module Name { ... }
    if keyword("module", rest).is_some() {
        return module_decl(rest).map(Some);
    }
    // my package Name { ... }
    if keyword("package", rest).is_some() {
        return package_decl(rest).map(Some);
    }
    // my subset Name of BaseType where ...
    if keyword("subset", rest).is_some() {
        return subset_decl(rest).map(Some);
    }
    // my constant $x = ...
    if keyword("constant", rest).is_some() {
        let (r, mut stmt) = constant_decl(rest)?;
        if let Stmt::VarDecl {
            is_our: ref mut our_flag,
            ..
        } = stmt
        {
            *our_flag = is_our;
        }
        if apply_modifier {
            return parse_statement_modifier(r, stmt).map(Some);
        }
        return Ok(Some((r, stmt)));
    }
    // my regex/token/rule Name { ... }
    if keyword("regex", rest).is_some()
        || keyword("token", rest).is_some()
        || keyword("rule", rest).is_some()
    {
        let (rest, stmt) = super::super::class::token_decl(rest)?;
        if apply_modifier {
            return parse_statement_modifier(rest, stmt).map(Some);
        }
        return Ok(Some((rest, stmt)));
    }

    Ok(None)
}
