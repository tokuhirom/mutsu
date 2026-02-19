use super::super::helpers::{ws, ws1};
use super::super::parse_result::{PError, PResult, opt_char, parse_char};

use crate::ast::Stmt;

use super::{block, ident, keyword, qualified_ident};

use super::{parse_param_list, parse_sub_traits};

/// Parse `class` declaration.
pub(super) fn class_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("class", input).ok_or_else(|| PError::expected("class declaration"))?;
    let (rest, _) = ws1(rest)?;
    class_decl_body(rest)
}

/// Parse the body of a class declaration (after `class` keyword and whitespace).
pub(super) fn class_decl_body(input: &str) -> PResult<'_, Stmt> {
    let (rest, name) = qualified_ident(input)?;
    let (rest, _) = ws(rest)?;

    // Parent classes: is Parent
    let mut parents = Vec::new();
    let mut r = rest;
    while let Some(r2) = keyword("is", r) {
        let (r2, _) = ws1(r2)?;
        let (r2, parent) = qualified_ident(r2)?;
        parents.push(parent);
        let (r2, _) = ws(r2)?;
        r = r2;
    }
    // does Role
    while let Some(r2) = keyword("does", r) {
        let (r2, _) = ws1(r2)?;
        let (r2, role_name) = qualified_ident(r2)?;
        // Treat "does" as parent for now
        parents.push(role_name);
        let (r2, _) = ws(r2)?;
        r = r2;
    }

    let (rest, body) = block(r)?;
    Ok((
        rest,
        Stmt::ClassDecl {
            name,
            parents,
            body,
        },
    ))
}

/// Parse `role` declaration.
pub(super) fn role_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("role", input).ok_or_else(|| PError::expected("role declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::RoleDecl { name, body }))
}

/// Parse `does` declaration.
pub(super) fn does_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("does", input).ok_or_else(|| PError::expected("does declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::DoesDecl { name }))
}

/// Parse a `token` or `rule` declaration.
pub(super) fn token_decl(input: &str) -> PResult<'_, Stmt> {
    let is_rule = keyword("rule", input).is_some();
    let rest = keyword("token", input)
        .or_else(|| keyword("rule", input))
        .ok_or_else(|| PError::expected("token/rule declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = ident(rest)?;
    let (rest, _) = ws(rest)?;

    // Optional params
    let (rest, (params, param_defs)) = if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, pd) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let names: Vec<String> = pd.iter().map(|p| p.name.clone()).collect();
        (r, (names, pd))
    } else {
        (rest, (Vec::new(), Vec::new()))
    };

    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;

    if is_rule {
        Ok((
            rest,
            Stmt::RuleDecl {
                name,
                params,
                param_defs,
                body,
                multi: false,
            },
        ))
    } else {
        Ok((
            rest,
            Stmt::TokenDecl {
                name,
                params,
                param_defs,
                body,
                multi: false,
            },
        ))
    }
}

/// Parse `grammar` declaration.
pub(super) fn grammar_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("grammar", input).ok_or_else(|| PError::expected("grammar declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Package { name, body }))
}

/// Parse `unit module` or `unit class` statement.
pub(super) fn unit_module_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("unit", input).ok_or_else(|| PError::expected("unit statement"))?;
    let (rest, _) = ws1(rest)?;
    // unit class Name;
    if let Some(r) = keyword("class", rest) {
        let (r, _) = ws1(r)?;
        let (r, name) = qualified_ident(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::ClassDecl {
                name,
                parents: Vec::new(),
                body: Vec::new(),
            },
        ));
    }
    let rest = keyword("module", rest).ok_or_else(|| PError::expected("'module' after 'unit'"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::Package {
            name,
            body: Vec::new(),
        },
    ))
}

/// Parse `package` declaration.
pub(super) fn package_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("package", input).ok_or_else(|| PError::expected("package declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Package { name, body }))
}

/// Parse `proto` declaration.
pub(super) fn proto_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("proto", input).ok_or_else(|| PError::expected("proto declaration"))?;
    let (rest, _) = ws1(rest)?;
    // proto token | proto rule | proto sub | proto method
    let _is_token = keyword("token", rest).is_some() || keyword("rule", rest).is_some();
    let rest = if let Some(r) = keyword("token", rest)
        .or_else(|| keyword("rule", rest))
        .or_else(|| keyword("sub", rest))
        .or_else(|| keyword("method", rest))
    {
        let (r, _) = ws1(r)?;
        r
    } else {
        rest
    };
    let (rest, name) = ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, param_defs) = if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, pd) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        (r, pd)
    } else {
        (rest, Vec::new())
    };
    let params: Vec<String> = param_defs.iter().map(|p| p.name.clone()).collect();
    let (rest, _) = ws(rest)?;
    // Parse traits (is export, etc.)
    let (rest, traits) = parse_sub_traits(rest)?;
    let (rest, _) = ws(rest)?;
    // May have {*} body or just semicolon
    if rest.starts_with('{') {
        let (rest, _body) = block(rest)?;
        return Ok((
            rest,
            Stmt::ProtoDecl {
                name,
                params,
                param_defs,
                is_export: traits.is_export,
            },
        ));
    }
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::ProtoDecl {
            name,
            params,
            param_defs,
            is_export: traits.is_export,
        },
    ))
}
