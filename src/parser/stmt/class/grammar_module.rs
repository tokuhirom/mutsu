use super::*;

use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

use crate::parser::helpers::{ws, ws1};
use crate::parser::parse_result::{PError, PResult, opt_char, parse_char};
use crate::parser::stmt::{block, keyword, parse_param_list, qualified_ident};

/// Parse `does` declaration.
pub(crate) fn does_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("does", input).ok_or_else(|| PError::expected("does declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = parse_token_like_name(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::DoesDecl {
            name: Symbol::intern(&name),
        },
    ))
}

/// Parse `trusts` declaration.
pub(crate) fn trusts_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("trusts", input).ok_or_else(|| PError::expected("trusts declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::TrustsDecl {
            name: Symbol::intern(&name),
        },
    ))
}

/// Parse a `token`, `regex`, or `rule` declaration.
pub(crate) fn token_decl(input: &str) -> PResult<'_, Stmt> {
    let is_rule = keyword("rule", input).is_some();
    let is_regex = keyword("regex", input).is_some();
    let is_ratchet = !is_regex; // token and rule are ratcheting
    let rest = keyword("token", input)
        .or_else(|| keyword("rule", input))
        .or_else(|| keyword("regex", input))
        .ok_or_else(|| PError::expected("token/regex/rule declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = parse_token_like_name(rest)?;
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
    let (rest, mut pattern) = parse_raw_braced_regex_body(rest)?;
    // An empty `token`/`regex`/`rule` body is a null regex.
    if pattern.trim().is_empty() {
        return Err(null_regex_error());
    }
    pattern = normalize_token_pattern(&pattern);
    if is_rule {
        pattern = inject_implicit_rule_ws(&pattern);
        pattern = inject_separator_ws(&pattern);
        if name.contains(":sym<") || name.contains(":sym\u{ab}") {
            if !pattern.ends_with(' ') {
                pattern.push(' ');
            }
            pattern.push_str("<.ws>?");
        }
    }
    if is_ratchet {
        pattern = format!(":ratchet {pattern}");
    }
    let body = vec![Stmt::Expr(Expr::Literal(Value::regex(pattern)))];

    if is_rule {
        Ok((
            rest,
            Stmt::RuleDecl {
                name: Symbol::intern(&name),
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
                name: Symbol::intern(&name),
                params,
                param_defs,
                body,
                multi: false,
                is_my: false,
                is_our: false,
            },
        ))
    }
}

/// Parse `grammar` declaration.
pub(crate) fn grammar_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("grammar", input).ok_or_else(|| PError::expected("grammar declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    check_pseudo_package_in_decl(&name)?;
    // Consume optional type adverbs (`:ver<...>`, etc.) on the grammar name
    // before the `is`/`does` parent clauses (e.g. `grammar Foo:ver<1> is Bar`).
    let (rest, _traits) = parse_declarator_traits(rest)?;
    let (rest, _) = ws(rest)?;
    let mut r = rest;
    let mut parents = Vec::new();
    while let Some(r2) = keyword("is", r) {
        let (r2, _) = ws1(r2)?;
        let (r2, parent_name) = qualified_ident(r2)?;
        parents.push(parent_name);
        let (r2, _) = ws(r2)?;
        r = r2;
    }
    let mut does_parents = Vec::new();
    while let Some(r2) = keyword("does", r) {
        let (r2, _) = ws1(r2)?;
        let (r2, role_name) = qualified_ident(r2)?;
        does_parents.push(role_name);
        let (r2, _) = ws(r2)?;
        let (r2, _) = skip_optional_role_args(r2)?;
        r = r2;
    }
    // Default parent is Grammar if no `is` clause (unless the grammar itself is named Grammar)
    if parents.is_empty() && name != "Grammar" {
        parents.push("Grammar".to_string());
    }
    let (rest, body) = block(r)?;
    super::super::simple::register_user_type(&name);
    Ok((
        rest,
        Stmt::ClassDecl {
            name: Symbol::intern(&name),
            name_expr: None,
            parents,
            class_is_rw: false,
            is_hidden: false,
            is_lexical: false,
            hidden_parents: vec![],
            does_parents,
            repr: None,
            body,
            language_version: super::super::simple::current_language_version(),
            custom_traits: Vec::new(),
            is_unit: false,
            decl_id: crate::ast::next_class_decl_id(),
        },
    ))
}

/// Parse `module Name { ... }` declaration (non-unit form).
pub(crate) fn module_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("module", input).ok_or_else(|| PError::expected("module declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    check_pseudo_package_in_decl(&name)?;
    let (rest, traits) = parse_declarator_traits(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    // Two `is export` declarations of the same symbol in one module clash.
    if let Some(clash) = find_export_name_clash(&body) {
        return Err(export_name_clash_error(&clash));
    }
    // Record exported subs from inline module so `import` can register them at parse time.
    let exported = extract_exported_subs(&body);
    if !exported.is_empty() {
        super::super::simple::register_inline_module_exports(&name, exported);
    }
    let mut stmts = Vec::new();
    for (trait_name, trait_value) in traits {
        if trait_name == "ver" || trait_name == "auth" {
            stmts.push(meta_setter_stmt(&name, &trait_name, trait_value));
        }
    }
    let package_stmt = Stmt::Package {
        name: Symbol::intern(&name),
        body,
        kind: crate::ast::PackageKind::Module,
        is_unit: false,
        is_my: false,
    };
    if stmts.is_empty() {
        return Ok((rest, package_stmt));
    }
    stmts.push(package_stmt);
    Ok((rest, Stmt::SyntheticBlock(stmts)))
}
