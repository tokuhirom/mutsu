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
            args: None,
            from_is: false,
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

/// Parse a `token`, `regex`, or `rule` declaration (optionally `multi`-marked:
/// `multi rule expr($p) {...}` — candidates accumulate instead of replacing).
pub(crate) fn token_decl(input: &str) -> PResult<'_, Stmt> {
    let (input, is_multi) = match keyword("multi", input).and_then(|r| ws1(r).ok()) {
        // Only treat `multi` as a marker when a rule-ish keyword follows
        // (`multi rule ...`); a bare `multi sub`/`multi method` belongs to
        // other parsers.
        Some((r, _))
            if keyword("token", r).is_some()
                || keyword("rule", r).is_some()
                || keyword("regex", r).is_some() =>
        {
            (r, true)
        }
        _ => (input, false),
    };
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

    // Traits between the signature and the body: `my token n is export { ... }`,
    // `token n is export(:tag) { ... }`. A regex declarator is a routine, so it
    // takes the routine trait grammar; `is export` is the one with semantics
    // here (the Regex becomes importable under `&n`) and the rest are accepted
    // and dropped, which is strictly better than the hard parse error a single
    // unimplemented trait used to make of the whole compilation unit.
    let (rest, traits) = crate::parser::stmt::parse_sub_traits_pub(rest)?;
    let (is_export, export_tags) = (traits.is_export, traits.export_tags);

    let (rest, _) = ws(rest)?;
    let (rest, mut pattern) = parse_raw_braced_regex_body(rest)?;
    // An empty `token`/`regex`/`rule` body is a null regex.
    if pattern.trim().is_empty() {
        return Err(null_regex_error());
    }
    let source_pattern = normalize_token_pattern(&pattern);
    let source_regex = crate::regex_tree::RegexTree::parse_static(&source_pattern, true);
    pattern = source_pattern;
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
                source_regex,
                multi: is_multi,
                is_export,
                export_tags,
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
                source_regex,
                regex_kind: if is_regex {
                    crate::regex_tree::RegexDeclKind::Regex
                } else {
                    crate::regex_tree::RegexDeclKind::Token
                },
                multi: is_multi,
                is_my: false,
                is_our: false,
                is_export,
                export_tags,
            },
        ))
    }
}

/// Parse a plain (non-`my`/`our`-prefixed) `grammar` declaration. Package-
/// scoped like a bare `class Foo { }`: not lexical.
pub(crate) fn grammar_decl(input: &str) -> PResult<'_, Stmt> {
    grammar_decl_inner(input, false)
}

/// Parse a `my`/`our`-prefixed `grammar` declaration (called from the
/// my/our keyword dispatcher, which has already stripped the `my`/`our`
/// keyword off `input` and knows whether it was `my`). `is_lexical` mirrors
/// `class_decl_body`'s `!is_our` threading — without it, `my grammar Foo { }`
/// silently registered exactly like a package-scoped `grammar Foo { }` (the
/// hardcoded `is_lexical: false` below never varied), so it got none of
/// ADR-0047's per-declaration-site identity protection: two sibling
/// `my grammar Foo { }` blocks collapsed to the same registry entry instead
/// of two distinct grammars.
pub(crate) fn grammar_decl_my(input: &str, is_lexical: bool) -> PResult<'_, Stmt> {
    grammar_decl_inner(input, is_lexical)
}

fn grammar_decl_inner(input: &str, is_lexical: bool) -> PResult<'_, Stmt> {
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
    let mut parent_args: Vec<(String, Vec<Expr>)> = Vec::new();
    while let Some(r2) = keyword("is", r) {
        let (r2, _) = ws1(r2)?;
        let (r2, parent_name) = qualified_ident(r2)?;
        // A lowercase `is` name (`export`, `rw`, `repr('...')`, a custom trait)
        // is a trait, NOT a parent grammar — only an uppercase/indirect name is
        // a superclass. Without this, `grammar Foo is export { }` would try to
        // inherit from a nonexistent `export`. Mirrors the class-decl loop.
        if parent_name.starts_with(|c: char| c.is_ascii_uppercase())
            || parent_name.starts_with("::")
        {
            let (r2, bracket_suffix) =
                crate::parser::stmt::class::parse_optional_bracket_suffix(r2)?;
            let full_name = format!("{}{}", parent_name, bracket_suffix);
            if let Some(exprs) = crate::parser::stmt::class::parse_bracket_arg_exprs(bracket_suffix)
            {
                parent_args.push((full_name.clone(), exprs));
            }
            parents.push(full_name);
            let (r2, _) = ws(r2)?;
            r = r2;
        } else {
            let r2 = crate::parser::helpers::skip_balanced_parens(r2);
            let (r2, _) = ws(r2)?;
            r = r2;
        }
    }
    // Default parent is Grammar if no `is` clause. A module-local
    // `grammar Grammar` (qualified to `Mod::Grammar`) must still inherit the
    // built-in Grammar; only a genuine top-level `grammar Grammar` that IS the
    // built-in would self-parent, and that is dropped at registration (see the
    // self-parent filter in `exec_register_class_op`). Decided before the
    // `does` clauses are read: a composed role is not an `is` parent, so
    // `grammar G does R { }` must still inherit Grammar.
    let mut implicit_grammar_parent = parents.is_empty();
    if implicit_grammar_parent {
        parents.push("Grammar".to_string());
    }
    let mut does_parents = Vec::new();
    while let Some(r2) = keyword("does", r) {
        let (r2, _) = ws1(r2)?;
        let (r2, role_name) = qualified_ident(r2)?;
        let (r2, _) = ws(r2)?;
        let (r2, bracket_suffix) = crate::parser::stmt::class::parse_optional_bracket_suffix(r2)?;
        let full_name = format!("{}{}", role_name, bracket_suffix);
        if let Some(exprs) = crate::parser::stmt::class::parse_bracket_arg_exprs(bracket_suffix) {
            parent_args.push((full_name.clone(), exprs));
        }
        // The role-composition loop in `register_class_decl` walks `parents`
        // and uses `does_parents` only to tell composition from punning, so a
        // `does` role must appear in both — otherwise the grammar composes
        // nothing at all.
        parents.push(full_name.clone());
        does_parents.push(full_name);
        let (r2, _) = ws(r2)?;
        r = r2;
    }
    let (rest, mut body) = {
        let _pkg = super::super::simple::push_package_path(&name);
        block(r)?
    };
    // A `grammar G { also is Base; }` body carries its parent the same way a
    // `class` body does; without this extraction the bare `is(also, Base)`
    // infix expression would reach the runtime as "two terms in a row".
    let mut body_parents: Vec<String> = Vec::new();
    body.retain(|stmt| {
        if let Some(parent_name) = crate::parser::stmt::class::stmt_also_is_parent(stmt) {
            crate::parser::stmt::class::push_also_is_parent(
                &mut parents,
                &mut body_parents,
                &mut implicit_grammar_parent,
                parent_name,
            );
            false
        } else {
            true
        }
    });
    super::super::simple::register_user_type(&name);
    // `grammar G { ... }.parse($s)` is one expression; see `reject_trailing_postfix`.
    super::reject_trailing_postfix(rest)?;
    Ok((
        rest,
        Stmt::ClassDecl {
            name: Symbol::intern(&name),
            name_expr: None,
            parents,
            class_is_rw: false,
            is_hidden: false,
            is_lexical,
            hidden_parents: vec![],
            does_parents,
            repr: None,
            body,
            language_version: super::super::simple::current_language_version(),
            custom_traits: Vec::new(),
            is_unit: false,
            implicit_grammar_parent,
            is_grammar: true,
            decl_id: crate::ast::next_class_decl_id(),
            parent_args,
            body_parents,
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
    let (rest, body) = {
        let _pkg = super::super::simple::push_package_path(&name);
        block(rest)?
    };
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
        if trait_name == "ver" || trait_name == "auth" || trait_name == "api" {
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
