use super::*;
use crate::ast::{Expr, Stmt};
use crate::parser::helpers::{ws, ws1};
use crate::parser::parse_result::{PError, PResult};
use crate::parser::primary::var::parse_ident_with_hyphens;
use crate::parser::stmt::keyword;
use crate::symbol::Symbol;
use std::sync::atomic::{AtomicU64, Ordering};

static ANON_CLASS_COUNTER: AtomicU64 = AtomicU64::new(0);
static ANON_ROLE_COUNTER: AtomicU64 = AtomicU64::new(0);

fn parse_qualified_ident_with_hyphens(input: &str) -> PResult<'_, String> {
    let (mut rest, first) = parse_ident_with_hyphens(input)?;
    let mut full = first.to_string();
    while let Some(after) = rest.strip_prefix("::") {
        let (r2, part) = parse_ident_with_hyphens(after)?;
        full.push_str("::");
        full.push_str(part);
        rest = r2;
    }
    Ok((rest, full))
}

/// Parse a class expression: `class { ... }`, `class Foo { ... }`, or `class :: is Parent { ... }`
/// Named classes in expression context register the class AND return the type object.
pub(crate) fn anon_class_expr(input: &str) -> PResult<'_, Expr> {
    // Accept optional declarator prefixes used in expression context (e.g. `my class ...`).
    let input = if let Some(r) = keyword("my", input).or_else(|| keyword("our", input)) {
        let (r, _) = ws1(r)?;
        r
    } else {
        input
    };
    let rest = keyword("class", input).ok_or_else(|| PError::expected("anonymous class"))?;
    let (rest, _) = ws(rest)?;

    // Accept `class { ... }`, `class :: ...` (anonymous with optional traits),
    // or `class Name ...` (named class in expression context)
    let (rest, name, parents, does_roles) = if let Some(r) = rest.strip_prefix("::") {
        // Skip `::` (anonymous name placeholder)
        let (r, _) = ws(r)?;
        // Parse `is Parent` / `does Role` clauses
        let mut parents = Vec::new();
        let mut does_roles: Vec<String> = Vec::new();
        let mut r = r;
        loop {
            if let Some(r2) = keyword("is", r) {
                let (r2, _) = ws1(r2)?;
                let (r2, parent) = parse_qualified_ident_with_hyphens(r2)?;
                parents.push(parent);
                let (r2, _) = ws(r2)?;
                r = r2;
            } else if let Some(r2) = keyword("does", r) {
                let (r2, _) = ws1(r2)?;
                let (r2, role) = parse_qualified_ident_with_hyphens(r2)?;
                parents.push(role.clone());
                does_roles.push(role);
                let (r2, _) = ws(r2)?;
                r = r2;
            } else {
                break;
            }
        }
        let id = ANON_CLASS_COUNTER.fetch_add(1, Ordering::Relaxed);
        (r, format!("__ANON_CLASS_{id}__"), parents, does_roles)
    } else if rest.starts_with('{') {
        let id = ANON_CLASS_COUNTER.fetch_add(1, Ordering::Relaxed);
        (rest, format!("__ANON_CLASS_{id}__"), Vec::new(), Vec::new())
    } else if rest.starts_with(crate::parser::helpers::is_raku_identifier_start) {
        // Named class in expression context: `class Foo { ... }`. The name may
        // be QUALIFIED — `class X::Foo is Exception {}.new.throw` is the shape
        // roast/S04-exceptions/exceptions-alternatives.t uses — so stopping at
        // the first `::` would leave `::Foo is Exception` unparsed. Raku class
        // names may also begin with non-ASCII identifier characters.
        let (r, class_name) = parse_qualified_ident_with_hyphens(rest)?;
        let (r, _) = ws(r)?;
        // Parse optional `is Parent` / `does Role` clauses
        let mut parents = Vec::new();
        let mut does_roles: Vec<String> = Vec::new();
        let mut r = r;
        loop {
            if let Some(r2) = keyword("is", r) {
                let (r2, _) = ws1(r2)?;
                let (r2, parent) = parse_qualified_ident_with_hyphens(r2)?;
                parents.push(parent);
                let (r2, _) = ws(r2)?;
                r = r2;
            } else if let Some(r2) = keyword("does", r) {
                let (r2, _) = ws1(r2)?;
                let (r2, role) = parse_qualified_ident_with_hyphens(r2)?;
                parents.push(role.clone());
                does_roles.push(role);
                let (r2, _) = ws(r2)?;
                r = r2;
            } else {
                break;
            }
        }
        (r, class_name.to_string(), parents, does_roles)
    } else {
        return Err(PError::expected("'{' for anonymous class"));
    };

    if !rest.starts_with('{') {
        return Err(PError::expected("'{' for anonymous class body"));
    }

    let (rest, mut body) = parse_block_body(rest)?;
    // Insert DoesDecl statements at the beginning of the body for `does` clauses
    for role_name in does_roles.iter().rev() {
        body.insert(
            0,
            Stmt::DoesDecl {
                name: Symbol::intern(role_name),
                args: None,
            },
        );
    }
    Ok((
        rest,
        Expr::DoStmt(Box::new(Stmt::ClassDecl {
            name: Symbol::intern(&name),
            name_expr: None,
            parents,
            class_is_rw: false,
            is_hidden: false,
            is_lexical: false,
            hidden_parents: Vec::new(),
            does_parents: does_roles,
            repr: None,
            body,
            language_version: crate::parser::current_language_version(),
            custom_traits: Vec::new(),
            is_unit: false,
            decl_id: crate::ast::next_class_decl_id(),
            parent_args: Vec::new(),
        })),
    ))
}

/// Parse a grammar expression: `grammar { ... }`, `grammar :: { ... }`, or the
/// named `grammar G { ... }` — the last one reached through the expression path
/// whenever a postfix follows the closing brace (`grammar G { … }.parse($s)`),
/// which the statement parser declines for exactly that reason.
pub(crate) fn anon_grammar_expr(input: &str) -> PResult<'_, Expr> {
    let rest = keyword("grammar", input).ok_or_else(|| PError::expected("anonymous grammar"))?;
    let (rest, _) = ws(rest)?;
    let rest = rest.strip_prefix("::").map_or(rest, |r| r.trim_start());
    let (rest, name) = if rest.starts_with('{') {
        let id = ANON_CLASS_COUNTER.fetch_add(1, Ordering::Relaxed);
        (rest, format!("__ANON_GRAMMAR_{id}__"))
    } else if rest.starts_with(crate::parser::helpers::is_raku_identifier_start) {
        // Same identifier-start class as the class/role expression paths: a
        // grammar name may begin with any Unicode identifier character, so an
        // ASCII-only gate here would reject `anon grammar þ { ... }` (and the
        // postfixed `(grammar þ { ... }).^name`) that the statement path accepts.
        let (r, grammar_name) = parse_qualified_ident_with_hyphens(rest)?;
        let (r, _) = ws(r)?;
        (r, grammar_name)
    } else {
        return Err(PError::expected("'{' for anonymous grammar"));
    };
    if !rest.starts_with('{') {
        return Err(PError::expected("'{' for anonymous grammar"));
    }
    let (rest, body) = parse_block_body(rest)?;
    Ok((
        rest,
        Expr::DoStmt(Box::new(Stmt::Package {
            name: Symbol::intern(&name),
            body,
            kind: crate::ast::PackageKind::Grammar,
            is_unit: false,
            is_my: false,
        })),
    ))
}

/// Parse a role expression: `role { ... }`, `role :: { ... }`, or the named
/// `role R { ... }` — the last one reached through the expression path whenever
/// a postfix follows the closing brace (`role R { … }.^name`), which the
/// statement parser declines for exactly that reason.
pub(crate) fn anon_role_expr(input: &str) -> PResult<'_, Expr> {
    let rest = keyword("role", input).ok_or_else(|| PError::expected("anonymous role"))?;
    let (rest, _) = ws(rest)?;
    // Accept optional `::` (null name) before the block
    let rest = if let Some(r) = rest.strip_prefix("::") {
        let (r, _) = ws(r)?;
        r
    } else {
        rest
    };
    let (rest, name) = if rest.starts_with('{') {
        let id = ANON_ROLE_COUNTER.fetch_add(1, Ordering::Relaxed);
        (rest, format!("__ANON_ROLE_{id}__"))
    } else if rest.starts_with(crate::parser::helpers::is_raku_identifier_start) {
        // Not just uppercase/`_`: a role name in expression/argument position
        // (e.g. `.^mixin(role is-marked { ... })`) is a plain identifier and
        // may be lowercase and/or kebab-cased, same as at statement position.
        let (r, role_name) = parse_qualified_ident_with_hyphens(rest)?;
        let (r, _) = ws(r)?;
        (r, role_name)
    } else {
        return Err(PError::expected("'{' for anonymous role"));
    };
    // An expression-position role declaration accepts the same parametric
    // signature as a statement-level `role Name[...] { ... }`. Without this,
    // the parser left `[...]` behind and backtracked to treating `role` as a
    // bareword (then `Name` could be misread as a `Z` metaoperator).
    let (rest, (type_params, type_param_defs)) =
        crate::parser::stmt::class::parse_optional_role_type_params(rest)?;
    if !rest.starts_with('{') {
        return Err(PError::expected("'{' for anonymous role"));
    }
    let (rest, body) = parse_block_body(rest)?;
    Ok((
        rest,
        Expr::DoStmt(Box::new(Stmt::RoleDecl {
            name: Symbol::intern(&name),
            type_params,
            type_param_defs,
            is_export: false,
            export_tags: Vec::new(),
            body,
            is_rw: false,
            language_version: crate::parser::current_language_version(),
            custom_traits: Vec::new(),
        })),
    ))
}

/// Indirect object notation: `new Foo:` / `method Type: args` desugars to
/// `Type.method(args)` (rakudo still accepts this Perl-5-style form —
/// integration/weird-errors.t 32 uses `$ = new Foo:`). Deliberately narrow:
/// the invocant must be a type-looking identifier (uppercase start, optional
/// `::` qualification) with the colon attached directly and followed by
/// whitespace or a statement/expression terminator, so labels (`Foo:` at
/// statement start), `::`-qualified names, smileys (`Foo:D`) and colonpair
/// adverbs never match.
pub(crate) fn indirect_method_call(input: &str) -> PResult<'_, Expr> {
    let (r, method) = crate::parser::stmt::ident_pub(input)?;
    if crate::parser::primary::ident::is_keyword(&method) {
        return Err(PError::expected("indirect method call"));
    }
    if !r.starts_with([' ', '\t']) {
        return Err(PError::expected("indirect method call"));
    }
    let (r, _) = ws(r)?;
    let (r, type_name) = crate::parser::stmt::ident_pub(r)?;
    if !type_name.starts_with(|c: char| c.is_uppercase()) {
        return Err(PError::expected("indirect method call"));
    }
    let r = r
        .strip_prefix(':')
        .ok_or_else(|| PError::expected("indirect method call"))?;
    match r.chars().next() {
        None => {}
        Some(c) if c.is_whitespace() || matches!(c, ';' | ')' | '}' | ',' | '#') => {}
        _ => return Err(PError::expected("indirect method call")),
    }
    // Optional comma-separated argument list on the same statement.
    let mut args = Vec::new();
    let (mut rest, _) = ws_inner(r);
    if !(rest.is_empty() || rest.starts_with(';') || rest.starts_with('}') || rest.starts_with(')'))
    {
        loop {
            let Ok((r2, arg)) = crate::parser::expr::expression(rest) else {
                break;
            };
            args.push(arg);
            let (r2, _) = ws_inner(r2);
            if let Some(r3) = r2.strip_prefix(',') {
                let (r3, _) = ws_inner(r3);
                rest = r3;
            } else {
                rest = r2;
                break;
            }
        }
    }
    Ok((
        rest,
        Expr::MethodCall {
            target: Box::new(Expr::BareWord(type_name)),
            name: crate::symbol::Symbol::intern(&method),
            args,
            modifier: None,
            quoted: false,
        },
    ))
}
