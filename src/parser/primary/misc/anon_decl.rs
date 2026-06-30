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
    } else if rest.starts_with(|c: char| c.is_ascii_uppercase() || c == '_') {
        // Named class in expression context: `class Foo { ... }`
        let (r, class_name) = parse_ident_with_hyphens(rest)?;
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
        })),
    ))
}

/// Parse an anonymous grammar expression: `grammar { ... }`
pub(crate) fn anon_grammar_expr(input: &str) -> PResult<'_, Expr> {
    let rest = keyword("grammar", input).ok_or_else(|| PError::expected("anonymous grammar"))?;
    let (rest, _) = ws(rest)?;
    if !rest.starts_with('{') {
        return Err(PError::expected("'{' for anonymous grammar"));
    }
    let id = ANON_CLASS_COUNTER.fetch_add(1, Ordering::Relaxed);
    let name = format!("__ANON_GRAMMAR_{id}__");
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

/// Parse an anonymous role expression: `role { ... }` or `role :: { ... }`
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
    // Must be followed by '{' (no name) to be an anonymous role
    if !rest.starts_with('{') {
        return Err(PError::expected("'{' for anonymous role"));
    }
    let id = ANON_ROLE_COUNTER.fetch_add(1, Ordering::Relaxed);
    let name = format!("__ANON_ROLE_{id}__");
    let (rest, body) = parse_block_body(rest)?;
    Ok((
        rest,
        Expr::DoStmt(Box::new(Stmt::RoleDecl {
            name: Symbol::intern(&name),
            type_params: Vec::new(),
            type_param_defs: Vec::new(),
            is_export: false,
            export_tags: Vec::new(),
            body,
            is_rw: false,
            language_version: crate::parser::current_language_version(),
            custom_traits: Vec::new(),
        })),
    ))
}
