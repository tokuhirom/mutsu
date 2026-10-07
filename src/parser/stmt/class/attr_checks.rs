use crate::ast::{Expr, RoutineDeclarator, Stmt};
use crate::ast_visit::{NameKind, Visit, walk_expr, walk_stmt};
use crate::parser::parse_result::PError;
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

/// Whether `expr` mentions an attribute through its twigil (`$!a`, `$.a`,
/// `@!a`, ...) somewhere a missing `self` makes that an
/// `X::Syntax::NoSelf` error.
// Cost: O(n), n = size of the AST of `expr`.
pub(crate) fn expr_uses_attr_twigil(expr: &Expr) -> bool {
    let mut scan = AttrTwigilScan::default();
    scan.visit_expr(expr);
    scan.found
}

/// [`expr_uses_attr_twigil`] over a statement.
// Cost: O(n), n = size of the AST of `stmt`.
pub(crate) fn stmt_uses_attr_twigil(stmt: &Stmt) -> bool {
    let mut scan = AttrTwigilScan::default();
    scan.visit_stmt(stmt);
    scan.found
}

/// The walk of [`expr_uses_attr_twigil`] (ADR-0137): every position of a
/// `self`-less body, except the nested declarations that bring a `self` of
/// their own.
#[derive(Default)]
struct AttrTwigilScan {
    found: bool,
}

impl<'ast> Visit<'ast> for AttrTwigilScan {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        match stmt {
            _ if self.found => {}
            // A nested method has its own `self` (rakudo accepts
            // `sub f { my method m { $!a } }`); a nested type declares and
            // checks its own attributes; a regex body is diagnosed by the regex
            // compiler ("Attribute '$!a' not available inside of a regex").
            Stmt::MethodDecl { .. }
            | Stmt::TokenDecl { .. }
            | Stmt::RuleDecl { .. }
            | Stmt::ClassDecl { .. }
            | Stmt::RoleDecl { .. } => {}
            Stmt::ProtoDecl {
                is_method: true, ..
            } => {}
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        match expr {
            _ if self.found => {}
            // A method literal (`method { $!a }`, `anon method m { }`) has its
            // own `self` too.
            Expr::AnonSubParams {
                declarator: RoutineDeclarator::Method | RoutineDeclarator::Submethod,
                ..
            } => {}
            _ => walk_expr(self, expr),
        }
    }

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        // A `$` variable is spelled without its sigil, an `@`/`%` one too,
        // and an assignment target with it (`@!a = ...`).
        let name = match kind {
            NameKind::Var | NameKind::ArrayVar | NameKind::HashVar => name,
            NameKind::AssignTarget => name.strip_prefix(['@', '%']).unwrap_or(name),
            _ => return,
        };
        // A bare "!" is `$!` (the error variable, e.g. from a no-argument
        // `die`/`fail`), not the `$!attr` private-twigil form — same
        // `name.len() > 1` guard used by every other twigil-detection site.
        // Without it, a bare `die` inside a plain `sub` nested in a class body
        // was misdiagnosed as X::Syntax::NoSelf.
        if name.len() > 1 && name.starts_with(['.', '!']) {
            self.found = true;
        }
    }
}

pub(crate) fn no_self_error() -> PError {
    let msg = "X::Syntax::NoSelf".to_string();
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Syntax::NoSelf"), attrs);
    PError::fatal_with_exception(msg, Box::new(ex))
}

/// `X::Syntax::Regex::NullRegex` — an empty `token`/`regex`/`rule` body (e.g.
/// `regex foo { }`) is a null regex, rejected by Raku at parse time.
pub(crate) fn null_regex_error() -> PError {
    let msg = "Null regex not allowed".to_string();
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Syntax::Regex::NullRegex"), attrs);
    PError::fatal_with_exception(msg, Box::new(ex))
}

pub(crate) fn expr_is_bare_ident(expr: &Expr, ident: &str) -> bool {
    matches!(expr, Expr::Var(name) | Expr::BareWord(name) if name == ident)
}

pub(crate) fn stmt_is_also_is_rw(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Expr(Expr::InfixFunc {
            name, left, right, ..
        }) => {
            name == "is"
                && expr_is_bare_ident(left, "also")
                && right.len() == 1
                && expr_is_bare_ident(&right[0], "rw")
        }
        Stmt::Expr(Expr::Binary {
            left, op, right, ..
        }) => {
            matches!(op, TokenKind::Ident(name) if name == "is")
                && expr_is_bare_ident(left, "also")
                && expr_is_bare_ident(right, "rw")
        }
        _ => false,
    }
}

/// Extract the parent class name from `also is <ClassName>` statements
/// (where ClassName is not `rw`).
pub(crate) fn stmt_also_is_parent(stmt: &Stmt) -> Option<String> {
    match stmt {
        Stmt::Expr(Expr::InfixFunc {
            name, left, right, ..
        }) if name == "is" && expr_is_bare_ident(left, "also") && right.len() == 1 => {
            if let Expr::BareWord(parent) = &right[0]
                && parent != "rw"
            {
                return Some(parent.clone());
            }
            None
        }
        Stmt::Expr(Expr::Binary {
            left, op, right, ..
        }) => {
            if matches!(op, TokenKind::Ident(name) if name == "is")
                && expr_is_bare_ident(left, "also")
                && let Expr::BareWord(parent) = right.as_ref()
                && parent != "rw"
            {
                return Some(parent.clone());
            }
            None
        }
        _ => None,
    }
}

/// Record an `also is <Parent>` parent on a package declaration.
///
/// A `grammar` declarator with no `is` clause carries an implicit `Grammar`
/// parent. An `also is Parent` in the body **replaces** that implicit parent
/// instead of adding a second one, which is what Rakudo does:
/// `grammar G { also is Base }` linearizes as `G, Base, Grammar, ...` — exactly
/// like `grammar G is Base { }` — and never as multiple inheritance from both.
/// Keeping both would make the C3 merge inconsistent for every grammar whose
/// base is itself a grammar ("Inconsistent class hierarchy"), which is how
/// CSS::Grammar::CSS21 (`unit grammar ...; also is CSS::Grammar;`) failed to
/// load.
/// `body_parents` records the same name a second time, marking it as
/// body-positioned so registration can defer resolving it until after the body
/// has run (`Stmt::ClassDecl::body_parents`).
pub(crate) fn push_also_is_parent(
    parents: &mut Vec<String>,
    body_parents: &mut Vec<String>,
    implicit_grammar_parent: &mut bool,
    parent_name: String,
) {
    if *implicit_grammar_parent {
        parents.retain(|p| p != "Grammar");
        *implicit_grammar_parent = false;
    }
    body_parents.push(parent_name.clone());
    parents.push(parent_name);
}

pub(crate) fn reject_no_self_in_subs(body: &[Stmt]) -> Result<(), PError> {
    for stmt in body {
        // The whole declaration: a parameter default is evaluated without a
        // `self` too (`sub f(:$x = $!a) { }`).
        if matches!(stmt, Stmt::SubDecl { .. }) && stmt_uses_attr_twigil(stmt) {
            return Err(no_self_error());
        }
    }
    Ok(())
}

/// Reject attribute-twigil references (`$!a`, `$.a`, ...) inside a `where`
/// constraint on an attribute declaration. The `where` clause is evaluated as a
/// thunk that has no `self`, so such a reference is an X::Syntax::NoSelf error.
/// Note: this only applies to the `where` constraint, not to the `= default`
/// initializer (where `$!a` is allowed, since defaults run with `self`).
pub(crate) fn reject_no_self_in_attr_where(body: &[Stmt]) -> Result<(), PError> {
    for stmt in body {
        if let Stmt::HasDecl {
            where_constraint: Some(wc),
            ..
        } = stmt
            && expr_uses_attr_twigil(wc)
        {
            return Err(no_self_error());
        }
    }
    Ok(())
}

/// Collect no-twigil attribute names from HasDecl statements in a class body.
pub(crate) fn collect_no_twigil_attr_names(body: &[Stmt]) -> Vec<String> {
    let mut names = Vec::new();
    for stmt in body {
        if let Stmt::HasDecl {
            name,
            is_alias: true,
            ..
        } = stmt
        {
            names.push(name.to_string());
        }
    }
    names
}

/// The walk of [`stmt_uses_var_name_at_body_level`] (ADR-0137): does a
/// statement read one of the no-twigil attribute aliases `names` (`has $x`
/// declares `$!x` with the alias `$x`)?
struct VarNameScan<'a> {
    names: &'a [String],
    found: bool,
}

impl<'ast> Visit<'ast> for VarNameScan<'_> {
    // A statement reached from the scanned expression or statement sits in a
    // nested block, which may declare a lexical of the same name
    // (`my $y = { my $x = 1; $x }` is accepted by rakudo), so it is not
    // searched.
    fn visit_stmt(&mut self, _stmt: &'ast Stmt) {}

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if !self.found {
            walk_expr(self, expr);
        }
    }

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        if matches!(kind, NameKind::Var | NameKind::AssignTarget)
            && self.names.iter().any(|n| n == name)
        {
            self.found = true;
        }
    }
}

/// Check if a statement at class body level uses a no-twigil attribute variable.
// Cost: O(n * k), n = size of the statement's own expressions, k = `names.len()`.
pub(crate) fn stmt_uses_var_name_at_body_level(stmt: &Stmt, names: &[String]) -> bool {
    match stmt {
        // Skip method/sub/token/rule declarations — they have `self`
        Stmt::MethodDecl { .. }
        | Stmt::SubDecl { .. }
        | Stmt::TokenDecl { .. }
        | Stmt::RuleDecl { .. }
        | Stmt::ProtoDecl { .. }
        // A nested type has attributes of its own.
        | Stmt::ClassDecl { .. }
        | Stmt::RoleDecl { .. }
        | Stmt::Package { .. } => false,
        // Skip HasDecl — declaring the attr is fine, and its default runs with
        // `self`.
        Stmt::HasDecl { .. } => false,
        _ => {
            let mut scan = VarNameScan {
                names,
                found: false,
            };
            walk_stmt(&mut scan, stmt);
            scan.found
        }
    }
}

/// Reject no-twigil attribute variable usage at class body level (outside methods).
pub(crate) fn reject_no_twigil_attr_at_body_level(body: &[Stmt]) -> Result<(), PError> {
    let names = collect_no_twigil_attr_names(body);
    if names.is_empty() {
        return Ok(());
    }
    for stmt in body {
        if stmt_uses_var_name_at_body_level(stmt, &names) {
            return Err(no_self_error());
        }
    }
    Ok(())
}
