//! Ordering / cross-scope checks for bare `$name` vs placeholder `$^name`
//! uses within the same lexical block.
//!
//! A placeholder parameter (`$^name`) declares its block's `$name` under the
//! *plain* name, so:
//!  - a bare `$name` written **before** the `$^name` that declares it, in the
//!    SAME block, is `X::Placeholder::NonPlaceholder` (if `$name` also
//!    already exists in an outer scope) or `X::Undeclared` (otherwise) —
//!    `bare_precedes_placeholder` below;
//!  - a bare `$name` in a block that has no `$^name` of its own, but where a
//!    STRICTLY NESTED block (an `if`/`for`/`given` BLOCK body, `whenever`, or
//!    a closure) does use `$^name`, is *also* `X::Undeclared` — the inner
//!    block owns that placeholder; it does not leak outward —
//!    `bare_name_shadowed_by_nested_placeholder` below.
//!
//! Both checks need the same notion of "this block's own placeholder scope"
//! that `collect_placeholders_shallow` (in `ast.rs`) already encodes for
//! building a block's own signature. `check_bare_var_stmt`/`check_bare_var_expr`
//! mirror that collector's (`collect_ph_stmt_shallow`/`collect_ph_expr_shallow`)
//! scope-boundary decisions exactly, just checking for one target variable
//! name instead of collecting the vec of a block's own placeholders.

use crate::ast::{CallArg, Expr, Stmt};

/// Check if a bare variable reference (`$name` or `$name = ...`) appears
/// before the corresponding placeholder variable (`$^name`) in statement
/// order, within this block's own placeholder scope (see module docs).
pub(crate) fn bare_precedes_placeholder(stmts: &[Stmt], bare_name: &str) -> bool {
    let ph_name = format!("^{}", bare_name);
    let mut ph_seen = false;
    for stmt in stmts {
        if stmt_contains_var_named(stmt, &ph_name) {
            ph_seen = true;
        }
        if !ph_seen && stmt_references_bare(stmt, bare_name) {
            return true;
        }
    }
    false
}

/// Find a bare name that is referenced in `body`'s own placeholder scope
/// (mirroring `collect_placeholders_shallow`'s boundaries) but is declared as
/// a placeholder (`$^name`) only in a block STRICTLY NESTED inside `body` —
/// e.g. `{ for 1 { $^b }; say $b }`: the inner `for` block owns `$^b`, so it
/// does not make `$b` this block's parameter, and the outer `$b` was never
/// declared. `own_placeholders` is `body`'s own placeholder list (from
/// `collect_placeholders_shallow`); a name already in it is handled by
/// `bare_precedes_placeholder`'s same-scope ordering check instead.
///
/// Returns the first such bare name found (undecorated, no `$` sigil).
pub(crate) fn bare_name_shadowed_by_nested_placeholder(
    body: &[Stmt],
    own_placeholders: &[String],
) -> Option<String> {
    for ph in crate::ast::collect_placeholders(body) {
        // Only scalar placeholders (`^name`, no `@`/`%`/`&` prefix) share a
        // plain name with a bare `$name` use.
        let Some(bare_name) = ph.strip_prefix('^') else {
            continue;
        };
        if own_placeholders.iter().any(|p| p == &ph) {
            continue;
        }
        if body.iter().any(|s| stmt_references_bare(s, bare_name)) {
            return Some(bare_name.to_string());
        }
    }
    None
}

/// Check if a statement contains a variable reference `Var(name)`, within
/// this block's own placeholder scope.
fn stmt_contains_var_named(stmt: &Stmt, var_name: &str) -> bool {
    let mut found = false;
    check_bare_var_stmt(stmt, var_name, &mut found);
    found
}

/// Check if a statement references a bare variable (`Var(name)` or an
/// assignment target named `name`), within this block's own placeholder scope.
fn stmt_references_bare(stmt: &Stmt, bare_name: &str) -> bool {
    // Check assignment target name.
    if let Stmt::Assign { name, .. } = stmt
        && name == bare_name
    {
        return true;
    }
    let mut found = false;
    check_bare_var_stmt(stmt, bare_name, &mut found);
    found
}

/// Scope-aware statement walk for a single target variable name (either a
/// bare name like `"b"` or a placeholder name like `"^b"`). Mirrors
/// `collect_ph_stmt_shallow`'s boundary decisions: descends through
/// statement headers, `while`/`loop`/block-style non-boundary bodies, and
/// statement-MODIFIER bodies, but stops at every construct that opens its own
/// placeholder scope (`if`/`for`/`given` BLOCK bodies, `whenever`, closures).
fn check_bare_var_stmt(stmt: &Stmt, var_name: &str, found: &mut bool) {
    if *found {
        return;
    }
    match stmt {
        Stmt::Expr(e)
        | Stmt::Return(e)
        | Stmt::Die(e)
        | Stmt::Fail(e)
        | Stmt::Take(e, _)
        | Stmt::Goto(e) => {
            check_bare_var_expr(e, var_name, found);
        }
        Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
            check_bare_var_expr(expr, var_name, found)
        }
        Stmt::Say(es) | Stmt::Put(es) | Stmt::Print(es) | Stmt::Note(es) => {
            for e in es {
                check_bare_var_expr(e, var_name, found);
            }
        }
        Stmt::Call { args, .. } => {
            for arg in args {
                match arg {
                    CallArg::Positional(e) | CallArg::Invocant(e) | CallArg::Slip(e) => {
                        check_bare_var_expr(e, var_name, found)
                    }
                    CallArg::Named { value: Some(e), .. } => {
                        check_bare_var_expr(e, var_name, found)
                    }
                    CallArg::Named { value: None, .. } => {}
                }
            }
        }
        // The condition is evaluated in THIS scope; the branches are their
        // OWN `{}` block scope (mirrors `collect_ph_stmt_shallow`'s If arm).
        Stmt::If { cond, .. } => check_bare_var_expr(cond, var_name, found),
        Stmt::While { cond, body, .. } => {
            check_bare_var_expr(cond, var_name, found);
            for s in body {
                check_bare_var_stmt(s, var_name, found);
            }
        }
        // A `for` statement MODIFIER is not a block: its body runs in this
        // scope. A `for` BLOCK owns its own placeholder scope: only the
        // iterable is checked here.
        Stmt::For {
            iterable,
            body,
            is_statement_modifier,
            ..
        } => {
            check_bare_var_expr(iterable, var_name, found);
            if *is_statement_modifier {
                for s in body {
                    check_bare_var_stmt(s, var_name, found);
                }
            }
        }
        Stmt::Loop { body, .. } | Stmt::React { body } => {
            for s in body {
                check_bare_var_stmt(s, var_name, found);
            }
        }
        // A `whenever` body is its own block scope (mirrors
        // `collect_ph_stmt_shallow`'s Whenever arm): only the supply source
        // is checked here.
        Stmt::Whenever { supply, .. } => check_bare_var_expr(supply, var_name, found),
        Stmt::Block(body)
        | Stmt::SyntheticBlock(body)
        | Stmt::Default(body)
        | Stmt::Catch(body)
        | Stmt::Control(body)
        | Stmt::RoleDecl { body, .. } => {
            for s in body {
                check_bare_var_stmt(s, var_name, found);
            }
        }
        Stmt::Phaser { body, .. } => {
            for s in body {
                check_bare_var_stmt(s, var_name, found);
            }
        }
        // The topic is evaluated in THIS scope; the given/with BLOCK body is
        // its own `{}` block scope unless this is a statement modifier
        // (mirrors `collect_ph_stmt_shallow`'s Given arm).
        Stmt::Given {
            topic,
            body,
            is_statement_modifier,
        } => {
            check_bare_var_expr(topic, var_name, found);
            if *is_statement_modifier {
                for s in body {
                    check_bare_var_stmt(s, var_name, found);
                }
            }
        }
        Stmt::When { cond, body } => {
            check_bare_var_expr(cond, var_name, found);
            for s in body {
                check_bare_var_stmt(s, var_name, found);
            }
        }
        Stmt::Let { value, index, .. } => {
            if let Some(e) = value {
                check_bare_var_expr(e, var_name, found);
            }
            if let Some(e) = index {
                check_bare_var_expr(e, var_name, found);
            }
        }
        Stmt::TempMethodAssign {
            method_args, value, ..
        } => {
            for e in method_args {
                check_bare_var_expr(e, var_name, found);
            }
            check_bare_var_expr(value, var_name, found);
        }
        Stmt::Label { stmt, .. } => check_bare_var_stmt(stmt, var_name, found),
        Stmt::SubsetDecl {
            predicate: Some(predicate),
            ..
        } => check_bare_var_expr(predicate, var_name, found),
        _ => {}
    }
}

/// Scope-aware expression walk, mirroring `collect_ph_expr_shallow`'s
/// boundary decisions: stops at real closure boundaries
/// (`AnonSub`/`AnonSubParams`/`Lambda`), but descends through a
/// `WhateverCode`'s body (it owns only its `*`-derived params, not `$^name`
/// placeholders) like that collector does.
fn check_bare_var_expr(expr: &Expr, var_name: &str, found: &mut bool) {
    if *found {
        return;
    }
    match expr {
        Expr::Var(name) if name == var_name => *found = true,
        Expr::Binary { left, right, .. } => {
            check_bare_var_expr(left, var_name, found);
            check_bare_var_expr(right, var_name, found);
        }
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => {
            check_bare_var_expr(expr, var_name, found)
        }
        Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
            check_bare_var_expr(target, var_name, found);
            for a in args {
                check_bare_var_expr(a, var_name, found);
            }
        }
        Expr::DynamicMethodCall {
            target,
            name_expr,
            args,
            ..
        }
        | Expr::HyperMethodCallDynamic {
            target,
            name_expr,
            args,
            ..
        } => {
            check_bare_var_expr(target, var_name, found);
            check_bare_var_expr(name_expr, var_name, found);
            for a in args {
                check_bare_var_expr(a, var_name, found);
            }
        }
        Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => {
            for a in args {
                check_bare_var_expr(a, var_name, found);
            }
        }
        Expr::CallOn { target, args } => {
            check_bare_var_expr(target, var_name, found);
            for a in args {
                check_bare_var_expr(a, var_name, found);
            }
        }
        Expr::Index { target, index, .. } => {
            check_bare_var_expr(target, var_name, found);
            check_bare_var_expr(index, var_name, found);
        }
        Expr::IndexAssign {
            target,
            index,
            value,
            ..
        } => {
            check_bare_var_expr(target, var_name, found);
            check_bare_var_expr(index, var_name, found);
            check_bare_var_expr(value, var_name, found);
        }
        Expr::MultiDimIndexAssign {
            target,
            dimensions,
            value,
        } => {
            check_bare_var_expr(target, var_name, found);
            for d in dimensions {
                check_bare_var_expr(d, var_name, found);
            }
            check_bare_var_expr(value, var_name, found);
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            check_bare_var_expr(cond, var_name, found);
            check_bare_var_expr(then_expr, var_name, found);
            check_bare_var_expr(else_expr, var_name, found);
        }
        Expr::AssignExpr { expr, .. } | Expr::PositionalPair(expr) | Expr::ZenSlice(expr) => {
            check_bare_var_expr(expr, var_name, found)
        }
        Expr::Exists { target, arg, .. } => {
            check_bare_var_expr(target, var_name, found);
            if let Some(a) = arg {
                check_bare_var_expr(a, var_name, found);
            }
        }
        Expr::ArrayLiteral(es)
        | Expr::BracketArray(es, _)
        | Expr::StringInterpolation(es)
        | Expr::CaptureLiteral(es) => {
            for e in es {
                check_bare_var_expr(e, var_name, found);
            }
        }
        // A WhateverCode owns only its `*`-derived params, not `$^name`
        // placeholders — descend through it (mirrors
        // `collect_ph_expr_shallow`).
        Expr::AnonSubParams {
            body,
            is_whatever_code: true,
            ..
        }
        | Expr::Lambda {
            body,
            is_whatever_code: true,
            ..
        } => {
            for s in body {
                check_bare_var_stmt(s, var_name, found);
            }
        }
        // Real closure boundaries: stop, they own their own scope.
        Expr::AnonSub { .. } | Expr::AnonSubParams { .. } | Expr::Lambda { .. } => {}
        Expr::Block(stmts) | Expr::Gather(stmts) => {
            for s in stmts {
                check_bare_var_stmt(s, var_name, found);
            }
        }
        Expr::DoBlock { body, .. } => {
            for s in body {
                check_bare_var_stmt(s, var_name, found);
            }
        }
        Expr::DoStmt(stmt) => check_bare_var_stmt(stmt, var_name, found),
        Expr::Try { body, catch } => {
            for s in body {
                check_bare_var_stmt(s, var_name, found);
            }
            if let Some(c) = catch {
                for s in c {
                    check_bare_var_stmt(s, var_name, found);
                }
            }
        }
        Expr::PhaserExpr { body, .. } | Expr::Once { body } => {
            for s in body {
                check_bare_var_stmt(s, var_name, found);
            }
        }
        Expr::Reduction { expr, .. }
        | Expr::Eager(expr)
        | Expr::Itemize(expr)
        | Expr::Grouped(expr)
        | Expr::DeitemizeForBind(expr) => check_bare_var_expr(expr, var_name, found),
        Expr::HyperOp { left, right, .. }
        | Expr::HyperFuncOp { left, right, .. }
        | Expr::MetaOp { left, right, .. } => {
            check_bare_var_expr(left, var_name, found);
            check_bare_var_expr(right, var_name, found);
        }
        Expr::InfixFunc { left, right, .. } => {
            check_bare_var_expr(left, var_name, found);
            for e in right {
                check_bare_var_expr(e, var_name, found);
            }
        }
        Expr::Hash(pairs) => {
            for (_, v) in pairs {
                if let Some(e) = v {
                    check_bare_var_expr(e, var_name, found);
                }
            }
        }
        _ => {}
    }
}
