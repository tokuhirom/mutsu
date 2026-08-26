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

use crate::ast::{
    CallArg, Expr, PlaceholderBodyKind, Stmt, placeholder_body_kind, placeholder_body_kind_expr,
};

/// Check if a bare variable reference (`$name` or `$name = ...`) appears
/// before the corresponding placeholder variable (`$^name`) in statement
/// order, within this block's own placeholder scope (see module docs).
///
/// This must be a single left-to-right walk across all of `stmts`, not two
/// independent whole-statement containment checks: `$b + $^b` in ONE
/// statement has the placeholder appear textually after the bare use, but
/// checking "does this statement contain `$^b` anywhere" before "does it
/// contain a bare `$b` anywhere" sees both as true regardless of their
/// relative position within the expression tree, so the ordering violation
/// was missed for same-statement cases. `OrderState` threads a running
/// "have we passed the placeholder yet" flag through the walk itself,
/// mirroring AST child evaluation order (e.g. left-then-right for
/// `Expr::Binary`), so a bare use is only flagged if the placeholder truly
/// has not been evaluated yet at that point in the tree.
pub(crate) fn bare_precedes_placeholder(stmts: &[Stmt], bare_name: &str) -> bool {
    let ph_name = format!("^{bare_name}");
    let mut state = OrderState {
        bare_name,
        ph_name: &ph_name,
        ph_seen: false,
        bare_before: false,
    };
    for stmt in stmts {
        order_check_stmt(stmt, &mut state);
        if state.bare_before {
            return true;
        }
    }
    false
}

/// Running state for the order-sensitive walk in `bare_precedes_placeholder`.
struct OrderState<'a> {
    bare_name: &'a str,
    ph_name: &'a str,
    ph_seen: bool,
    bare_before: bool,
}

impl OrderState<'_> {
    fn visit_var(&mut self, name: &str) {
        if self.bare_before {
            return;
        }
        if name == self.ph_name {
            self.ph_seen = true;
        } else if name == self.bare_name && !self.ph_seen {
            self.bare_before = true;
        }
    }
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
        // The header is always checked in THIS scope; from `While` on below,
        // the body joins this scope exactly when the ADR-0048 oracle says
        // `Transparent` — see `placeholder_body_kind` in `ast.rs` for the
        // full table. `If`'s body is a deliberate, pre-existing exception:
        // unlike `collect_ph_stmt_shallow`, this walk never descends an
        // `If`'s branches even for a statement modifier, so it is left
        // exactly as before rather than driven by the oracle.
        Stmt::If { cond, .. } => check_bare_var_expr(cond, var_name, found),
        Stmt::While { cond, body, .. } => {
            check_bare_var_expr(cond, var_name, found);
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    check_bare_var_stmt(s, var_name, found);
                }
            }
        }
        Stmt::For { iterable, body, .. } => {
            check_bare_var_expr(iterable, var_name, found);
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    check_bare_var_stmt(s, var_name, found);
                }
            }
        }
        Stmt::Loop { body, .. } | Stmt::React { body } => {
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    check_bare_var_stmt(s, var_name, found);
                }
            }
        }
        // Only the supply source is checked here — see `placeholder_body_kind`'s
        // `Whenever` doc for why the body is never descended in this walk.
        Stmt::Whenever { supply, .. } => check_bare_var_expr(supply, var_name, found),
        Stmt::Block(body)
        | Stmt::SyntheticBlock(body)
        | Stmt::Default(body)
        | Stmt::Catch(body)
        | Stmt::Control(body)
        | Stmt::RoleDecl { body, .. } => {
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    check_bare_var_stmt(s, var_name, found);
                }
            }
        }
        Stmt::Phaser { body, .. } => {
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    check_bare_var_stmt(s, var_name, found);
                }
            }
        }
        Stmt::Given { topic, body, .. } => {
            check_bare_var_expr(topic, var_name, found);
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    check_bare_var_stmt(s, var_name, found);
                }
            }
        }
        Stmt::When { cond, body, .. } => {
            check_bare_var_expr(cond, var_name, found);
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    check_bare_var_stmt(s, var_name, found);
                }
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
        // `collect_ph_expr_shallow`). ADR-0033: at this (pre-compile) stage a
        // curry is still an un-expanded `WhateverCurry` marker, not a built
        // `AnonSubParams`/`Lambda`, so descend into its body directly.
        Expr::WhateverCurry(body) => check_bare_var_expr(body, var_name, found),
        Expr::AnonSubParams { body, .. } | Expr::Lambda { body, .. } => {
            if matches!(
                placeholder_body_kind_expr(expr),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    check_bare_var_stmt(s, var_name, found);
                }
            }
        }
        // Real closure boundary: stop, it owns its own scope.
        Expr::AnonSub { .. } => {}
        Expr::Block(stmts) | Expr::Gather(stmts) => {
            if matches!(
                placeholder_body_kind_expr(expr),
                PlaceholderBodyKind::Transparent
            ) {
                for s in stmts {
                    check_bare_var_stmt(s, var_name, found);
                }
            }
        }
        Expr::DoBlock { body, .. } => {
            if matches!(
                placeholder_body_kind_expr(expr),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    check_bare_var_stmt(s, var_name, found);
                }
            }
        }
        Expr::DoStmt(stmt) => check_bare_var_stmt(stmt, var_name, found),
        Expr::Try { body, catch } => {
            if matches!(
                placeholder_body_kind_expr(expr),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    check_bare_var_stmt(s, var_name, found);
                }
                if let Some(c) = catch {
                    for s in c {
                        check_bare_var_stmt(s, var_name, found);
                    }
                }
            }
        }
        Expr::PhaserExpr { body, .. } | Expr::Once { body } => {
            if matches!(
                placeholder_body_kind_expr(expr),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    check_bare_var_stmt(s, var_name, found);
                }
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

/// Order-sensitive statement walk used by `bare_precedes_placeholder`.
/// Mirrors `check_bare_var_stmt`'s scope-boundary decisions exactly, but
/// visits both the bare name and the placeholder name in one pass via
/// `OrderState::visit_var`, so relative position within an expression tree
/// is preserved.
fn order_check_stmt(stmt: &Stmt, state: &mut OrderState) {
    if state.bare_before {
        return;
    }
    match stmt {
        Stmt::Expr(e)
        | Stmt::Return(e)
        | Stmt::Die(e)
        | Stmt::Fail(e)
        | Stmt::Take(e, _)
        | Stmt::Goto(e) => {
            order_check_expr(e, state);
        }
        Stmt::VarDecl { expr, .. } => order_check_expr(expr, state),
        Stmt::Assign { name, expr, .. } => {
            // The assignment target is itself a variable reference.
            state.visit_var(name);
            order_check_expr(expr, state);
        }
        Stmt::Say(es) | Stmt::Put(es) | Stmt::Print(es) | Stmt::Note(es) => {
            for e in es {
                order_check_expr(e, state);
            }
        }
        Stmt::Call { args, .. } => {
            for arg in args {
                match arg {
                    CallArg::Positional(e) | CallArg::Invocant(e) | CallArg::Slip(e) => {
                        order_check_expr(e, state)
                    }
                    CallArg::Named { value: Some(e), .. } => order_check_expr(e, state),
                    CallArg::Named { value: None, .. } => {}
                }
            }
        }
        // `If`'s body is a deliberate, pre-existing exception: unlike
        // `collect_ph_stmt_shallow`, this walk never descends an `If`'s
        // branches even for a statement modifier (mirrors
        // `check_bare_var_stmt`'s same exception), so it is left as-is
        // rather than driven by the oracle. Every other arm below joins the
        // body to this scope exactly when `placeholder_body_kind` (ast.rs,
        // ADR-0048) says `Transparent`.
        Stmt::If { cond, .. } => order_check_expr(cond, state),
        Stmt::While { cond, body, .. } => {
            order_check_expr(cond, state);
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    order_check_stmt(s, state);
                }
            }
        }
        Stmt::For { iterable, body, .. } => {
            order_check_expr(iterable, state);
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    order_check_stmt(s, state);
                }
            }
        }
        Stmt::Loop { body, .. } | Stmt::React { body } => {
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    order_check_stmt(s, state);
                }
            }
        }
        Stmt::Whenever { supply, .. } => order_check_expr(supply, state),
        Stmt::Block(body)
        | Stmt::SyntheticBlock(body)
        | Stmt::Default(body)
        | Stmt::Catch(body)
        | Stmt::Control(body)
        | Stmt::RoleDecl { body, .. } => {
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    order_check_stmt(s, state);
                }
            }
        }
        Stmt::Phaser { body, .. } => {
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    order_check_stmt(s, state);
                }
            }
        }
        Stmt::Given { topic, body, .. } => {
            order_check_expr(topic, state);
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    order_check_stmt(s, state);
                }
            }
        }
        Stmt::When { cond, body, .. } => {
            order_check_expr(cond, state);
            if matches!(
                placeholder_body_kind(stmt),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    order_check_stmt(s, state);
                }
            }
        }
        Stmt::Let { value, index, .. } => {
            if let Some(e) = value {
                order_check_expr(e, state);
            }
            if let Some(e) = index {
                order_check_expr(e, state);
            }
        }
        Stmt::TempMethodAssign {
            method_args, value, ..
        } => {
            for e in method_args {
                order_check_expr(e, state);
            }
            order_check_expr(value, state);
        }
        Stmt::Label { stmt, .. } => order_check_stmt(stmt, state),
        Stmt::SubsetDecl {
            predicate: Some(predicate),
            ..
        } => order_check_expr(predicate, state),
        _ => {}
    }
}

/// Order-sensitive expression walk used by `bare_precedes_placeholder`.
/// Mirrors `check_bare_var_expr`'s scope-boundary decisions exactly, but
/// visits both the bare name and the placeholder name in one pass via
/// `OrderState::visit_var`, so relative position within the tree is
/// preserved (e.g. left-then-right for `Expr::Binary`).
fn order_check_expr(expr: &Expr, state: &mut OrderState) {
    if state.bare_before {
        return;
    }
    match expr {
        Expr::Var(name) => state.visit_var(name),
        Expr::Binary { left, right, .. } => {
            order_check_expr(left, state);
            order_check_expr(right, state);
        }
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => order_check_expr(expr, state),
        Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
            order_check_expr(target, state);
            for a in args {
                order_check_expr(a, state);
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
            order_check_expr(target, state);
            order_check_expr(name_expr, state);
            for a in args {
                order_check_expr(a, state);
            }
        }
        Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => {
            for a in args {
                order_check_expr(a, state);
            }
        }
        Expr::CallOn { target, args } => {
            order_check_expr(target, state);
            for a in args {
                order_check_expr(a, state);
            }
        }
        Expr::Index { target, index, .. } => {
            order_check_expr(target, state);
            order_check_expr(index, state);
        }
        Expr::IndexAssign {
            target,
            index,
            value,
            ..
        } => {
            order_check_expr(target, state);
            order_check_expr(index, state);
            order_check_expr(value, state);
        }
        Expr::MultiDimIndexAssign {
            target,
            dimensions,
            value,
        } => {
            order_check_expr(target, state);
            for d in dimensions {
                order_check_expr(d, state);
            }
            order_check_expr(value, state);
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            order_check_expr(cond, state);
            order_check_expr(then_expr, state);
            order_check_expr(else_expr, state);
        }
        Expr::AssignExpr { expr, .. } | Expr::PositionalPair(expr) | Expr::ZenSlice(expr) => {
            order_check_expr(expr, state)
        }
        Expr::Exists { target, arg, .. } => {
            order_check_expr(target, state);
            if let Some(a) = arg {
                order_check_expr(a, state);
            }
        }
        Expr::ArrayLiteral(es)
        | Expr::BracketArray(es, _)
        | Expr::StringInterpolation(es)
        | Expr::CaptureLiteral(es) => {
            for e in es {
                order_check_expr(e, state);
            }
        }
        // ADR-0033: descend into an un-expanded WhateverCurry body the same
        // way as an already-built WhateverCode (see `check_bare_var_expr`
        // above for the matching rationale).
        Expr::WhateverCurry(body) => order_check_expr(body, state),
        Expr::AnonSubParams { body, .. } | Expr::Lambda { body, .. } => {
            if matches!(
                placeholder_body_kind_expr(expr),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    order_check_stmt(s, state);
                }
            }
        }
        Expr::AnonSub { .. } => {}
        Expr::Block(stmts) | Expr::Gather(stmts) => {
            if matches!(
                placeholder_body_kind_expr(expr),
                PlaceholderBodyKind::Transparent
            ) {
                for s in stmts {
                    order_check_stmt(s, state);
                }
            }
        }
        Expr::DoBlock { body, .. } => {
            if matches!(
                placeholder_body_kind_expr(expr),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    order_check_stmt(s, state);
                }
            }
        }
        Expr::DoStmt(stmt) => order_check_stmt(stmt, state),
        Expr::Try { body, catch } => {
            if matches!(
                placeholder_body_kind_expr(expr),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    order_check_stmt(s, state);
                }
                if let Some(c) = catch {
                    for s in c {
                        order_check_stmt(s, state);
                    }
                }
            }
        }
        Expr::PhaserExpr { body, .. } | Expr::Once { body } => {
            if matches!(
                placeholder_body_kind_expr(expr),
                PlaceholderBodyKind::Transparent
            ) {
                for s in body {
                    order_check_stmt(s, state);
                }
            }
        }
        Expr::Reduction { expr, .. }
        | Expr::Eager(expr)
        | Expr::Itemize(expr)
        | Expr::Grouped(expr)
        | Expr::DeitemizeForBind(expr) => order_check_expr(expr, state),
        Expr::HyperOp { left, right, .. }
        | Expr::HyperFuncOp { left, right, .. }
        | Expr::MetaOp { left, right, .. } => {
            order_check_expr(left, state);
            order_check_expr(right, state);
        }
        Expr::InfixFunc { left, right, .. } => {
            order_check_expr(left, state);
            for e in right {
                order_check_expr(e, state);
            }
        }
        Expr::Hash(pairs) => {
            for (_, v) in pairs {
                if let Some(e) = v {
                    order_check_expr(e, state);
                }
            }
        }
        _ => {}
    }
}
