//! Post-parse check for `&?ROUTINE` used outside the lexical scope of a routine
//! in an `EVAL`'d compilation unit.
//!
//! `&?ROUTINE` is resolved *lexically at compile time* to the innermost
//! enclosing `sub`/`method`/`token`/`rule`/`regex`. An `EVAL`'d string is its own
//! compilation unit, so its mainline has no enclosing routine no matter what the
//! caller's runtime routine stack looks like — rakudo answers
//! `X::Undeclared::Symbols` for `EVAL '&?ROUTINE'` even when the `EVAL` itself
//! sits inside a `sub`, and conversely accepts
//! `EVAL 'sub g { &?ROUTINE.name }; g()'` from the mainline.
//!
//! mutsu used to approximate this with a textual `code.contains("&?ROUTINE")`
//! test gated on `self.routine_stack.is_empty()`, which was wrong in *both*
//! directions: it accepted a mainline `&?ROUTINE` inside the snippet whenever the
//! caller happened to be in a routine (this is what let
//! `throws-like { EVAL 'my $baz = try { &?ROUTINE.name };' }` report "code did
//! not die" under the real `Test` module, whose `throws-like` calls the Callable
//! from Raku-level code), and rejected a snippet that declared its own routine
//! around the use.
//!
//! This walker mirrors the lexical rule structurally instead. It carries one
//! boolean, `in_routine`:
//!
//! * `sub`/`method`/`token`/`rule`/`regex`/`proto` declarations and anonymous
//!   `sub { }` expressions set it true for their body — they *are* `Routine`s.
//! * A bare block, a pointy `-> { }` (`Block`, not `Routine`), a `class`/`role`
//!   body and every control-flow construct preserve it, so `&?ROUTINE` inside a
//!   block nested in a routine is fine, and inside a pointy at unit mainline is
//!   not (measured against `raku`).
//!
//! Like `parser::whenever_scope`, the walker is deliberately conservative: an
//! unhandled container is simply not recursed into, which can only *miss* an
//! offending use (leaving today's behaviour), never invent one.

use crate::ast::{Expr, Stmt};
use crate::runtime::Interpreter;
use crate::value::RuntimeError;

impl Interpreter {
    /// Reject `&?ROUTINE` used outside a routine in an `EVAL`'d unit, the way
    /// rakudo's compile-time lexical lookup does. Run alongside the other
    /// `check_eval_*` passes, on the snippet's own parsed statements — the
    /// caller's runtime routine stack is irrelevant (see the module docs).
    pub(crate) fn check_eval_routine_magicals(stmts: &[Stmt]) -> Result<(), RuntimeError> {
        match find_routine_magical_outside_routine(stmts) {
            Some(name) => Err(RuntimeError::undeclared_symbols(format!(
                "Undeclared name:\n    {name} used at line 1"
            ))),
            None => Ok(()),
        }
    }
}

/// The name of the first routine-scoped magical used outside a routine, or
/// `None` when every use is properly enclosed.
pub(crate) fn find_routine_magical_outside_routine(stmts: &[Stmt]) -> Option<String> {
    let mut found: Option<String> = None;
    walk_stmts(stmts, false, &mut found);
    found
}

fn walk_stmts(stmts: &[Stmt], in_routine: bool, found: &mut Option<String>) {
    for s in stmts {
        walk_stmt(s, in_routine, found);
    }
}

fn walk_stmt(stmt: &Stmt, in_routine: bool, found: &mut Option<String>) {
    if found.is_some() {
        return;
    }
    match stmt {
        // Routine boundaries: everything below them has an enclosing routine.
        Stmt::SubDecl { body, .. }
        | Stmt::MethodDecl { body, .. }
        | Stmt::TokenDecl { body, .. }
        | Stmt::RuleDecl { body, .. }
        | Stmt::ProtoDecl { body, .. } => walk_stmts(body, true, found),

        // Package-like bodies are not routines: `class C { &?ROUTINE }` is as
        // undeclared as a mainline use, while `class C { method m { … } }` is
        // covered by the MethodDecl arm above.
        Stmt::ClassDecl { body, .. }
        | Stmt::RoleDecl { body, .. }
        | Stmt::Package { body, .. }
        | Stmt::Block(body)
        | Stmt::SyntheticBlock(body)
        | Stmt::Default(body)
        | Stmt::Catch(body)
        | Stmt::Control(body)
        | Stmt::Given { body, .. }
        | Stmt::When { body, .. }
        | Stmt::While { body, .. }
        | Stmt::Subtest { body, .. }
        | Stmt::React { body, .. } => walk_stmts(body, in_routine, found),
        Stmt::Whenever { supply, body, .. } => {
            walk_expr(supply, in_routine, found);
            walk_stmts(body, in_routine, found);
        }
        Stmt::Phaser { body, .. } => walk_stmts(body, in_routine, found),
        Stmt::For { body, iterable, .. } => {
            walk_expr(iterable, in_routine, found);
            walk_stmts(body, in_routine, found);
        }
        Stmt::Loop { body, init, .. } => {
            if let Some(init) = init {
                walk_stmt(init, in_routine, found);
            }
            walk_stmts(body, in_routine, found);
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            walk_expr(cond, in_routine, found);
            walk_stmts(then_branch, in_routine, found);
            walk_stmts(else_branch, in_routine, found);
        }
        Stmt::Label { stmt, .. } => walk_stmt(stmt, in_routine, found),

        Stmt::Expr(e)
        | Stmt::VarDecl { expr: e, .. }
        | Stmt::Assign { expr: e, .. }
        | Stmt::Return(e)
        | Stmt::Die(e)
        | Stmt::Fail(e)
        | Stmt::Take(e, _) => walk_expr(e, in_routine, found),
        Stmt::Say(es) | Stmt::Put(es) | Stmt::Print(es) | Stmt::Note(es) => {
            for e in es {
                walk_expr(e, in_routine, found);
            }
        }

        _ => {}
    }
}

fn walk_expr(expr: &Expr, in_routine: bool, found: &mut Option<String>) {
    if found.is_some() {
        return;
    }
    match expr {
        // The use itself. `&?BLOCK` is deliberately NOT checked: every block —
        // the unit mainline included — is a `Block`, so it is always declared.
        Expr::CodeVar(name) if name == "?ROUTINE" => {
            if !in_routine {
                *found = Some(name.clone());
            }
        }

        // `AnonSub` carries `is_block`, which separates a bare block `{ }` (a
        // `Block`: NOT a routine boundary) from an anonymous `sub { }` (a
        // `Routine`: it does supply `&?ROUTINE`).
        Expr::AnonSub {
            body,
            is_block: true,
            ..
        } => walk_stmts(body, in_routine, found),
        Expr::AnonSub { body, .. } => walk_stmts(body, true, found),

        // `AnonSubParams` and `Lambda` are ambiguous in the AST: a pointy block
        // `-> { }` / `-> $x { }` (a `Block`, which does NOT supply `&?ROUTINE` —
        // measured: `EVAL 'my $z = -> { &?ROUTINE }; $z()'` is
        // X::Undeclared::Symbols in raku) and a parameterised anonymous
        // `sub ($x) { }` (which does) both lower to them, with nothing left to
        // tell them apart. Treat them as routine boundaries: per this module's
        // conservatism rule that can only *miss* an offending pointy-block use,
        // where the alternative would wrongly reject a legal `sub ($x) { … }`.
        Expr::AnonSubParams { body, .. } | Expr::Lambda { body, .. } => {
            walk_stmts(body, true, found)
        }
        Expr::Block(body) | Expr::Gather(body) => walk_stmts(body, in_routine, found),
        Expr::DoBlock { body, .. } => walk_stmts(body, in_routine, found),
        Expr::DoStmt(s) => walk_stmt(s, in_routine, found),
        Expr::Try { body, catch } => {
            walk_stmts(body, in_routine, found);
            if let Some(catch) = catch {
                walk_stmts(catch, in_routine, found);
            }
        }

        Expr::Grouped(inner)
        | Expr::WhateverCurry(inner)
        | Expr::Itemize(inner)
        | Expr::Eager(inner)
        | Expr::ZenSlice(inner)
        | Expr::PositionalPair(inner)
        | Expr::DeitemizeForBind(inner) => walk_expr(inner, in_routine, found),
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => {
            walk_expr(expr, in_routine, found)
        }
        Expr::Binary { left, right, .. } => {
            walk_expr(left, in_routine, found);
            walk_expr(right, in_routine, found);
        }
        Expr::AssignExpr { expr, .. } => walk_expr(expr, in_routine, found),
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            walk_expr(cond, in_routine, found);
            walk_expr(then_expr, in_routine, found);
            walk_expr(else_expr, in_routine, found);
        }
        Expr::ChainedCompare { operands, .. } => {
            for o in operands {
                walk_expr(o, in_routine, found);
            }
        }
        Expr::InfixFunc { left, right, .. } => {
            walk_expr(left, in_routine, found);
            for r in right {
                walk_expr(r, in_routine, found);
            }
        }
        Expr::ArrayLiteral(items)
        | Expr::BracketArray(items, _)
        | Expr::CaptureLiteral(items)
        | Expr::StringInterpolation(items) => {
            for i in items {
                walk_expr(i, in_routine, found);
            }
        }
        Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => {
            for a in args {
                walk_expr(a, in_routine, found);
            }
        }
        Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
            walk_expr(target, in_routine, found);
            for a in args {
                walk_expr(a, in_routine, found);
            }
        }
        Expr::Index { target, index, .. } => {
            walk_expr(target, in_routine, found);
            walk_expr(index, in_routine, found);
        }

        _ => {}
    }
}
