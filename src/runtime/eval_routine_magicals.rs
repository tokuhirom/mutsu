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
//! The walk is the typed AST visitor (ADR-0137), so every child — parameter
//! defaults, regex code blocks, hash values, ... — is searched.

use crate::ast::{Expr, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_stmt, walk_stmts};
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
// Cost: O(n), n = size of the AST.
pub(crate) fn find_routine_magical_outside_routine(stmts: &[Stmt]) -> Option<String> {
    let mut scan = RoutineMagicals::default();
    walk_stmts(&mut scan, stmts);
    scan.found
}

#[derive(Default)]
struct RoutineMagicals {
    /// Whether a routine lexically encloses the current node.
    in_routine: bool,
    found: Option<String>,
}

impl RoutineMagicals {
    fn in_routine(&mut self, f: impl FnOnce(&mut Self)) {
        let saved = std::mem::replace(&mut self.in_routine, true);
        f(self);
        self.in_routine = saved;
    }
}

impl Visit for RoutineMagicals {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        if self.found.is_some() {
            return;
        }
        match stmt {
            // Routine boundaries: everything below them has an enclosing
            // routine. Package-like bodies are not routines: `class C {
            // &?ROUTINE }` is as undeclared as a mainline use.
            Stmt::SubDecl { .. }
            | Stmt::MethodDecl { .. }
            | Stmt::TokenDecl { .. }
            | Stmt::RuleDecl { .. }
            | Stmt::ProtoDecl { .. } => self.in_routine(|v| walk_stmt(v, stmt)),
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        if self.found.is_some() {
            return;
        }
        match expr {
            // The use itself. `&?BLOCK` is deliberately NOT checked: every
            // block — the unit mainline included — is a `Block`, so it is
            // always declared.
            Expr::CodeVar(name) if name == "?ROUTINE" => {
                if !self.in_routine {
                    self.found = Some(name.clone());
                }
            }
            // `AnonSub` carries `is_block`, which separates a bare block `{ }`
            // (a `Block`: NOT a routine boundary) from an anonymous `sub { }`
            // (a `Routine`: it does supply `&?ROUTINE`).
            Expr::AnonSub {
                is_block: false, ..
            } => self.in_routine(|v| walk_expr(v, expr)),
            // `AnonSubParams` and `Lambda` are ambiguous in the AST: a pointy
            // block `-> { }` (a `Block`, which does NOT supply `&?ROUTINE` --
            // measured: `EVAL 'my $z = -> { &?ROUTINE }; $z()'` is
            // X::Undeclared::Symbols in raku) and a parameterised anonymous
            // `sub ($x) { }` (which does) both lower to them, with nothing left
            // to tell them apart. Treat them as routine boundaries: that can
            // only *miss* an offending pointy-block use, where the alternative
            // would wrongly reject a legal `sub ($x) { … }`.
            Expr::AnonSubParams { .. } | Expr::Lambda { .. } => {
                self.in_routine(|v| walk_expr(v, expr))
            }
            _ => walk_expr(self, expr),
        }
    }
}
