//! Questions about a routine body that its declaration plan and the VM's
//! dispatch gates ask once per routine, answered by walking the typed AST
//! visitor (ADR-0137). Where a scan stops is part of its answer; each stop is
//! an explicit hook arm with its reason.

use crate::ast::scope_scan::{
    is_code_object, is_scope_declaration, opens_own_scope, walk_stmt_own_scope,
};
use crate::ast::{Expr, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_stmt, walk_stmts};
use crate::regex_tree::RegexNode;

/// Finds an explicit `return-rw` anywhere a routine's return value can come
/// from: a statement, a branch, a loop body, an operand, a ternary arm
/// (`$flag ?? return-rw c<x> !! return-rw c<y>`, `1 and return-rw $x`).
#[derive(Default)]
struct ReturnRwScan {
    found: bool,
}

impl<'ast> Visit<'ast> for ReturnRwScan {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found {
            return;
        }
        match stmt {
            Stmt::Call { name, .. } if name == "return-rw" => self.found = true,
            // A nested routine or package returns from itself; a phaser's
            // value is not the routine's.
            Stmt::Phaser { .. } | Stmt::DocPhaser(_) => {}
            s if is_scope_declaration(s) => {}
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.found {
            return;
        }
        match expr {
            Expr::Call { name, .. } if name == "return-rw" => self.found = true,
            Expr::MethodCall { name, .. } if name == "return-rw" => self.found = true,
            // A code object's body is analysed as a routine of its own when
            // it is compiled; a lazy `gather` returns nothing to the routine.
            e if is_code_object(e) => {}
            Expr::Gather(_) | Expr::PhaserExpr { .. } => {}
            _ => walk_expr(self, expr),
        }
    }

    // A regex code block is a closure of its own.
    fn visit_regex_node(&mut self, _node: &'ast RegexNode) {}
}

/// Whether a routine body hands its caller a container through an explicit
/// `return-rw` (ADR-0059). See [`ReturnRwScan`].
// Cost: O(n), n = size of `stmts` outside nested code objects.
pub(crate) fn uses_return_rw(stmts: &[Stmt]) -> bool {
    let mut scan = ReturnRwScan::default();
    walk_stmts(&mut scan, stmts);
    scan.found
}

/// Finds a `return` with a non-Nil argument in the routine's own scope. A
/// `return` inside a nested block is not checked by rakudo (it resets
/// `%*SIG_INFO` in every `block`/`pblock`), so blocks, branches and loop
/// bodies are not entered — but a statement modifier opens no block, and an
/// expression-position `return` (`1 and return 5`) is in the routine's scope.
#[derive(Default)]
struct NonNilReturnScan {
    found: bool,
}

impl NonNilReturnScan {
    fn check(&mut self, arg: Option<&Expr>) {
        self.found |= arg.is_some_and(|e| !matches!(e, Expr::Literal(value) if value.is_nil()));
    }
}

impl<'ast> Visit<'ast> for NonNilReturnScan {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found {
            return;
        }
        match stmt {
            Stmt::Return(expr) => self.check(Some(expr)),
            _ => walk_stmt_own_scope(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.found || opens_own_scope(expr) {
            return;
        }
        match expr {
            Expr::Call { name, args, .. } if name == "return" && args.len() <= 1 => {
                self.check(args.first())
            }
            // `return 1, 2` returns a list: never Nil.
            Expr::Call { name, args, .. } if name == "return" && args.len() > 1 => {
                self.found = true
            }
            _ => walk_expr(self, expr),
        }
    }

    // A regex code block is a closure of its own.
    fn visit_regex_node(&mut self, _node: &'ast RegexNode) {}
}

/// Whether a routine body `return`s a non-Nil argument in its own scope — the
/// statements rakudo checks against a definite return value (`--> True`,
/// `--> Nil`, `--> 42`). See [`NonNilReturnScan`].
// Cost: O(n), n = size of the routine's own scope (blocks are not entered).
pub(crate) fn has_non_nil_return(stmts: &[Stmt]) -> bool {
    let mut scan = NonNilReturnScan::default();
    walk_stmts(&mut scan, stmts);
    scan.found
}

/// Finds a `class`/`role` declaration at the routine's statement level, or
/// hosted by an expression statement (a `do` statement, a bare block, an
/// operand or argument).
#[derive(Default)]
struct TypeDeclScan {
    found: bool,
}

impl<'ast> Visit<'ast> for TypeDeclScan {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found {
            return;
        }
        match stmt {
            Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } => self.found = true,
            Stmt::Expr(_) => walk_stmt(self, stmt),
            // Only the top-level and expression-hosted declarations are the
            // shape this gate was measured for: a declaration nested in a
            // control-flow body or a routine was verified OTF-safe
            // (t/module-sub-otf-interpreter-constructs.t).
            _ => {}
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        match expr {
            // A closure's body is compiled as a routine of its own.
            Expr::Lambda { .. } | Expr::AnonSub { .. } | Expr::AnonSubParams { .. } => {}
            _ if !self.found => walk_expr(self, expr),
            _ => {}
        }
    }

    // A regex code block is a closure of its own.
    fn visit_regex_node(&mut self, _node: &'ast RegexNode) {}
}

/// Whether a routine body declares a `class`/`role` where the VM's
/// on-the-fly compilation gate still routes it to the interpreter. See
/// [`TypeDeclScan`].
// Cost: O(n), n = size of the body's expression statements.
pub(crate) fn declares_type_at_top(stmts: &[Stmt]) -> bool {
    let mut scan = TypeDeclScan::default();
    walk_stmts(&mut scan, stmts);
    scan.found
}

/// Finds a `state` declaration anywhere in a routine body, nested blocks
/// included. A nested routine owns its own `state`, and a closure's `state`
/// lives in each closure clone, so neither is entered.
#[derive(Default)]
struct RoutineStateScan {
    found: bool,
}

impl<'ast> Visit<'ast> for RoutineStateScan {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found {
            return;
        }
        match stmt {
            Stmt::VarDecl { is_state: true, .. } => self.found = true,
            s if is_scope_declaration(s) => {}
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        match expr {
            Expr::Lambda { .. } | Expr::AnonSub { .. } | Expr::AnonSubParams { .. } => {}
            _ if !self.found => walk_expr(self, expr),
            _ => {}
        }
    }

    // A regex code block is a closure of its own.
    fn visit_regex_node(&mut self, _node: &'ast RegexNode) {}
}

/// Whether a routine body declares a `state` variable. See
/// [`RoutineStateScan`].
// Cost: O(n), n = size of `stmts` outside nested closures and routines.
pub(crate) fn declares_state(stmts: &[Stmt]) -> bool {
    let mut scan = RoutineStateScan::default();
    walk_stmts(&mut scan, stmts);
    scan.found
}
