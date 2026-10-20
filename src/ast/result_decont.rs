//! Whether a routine's implicit result is a decontainerized value its caller
//! sinks (#11537). Asked once per routine at declaration time
//! (`RoutineBodyFacts::result_is_decontainerized`), by the typed AST visitor
//! (ADR-0137).

use crate::ast::scope_scan::{is_code_object, is_scope_declaration};
use crate::ast::{Expr, ParamDef, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_stmt, walk_stmts};
use crate::regex_tree::RegexNode;

/// Finds an explicit `return` anywhere the routine's own return can come
/// from: a nested block or branch returns from the routine too, a nested
/// routine or closure returns from itself.
#[derive(Default)]
struct ReturnScan {
    found: bool,
}

impl<'ast> Visit<'ast> for ReturnScan {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found {
            return;
        }
        match stmt {
            Stmt::Return(_) => self.found = true,
            Stmt::Call { name, .. } if name == "return" => self.found = true,
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
            Expr::Call { name, .. } if name == "return" => self.found = true,
            e if is_code_object(e) => {}
            Expr::Gather(_) | Expr::PhaserExpr { .. } => {}
            _ => walk_expr(self, expr),
        }
    }

    // A regex code block is a closure of its own.
    fn visit_regex_node(&mut self, _node: &'ast RegexNode) {}
}

/// Whether a routine's implicit result is a plain value its caller sinks
/// (#11537): rakudo decontainerizes the last statement's value of a routine
/// that is not `is rw`/`is raw`, so `sub g { $x }; g;` runs `$x`'s `sink`.
/// The rule is measured against rakudo, and is conservative where it is not
/// settled: an explicit `return` anywhere (`return $x` hands the container
/// back, and rakudo does not sink `return S.new if True` either) and a tail
/// that is a readonly `$` parameter (`sub g($v) { $v }`, also in an `if`
/// branch or a ternary arm) leave the result unsunk. The traits are the
/// caller's to check (`returns_container`).
// Cost: O(n), n = size of the body outside nested code objects.
pub(crate) fn result_is_decontainerized(
    param_defs: &[ParamDef],
    stmts: &[Stmt],
) -> bool {
    let mut scan = ReturnScan::default();
    walk_stmts(&mut scan, stmts);
    !scan.found && !tail_is_readonly_param(param_defs, stmts)
}

/// Whether the value of `stmts` can be a readonly `$` parameter of the
/// routine: its last value statement, an `if` branch's or a ternary arm's.
// Cost: O(d), d = nesting of the tail's branches.
fn tail_is_readonly_param(param_defs: &[ParamDef], stmts: &[Stmt]) -> bool {
    match super::last_value_stmt(stmts, super::TailSkip::Markers) {
        Some(Stmt::Expr(expr)) => expr_is_readonly_param(param_defs, expr),
        Some(Stmt::If {
            then_branch,
            else_branch,
            ..
        }) => {
            tail_is_readonly_param(param_defs, then_branch)
                || tail_is_readonly_param(param_defs, else_branch)
        }
        _ => false,
    }
}

// Cost: O(p + d), p = parameters, d = nesting of ternary arms.
fn expr_is_readonly_param(param_defs: &[ParamDef], expr: &Expr) -> bool {
    match expr {
        Expr::Var(name) => param_defs.iter().any(|pd| {
            !pd.sigilless
                && pd.name == *name
                && !pd.traits.iter().any(|t| t == "rw" || t == "copy")
        }),
        Expr::Ternary {
            then_expr,
            else_expr,
            ..
        } => {
            expr_is_readonly_param(param_defs, then_expr)
                || expr_is_readonly_param(param_defs, else_expr)
        }
        _ => false,
    }
}
