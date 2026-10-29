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
pub(crate) fn result_is_decontainerized(param_defs: &[ParamDef], stmts: &[Stmt]) -> bool {
    let mut scan = ReturnScan::default();
    walk_stmts(&mut scan, stmts);
    !scan.found && !tail_is_readonly_param(param_defs, stmts)
}

/// Whether the value of `stmts` can be a readonly `$` parameter of the
/// routine: its last value statement, an `if` branch's or a ternary arm's.
/// Follows only the tail, with a worklist.
// Cost: O(t + p * v), t = tail statements and arms followed, p = parameters,
// v = variables reached.
fn tail_is_readonly_param(param_defs: &[ParamDef], stmts: &[Stmt]) -> bool {
    let mut bodies: Vec<&[Stmt]> = vec![stmts];
    let mut values: Vec<&Expr> = Vec::new();
    while let Some(body) = bodies.pop() {
        match super::last_value_stmt(body, super::TailSkip::Markers) {
            Some(Stmt::Expr(expr)) => values.push(expr),
            Some(Stmt::If {
                then_branch,
                else_branch,
                ..
            }) => {
                bodies.push(then_branch);
                bodies.push(else_branch);
            }
            _ => {}
        }
    }
    while let Some(expr) = values.pop() {
        match expr {
            Expr::Var(name) => {
                if param_defs.iter().any(|pd| {
                    !pd.sigilless
                        && pd.name == *name
                        && !pd.traits.iter().any(|t| t == "rw" || t == "copy")
                }) {
                    return true;
                }
            }
            Expr::Ternary {
                then_expr,
                else_expr,
                ..
            } => {
                values.push(then_expr);
                values.push(else_expr);
            }
            _ => {}
        }
    }
    false
}

/// Whether a block (not a routine) evaluates to the container of its last
/// statement: a scalar assignment (`{ $z = f() }`), a scalar declaration with
/// an initializer, or a bare scalar variable (`{ $z }`). Rakudo hands that
/// container back to the caller, so a sunk call of the block does not sink
/// the value inside it (a stored `Failure` is not thrown); a routine
/// decontainerizes its implicit result and does.
// Cost: O(k), k = trailing marker statements skipped.
pub(crate) fn block_result_is_container(stmts: &[Stmt]) -> bool {
    match super::last_value_stmt(stmts, super::TailSkip::Markers) {
        Some(Stmt::Assign {
            name,
            target_is_sigilless: false,
            ..
        }) => !name.starts_with(['@', '%']),
        Some(Stmt::VarDecl { name, .. }) => !name.starts_with(['@', '%', '&']),
        Some(Stmt::Expr(Expr::Var(name))) => !name.starts_with(['@', '%', '&']),
        _ => false,
    }
}
