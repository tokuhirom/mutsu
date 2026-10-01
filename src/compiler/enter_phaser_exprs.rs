//! The `ENTER`-expression hoist of phaser lowering, on the AST visitors
//! (ADR-10499): a probe for an `ENTER` expression embedded in a body's
//! statements, and the extraction of each one to a temp. Both stop at the
//! same frame boundary: an `ENTER` in a closure, a loop body, a phaser body or
//! a nested routine runs on entry to *that*.

use crate::ast::{Expr, PhaserKind, Stmt};
use crate::ast_visit::{Visit, VisitMut, walk_expr, walk_expr_mut, walk_stmt, walk_stmt_mut};

/// True when a statement of `stmts` embeds an `ENTER` expression.
// Cost: O(n), n = size of `stmts`' subtree.
pub(super) fn stmts_have_enter_expr(stmts: &[Stmt]) -> bool {
    let mut probe = EnterProbe(false);
    for s in stmts {
        probe.visit_stmt(s);
    }
    probe.0
}

/// Finds an `ENTER` expression that runs on entry to the block being
/// compiled: one in a closure or a nested routine runs on entry to *that*.
struct EnterProbe(bool);

impl Visit for EnterProbe {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        if !self.0 && !is_own_frame_stmt(stmt) {
            walk_stmt(self, stmt);
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        if self.0 {
            return;
        }
        match expr {
            Expr::PhaserExpr {
                kind: PhaserKind::Enter,
                ..
            } => self.0 = true,
            e if is_own_frame_expr(e) => {}
            _ => walk_expr(self, expr),
        }
    }
}

/// Take every `ENTER` expression embedded in the statements of `stmts` that
/// `pick` selects out to a temp, returning `[(temp name, phaser body)]`. The
/// temps are `__mutsu_enter_expr_N` in walk order.
// Cost: O(n), n = size of `stmts`' subtree.
pub(super) fn extract_enter_exprs(
    stmts: &mut [Stmt],
    pick: impl Fn(&Stmt) -> bool,
) -> Vec<(String, Vec<Stmt>)> {
    let mut x = EnterExtract {
        extracted: Vec::new(),
    };
    for s in stmts.iter_mut().filter(|s| pick(s)) {
        x.visit_stmt_mut(s);
    }
    x.extracted
}

struct EnterExtract {
    extracted: Vec<(String, Vec<Stmt>)>,
}

impl VisitMut for EnterExtract {
    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        if !is_own_frame_stmt(stmt) {
            walk_stmt_mut(self, stmt);
        }
    }

    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        match expr {
            Expr::PhaserExpr {
                kind: PhaserKind::Enter,
                body,
            } => {
                let tmp = format!("__mutsu_enter_expr_{}", self.extracted.len());
                self.extracted.push((tmp.clone(), std::mem::take(body)));
                *expr = Expr::Var(tmp);
            }
            e if is_own_frame_expr(e) => {}
            _ => walk_expr_mut(self, expr),
        }
    }
}

/// A statement whose body runs on entry to its own frame, not this one's.
fn is_own_frame_stmt(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::Phaser { .. }
            | Stmt::SubDecl { .. }
            | Stmt::MethodDecl { .. }
            | Stmt::ProtoDecl { .. }
            | Stmt::TokenDecl { .. }
            | Stmt::RuleDecl { .. }
            | Stmt::ClassDecl { .. }
            | Stmt::RoleDecl { .. }
            | Stmt::Package { .. }
            | Stmt::AugmentClass { .. }
            | Stmt::For { .. }
            | Stmt::While { .. }
            | Stmt::Loop { .. }
            | Stmt::Whenever { .. }
    )
}

/// An expression whose body runs on entry to its own frame.
fn is_own_frame_expr(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::AnonSub { .. }
            | Expr::AnonSubParams { .. }
            | Expr::Lambda { .. }
            | Expr::Gather(_)
            | Expr::Block(_)
            | Expr::PhaserExpr { .. }
    )
}
