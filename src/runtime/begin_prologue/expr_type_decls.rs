//! Package declarations in term position of a unit-level statement.
//!
//! `my $rs = model M is rs-class<R> { has $.a is column }.^rs;` declares `M`
//! while compiling, like a statement-form `model M { ... }`: it completes an
//! earlier forward declaration (`model M { ... }`) and applies its attribute
//! traits at BEGIN time, so code running before the statement already sees the
//! finished type. mutsu registers a term-position declaration only when its
//! expression runs, so this pass lifts the declaration out as a statement of
//! its own, ahead of the statement that used it, and leaves a read of the type
//! name where the declaration stood (a declaration's value is its type object).
//! The statement-form declaration then takes the usual BEGIN-prologue route.
//!
//! Only the unit's own statements are searched, and not inside a closure or a
//! nested declaration: those run their own scope.

use crate::ast::{Expr, Stmt};
use crate::ast_visit::{VisitMut, walk_expr_mut, walk_stmt_mut};

#[derive(Default)]
struct Lift {
    lifted: Vec<Stmt>,
}

impl VisitMut for Lift {
    // Cost: O(n), n = nodes of the statement outside closures.
    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        match expr {
            Expr::DoStmt(inner)
                if matches!(
                    inner.as_ref(),
                    Stmt::ClassDecl {
                        is_lexical: false,
                        is_unit: false,
                        name_expr: None,
                        body,
                        marks,
                        ..
                    } if !marks.anon && !crate::compiler::Compiler::is_stub_class_body(body)
                ) =>
            {
                let Stmt::ClassDecl { name, .. } = inner.as_ref() else {
                    return;
                };
                let name = name.resolve();
                let decl = std::mem::replace(inner.as_mut(), Stmt::SetLine(0, Default::default()));
                self.lifted.push(decl);
                *expr = Expr::BareWord(name);
            }
            Expr::AnonSub { .. }
            | Expr::AnonSubParams { .. }
            | Expr::Lambda { .. }
            | Expr::Block(_) => {}
            _ => walk_expr_mut(self, expr),
        }
    }

    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::SubDecl { .. }
            | Stmt::ClassDecl { .. }
            | Stmt::RoleDecl { .. }
            | Stmt::Block(_) => {}
            _ => walk_stmt_mut(self, stmt),
        }
    }
}

/// Lift every term-position class declaration of the unit's top-level
/// statements out to a statement ahead of the one that held it.
// Cost: O(n), n = size of the unit's top-level statements outside closures.
pub(super) fn lift_expr_type_decls(stmts: &mut Vec<Stmt>) {
    let mut out = Vec::with_capacity(stmts.len());
    for mut stmt in std::mem::take(stmts) {
        let mut lift = Lift::default();
        if !matches!(
            stmt,
            Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } | Stmt::SubDecl { .. }
        ) {
            lift.visit_stmt_mut(&mut stmt);
        }
        out.append(&mut lift.lifted);
        out.push(stmt);
    }
    *stmts = out;
}
