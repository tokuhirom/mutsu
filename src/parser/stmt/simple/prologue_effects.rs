//! Whether a finished top-level statement of the unit leaves an observable
//! effect to the unit's BEGIN-time prologue (ADR-0134), which runs after the
//! parse. A `BEGIN` the parser reaches after one cannot run at parse time
//! (ADR-17558 slice 5): it would run ahead of that effect, which rakudo runs
//! first. Code the prologue only declares (a routine, a class) is not such an
//! effect: a `BEGIN` that may run at parse time names none of it.

use crate::ast::{Expr, PhaserKind, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_stmt};

/// Whether `stmt` holds an effect the prologue runs: a `BEGIN` (statement or
/// value form, a `will begin` trait), a `constant` or `enum` value that is
/// computed, a user trait on a declaration (applied at BEGIN time), a `use`
/// the parse did not load (`use lib` included) other than an inert pragma, a
/// `need` or an `import`.
// Cost: O(n), n = size of `stmt`.
pub(super) fn has_prologue_effect(stmt: &Stmt) -> bool {
    let mut finder = Finder::default();
    finder.visit_stmt(stmt);
    finder.found
}

#[derive(Default)]
struct Finder {
    found: bool,
}

impl<'ast> Visit<'ast> for Finder {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found {
            return;
        }
        self.found = match stmt {
            Stmt::Phaser {
                kind: PhaserKind::Begin,
                ..
            }
            | Stmt::Need { .. }
            | Stmt::Import { .. } => true,
            // `will begin` (the parser keeps the phasers beside the declaration).
            Stmt::Ast(node) => matches!(
                node.as_ref(),
                crate::ast::parsed::ParsedStmt::WillDecl { phasers, .. }
                    if phasers.iter().any(|(kind, _)| *kind == PhaserKind::Begin)
            ),
            Stmt::VarDecl {
                constant,
                expr,
                custom_traits,
                ..
            } => {
                (constant.is_some() && !matches!(expr, Expr::Literal(_)))
                    || !custom_traits.is_empty()
            }
            Stmt::EnumDecl { variants, .. } => variants.iter().any(|(_, value)| {
                value
                    .as_ref()
                    .is_some_and(|v| !matches!(v, Expr::Literal(_)))
            }),
            Stmt::SubDecl { custom_traits, .. }
            | Stmt::ClassDecl { custom_traits, .. }
            | Stmt::RoleDecl { custom_traits, .. }
            | Stmt::Package { custom_traits, .. } => !custom_traits.is_empty(),
            Stmt::HasDecl { unknown_traits, .. } => !unknown_traits.is_empty(),
            // A `use` the parse loaded has `site` set; its load already ran, in
            // source order.
            Stmt::Use { module, site, .. } => *site == 0 && !is_inert_pragma(module),
            _ => false,
        };
        if !self.found {
            walk_stmt(self, stmt);
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.found {
            return;
        }
        self.found = matches!(
            expr,
            Expr::PhaserExpr {
                kind: PhaserKind::Begin,
                ..
            }
        );
        if !self.found {
            walk_expr(self, expr);
        }
    }
}

/// A `use` whose prologue effect no `BEGIN` that may run at parse time can
/// observe: a language version, a pragma that only steers the parse or the
/// compiler, and `Test`, whose load defines routines and prints nothing.
// Cost: O(1).
fn is_inert_pragma(module: &str) -> bool {
    module.starts_with("v6")
        || matches!(
            module,
            "Test"
                | "strict"
                | "experimental"
                | "isms"
                | "worries"
                | "precompilation"
                | "trace"
                | "MONKEY"
                | "MONKEY-SEE-NO-EVAL"
                | "MONKEY-TYPING"
                | "MONKEY-GUTS"
                | "MONKEY-BARS"
                | "MONKEY-SHINE"
                | "MONKEY-BRAINS"
        )
}
