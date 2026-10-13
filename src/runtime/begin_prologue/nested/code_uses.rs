//! How a piece of code a lifted BEGIN runs uses each routine it reads as
//! `&name` (ADR-0134, slice 2; [`super::scan`]): whether the use can change
//! the routine object, which a lifted copy of a multi's proto would lose.

use crate::ast::{Expr, Stmt};
use crate::ast_visit::{Visit, walk_expr};
use std::collections::HashSet;

/// The `Routine` methods that only read the routine object (a proto's
/// candidates, its signature, its name, a curried copy). Any other method
/// may change it.
const READ_ONLY_METHODS: &[&str] = &[
    "candidates",
    "dispatchees",
    "cando",
    "is_dispatcher",
    "is-wrapped",
    "name",
    "signature",
    "arity",
    "count",
    "of",
    "returns",
    "package",
    "file",
    "line",
    "multi",
    "onlystar",
    "soft",
    "yada",
    "assuming",
    "gist",
    "raku",
    "perl",
    "Str",
    "WHAT",
    "WHICH",
    "WHY",
    "defined",
    "DEFINITE",
    "Bool",
];

/// The routines some code reads as `&name` other than through a read-only
/// method or a call (`&p(...)`): passed on, bound or stored, or sent a method
/// that may change it.
#[derive(Default)]
pub(super) struct CodeUses {
    unbounded: HashSet<String>,
    /// The next expression visited is the invocant of a read-only method or
    /// the callee of a call, which the walk visits first.
    harmless_target: bool,
}

impl CodeUses {
    pub(super) fn of(stmts: &[Stmt]) -> CodeUses {
        let mut uses = CodeUses::default();
        for stmt in stmts {
            uses.visit_stmt(stmt);
        }
        uses
    }

    /// Whether the code may change the routine it reads as `&name`.
    pub(super) fn may_change(&self, name: &str) -> bool {
        self.unbounded.contains(name)
    }
}

impl<'ast> Visit<'ast> for CodeUses {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        let harmless = std::mem::take(&mut self.harmless_target);
        match expr {
            Expr::CodeVar(name) if !harmless => {
                self.unbounded.insert(name.clone());
            }
            Expr::MethodCall { name, modifier, .. } => {
                self.harmless_target =
                    modifier.is_none() && READ_ONLY_METHODS.contains(&name.resolve().as_str());
            }
            // `&p(...)`, `&p.(...)`: a call through the routine.
            Expr::CallOn { .. } => self.harmless_target = true,
            _ => {}
        }
        walk_expr(self, expr);
    }
}
