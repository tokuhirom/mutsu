//! Post-parse check for `whenever` blocks that appear outside the lexical scope
//! of a `react` / `supply` block.
//!
//! Rakudo raises a compile-time `X::Comp::WheneverOutOfScope`
//! ("Cannot have a 'whenever' block outside the scope of a 'supply' or 'react'
//! block") when a `whenever` is not lexically enclosed by a `react` block or a
//! `supply` block. The check is purely *lexical*: a `whenever` is valid as long
//! as some `supply`/`react` block encloses it in the source, at any nesting
//! depth — routine boundaries do NOT break the enclosure. So
//! `supply { my sub g { whenever … } }` is valid (a `sub`/`method`/pointy/class
//! nested inside a supply keeps the enclosure), while a `whenever` in a
//! top-level `sub`/pointy with no supply/react ancestor is rejected.
//!
//! This walker mirrors that rule structurally on the AST. It tracks a single
//! boolean `in_scope` ("is there a react/supply block lexically enclosing here?"):
//!
//! * `react { ... }` sets it true for the body.
//! * The emitter closure that `supply { ... }` lowers to (a `Lambda` whose
//!   parameter name starts with `__mutsu_supply_emitter_`) sets it true.
//! * Every other construct — including routine/closure boundaries — preserves it,
//!   so once inside a supply/react the enclosure holds through nested subs.
//!
//! When a `whenever` is reached with `in_scope == false`, we record its line.
//!
//! The walk is the typed AST visitor (ADR-0137): every construct is descended
//! into, so a `whenever` hidden in a condition, an argument list or any other
//! child is found too.

use crate::ast::{Expr, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_stmt, walk_stmts};

use super::SUPPLY_EMITTER_PREFIX;

/// Returns the 1-based source line of the first `whenever` block found outside
/// the scope of a `react`/`supply` block, or `None` if every `whenever` is
/// properly scoped.
// Cost: O(n), n = size of the AST.
pub(crate) fn find_out_of_scope_whenever(stmts: &[Stmt]) -> Option<i64> {
    let mut scan = WheneverScope::default();
    walk_stmts(&mut scan, stmts);
    scan.found
}

#[derive(Default)]
struct WheneverScope {
    /// Whether some `react`/`supply` block lexically encloses the current node.
    in_scope: bool,
    line: i64,
    found: Option<i64>,
}

impl WheneverScope {
    fn scoped(&mut self, in_scope: bool, f: impl FnOnce(&mut Self)) {
        let saved = std::mem::replace(&mut self.in_scope, in_scope);
        f(self);
        self.in_scope = saved;
    }
}

impl<'ast> Visit<'ast> for WheneverScope {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found.is_some() {
            return;
        }
        match stmt {
            Stmt::SetLine(n) => self.line = *n,
            Stmt::React { body } => self.scoped(true, |v| walk_stmts(v, body)),
            // Nested `whenever` blocks inside an in-scope one stay in scope.
            Stmt::Whenever { .. } if !self.in_scope => self.found = Some(self.line),
            // Routine, package and closure boundaries do NOT break the
            // enclosure: Rakudo's check is purely lexical, so a `whenever`
            // inside a `sub`/`method`/class nested in a `supply`/`react` block
            // is valid (`supply { my sub g { whenever … } }`).
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.found.is_some() {
            return;
        }
        match expr {
            // The `supply { }` sugar lowers to `Supply.on-demand(-> $emitter
            // { ... })`, where the emitter closure's parameter name marks a
            // genuine supply scope. Any other closure keeps the current scope.
            Expr::Lambda { param, .. } if param.starts_with(SUPPLY_EMITTER_PREFIX) => {
                self.scoped(true, |v| walk_expr(v, expr))
            }
            _ => walk_expr(self, expr),
        }
    }
}
