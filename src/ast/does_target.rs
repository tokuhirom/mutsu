//! Parameters a routine body applies `does` to (ADR-14144).
//!
//! `$v does R` on a value type (`Int`, `Str`, ...) composes a new `Mixin` value
//! and stores it into `$v`. Rakudo reblesses the shared object instead, so the
//! caller's container sees the role too. The binder reproduces that by writing
//! the parameter back through the argument's source container, but only for a
//! parameter marked here, so no other call pays for it.

use super::{Expr, ParamDef, Stmt};
use crate::ast_visit::{Visit, walk_expr};
use crate::token_kind::TokenKind;

#[derive(Default)]
struct DoesTargets(Vec<String>);

impl<'ast> Visit<'ast> for DoesTargets {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        if let Expr::Binary { left, op, .. } = expr
            && matches!(op, TokenKind::Ident(name) if name == "does")
            && let Expr::Var(name) = left.as_ref()
        {
            self.0.push(name.clone());
        }
        walk_expr(self, expr);
    }
}

impl ParamDef {
    /// True for a parameter its routine body applies `does` to.
    // Cost: O(1).
    pub(crate) fn is_does_target(&self) -> bool {
        self.does_target
    }
}

/// Mark each positional scalar parameter of `params` that `body` applies `does`
/// to (statement, expression or nested-closure form).
// Cost: O(b + p), b = size of the body, p = parameter count.
pub(crate) fn mark_does_targets(name: &str, params: &mut [ParamDef], body: &[Stmt]) {
    // A `trait_mod:<...>` routine is called by the declarator machinery with the
    // declared object itself, which its own `does` already reaches by reference.
    if name.starts_with("trait_mod:") {
        return;
    }
    if params
        .iter()
        .all(|p| p.named || p.slurpy || p.double_slurpy)
    {
        return;
    }
    let mut targets = DoesTargets::default();
    for stmt in body {
        targets.visit_stmt(stmt);
    }
    if targets.0.is_empty() {
        return;
    }
    for pd in params.iter_mut() {
        if !pd.named
            && !pd.slurpy
            && !pd.double_slurpy
            && !pd.sigilless
            && !pd.name.starts_with(['@', '%', '&'])
            && targets.0.contains(&pd.name)
            && !pd.is_does_target()
        {
            pd.does_target = true;
        }
    }
}
