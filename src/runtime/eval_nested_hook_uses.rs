//! The `use` statements nested in a block of an `EVAL`'d unit (#13441).
//!
//! Rakudo runs every `use` at BEGIN time, so what a module imports is known to
//! the unit's CHECK-time validation wherever the `use` sits.
//! [`Interpreter::run_eval_hook_uses`](super::Interpreter) already runs the
//! top-level ones ahead of the validation; this lifts the ones nested in a
//! block, branch or routine body to the top of the unit so the same pre-run
//! covers them. Every such `use` stays in its block, where the in-position
//! run records the block's lexical import set, and a copy is pre-run. A hook
//! module's copy records what its `sub EXPORT` returned, and the in-position
//! run replays that map instead of running the hook twice
//! (`ModuleState::eval_hook_results`).

use crate::ast::{Expr, Stmt};
use crate::ast_visit::VisitMut;

/// Collects a copy of every nested `use` whose arguments are literals, in
/// source order; the statement stays in its block.
///
/// The import is then wider than the block that wrote it: a name the hook
/// returned is visible after the block too. Nothing narrower is available to
/// the pre-run, which has no import-set id for the block (ADR-8751); the
/// validation was equally lenient about such a unit before (it did not judge
/// the names of a hook module at all).
// Cost: O(n), n = size of the unit's AST.
pub(super) fn take_nested_hook_uses(stmts: &mut Vec<Stmt>) -> Vec<Stmt> {
    let mut lifter = Lifter {
        lifted: Vec::new(),
        depth: 0,
    };
    lifter.visit_stmts_mut(stmts);
    lifter.lifted
}

struct Lifter {
    lifted: Vec<Stmt>,
    /// Nesting of statement lists: the unit's own top level is depth 1 and is
    /// left to the pre-run.
    depth: usize,
}

/// Whether a `use` argument can be evaluated ahead of its block: a literal or
/// a flat list of literals (`use M <A B>`, `use M "a", 3`).
pub(super) fn is_literal_arg(expr: &Expr) -> bool {
    match expr {
        Expr::ArrayLiteral(items) => items.iter().all(|i| matches!(i, Expr::Literal(_))),
        other => matches!(other, Expr::Literal(_)),
    }
}

impl VisitMut for Lifter {
    fn visit_stmts_mut(&mut self, body: &mut Vec<Stmt>) {
        self.depth += 1;
        if self.depth > 1 {
            let mut kept = Vec::with_capacity(body.len());
            for stmt in std::mem::take(body) {
                let preruns = matches!(
                    &stmt,
                    Stmt::Use { module, arg, condition: None, if_imports, .. }
                        if if_imports.is_empty()
                            && arg.as_ref().is_none_or(is_literal_arg)
                            && crate::compiler::begin_use::loads_module_in_position(&stmt)
                                .is_some_and(|m| m == module)
                );
                if preruns {
                    self.lifted.push(stmt.clone());
                }
                kept.push(stmt);
            }
            *body = kept;
        }
        crate::ast_visit::walk_stmts_mut(self, body);
        self.depth -= 1;
    }
}
