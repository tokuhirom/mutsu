//! The `use` of a `sub EXPORT` hook module nested in a block of an `EVAL`'d
//! unit (#13441).
//!
//! Rakudo runs every `use` at BEGIN time, so the names the hook returns are
//! known to the unit's CHECK-time validation wherever the `use` sits.
//! [`Interpreter::run_eval_hook_uses`](super::Interpreter) already runs the
//! top-level ones ahead of the validation; this lifts the ones nested in a
//! block, branch or routine body to the top of the unit so the same pre-run
//! covers them.

use crate::ast::{Expr, Stmt};
use crate::ast_visit::VisitMut;

/// Removes every nested `use` of a hook module (per `is_hook`) whose arguments
/// are literals, and returns them in source order.
///
/// The import is then wider than the block that wrote it: a name the hook
/// returned is visible after the block too. Nothing narrower is available to
/// the pre-run, which has no import-set id for the block (ADR-8751); the
/// validation was equally lenient about such a unit before (it did not judge
/// the names of a hook module at all).
// Cost: O(n), n = size of the unit's AST, plus one `is_hook` call per nested
// `use`.
pub(super) fn take_nested_hook_uses(
    stmts: &mut Vec<Stmt>,
    is_hook: &dyn Fn(&str) -> bool,
) -> Vec<Stmt> {
    let mut lifter = Lifter {
        is_hook,
        lifted: Vec::new(),
        depth: 0,
    };
    lifter.visit_stmts_mut(stmts);
    lifter.lifted
}

struct Lifter<'a> {
    is_hook: &'a dyn Fn(&str) -> bool,
    lifted: Vec<Stmt>,
    /// Nesting of statement lists: the unit's own top level is depth 1 and is
    /// left to the pre-run.
    depth: usize,
}

/// Whether a `use` argument can be evaluated ahead of its block: a literal or
/// a flat list of literals (`use M <A B>`, `use M "a", 3`).
fn is_literal_arg(expr: &Expr) -> bool {
    match expr {
        Expr::ArrayLiteral(items) => items.iter().all(|i| matches!(i, Expr::Literal(_))),
        other => matches!(other, Expr::Literal(_)),
    }
}

impl VisitMut for Lifter<'_> {
    fn visit_stmts_mut(&mut self, body: &mut Vec<Stmt>) {
        self.depth += 1;
        if self.depth > 1 {
            let mut kept = Vec::with_capacity(body.len());
            for stmt in std::mem::take(body) {
                match &stmt {
                    Stmt::Use {
                        module,
                        arg,
                        condition: None,
                        if_imports,
                        ..
                    } if if_imports.is_empty()
                        && arg.as_ref().is_none_or(is_literal_arg)
                        && crate::compiler::begin_use::loads_module_in_position(&stmt)
                            .is_some_and(|m| m == module && (self.is_hook)(m)) =>
                    {
                        self.lifted.push(stmt);
                    }
                    _ => kept.push(stmt),
                }
            }
            *body = kept;
        }
        crate::ast_visit::walk_stmts_mut(self, body);
        self.depth -= 1;
    }
}
