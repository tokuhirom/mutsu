//! Origins of expression nodes (ADR-10723 Stage 2, expression families).
//!
//! A parse the RakuAST conversion reads keeps where each term was written, as
//! an `Expr::Spelled` wrapper with `Spelling::Origin`. The conversion's shape
//! matches look at the expressions themselves, so before converting a unit
//! the wrappers are taken off again in place and their ranges kept by the
//! address of the slot each term now sits in. The tree is not moved or
//! changed while it is converted, so the address names the slot for the
//! length of the conversion; the table is dropped with it.

use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::spelled::Spelling;
use crate::ast::{Expr, SrcSpan, Stmt};
use crate::ast_visit::VisitMut;

thread_local! {
    static ORIGINS: RefCell<HashMap<usize, SrcSpan>> = RefCell::new(HashMap::new());
}

/// While alive, the ranges [`strip`] took off a unit answer [`of`].
pub(super) struct Scope {
    previous: HashMap<usize, SrcSpan>,
}

impl Drop for Scope {
    fn drop(&mut self) {
        let previous = std::mem::take(&mut self.previous);
        ORIGINS.with(|o| *o.borrow_mut() = previous);
    }
}

struct Strip(HashMap<usize, SrcSpan>);

impl VisitMut for Strip {
    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        let mut span = None;
        while let Expr::Spelled(spelled) = expr
            && let Spelling::Origin(at) = spelled.spelling
        {
            span = span.or(Some(at));
            let Expr::Spelled(spelled) = std::mem::replace(expr, Expr::Literal(crate::value::Value::NIL)) else {
                unreachable!("matched above");
            };
            *expr = spelled.expr;
        }
        if let Some(span) = span {
            self.0.insert(expr as *const Expr as usize, span);
        }
        crate::ast_visit::walk_expr_mut(self, expr);
    }
}

/// Take the origin wrappers off `stmts`, keeping their ranges for [`of`] until
/// the returned scope ends.
// Cost: O(n), n = size of the tree.
pub(super) fn strip(stmts: &mut [Stmt]) -> Scope {
    let mut strip = Strip(HashMap::new());
    crate::ast_visit::walk_stmts_mut(&mut strip, stmts);
    let previous = ORIGINS.with(|o| std::mem::replace(&mut *o.borrow_mut(), strip.0));
    Scope { previous }
}

/// Where the term in the slot `expr` was written, when a stripped unit holds it.
// Cost: O(1).
pub(super) fn of(expr: &Expr) -> Option<SrcSpan> {
    ORIGINS.with(|o| o.borrow().get(&(expr as *const Expr as usize)).copied())
}
