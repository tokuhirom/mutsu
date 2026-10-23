//! Where the terms of a `.AST` parse were written (ADR-10723 Stage 2,
//! expression families).
//!
//! A term's RakuAST node is its `Expr` plus the range the parser read it from
//! (the §4 amendment's "lossless family" case). The parser makes many
//! decisions by looking at the shape of a term it has just read (is it a
//! variable, a pair, a `*`?), so a term cannot carry its range while the unit
//! is still being parsed: every such decision would have to look through it.
//! Instead a parse that keeps origins notes, for each term, the range and a
//! structural fingerprint of what it read, and [`attach`] puts the ranges onto
//! the finished tree, as `Expr::Spelled` wrappers with `Spelling::Origin`,
//! once nothing in the parser looks at it any more.
//!
//! A note finds its term again by fingerprint, in source order, within the
//! statement it was written in. A term the parser later rewrote (a desugared
//! form) has a different fingerprint and simply gets no origin; an
//! alternative the parser tried and abandoned leaves a note no term matches.

use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use crate::ast::spelled::{Spelled, Spelling};
use crate::ast::{Expr, SrcSpan, Stmt};
use crate::ast_visit::VisitMut;

thread_local! {
    /// The notes of the innermost parse that keeps origins, `None` while the
    /// innermost parse does not.
    static NOTES: RefCell<Option<Vec<(u64, SrcSpan)>>> = const { RefCell::new(None) };
}

/// While alive, terms are noted when `keep` was set; restores the enclosing
/// parse's notes on drop.
pub(crate) struct Recording {
    previous: Option<Vec<(u64, SrcSpan)>>,
}

impl Drop for Recording {
    fn drop(&mut self) {
        let previous = self.previous.take();
        NOTES.with(|n| *n.borrow_mut() = previous);
    }
}

/// Start noting terms for a parse that keeps origins (`keep`), or stop noting
/// them for one that does not.
// Cost: O(1).
pub(crate) fn begin(keep: bool) -> Recording {
    let previous = NOTES.with(|n| n.replace(keep.then(Vec::new)));
    Recording { previous }
}

// Cost: O(n), n = size of `expr`.
fn fingerprint(expr: &Expr) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    expr.hash(&mut hasher);
    hasher.finish()
}

/// Note that the parser read `expr` from `span`.
// Cost: O(1) when not noting, O(n) otherwise, n = size of `expr`.
pub(crate) fn note(expr: &Expr, span: SrcSpan) {
    if !span.is_some() {
        return;
    }
    NOTES.with(|n| {
        if let Some(notes) = n.borrow_mut().as_mut() {
            notes.push((fingerprint(expr), span));
        }
    });
}

/// Put the noted ranges onto the terms of `stmts`, the finished unit.
// Cost: O(n·d + k log k), n = expressions in the unit, d = their depth,
// k = notes.
pub(crate) fn attach(stmts: &mut [Stmt]) {
    let Some(notes) = NOTES.with(|n| n.borrow_mut().take()) else {
        return;
    };
    let mut by_print: HashMap<u64, Vec<SrcSpan>> = HashMap::new();
    for (print, span) in notes {
        by_print.entry(print).or_default().push(span);
    }
    for spans in by_print.values_mut() {
        spans.sort_by_key(|s| (s.from, s.to));
        spans.dedup();
    }
    let mut attach = Attach {
        by_print,
        statement: SrcSpan::NONE,
    };
    crate::ast_visit::walk_stmts_mut(&mut attach, stmts);
}

struct Attach {
    by_print: HashMap<u64, Vec<SrcSpan>>,
    /// The range of the statement being walked.
    statement: SrcSpan,
}

impl Attach {
    /// The first unclaimed range noted for `print` inside the current
    /// statement.
    // Cost: O(c), c = ranges noted for `print`.
    fn claim(&mut self, print: u64) -> Option<SrcSpan> {
        let spans = self.by_print.get_mut(&print)?;
        let stmt = self.statement;
        let at = spans
            .iter()
            .position(|s| !stmt.is_some() || (s.from >= stmt.from && s.to <= stmt.to))?;
        Some(spans.remove(at))
    }
}

impl VisitMut for Attach {
    fn visit_stmts_mut(&mut self, body: &mut Vec<Stmt>) {
        let outer = self.statement;
        crate::ast_visit::walk_stmts_mut(self, body);
        self.statement = outer;
    }

    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        if let Stmt::SetLine(_, span) = stmt
            && span.is_some()
        {
            self.statement = *span;
        }
        crate::ast_visit::walk_stmt_mut(self, stmt);
    }

    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        if !matches!(expr, Expr::Spelled(_))
            && let Some(span) = self.claim(fingerprint(expr))
        {
            let inner = std::mem::replace(expr, Expr::Whatever);
            *expr = Expr::Spelled(Box::new(Spelled {
                expr: inner,
                spelling: Spelling::Origin(span),
            }));
            if let Expr::Spelled(spelled) = expr {
                crate::ast_visit::walk_expr_mut(self, &mut spelled.expr);
            }
            return;
        }
        crate::ast_visit::walk_expr_mut(self, expr);
    }
}
