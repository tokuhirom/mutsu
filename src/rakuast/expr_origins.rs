//! Origins of expression nodes (ADR-10723 Stage 2, expression families).
//!
//! A parse the RakuAST conversion reads attaches where each term was written,
//! as an `Expr::Spelled` wrapper with `Spelling::Origin` (`parser::term_origins`).
//! The conversion's shape matches look at the expressions themselves, so before
//! converting a unit the wrappers are taken off again in place. Each range is
//! kept by the address of the slot its term now sits in (the tree is not moved
//! or changed while it is converted, so the address names the slot for the
//! length of the conversion) and by the term's structural fingerprint, which
//! finds it again when the conversion converts a copy of the term instead of
//! the slot itself. The table is dropped with the conversion.

use std::cell::RefCell;
use std::hash::{Hash, Hasher};

use rustc_hash::{FxHashMap as HashMap, FxHasher};

use crate::ast::spelled::Spelling;
use crate::ast::{Expr, SrcSpan, Stmt};
use crate::ast_visit::VisitMut;

#[derive(Default)]
struct Table {
    /// Slot address → the term's fingerprint and range.
    by_slot: HashMap<usize, (u64, SrcSpan)>,
    /// Fingerprint → the unclaimed ranges of terms with it, in source order.
    by_print: HashMap<u64, Vec<SrcSpan>>,
}

thread_local! {
    static ORIGINS: RefCell<Table> = RefCell::new(Table::default());
    /// The range of the statement being converted: a copy the conversion
    /// made of a term is a copy of a term of that statement.
    static STATEMENT: std::cell::Cell<SrcSpan> = const { std::cell::Cell::new(SrcSpan::NONE) };
}

/// While alive, the statement written at the range it was entered with is
/// the one being converted (see [`of`]).
pub(super) struct InStatement(Option<SrcSpan>);

impl InStatement {
    /// Enter the statement written at `span`; with no range, the enclosing
    /// statement stays the one being converted.
    // Cost: O(1).
    pub(super) fn enter(span: Option<SrcSpan>) -> InStatement {
        InStatement(
            span.filter(|s| s.is_some())
                .map(|s| STATEMENT.with(|c| c.replace(s))),
        )
    }
}

impl Drop for InStatement {
    fn drop(&mut self) {
        if let Some(previous) = self.0 {
            STATEMENT.with(|c| c.set(previous));
        }
    }
}

/// While alive, the ranges [`strip`] took off a unit answer [`of`].
pub(super) struct Scope {
    previous: Table,
}

impl Drop for Scope {
    fn drop(&mut self) {
        let previous = std::mem::take(&mut self.previous);
        ORIGINS.with(|o| *o.borrow_mut() = previous);
    }
}

// Cost: O(n), n = size of `expr`.
fn fingerprint(expr: &Expr) -> u64 {
    let mut hasher = FxHasher::default();
    expr.hash(&mut hasher);
    hasher.finish()
}

/// Takes the wrappers off, keeping each range by slot.
struct Strip(HashMap<usize, SrcSpan>);

impl VisitMut for Strip {
    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        let mut span = None;
        while let Expr::Spelled(spelled) = expr
            && let Spelling::Origin(at) = spelled.spelling
        {
            span = span.or(Some(at));
            let Expr::Spelled(spelled) = std::mem::replace(expr, Expr::Whatever) else {
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

/// Fingerprints the stripped terms whose slots hold a range.
struct Print<'a> {
    spans: &'a HashMap<usize, SrcSpan>,
    table: Table,
}

impl VisitMut for Print<'_> {
    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        let slot = expr as *const Expr as usize;
        if let Some(span) = self.spans.get(&slot) {
            let print = fingerprint(expr);
            self.table.by_slot.insert(slot, (print, *span));
            self.table.by_print.entry(print).or_default().push(*span);
        }
        crate::ast_visit::walk_expr_mut(self, expr);
    }
}

/// Take the origin wrappers off `stmts`, keeping their ranges for [`of`] until
/// the returned scope ends.
// Cost: O(n·d), n = expressions in the tree, d = their depth.
pub(super) fn strip(stmts: &mut [Stmt]) -> Scope {
    let mut strip = Strip(HashMap::default());
    crate::ast_visit::walk_stmts_mut(&mut strip, stmts);
    let mut print = Print {
        spans: &strip.0,
        table: Table::default(),
    };
    crate::ast_visit::walk_stmts_mut(&mut print, stmts);
    let mut table = print.table;
    for spans in table.by_print.values_mut() {
        spans.sort_by_key(|s| (s.from, s.to));
    }
    let previous = ORIGINS.with(|o| std::mem::replace(&mut *o.borrow_mut(), table));
    Scope { previous }
}

/// Where the term `expr` was written, when a stripped unit holds it: by its
/// slot, or, for a copy the conversion made, by the one unclaimed range of a
/// term with the same shape in the statement being converted.
// Cost: O(1) for a slot, O(n + c) otherwise, n = size of `expr`, c = ranges
// of terms with its shape.
pub(super) fn of(expr: &Expr) -> Option<SrcSpan> {
    ORIGINS.with(|o| {
        let mut table = o.borrow_mut();
        if table.by_slot.is_empty() && table.by_print.is_empty() {
            return None;
        }
        let (print, span) = match table.by_slot.remove(&(expr as *const Expr as usize)) {
            Some(found) => found,
            None => {
                // A copy takes the range of the one unclaimed term of its
                // shape in its own statement; with several, which one it
                // copies cannot be told, and a term written elsewhere in the
                // unit is never one it copies.
                let statement = STATEMENT.with(|c| c.get());
                if !statement.is_some() {
                    return None;
                }
                let print = fingerprint(expr);
                let mut inside = table
                    .by_print
                    .get(&print)?
                    .iter()
                    .filter(|s| statement.from <= s.from && s.to <= statement.to);
                let span = *inside.next()?;
                if inside.next().is_some() {
                    return None;
                }
                (print, span)
            }
        };
        if let Some(spans) = table.by_print.get_mut(&print)
            && let Some(at) = spans.iter().position(|s| *s == span)
        {
            spans.remove(at);
        }
        Some(span)
    })
}
