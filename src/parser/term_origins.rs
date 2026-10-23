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
//! An origin is exact or absent, never a guess. A note belongs to the
//! innermost statement whose range holds it, and within that statement the
//! k-th term of a fingerprint in source order takes the k-th noted range of
//! that fingerprint. Every term the parser read left a note, so:
//!
//! - **More notes than terms** means a note no term owns: an alternative the
//!   parser tried and abandoned. Which note is the stray one cannot be told,
//!   so that fingerprint gets no origins in that statement at all.
//! - **Fewer notes than terms** means terms the parser made rather than read:
//!   a copy a desugaring made (`$x += 1` keeps `$x` twice) or a synthesized
//!   term (the `__mutsu_test_callsite_line` literal of a `Test` call, the
//!   `Nil` of `my $x`). A copy spells the same source text, so either copy's
//!   range is right; a synthesized term is not rendered by the RakuAST view,
//!   so a range it takes is never shown, and the term it took it from shows
//!   none. Both outcomes are exact or absent.
//!
//! An attached range must also lie inside the range of the nearest enclosing
//! term that has one; a range that does not is dropped.

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

/// A term's structural fingerprint, read through `Expr::WhateverCurry`
/// markers: the whole-unit pass that places them (`whatever_curry::mark`)
/// runs between the parser noting a term and [`attach`] finding it again, so
/// a term holding a `*` (`.map(* + 1)`) must fingerprint the same with or
/// without them.
// Cost: O(n), n = size of `expr` (twice when it holds a marker).
fn fingerprint(expr: &Expr) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    if HasMarker::in_expr(expr) {
        let mut plain = expr.clone();
        Uncurry.visit_expr_mut(&mut plain);
        plain.hash(&mut hasher);
    } else {
        expr.hash(&mut hasher);
    }
    hasher.finish()
}

/// Whether an expression holds a `WhateverCurry` marker or a curried `*`.
struct HasMarker(bool);

impl HasMarker {
    // Cost: O(n), n = size of `expr`.
    fn in_expr(expr: &Expr) -> bool {
        let mut found = HasMarker(false);
        crate::ast_visit::Visit::visit_expr(&mut found, expr);
        found.0
    }
}

impl<'ast> crate::ast_visit::Visit<'ast> for HasMarker {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.0 {
            return;
        }
        if matches!(expr, Expr::WhateverCurry(_) | Expr::WhateverArg) {
            self.0 = true;
            return;
        }
        crate::ast_visit::walk_expr(self, expr);
    }
}

/// Takes the `WhateverCurry` markers out of an expression, and turns the
/// `WhateverArg`s they curry back into the `*` the parser read.
struct Uncurry;

impl VisitMut for Uncurry {
    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        while matches!(expr, Expr::WhateverCurry(_)) {
            let Expr::WhateverCurry(inner) = std::mem::replace(expr, Expr::Whatever) else {
                unreachable!("matched above");
            };
            *expr = *inner;
        }
        // The pass also turns the `*` it curries into its argument.
        if matches!(expr, Expr::WhateverArg) {
            *expr = Expr::Whatever;
        }
        crate::ast_visit::walk_expr_mut(self, expr);
    }
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
// Cost: O(n·d + k log k + k·h), n = expressions in the unit, d = their depth,
// k = notes, h = statement nesting depth.
pub(crate) fn attach(stmts: &mut [Stmt]) {
    let Some(notes) = NOTES.with(|n| n.borrow_mut().take()) else {
        return;
    };
    // Pass 1: every term's statement and fingerprint, in visit order.
    let mut walk = Walk {
        mode: Mode::Collect,
        statement: SrcSpan::NONE,
        terms: Vec::new(),
        statements: Vec::new(),
        next: 0,
        ranges: HashMap::new(),
        enclosing: Vec::new(),
    };
    crate::ast_visit::walk_stmts_mut(&mut walk, stmts);
    let ranges = matched_ranges(notes, &walk.terms, walk.statements.clone());
    if ranges.is_empty() {
        return;
    }
    // Pass 2: the same walk, attaching.
    walk.mode = Mode::Attach;
    walk.statement = SrcSpan::NONE;
    walk.ranges = ranges;
    crate::ast_visit::walk_stmts_mut(&mut walk, stmts);
}

/// A statement range and a fingerprint.
type Key = (u32, u32, u64);

// Cost: O(1).
fn key(statement: SrcSpan, print: u64) -> Key {
    (statement.from, statement.to, print)
}

/// The ranges each (statement, fingerprint) pair may attach, in source order:
/// only the pairs with no more notes than terms (see the module docs).
// Cost: O(n + k log k + s log s + k·h), n = terms, k = notes, s = statements,
// h = statement nesting depth.
fn matched_ranges(
    notes: Vec<(u64, SrcSpan)>,
    terms: &[(SrcSpan, u64)],
    statements: Vec<SrcSpan>,
) -> HashMap<Key, std::collections::VecDeque<SrcSpan>> {
    let mut wanted: HashMap<Key, usize> = HashMap::new();
    for &(statement, print) in terms {
        *wanted.entry(key(statement, print)).or_default() += 1;
    }
    let mut noted: HashMap<Key, Vec<SrcSpan>> = HashMap::new();
    for (statement, print, span) in bucket_by_statement(notes, statements) {
        noted.entry(key(statement, print)).or_default().push(span);
    }
    noted
        .into_iter()
        .filter_map(|(k, mut spans)| {
            spans.sort_by_key(|s| (s.from, s.to));
            spans.dedup();
            (spans.len() <= wanted.get(&k).copied().unwrap_or(0)).then(|| (k, spans.into()))
        })
        .collect()
}

/// Each note with the innermost statement range that holds it (`NONE` when
/// none does). Statement ranges nest, so the ranges open at a note's start
/// form a chain; the innermost one that also reaches its end owns it.
// Cost: O(k log k + s log s + k·h), k = notes, s = statements, h = statement
// nesting depth.
fn bucket_by_statement(
    mut notes: Vec<(u64, SrcSpan)>,
    mut statements: Vec<SrcSpan>,
) -> Vec<(SrcSpan, u64, SrcSpan)> {
    notes.sort_by_key(|(_, s)| (s.from, s.to));
    statements.retain(|s| s.is_some());
    statements.sort_by_key(|s| (s.from, std::cmp::Reverse(s.to)));
    statements.dedup();
    let mut open: Vec<SrcSpan> = Vec::new();
    let mut next = 0;
    let mut out = Vec::with_capacity(notes.len());
    for (print, span) in notes {
        while next < statements.len() && statements[next].from <= span.from {
            let start = statements[next];
            while open.last().is_some_and(|top| top.to <= start.from) {
                open.pop();
            }
            open.push(start);
            next += 1;
        }
        while open.last().is_some_and(|top| top.to <= span.from) {
            open.pop();
        }
        let owner = open
            .iter()
            .rev()
            .find(|s| s.to >= span.to)
            .copied()
            .unwrap_or(SrcSpan::NONE);
        out.push((owner, print, span));
    }
    out
}

#[derive(PartialEq)]
enum Mode {
    Collect,
    Attach,
}

/// Both passes of [`attach`]: one traversal, so the k-th term of the second
/// pass is the k-th term the first pass fingerprinted.
struct Walk {
    mode: Mode,
    /// The range of the statement being walked.
    statement: SrcSpan,
    /// Pass 1's output: each term's statement and fingerprint.
    terms: Vec<(SrcSpan, u64)>,
    /// Pass 1's output: every statement range seen.
    statements: Vec<SrcSpan>,
    /// Pass 2: the index into `terms` of the next term.
    next: usize,
    /// Pass 2: the ranges still to attach per (statement, fingerprint).
    ranges: HashMap<Key, std::collections::VecDeque<SrcSpan>>,
    /// Pass 2: the ranges of the attached terms enclosing the current one.
    enclosing: Vec<SrcSpan>,
}

impl Walk {
    /// The range the current term takes, if its pair matched and the range
    /// lies inside the nearest attached enclosing term.
    // Cost: O(1).
    fn claim(&mut self, print: u64) -> Option<SrcSpan> {
        let span = self
            .ranges
            .get_mut(&key(self.statement, print))?
            .pop_front()?;
        let inside = self
            .enclosing
            .last()
            .is_none_or(|outer| outer.from <= span.from && span.to <= outer.to);
        inside.then_some(span)
    }
}

impl VisitMut for Walk {
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
            if self.mode == Mode::Collect {
                self.statements.push(*span);
            }
        }
        crate::ast_visit::walk_stmt_mut(self, stmt);
    }

    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        // A term another parse already gave an origin to (a nested `.AST`
        // unit) keeps it. Other spellings (`ImportedTerm`, `Words`, ...) are
        // terms the parser read and noted like any other.
        if let Expr::Spelled(spelled) = expr
            && matches!(spelled.spelling, Spelling::Origin(_))
        {
            crate::ast_visit::walk_expr_mut(self, expr);
            return;
        }
        if self.mode == Mode::Collect {
            self.terms.push((self.statement, fingerprint(expr)));
            crate::ast_visit::walk_expr_mut(self, expr);
            return;
        }
        let print = self.terms[self.next].1;
        self.next += 1;
        let claimed = self.claim(print);
        // A `*` the whole-unit pass curried is the curried code's argument,
        // which Rakudo gives no origin: it takes its note (so the `*`s after
        // it keep theirs) and attaches none.
        let Some(span) = claimed.filter(|_| !matches!(expr, Expr::WhateverArg)) else {
            crate::ast_visit::walk_expr_mut(self, expr);
            return;
        };
        let inner = std::mem::replace(expr, Expr::Whatever);
        *expr = Expr::Spelled(Box::new(Spelled {
            expr: inner,
            spelling: Spelling::Origin(span),
        }));
        if let Expr::Spelled(spelled) = expr {
            self.enclosing.push(span);
            crate::ast_visit::walk_expr_mut(self, &mut spelled.expr);
            self.enclosing.pop();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sp(from: u32, to: u32) -> SrcSpan {
        SrcSpan { from, to }
    }

    #[test]
    fn notes_belong_to_the_innermost_statement() {
        // `f({ $x }, $x)`: the closure's statement 3..5 sits inside 0..12.
        let out = bucket_by_statement(
            vec![(1, sp(9, 11)), (1, sp(3, 5))],
            vec![sp(0, 12), sp(3, 5)],
        );
        assert_eq!(
            out,
            vec![(sp(3, 5), 1, sp(3, 5)), (sp(0, 12), 1, sp(9, 11))]
        );
    }

    #[test]
    fn a_surplus_note_attaches_nothing_for_its_fingerprint() {
        let stmt = sp(0, 20);
        // One term of fingerprint 7, but an abandoned alternative noted a
        // second range for it: neither is attached.
        let ranges = matched_ranges(
            vec![(7, sp(2, 4)), (7, sp(10, 12)), (8, sp(14, 16))],
            &[(stmt, 7), (stmt, 8)],
            vec![stmt],
        );
        assert!(!ranges.contains_key(&key(stmt, 7)));
        assert_eq!(ranges[&key(stmt, 8)], [sp(14, 16)]);
    }

    #[test]
    fn a_copied_or_synthesized_term_leaves_the_range_to_the_first() {
        let stmt = sp(0, 20);
        // `$x += 1` keeps two copies of `$x`, read once.
        let ranges = matched_ranges(vec![(7, sp(0, 2))], &[(stmt, 7), (stmt, 7)], vec![stmt]);
        assert_eq!(ranges[&key(stmt, 7)], [sp(0, 2)]);
    }

    #[test]
    fn equal_terms_take_their_ranges_in_source_order() {
        let stmt = sp(0, 20);
        let ranges = matched_ranges(
            vec![(7, sp(10, 12)), (7, sp(2, 4)), (7, sp(2, 4))],
            &[(stmt, 7), (stmt, 7)],
            vec![stmt],
        );
        assert_eq!(ranges[&key(stmt, 7)], [sp(2, 4), sp(10, 12)]);
    }
}
