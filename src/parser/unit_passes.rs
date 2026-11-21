//! The parser's passes over a finished compilation unit, run one top-level
//! statement at a time (ADR-15760 §7.2 (b), slice 2d).
//!
//! Each pass that used to walk the whole unit after the parse keeps its state
//! across the unit's top-level statements instead, so it can run as each
//! statement is committed: [`UnitPasses::commit`] takes one statement, and
//! [`UnitPasses::finish`] settles what only the whole unit decides (which
//! diagnostic wins, the value-tail exemption of the sink analysis, the soft
//! sorries a `CHECK` phaser defers). The result is the one the whole-unit walks
//! gave: every pass visits the statements in source order, and a statement's
//! result depends only on it and the statements before it.

use std::cell::Cell;
use std::collections::HashSet;

use crate::ast::Stmt;
use crate::ast_visit::Visit;

use super::outer_redecl::{ScopeDiagnostic, UnitScan};
use super::sink_warn;
use super::whenever_scope::WheneverScope;

/// A parse warning as `PARSE_WARNINGS` holds it.
type Warning = (Option<String>, String);

/// The per-unit passes of one parse, fed its top-level statements in order.
pub(super) struct UnitPasses {
    whenever: WheneverScope,
    redecl: UnitScan,
    /// `None` for a parse whose sink warnings are suppressed.
    sink: Option<UnitSink>,
    /// A top-level `CHECK` phaser defers the grammar's soft sorries.
    has_check: bool,
}

impl UnitPasses {
    // Cost: O(1).
    pub(super) fn new(value_tail: bool, suppress_sink: bool) -> Self {
        UnitPasses {
            whenever: WheneverScope::default(),
            redecl: UnitScan::new(),
            sink: (!suppress_sink).then(|| UnitSink::new(value_tail)),
            has_check: false,
        }
    }

    /// Run the passes on the unit's next top-level statement.
    // Cost: O(s·d), s = size of the statement, d = its lexical scope depth.
    pub(super) fn commit(&mut self, stmt: &mut Stmt) {
        // ADR-0033 Phase 2: classify every `*` leaf as a value or a priming
        // argument; the CORE type fold (ADR-0115) rides the same walk.
        crate::whatever_curry::mark::mark_program(std::slice::from_mut(stmt));
        // `use trace` hooks hold their statement's source offset until the
        // unit's statement attempts before them are known.
        super::stmt::trace::number_statements_so_far(stmt);
        self.has_check |= matches!(
            stmt,
            Stmt::Phaser {
                kind: crate::ast::PhaserKind::Check,
                ..
            }
        );
        self.whenever.visit_stmt(stmt);
        self.redecl.scan(stmt);
        if let Some(sink) = &mut self.sink {
            sink.statement(stmt);
        }
    }

    /// The line of the first `whenever` outside a `supply` / `react` block.
    // Cost: O(1).
    pub(super) fn whenever_out_of_scope(&self) -> Option<i64> {
        self.whenever.found()
    }

    /// The first scope diagnostic of `outer_redecl`.
    // Cost: O(1).
    pub(super) fn scope_diagnostic(&self) -> Option<&ScopeDiagnostic> {
        self.redecl.found()
    }

    /// Whether a top-level `CHECK` phaser was committed.
    // Cost: O(1).
    pub(super) fn has_check(&self) -> bool {
        self.has_check
    }

    /// Publish the sink warnings of a unit whose parse succeeded.
    // Cost: O(w), w = sink warnings found.
    pub(super) fn publish_sink_warnings(self) {
        if let Some(sink) = self.sink {
            sink.publish();
        }
    }
}

/// The "Useless use of ... in sink context" analysis of the unit's mainline
/// statement list (`sink_warn`), statement by statement. Its warnings are
/// held until the unit is settled, then published in the order the whole-unit
/// walks gave them: the mainline walk's, then the `gather` bodies', then the
/// source `do` blocks'.
struct UnitSink {
    /// An `EVAL` unit: its last real statement is its value, not sunk.
    value_tail: bool,
    line: Cell<i64>,
    do_scan: sink_warn::SourceDoScan,
    /// The scalars the top-level list declared so far (see
    /// `sink_warn::is_decl_value_head`).
    declared: HashSet<String>,
    walked: Vec<Warning>,
    /// For a value-tail unit: the mainline warnings of the last real
    /// statement so far, and of the markers after it.
    pending: Vec<Warning>,
    after_pending: Vec<Warning>,
    gathers: Vec<Warning>,
    dos: Vec<Warning>,
}

impl UnitSink {
    // Cost: O(1).
    fn new(value_tail: bool) -> Self {
        UnitSink {
            value_tail,
            line: Cell::new(1),
            do_scan: sink_warn::SourceDoScan::new(),
            declared: HashSet::new(),
            walked: Vec::new(),
            pending: Vec::new(),
            after_pending: Vec::new(),
            gathers: Vec::new(),
            dos: Vec::new(),
        }
    }

    // Cost: O(s), s = size of the statement (plus the sink walk of each gather
    // and do-block body in it).
    fn statement(&mut self, stmt: &Stmt) {
        let mainline = super::capture_parse_warnings(|| {
            if self.value_tail {
                sink_warn::walk_stmt(stmt, false, &self.line);
            } else {
                self.walk_listed(stmt);
            }
        });
        if !self.value_tail {
            self.walked.extend(mainline);
        } else if crate::ast::TailSkip::Markers.skips(stmt) {
            self.after_pending.extend(mainline);
        } else {
            self.walked.append(&mut self.pending);
            self.walked.append(&mut self.after_pending);
            self.pending = mainline;
        }
        if let Some(name) = sink_warn::declared_scalar(stmt) {
            self.declared.insert(name.to_owned());
        }
        self.gathers.extend(super::capture_parse_warnings(|| {
            sink_warn::GatherScan.visit_stmt(stmt)
        }));
        let do_scan = &mut self.do_scan;
        self.dos
            .extend(super::capture_parse_warnings(|| do_scan.visit_stmt(stmt)));
    }

    /// One statement of the mainline list (`sink_warn`'s list walk): a
    /// declaration's trailing value list warns for its items only.
    // Cost: O(s), s = size of the statement.
    fn walk_listed(&self, stmt: &Stmt) {
        if let Stmt::Expr(crate::ast::Expr::ArrayLiteral(elems)) = stmt
            && sink_warn::decl_value_head(elems).is_some_and(|head| self.declared.contains(head))
        {
            for e in &elems[1..] {
                sink_warn::warn_expr_sink(e, false, self.line.get());
            }
            return;
        }
        sink_warn::walk_stmt(stmt, false, &self.line);
    }

    // Cost: O(w), w = warnings held.
    fn publish(mut self) {
        if !self.value_tail {
            self.walked.append(&mut self.pending);
        }
        self.walked.append(&mut self.after_pending);
        let warnings = self.walked.into_iter().chain(self.gathers).chain(self.dos);
        super::PARSE_WARNINGS.with(|w| w.borrow_mut().extend(warnings));
    }
}
