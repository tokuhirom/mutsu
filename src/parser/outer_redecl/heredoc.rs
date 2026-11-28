//! The heredoc body name checks of rakudo 2026.11 (#19284).
//!
//! A heredoc body is written below the line its heredoc starts on, but its
//! code runs where the heredoc starts, so its names bind in the scopes there.
//! A name in the body must therefore mean the same declaration in both
//! places. When it does not, rakudo refuses the body at compile time:
//!
//! - `X::Syntax::Heredoc::HiddenName`: the name is declared only in a block
//!   that closes on the heredoc's line, before the body;
//! - `X::Syntax::Heredoc::AmbiguousName`: the name means one declaration where
//!   the heredoc starts and another where its body is written. That happens
//!   when the closed block's name has an outer namesake, when a declaration
//!   later on the heredoc's line introduces it, or when a scope opened later
//!   on that line declares it after the body.
//!
//! The walk records each heredoc where it starts (a snapshot of the scopes
//! and what they declare so far), and compares that with the scopes where
//! the body is written. The parser counts the enclosing blocks that close on
//! the heredoc's line; the walk is where the body is written once it reaches
//! a statement on a later line, is about to close a further enclosing scope,
//! or ends the unit.

use super::{Ctx, ScopeDiagnostic, ref_key};
use crate::ast::Expr;
use crate::ast_visit::Visit;
use std::collections::{HashMap, HashSet};

/// A heredoc whose body has not been reached yet.
pub(super) struct PendingHeredoc {
    /// The line the walk was on where the heredoc starts.
    line: i64,
    /// The body's text, to place a name's line.
    content: String,
    /// How many of the scopes where the heredoc starts close on its line,
    /// before the body.
    closes: usize,
    /// The lexical names the body reads (`$x`, `@a`).
    keys: Vec<String>,
    /// The scopes where the heredoc starts, outermost first: each one's id
    /// and the names it had declared by then.
    start: Vec<(u64, HashSet<String>)>,
    /// What each of those scopes had declared when it closed, for the ones
    /// that closed before the body.
    closed: HashMap<u64, HashSet<String>>,
}

/// A name a body used that a scope opened later on its line must not declare
/// after the body.
pub(super) struct Watch {
    scope: u64,
    key: String,
    line: i64,
}

impl Ctx {
    /// Records a heredoc where it starts.
    // Cost: O(c + s·d), c = length of the body, s = names the start scopes
    // declare, d = scope depth.
    pub(super) fn start_heredoc(&mut self, content: &str, closes: u32) {
        // A body the `qq` grammar rejects is reported where it compiles.
        let Ok(body) = crate::parser::interpolate_heredoc_content(content) else {
            return;
        };
        let mut names = BodyNames::default();
        names.visit_expr(&body);
        if names.keys.is_empty() {
            return;
        }
        let start = self
            .scopes
            .iter()
            .map(|s| (s.id, s.declared.clone()))
            .collect();
        self.heredocs.push(PendingHeredoc {
            line: self.line,
            content: content.to_string(),
            closes: closes as usize,
            keys: names.keys,
            start,
            closed: HashMap::new(),
        });
    }

    /// The walk reached line `line`: a heredoc that started on an earlier
    /// line has its body written here.
    // Cost: O(h·k·d), h = pending heredocs, k = names per body, d = scope depth.
    pub(super) fn reach_line(&mut self, line: i64) {
        if self.heredocs.iter().all(|h| h.line >= line) {
            return;
        }
        let (due, rest): (Vec<_>, Vec<_>) = std::mem::take(&mut self.heredocs)
            .into_iter()
            .partition(|h| h.line < line);
        self.heredocs = rest;
        for heredoc in due {
            self.check_heredoc(&heredoc);
        }
    }

    /// The unit ended: every heredoc still pending has its body here, and
    /// every watched scope is closed.
    // Cost: as [`Self::reach_line`].
    pub(super) fn finish_heredocs(&mut self) {
        self.reach_line(i64::MAX);
        let ids: Vec<u64> = self.scopes.iter().map(|s| s.id).collect();
        for id in ids.into_iter().rev() {
            self.close_scope(id);
        }
    }

    /// The scope `id` closes with what it declared: a heredoc awaiting its
    /// body keeps that for the comparison, and a watch on it fires if it
    /// declared the watched name.
    // Cost: O(h + w), h = pending heredocs, w = watches.
    pub(super) fn close_scope(&mut self, id: u64) {
        // A start scope outside the ones that close on the heredoc's line
        // closes after the body: the body is written before it.
        let (due, rest): (Vec<_>, Vec<_>) = std::mem::take(&mut self.heredocs)
            .into_iter()
            .partition(|h| {
                h.start
                    .iter()
                    .position(|(sid, _)| *sid == id)
                    .is_some_and(|at| at + h.closes < h.start.len())
            });
        self.heredocs = rest;
        for heredoc in due {
            self.check_heredoc(&heredoc);
        }
        let Some(scope) = self.scopes.iter().find(|s| s.id == id) else {
            return;
        };
        let declared = &scope.declared;
        for heredoc in &mut self.heredocs {
            if heredoc.start.iter().any(|(sid, _)| *sid == id) {
                heredoc.closed.insert(id, declared.clone());
            }
        }
        let fired = self
            .watches
            .iter()
            .find(|w| w.scope == id && declared.contains(&w.key))
            // Rakudo reports it at the later declaration.
            .map(|w| {
                (
                    w.key.clone(),
                    scope.decl_lines.get(&w.key).copied().unwrap_or(w.line),
                )
            });
        self.watches.retain(|w| w.scope != id);
        if let Some((key, line)) = fired
            && self.found.is_none()
        {
            self.found = Some(ScopeDiagnostic::HeredocAmbiguousName(key, line));
        }
    }

    /// Compares where `heredoc` starts with where its body is written.
    // Cost: O(k·d), k = names of the body, d = scope depth.
    fn check_heredoc(&mut self, heredoc: &PendingHeredoc) {
        for key in &heredoc.keys {
            if self.found.is_some() {
                return;
            }
            let line = heredoc.line + 1 + body_line_of(&heredoc.content, key);
            match self.heredoc_refusal(heredoc, key) {
                Refusal::None => {}
                Refusal::Hidden => {
                    self.found = Some(ScopeDiagnostic::HeredocHiddenName(key.clone(), line));
                }
                Refusal::Ambiguous => {
                    self.found = Some(ScopeDiagnostic::HeredocAmbiguousName(key.clone(), line));
                }
                Refusal::Watch(scopes) => {
                    for scope in scopes {
                        self.watches.push(Watch {
                            scope,
                            key: key.clone(),
                            line,
                        });
                    }
                }
            }
        }
    }

    /// Why the body may not use `key`, as rakudo's `IMPL-HEREDOC-LOOKUP`
    /// decides it.
    // Cost: O(d), d = scope depth.
    fn heredoc_refusal(&self, heredoc: &PendingHeredoc, key: &str) -> Refusal {
        let start = &heredoc.start;
        // Where the heredoc starts: the innermost scope that declared `key`
        // by then. A scope inside it that declares `key` later, before the
        // body, makes the body text mean that later declaration.
        let found_at = start
            .iter()
            .rposition(|(_, declared)| declared.contains(key));
        let inner_from = found_at.map_or(0, |i| i + 1);
        let later = start[inner_from..].iter().any(|(id, declared_then)| {
            let declared_now = self
                .scopes
                .iter()
                .find(|s| s.id == *id)
                .map(|s| &s.declared)
                .or_else(|| heredoc.closed.get(id));
            !declared_then.contains(key) && declared_now.is_some_and(|d| d.contains(key))
        });
        let declaring = found_at.map(|i| start[i].0);
        if later {
            // With nothing found where the heredoc starts, the name is just
            // undeclared there, which the compiler reports.
            return if declaring.is_some() {
                Refusal::Ambiguous
            } else {
                Refusal::None
            };
        }
        // Where the body is written: the innermost open scope declaring `key`.
        let written_at = self.scopes.iter().rposition(|s| s.declared.contains(key));
        let written = written_at.map(|i| self.scopes[i].id);
        match (declaring, written) {
            (Some(d), Some(w)) if d == w => {
                // The same declaration. A scope opened later on the line that
                // the body is written in must not declare it after the body.
                let opened_later: Vec<u64> = self.scopes[written_at.unwrap_or(0) + 1..]
                    .iter()
                    .map(|s| s.id)
                    .filter(|id| !start.iter().any(|(sid, _)| sid == id))
                    .collect();
                if opened_later.is_empty() {
                    Refusal::None
                } else {
                    Refusal::Watch(opened_later)
                }
            }
            (Some(_), Some(_)) => Refusal::Ambiguous,
            (Some(_), None) => Refusal::Hidden,
            // Undeclared where the heredoc starts: the compiler reports it.
            (None, _) => Refusal::None,
        }
    }
}

enum Refusal {
    None,
    Hidden,
    Ambiguous,
    /// The same declaration in both places, but these scopes, opened later on
    /// the heredoc's line, must not declare the name after the body.
    Watch(Vec<u64>),
}

/// The 0-based line of the body that first mentions `key`.
// Cost: O(c), c = length of the body.
fn body_line_of(content: &str, key: &str) -> i64 {
    content
        .find(key)
        .map_or(0, |at| content[..at].matches('\n').count() as i64)
}

/// The lexical variables a heredoc body reads, in order, without repeats.
#[derive(Default)]
struct BodyNames {
    keys: Vec<String>,
}

impl<'ast> Visit<'ast> for BodyNames {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        let key = match expr {
            Expr::Var(n) => ref_key('$', n),
            Expr::ArrayVar(n) => ref_key('@', n),
            Expr::HashVar(n) => ref_key('%', n),
            _ => None,
        };
        if let Some(key) = key {
            if !self.keys.contains(&key) {
                self.keys.push(key);
            }
            return;
        }
        crate::ast_visit::walk_expr(self, expr);
    }
}
