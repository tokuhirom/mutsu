//! A class or package body the BEGIN prologue split in two (ADR-0134 §7):
//! the declaration with its BEGIN-time part, and later a
//! `Stmt::PackageRuntimeBody` with the body's run-time statements. Both
//! halves are one lexical scope, so a call in the run-time half sees the
//! routines the declaration's body declares.

use super::scan::Scan;
use crate::ast::Stmt;
use crate::ast_visit::{walk_stmt, walk_stmts};
use crate::symbol::Symbol;
use std::collections::HashSet;

impl Scan {
    /// Walk a class or package declaration, remembering its body's scope.
    // Cost: O(n + f), n = size of the declaration's AST, f = scopes it opens.
    pub(super) fn walk_package_decl(&mut self, stmt: &Stmt, name: Symbol) {
        let first = self.frames.len();
        walk_stmt(self, stmt);
        // The body is the last scope the declaration opened directly in the
        // current one.
        if let Some(body) = (first..self.frames.len())
            .rev()
            .find(|&i| self.frames[i].parent == self.current)
        {
            self.package_frames.insert(name, body);
        }
    }

    /// Walk the run-time half of a split body in the declaration's body
    /// scope (a fresh scope when the declaration was not walked).
    // Cost: O(n), n = size of the run-time statements' AST.
    pub(super) fn walk_package_runtime_body(&mut self, stmt: &Stmt, name: Symbol, body: &[Stmt]) {
        let Some(frame) = self.package_frames.get(&name).copied() else {
            walk_stmt(self, stmt);
            return;
        };
        let saved = self.current;
        self.current = Some(frame);
        self.terms.push(HashSet::new());
        walk_stmts(self, body);
        self.terms.pop();
        self.current = saved;
    }
}
