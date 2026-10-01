//! The AST half of the frame-lexical proof (ADR-0113): which mentions of a
//! candidate routine's name a routine body makes, read off its AST through
//! the typed visitor (ADR-0137). See `frame_lexical_routines.rs`.

use crate::ast::{Expr, Stmt};
use crate::ast_visit::{NameKind, Visit, contains_word, walk_expr, walk_stmt};
use std::collections::{HashMap, HashSet};

/// Names that make the whole body ineligible: they reach a routine by a name
/// computed at run time, or observe the routine as a code object or through
/// the dispatcher.
const REJECT_ALL_NAMES: &[&str] = &[
    "EVAL",
    "EVALFILE",
    "evalbytes",
    "callframe",
    "callframes",
    "samewith",
    "callsame",
    "nextsame",
    "callwith",
    "nextwith",
    "nextcallee",
    "lastcall",
    "?ROUTINE",
    "&?ROUTINE",
];

/// Pseudo-packages that can reach a lexical by name.
const REJECT_ALL_PREFIXES: &[&str] = &[
    "MY::",
    "OUTER::",
    "OUTERS::",
    "CALLER::",
    "CALLERS::",
    "LEXICAL::",
    "UNIT::",
    "DYNAMIC::",
];

#[derive(Default)]
pub(super) struct AstScan {
    pub(super) names: HashSet<String>,
    pub(super) calls: HashMap<String, usize>,
    pub(super) decls: HashMap<String, usize>,
    /// Names read as a code object (`&name`, the `CodeVar` node).
    pub(super) values: HashSet<String>,
    pub(super) rejected: HashSet<String>,
    pub(super) reject_all: bool,
}

impl AstScan {
    // Cost: O(n), n = size of `body`'s AST.
    pub(super) fn scan(&mut self, body: &[Stmt]) {
        for stmt in body {
            self.visit_stmt(stmt);
        }
    }
}

impl Visit for AstScan {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        if self.reject_all {
            return;
        }
        // A lexical type declaration makes a parameter's type constraint
        // resolve differently per call, which the once-per-interpreter
        // derivation could not follow.
        if matches!(
            stmt,
            Stmt::ClassDecl { .. }
                | Stmt::RoleDecl { .. }
                | Stmt::EnumDecl { .. }
                | Stmt::SubsetDecl { .. }
                | Stmt::Package { .. }
                | Stmt::AugmentClass { .. }
        ) {
            self.reject_all = true;
            return;
        }
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        if self.reject_all {
            return;
        }
        // Symbolic and indirect lookups reach a routine by a computed name.
        if matches!(
            expr,
            Expr::IndirectCodeLookup { .. }
                | Expr::IndirectTypeLookup(..)
                | Expr::IndirectTypeLookupAssign { .. }
                | Expr::SymbolicDeref { .. }
                | Expr::SymbolicDerefAssign { .. }
                | Expr::PseudoStash(..)
                | Expr::UserRoutineCall { .. }
                | Expr::RoutineMagic
        ) {
            self.reject_all = true;
            return;
        }
        walk_expr(self, expr);
    }

    fn visit_name(&mut self, s: &str, kind: NameKind) {
        if self.reject_all {
            return;
        }
        if REJECT_ALL_PREFIXES.iter().any(|p| s.contains(p)) {
            self.reject_all = true;
            return;
        }
        // Source text compiled later (a `s///` replacement, a regex code
        // block's text) can call anything it names.
        if kind == NameKind::Source {
            if REJECT_ALL_NAMES.iter().any(|w| contains_word(s, w)) {
                self.reject_all = true;
                return;
            }
            for name in &self.names {
                if contains_word(s, name) {
                    self.rejected.insert(name.clone());
                }
            }
            return;
        }
        if REJECT_ALL_NAMES.contains(&s) {
            self.reject_all = true;
            return;
        }
        if self.names.contains(s) {
            match kind {
                NameKind::Call => *self.calls.entry(s.to_string()).or_default() += 1,
                NameKind::SubDecl => *self.decls.entry(s.to_string()).or_default() += 1,
                NameKind::CodeVar => {
                    self.values.insert(s.to_string());
                }
                // A scalar or sigilless variable of the same name (`$name`
                // is `name` in the AST): a different symbol than `&name`.
                NameKind::Var
                | NameKind::MarkBoundContainer
                | NameKind::VarDecl
                | NameKind::AssignTarget
                | NameKind::MarkReadonly
                | NameKind::Param => {}
                _ => {
                    self.rejected.insert(s.to_string());
                }
            }
            return;
        }
        // `&name`, `Pkg::name`, `&Pkg::name`: the routine as a code object or
        // by a qualified name.
        let tail = s.rsplit("::").next().unwrap_or(s);
        let tail = tail.strip_prefix('&').unwrap_or(tail);
        if tail != s && self.names.contains(tail) {
            self.rejected.insert(tail.to_string());
        }
    }
}

/// Whether `stmt` names `sym` at all — as a call, a variable, a type, a
/// method, by a qualified name or in source text compiled later. String
/// literals do not count.
// Cost: O(n), n = size of `stmt`'s AST.
pub(super) fn stmt_names(stmt: &Stmt, sym: &str) -> bool {
    struct Find<'a> {
        sym: &'a str,
        found: bool,
    }
    impl Visit for Find<'_> {
        fn visit_stmt(&mut self, stmt: &Stmt) {
            if !self.found {
                walk_stmt(self, stmt);
            }
        }
        fn visit_expr(&mut self, expr: &Expr) {
            if !self.found {
                walk_expr(self, expr);
            }
        }
        fn visit_name(&mut self, s: &str, kind: NameKind) {
            let tail = s.rsplit("::").next().unwrap_or(s);
            let tail = tail.strip_prefix('&').unwrap_or(tail);
            self.found |= s == self.sym
                || tail == self.sym
                || (kind == NameKind::Source && contains_word(s, self.sym));
        }
    }
    let mut find = Find { sym, found: false };
    find.visit_stmt(stmt);
    find.found
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(src: &str) -> Vec<Stmt> {
        crate::parser::parse_program(src).expect("parse").0
    }

    fn scan(src: &str, name: &str) -> AstScan {
        let mut s = AstScan {
            names: [name.to_string()].into_iter().collect(),
            ..AstScan::default()
        };
        s.scan(&parse(src));
        s
    }

    #[test]
    fn string_literal_naming_a_reject_word_does_not_reject_all() {
        let s = scan(r#"my sub f() { 1 }; say "EVAL"; say 'samewith'; f()"#, "f");
        assert!(!s.reject_all);
        assert_eq!(s.calls.get("f"), Some(&1));
        assert_eq!(s.decls.get("f"), Some(&1));
    }

    #[test]
    fn a_call_to_eval_rejects_all() {
        assert!(scan(r#"my sub f() { 1 }; EVAL "f()""#, "f").reject_all);
    }

    #[test]
    fn a_string_literal_with_the_routine_name_is_not_a_mention() {
        let s = scan(r#"my sub f() { 1 }; say "f"; f()"#, "f");
        assert!(s.rejected.is_empty());
    }

    #[test]
    fn a_qualified_or_code_object_mention_is_recorded() {
        let s = scan("my sub f() { 1 }; say &f; f()", "f");
        assert!(s.values.contains("f"));
        let s = scan("my sub f() { 1 }; say $x.f; f()", "f");
        assert!(s.rejected.contains("f"));
    }

    #[test]
    fn stmt_names_ignores_string_literals() {
        let names = |src: &str| parse(src).iter().any(|s| stmt_names(s, "g"));
        assert!(!names(r#"say "g""#));
        assert!(names("g()"));
        assert!(names("say &Foo::g"));
    }
}
