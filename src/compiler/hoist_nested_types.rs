//! Compile-time installation of `our`-scoped types declared inside code
//! (#10470).
//!
//! Rakudo installs an `our` class/role into its package while compiling the
//! declaration, so `sub f { class K { } }; say K` prints `(K)` although `f`
//! never runs. mutsu registers such a declaration where it textually sits
//! (`RegisterDecl` in the routine's bytecode), i.e. only when the enclosing
//! routine or block runs. This pass walks the compilation unit for type
//! declarations nested in code and emits a declaration-only shell
//! registration for each at the head of the unit, exactly like
//! [`Compiler::hoist_type_decl_shells`] does for the unit's own forward
//! references. The in-place registration still runs on every entry of the
//! enclosing code and re-registers the full type under the same name, so the
//! class body's statements keep running at run time (as in Rakudo) and the
//! type object stays the same one.

use super::Compiler;
use crate::ast::Stmt;
use crate::ast_visit::{Visit, walk_stmt};

/// Collects class/role declarations nested in code, each with the package it
/// is installed in.
struct NestedTypeDecls<'a> {
    /// The packages enclosing the statement being visited, outermost first,
    /// each fully qualified. Empty = the unit's own package.
    packages: Vec<String>,
    /// Whether the statement being visited sits inside code (a routine,
    /// block, closure or statement body) rather than directly in a
    /// declaration scope (the unit, a package, class or role body), where the
    /// declaration registers before or as its scope's code runs.
    in_code: bool,
    /// `(enclosing package, declaration)`; `None` = the unit's own package.
    found: Vec<(Option<String>, Stmt)>,
    /// Qualifies an outermost package name against the unit's package.
    qualify_outer: &'a dyn Fn(&str) -> String,
}

impl NestedTypeDecls<'_> {
    // Cost: O(len(name)).
    fn qualified(&self, name: &str) -> String {
        if let Some(absolute) = name.strip_prefix("GLOBAL::") {
            return absolute.to_string();
        }
        match self.packages.last() {
            Some(outer) => format!("{outer}::{name}"),
            None => (self.qualify_outer)(name),
        }
    }

    /// Walk a package-like declaration's body with `name` as the enclosing
    /// package, in declaration (not code) context.
    // Cost: O(n), n = size of the declaration's subtree.
    fn walk_package(&mut self, name: &str, stmt: &Stmt) {
        let qualified = self.qualified(name);
        self.packages.push(qualified);
        let saved = std::mem::replace(&mut self.in_code, false);
        walk_stmt(self, stmt);
        self.in_code = saved;
        self.packages.pop();
    }

    fn record(&mut self, stmt: &Stmt) {
        self.found
            .push((self.packages.last().cloned(), stmt.clone()));
    }
}

impl Visit for NestedTypeDecls<'_> {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::ClassDecl {
                name,
                is_lexical,
                is_unit,
                name_expr,
                body,
                ..
            } => {
                // A lexical or runtime-named class is not installed in a
                // package, and neither is anything nested in it that this
                // pass could name.
                if *is_lexical || name_expr.is_some() {
                    return;
                }
                if self.in_code && !*is_unit && !Compiler::is_stub_class_body(body) {
                    self.record(stmt);
                }
                self.walk_package(&name.resolve(), stmt);
            }
            Stmt::RoleDecl {
                name,
                custom_traits,
                ..
            } => {
                if custom_traits.iter().any(|(t, _)| t == "__my_scoped") {
                    return;
                }
                if self.in_code {
                    self.record(stmt);
                }
                self.walk_package(&name.resolve(), stmt);
            }
            Stmt::Package {
                name,
                is_unit: true,
                ..
            } => {
                // `unit module M;` packages the rest of the scope: it stays
                // the enclosing package for every following statement.
                let qualified = self.qualified(&name.resolve());
                self.packages.push(qualified);
            }
            Stmt::Package { is_my: true, .. } => {}
            Stmt::Package { name, .. } => self.walk_package(&name.resolve(), stmt),
            // Transparent groupings: they open no scope that runs later.
            Stmt::SetLine(_) | Stmt::SyntheticBlock(_) => walk_stmt(self, stmt),
            _ => {
                let saved = std::mem::replace(&mut self.in_code, true);
                walk_stmt(self, stmt);
                self.in_code = saved;
            }
        }
    }
}

impl Compiler {
    /// Emit a declaration-only shell registration for every non-lexical
    /// class/role declared inside code anywhere in the unit (see the module
    /// doc comment). Only a mainline unit runs this pass: a routine body is
    /// part of a unit that already shelled its nested declarations.
    // Cost: O(n), n = size of the unit's AST.
    pub(super) fn hoist_nested_type_decl_shells(&mut self, stmts: &[Stmt]) {
        let found = {
            let qualify_outer = |name: &str| self.qualify_package_name(name);
            let mut collector = NestedTypeDecls {
                packages: Vec::new(),
                in_code: false,
                found: Vec::new(),
                qualify_outer: &qualify_outer,
            };
            for stmt in stmts {
                collector.visit_stmt(stmt);
            }
            collector.found
        };
        if found.is_empty() {
            return;
        }
        let original_package = self.current_package.clone();
        let original_in_unit_package = self.in_unit_package;
        for (package, decl) in &found {
            match package {
                Some(package) => {
                    self.current_package = package.clone();
                    self.in_unit_package = true;
                }
                None => {
                    self.current_package = original_package.clone();
                    self.in_unit_package = original_in_unit_package;
                }
            }
            self.emit_type_decl_shell(decl);
        }
        self.current_package = original_package;
        self.in_unit_package = original_in_unit_package;
    }
}
