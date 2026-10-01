//! Compile-time installation of `our`-scoped types declared inside code
//! (#10470, #10494).
//!
//! Rakudo installs an `our` class/role into its package while compiling the
//! declaration, and composes the class then, running its roles' bodies, so
//! `sub f { class K does R { } }; say K` prints `(K)` and has run `R`'s body
//! although `f` never runs. mutsu registers such a declaration where it
//! textually sits (`RegisterDecl` in the routine's bytecode), i.e. only when
//! the enclosing routine or block runs. So each one also gets a
//! declaration-only shell registration that runs at BEGIN time, exactly like
//! [`Compiler::hoist_type_decl_shells`] does for the unit's own forward
//! references. The in-place registration still runs on every entry of the
//! enclosing code and re-registers the full type under the same name, so the
//! class body's statements keep running at run time (as in Rakudo) and the
//! type object stays the same one.
//!
//! Where the shells run: the BEGIN prologue (ADR-0134) collects each
//! top-level statement's nested declarations ([`nested_type_decls`]) into a
//! [`Stmt::NestedTypeShells`] marker at that statement's place among the
//! unit's BEGIN-time effects, so a shell runs after the declarations that
//! precede it and sees the unit's lexicals in their static state: a role body
//! that bumps `my $n` declared above the routine leaves `$n` bumped. A unit
//! compiled without that partition shells its nested declarations at its head
//! instead ([`Compiler::hoist_nested_type_decl_shells`]).

use super::Compiler;
use crate::ast::{NestedTypeShell, Stmt};
use crate::ast_visit::{Visit, walk_stmt};

/// Collects class/role declarations nested in code, each with the packages
/// enclosing it.
struct NestedTypeDecls {
    /// The packages enclosing the statement being visited, outermost first,
    /// as written. Empty = the package the top-level statement is in.
    packages: Vec<String>,
    /// Whether the statement being visited sits inside code (a routine,
    /// block, closure or statement body) rather than directly in a
    /// declaration scope (the unit, a package, class or role body), where the
    /// declaration registers before or as its scope's code runs.
    in_code: bool,
    found: Vec<NestedTypeShell>,
}

impl NestedTypeDecls {
    /// Walk a package-like declaration's body with `name` as the enclosing
    /// package, in declaration (not code) context.
    // Cost: O(n), n = size of the declaration's subtree.
    fn walk_package(&mut self, name: &str, stmt: &Stmt) {
        self.packages.push(name.to_string());
        let saved = std::mem::replace(&mut self.in_code, false);
        walk_stmt(self, stmt);
        self.in_code = saved;
        self.packages.pop();
    }

    fn record(&mut self, stmt: &Stmt) {
        self.found.push(NestedTypeShell {
            packages: self.packages.clone(),
            decl: stmt.clone(),
        });
    }
}

impl Visit for NestedTypeDecls {
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
            // `unit module M;` packages the rest of the scope. The compiler
            // switches its package when it reaches the marker, so the
            // following statements' shells qualify against it there.
            Stmt::Package { is_unit: true, .. } => {}
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

/// The non-lexical classes and roles declared inside code anywhere in one
/// top-level statement, in source order (see the module doc comment).
// Cost: O(n), n = size of the statement's AST.
pub(crate) fn nested_type_decls(stmt: &Stmt) -> Vec<NestedTypeShell> {
    let mut collector = NestedTypeDecls {
        packages: Vec::new(),
        in_code: false,
        found: Vec::new(),
    };
    collector.visit_stmt(stmt);
    collector.found
}

impl Compiler {
    /// Emit the shell registration of one nested declaration, qualified
    /// against the packages enclosing it inside its top-level statement.
    // Cost: O(p + d), p = length of the package path, d = size of the
    // declaration's shell.
    pub(super) fn emit_nested_type_shell(&mut self, shell: &NestedTypeShell) {
        let saved = (self.current_package.clone(), self.in_unit_package);
        let mut package: Option<String> = None;
        for segment in &shell.packages {
            package = Some(match (segment.strip_prefix("GLOBAL::"), &package) {
                (Some(absolute), _) => absolute.to_string(),
                (None, Some(outer)) => format!("{outer}::{segment}"),
                (None, None) => self.qualify_package_name(segment),
            });
        }
        if let Some(package) = package {
            self.current_package = package;
            self.in_unit_package = true;
        }
        self.emit_type_decl_shell(&shell.decl, true);
        (self.current_package, self.in_unit_package) = saved;
    }

    /// Emit a declaration-only shell registration for every non-lexical
    /// class/role declared inside code anywhere in a unit compiled without
    /// the BEGIN prologue's partition (which places them itself, see the
    /// module doc comment). Only a mainline unit runs this pass: a routine
    /// body is part of a unit that already shelled its nested declarations.
    ///
    /// The shells are emitted in source order, the order Rakudo composes the
    /// types in at compile time. A nested declaration may compose a role or
    /// inherit a class declared at unit level before it (`role R { };
    /// sub f { class C does R { } }`), and a unit-level type that only
    /// declarations precede registers in place without a forward shell
    /// (`hoist_type_decl_shells`), so it would not exist yet at the head of
    /// the unit. Such a type gets a shell of its own here, ahead of the
    /// nested ones that follow it.
    // Cost: O(n), n = size of the unit's AST.
    pub(super) fn hoist_nested_type_decl_shells(&mut self, stmts: &[Stmt]) {
        if stmts
            .iter()
            .any(|stmt| matches!(stmt, Stmt::NestedTypeShells(_)))
        {
            return;
        }
        let per_stmt: Vec<Vec<NestedTypeShell>> = stmts.iter().map(nested_type_decls).collect();
        let Some(last) = per_stmt.iter().rposition(|found| !found.is_empty()) else {
            return;
        };
        let original_package = self.current_package.clone();
        let original_in_unit_package = self.in_unit_package;
        let mut in_prefix = true;
        for (i, stmt) in stmts[..=last].iter().enumerate() {
            if let Stmt::Package {
                name,
                is_unit: true,
                ..
            } = stmt
            {
                // As in `hoist_type_decl_shells`: the rest of the scope is in
                // the unit package.
                self.current_package = self.qualify_package_name(&name.resolve());
                self.in_unit_package = true;
            }
            in_prefix = in_prefix && Self::runs_no_user_code(stmt);
            if in_prefix && i < last && Self::is_shellable_unit_type_decl(stmt) {
                self.emit_type_decl_shell(stmt, true);
            }
            for shell in &per_stmt[i] {
                self.emit_nested_type_shell(shell);
            }
        }
        self.current_package = original_package;
        self.in_unit_package = original_in_unit_package;
    }

    /// Whether a unit-level declaration is one a shell can stand in for: an
    /// `our`-scoped, statically named, non-stub class, or an `our` role.
    // Cost: O(len(body)) for the stub check.
    fn is_shellable_unit_type_decl(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::ClassDecl {
                is_lexical: false,
                is_unit: false,
                name_expr: None,
                body,
                ..
            } => !Self::is_stub_class_body(body),
            Stmt::RoleDecl { custom_traits, .. } => {
                !custom_traits.iter().any(|(t, _)| t == "__my_scoped")
            }
            _ => false,
        }
    }
}
