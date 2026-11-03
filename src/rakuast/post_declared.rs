//! `Illegally post-declared type`, for every unit (ADR-15760 Amendment 1, slice 1a).
//!
//! A type name is lexical: a unit that uses `A` and only declares `class A {}`
//! afterwards is rakudo's compile error, whether the use is a statement of the
//! same list, inside a nested block, or inside the body of a routine or method
//! that is only called later. The old check (`runtime::eval_post_declared_types`)
//! ran for `EVAL` strings alone and compared top-level statement indexes; this
//! one is a pure walk over the finished statements, so the main program, a
//! module, an `EVAL` and `Str.AST` all run the same function.
//!
//! What counts as visible, in source order and per statement list:
//! - a declaration (`class`, `role`, `grammar`, `subset`, `enum`, `package`,
//!   `module`) declares its name at its own statement, so a stub `role R {...}`
//!   declares at the stub, and a use inside the declaration's own body is fine;
//! - a use is judged against the innermost statement list that declares the
//!   name, and is an error only when that declaration comes at a later
//!   statement of the list;
//! - the lists directly inside a class, role, grammar or package body are not
//!   judged (rakudo answers those with another error), their uses still
//!   resolve outward;
//! - a name that is core, or that the calling scope of an `EVAL` declares, is
//!   never post-declared.

use crate::ast::{Expr, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_stmt};
use crate::scope::NameScope;
use crate::value::RuntimeError;
use std::collections::HashMap;

/// Reports the first type name used before the statement that declares it.
// Cost: O(n), n = size of the unit's AST.
pub(crate) fn check(stmts: &[Stmt], names: &NameScope) -> Result<(), RuntimeError> {
    let mut walk = PostDeclared {
        names,
        frames: Vec::new(),
        next_list_is_type_body: false,
        found: None,
    };
    walk.visit_stmts(stmts);
    match walk.found {
        Some(name) => {
            let message = format!("Illegally post-declared type:\n    {name} used at line 1");
            Err(RuntimeError::post_declared_type_symbols(&name, message))
        }
        None => Ok(()),
    }
}

/// One statement list being walked.
struct Frame {
    /// The statement index each name is first declared at in this list.
    declared_at: HashMap<String, usize>,
    /// The statement being walked.
    current: usize,
    /// Whether a later declaration of a name used earlier is an error here.
    judged: bool,
}

struct PostDeclared<'n> {
    names: &'n NameScope,
    frames: Vec<Frame>,
    /// Set while entering a class, role, grammar or package: the next list is
    /// its body.
    next_list_is_type_body: bool,
    found: Option<String>,
}

impl PostDeclared<'_> {
    /// The type names a statement declares: its own name, and for a qualified
    /// one (`class X::Y::Z`) the namespaces it makes (`X`, `X::Y`).
    fn declared_names(stmt: &Stmt) -> Vec<String> {
        let name = match crate::ast::package_header::declaration(stmt).unwrap_or(stmt) {
            Stmt::ClassDecl { name, .. }
            | Stmt::RoleDecl { name, .. }
            | Stmt::SubsetDecl { name, .. }
            | Stmt::EnumDecl { name, .. }
            | Stmt::Package { name, .. } => name.resolve().to_string(),
            _ => return Vec::new(),
        };
        let mut names: Vec<String> = name
            .match_indices("::")
            .map(|(at, _)| name[..at].to_string())
            .filter(|prefix| !prefix.is_empty())
            .collect();
        names.push(name);
        names
    }

    fn is_core(&self, name: &str) -> bool {
        crate::known_types::is_known_type_constraint(name)
            || super::core_term_names::kind(name).is_some()
            || self.names.kind(name).is_some()
    }

    fn use_name(&mut self, name: &str) {
        // A qualified name (`X::Y::Z`) is looked up in its package when it
        // runs: rakudo compiles a use before the declaration (it fails only if
        // the symbol is still missing when the code executes), and a module
        // like Directory refers to an exception class it declares further
        // down the file. Only a single-segment name is a lexical type name.
        if self.found.is_some() || name.contains("::") || self.is_core(name) {
            return;
        }
        for frame in self.frames.iter().rev() {
            let Some(&declared) = frame.declared_at.get(name) else {
                continue;
            };
            if declared > frame.current && frame.judged {
                self.found = Some(name.to_string());
            }
            return;
        }
    }
}

impl<'ast> Visit<'ast> for PostDeclared<'_> {
    fn visit_stmts(&mut self, body: &'ast [Stmt]) {
        let mut declared_at = HashMap::new();
        for (index, stmt) in body.iter().enumerate() {
            for name in Self::declared_names(stmt) {
                declared_at.entry(name).or_insert(index);
            }
        }
        let judged = !std::mem::take(&mut self.next_list_is_type_body);
        self.frames.push(Frame {
            declared_at,
            current: 0,
            judged,
        });
        for (index, stmt) in body.iter().enumerate() {
            if let Some(frame) = self.frames.last_mut() {
                frame.current = index;
            }
            self.visit_stmt(stmt);
        }
        self.frames.pop();
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found.is_some() {
            return;
        }
        if matches!(
            stmt,
            Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } | Stmt::Package { .. }
        ) {
            self.next_list_is_type_body = true;
            walk_stmt(self, stmt);
            self.next_list_is_type_body = false;
        } else {
            walk_stmt(self, stmt);
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if let Expr::BareWord(name) = expr
            && name.starts_with(|c: char| c.is_ascii_uppercase())
        {
            self.use_name(name);
        }
        walk_expr(self, expr);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn verdict(source: &str) -> bool {
        let stmts = crate::parser::parse_program(source).expect("parse").0;
        check(&stmts, &NameScope::default()).is_err()
    }

    #[test]
    fn a_use_before_the_declaration_is_refused_at_any_depth() {
        assert!(verdict("say A; class A {}"));
        assert!(verdict("{ say B; class B {} }"));
        assert!(verdict("sub f { A.new }; class A {}"));
        assert!(verdict("class A { method m { B.new } }; class B {}"));
        assert!(verdict("my $x = B; class B {}"));
    }

    #[test]
    fn declared_first_stubs_and_core_names_are_fine() {
        assert!(!verdict("class A {}; say A"));
        assert!(!verdict("role R {...}; say R; role R { }"));
        assert!(!verdict("class A { method m { A.new } }"));
        assert!(!verdict("say Set; class Set {}"));
        assert!(!verdict("sub f { B.new }; sub g { }"));
        assert!(!verdict("class X::Y::Z { }; my \\x = X::Y; class X::Y { }"));
        // a qualified name is resolved when the code runs
        assert!(!verdict(
            "class A { method m { X::B::C.new } }; class X::B::C {}"
        ));
        assert!(!verdict("sub f { B::C.new }; class B::C { }"));
    }
}
