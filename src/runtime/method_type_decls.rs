//! Which classes have a method whose body declares a class/role.
//!
//! A `my class X {}` inside a method registers under the package current when
//! the declaration runs, and Rakudo names it after the enclosing package
//! (`Q::X`). Method dispatch only anchors `current_package` to the owner class
//! in a few cases (class-scoped subs, package lexicals, ...), so a class with a
//! nested-type-declaring method must be recorded for it to anchor as well.

use crate::ast::Stmt;
use crate::ast_visit::{Visit, walk_stmt};
use crate::opcode::CompiledMethodDecl;

/// Finds a class or role declaration anywhere in a statement tree.
struct FindsTypeDecl {
    found: bool,
}

impl Visit for FindsTypeDecl {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        if self.found {
            return;
        }
        if matches!(stmt, Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. }) {
            self.found = true;
            return;
        }
        walk_stmt(self, stmt);
    }
}

/// Whether any of `methods` declares a class or role in its body.
// Cost: O(n), n = total size of the method bodies' ASTs.
pub(crate) fn methods_declare_types(methods: &[CompiledMethodDecl]) -> bool {
    let mut finder = FindsTypeDecl { found: false };
    for method in methods {
        for stmt in &method.body {
            finder.visit_stmt(stmt);
        }
    }
    finder.found
}
