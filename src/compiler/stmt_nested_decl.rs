//! `my $f = my $g = EXPR`: hoisting the inner declaration of a declaration's
//! initializer.

use super::Compiler;
use crate::ast::{Expr, Stmt};

impl Compiler {
    /// The declaration an initializer consists of, `my $g = 2` in `my $f = my
    /// $g = 2` or `my $f = (my $g = 2)`.
    // Cost: O(1).
    fn hoistable_inner_decl(expr: &Expr) -> Option<&Stmt> {
        let expr = if let Expr::Grouped(inner) = expr {
            inner.as_ref()
        } else {
            expr
        };
        if let Expr::DoStmt(inner) = expr {
            Some(inner.as_ref())
        } else {
            None
        }
    }

    /// The inner declaration of `my $f = my $g = EXPR` is a lexical of the
    /// enclosing scope, exactly as if it had been written as its own statement
    /// first. Compiled in expression position it got no local slot, so a
    /// closure or named sub declared later never saw it (its writes were
    /// lost). Compile it as a statement, then initialize `$f` from `$g`.
    /// Returns whether `stmt` was such a declaration (and is compiled).
    // Cost: compile time only, O(1) beyond the two statements' own compile.
    pub(super) fn hoist_nested_decl_stmt(&mut self, stmt: &Stmt) -> bool {
        let Stmt::VarDecl {
            name: outer,
            expr,
            is_state: false,
            is_our: false,
            bind: None,
            constant: None,
            ..
        } = stmt
        else {
            return false;
        };
        let Some(inner) = Self::hoistable_inner_decl(expr) else {
            return false;
        };
        let Stmt::VarDecl {
            name: inner_name,
            is_state: false,
            is_our: false,
            bind: None,
            constant: None,
            has_initializer: true,
            ..
        } = inner
        else {
            return false;
        };
        if outer.starts_with(['\\']) || !Self::is_plain_lexical_name(inner_name) {
            return false;
        }
        let inner_name = inner_name.clone();
        self.compile_stmt(inner);
        let mut rewritten = stmt.clone();
        if let Stmt::VarDecl { expr, .. } = &mut rewritten {
            *expr = Expr::Var(inner_name);
        }
        self.compile_stmt(&rewritten);
        true
    }
}
