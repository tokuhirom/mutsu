//! Which sunk statements are element binds (#18260).

use super::*;

impl Compiler {
    /// Whether `expr`'s value is that of an element bind (`%h<k> := v`,
    /// `@a[0;1] := v`), directly or as the tail of a statement-modifier `if`
    /// (`%h<k> := v if $c`). These are fixed shapes, not a tree walk.
    // Cost: O(1).
    pub(super) fn stmt_value_is_index_bind(expr: &Expr) -> bool {
        fn is_index_bind(expr: &Expr) -> bool {
            match expr {
                Expr::IndexAssign { value, .. } | Expr::MultiDimIndexAssign { value, .. } => {
                    matches!(value.as_ref(), Expr::IndexBindValue(_))
                }
                _ => false,
            }
        }
        let Expr::DoStmt(inner) = expr else {
            return is_index_bind(expr);
        };
        match inner.as_ref() {
            Stmt::Expr(e) => is_index_bind(e),
            Stmt::If {
                then_branch,
                else_branch,
                is_statement_modifier: true,
                ..
            } if else_branch.is_empty() => {
                crate::ast::last_value_stmt(then_branch, crate::ast::TailSkip::Markers)
                    .is_some_and(|s| matches!(s, Stmt::Expr(e) if is_index_bind(e)))
            }
            _ => false,
        }
    }
}
