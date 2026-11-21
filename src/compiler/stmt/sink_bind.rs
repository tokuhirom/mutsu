//! Which sunk statements are element binds (#18260).

use super::*;

impl Compiler {
    /// Whether `expr`'s value is that of an element bind (`%h<k> := v`,
    /// `@a[0;1] := v`), directly or as the tail of a statement-modifier `if`.
    // Cost: O(d), d = statement-modifier nesting.
    pub(super) fn stmt_value_is_index_bind(expr: &Expr) -> bool {
        match expr {
            Expr::IndexAssign { value, .. } | Expr::MultiDimIndexAssign { value, .. } => {
                matches!(value.as_ref(), Expr::IndexBindValue(_))
            }
            Expr::DoStmt(inner) => match inner.as_ref() {
                Stmt::Expr(e) => Self::stmt_value_is_index_bind(e),
                Stmt::If {
                    then_branch,
                    else_branch,
                    is_statement_modifier: true,
                    ..
                } => {
                    else_branch.is_empty()
                        && crate::ast::last_value_stmt(then_branch, crate::ast::TailSkip::Markers)
                            .is_some_and(|s| {
                                matches!(s, Stmt::Expr(e) if Self::stmt_value_is_index_bind(e))
                            })
                }
                _ => false,
            },
            _ => false,
        }
    }
}
