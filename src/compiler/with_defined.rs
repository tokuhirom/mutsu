//! The `.defined` probe of a `with` / `without` / `orwith`.
//!
//! Both desugars ask `.defined` of a variable: the block form of the hidden
//! temp it declares in the condition (`(my $__with_tmp_N = X).defined`,
//! `crate::with_desugar::defined_condition`), the statement modifier of the
//! topic it gives (`given X { if .defined { S } }`). A method call on a
//! variable compiles to a by-name `CallMethodMut`, so that a method which
//! replaces the variable's contents (`.push` autovivifying an undefined
//! scalar) can write it back. `.defined` never does, and the by-name call
//! cost each `with` the general method path: ~21k instructions against ~2k
//! for the same call on a value (#18208). The compiler asks it of the value.

use crate::ast::{Expr, Stmt};

/// Whether `target` is the variable a `with` desugar asks `.defined` of: the
/// topic `$_`, or the declaration of a `with` temp.
// Cost: O(1).
pub(super) fn is_defined_probe_target(target: &Expr) -> bool {
    match target {
        Expr::Var(name) => name == "_",
        Expr::DoStmt(stmt) => matches!(
            stmt.as_ref(),
            Stmt::VarDecl { name, bind: None, .. } if name.starts_with("__with_tmp_")
        ),
        _ => false,
    }
}
