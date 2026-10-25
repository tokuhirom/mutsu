//! `my $x *= 5`: a declaration whose initializer is a compound assignment.
//!
//! The parser expands it into the declaration (with its default value) and an
//! assignment of `$x * 5` to it. Rakudo's tree keeps the source form: an
//! `ApplyInfix` whose `left` is the `VarDeclaration` and whose infix is
//! `MetaInfix::Assign` (measured on 2026.09). A `.AST` parse keeps that form as
//! `ParsedStmt::DeclCompound`, whose lowering (`rakuast::decl_compound_assign`)
//! builds [`expand`].

use super::{AssignOp, Expr, Stmt};

/// The declaration `decl` of `name`, then the assignment of `value` (the
/// compound operator applied to the declared variable) to it.
// Cost: O(1).
pub(crate) fn expand(decl: Stmt, name: String, value: Expr) -> Stmt {
    let assign = Stmt::Assign {
        name,
        expr: value,
        op: AssignOp::Assign,
        target_is_sigilless: false,
    };
    Stmt::SyntheticBlock(vec![decl, assign])
}
