//! A declaration or assignment with a loose tail: `my $x = 1 and 2`,
//! `my $x = 1, 2, 3`, `my $s = 1 .foo`.
//!
//! The parser parses the declaration first and re-attaches what follows it at
//! its own, looser precedence, as a scopeless
//! `SyntheticBlock([declaration, Expr(tail)])` whose tail re-reads the declared
//! variable as its leftmost operand. RakuAST has no such split: the whole
//! statement is one expression with the declaration itself as the leftmost
//! operand (`ApplyInfix(and, VarDeclaration, 2)`).
//!
//! [`expand`] builds the parser's shape, [`recognize`] takes it apart again.

use super::{Expr, Stmt};

/// `SyntheticBlock([head, Expr(tail)])`.
// Cost: O(1).
pub(crate) fn expand(head: Stmt, tail: Expr) -> Stmt {
    Stmt::SyntheticBlock(vec![head, Stmt::Expr(tail)])
}

/// The `(head, tail)` of a statement [`expand`] could have built: a
/// declaration or assignment of a variable, followed by one expression.
// Cost: O(1).
pub(crate) fn recognize(stmt: &Stmt) -> Option<(&Stmt, &Expr)> {
    let Stmt::SyntheticBlock(stmts) = stmt else {
        return None;
    };
    let [head, Stmt::Expr(tail)] = stmts.as_slice() else {
        return None;
    };
    matches!(head, Stmt::VarDecl { .. } | Stmt::Assign { .. }).then_some((head, tail))
}

/// The name a [`recognize`]d head declares or assigns.
// Cost: O(1).
pub(crate) fn head_name(head: &Stmt) -> Option<&str> {
    match head {
        Stmt::VarDecl { name, .. } | Stmt::Assign { name, .. } => Some(name),
        _ => None,
    }
}
