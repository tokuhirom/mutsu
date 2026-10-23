//! A declaration or variable assignment with a loose tail: `my $x = 1 and 2`,
//! `my ($a, $b) andthen 2`, `my $x = 1, 2, 3`, or `$x += 1 andthen 2`.
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
    let simple_head = matches!(head, Stmt::VarDecl { .. } | Stmt::Assign { .. })
        || matches!(head, Stmt::Expr(Expr::CompoundAssign { target, .. }) if variable_name(target).is_some());
    (simple_head || signature_declaration(head).is_some()).then_some((head, tail))
}

/// The name a [`recognize`]d head declares or assigns.
// Cost: O(1).
pub(crate) fn head_name(head: &Stmt) -> Option<&str> {
    match head {
        Stmt::VarDecl { name, .. } | Stmt::Assign { name, .. } => Some(name),
        Stmt::Expr(Expr::CompoundAssign { target, .. }) => variable_name(target),
        _ => None,
    }
}

/// The bare grouped declaration opened by a signature-declaration source form.
// Cost: O(1).
pub(crate) fn signature_declaration(head: &Stmt) -> Option<&super::SignatureDecl> {
    let Stmt::SyntheticBlock(stmts) = head else {
        return None;
    };
    match stmts.first()? {
        Stmt::SourceForm(form) => match form.as_ref() {
            super::SourceForm::SignatureDecl(decl) if decl.init.is_none() => Some(decl),
            _ => None,
        },
        _ => None,
    }
}

// Cost: O(1).
fn variable_name(target: &Expr) -> Option<&str> {
    match target {
        Expr::Var(name) => Some(name),
        _ => None,
    }
}
