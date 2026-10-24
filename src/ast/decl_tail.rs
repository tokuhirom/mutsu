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
//! The parser keeps the head and the tail (`ParsedStmt::DeclTail`); [`expand`]
//! builds the compiler's shape from them.

use super::{Expr, Stmt};

/// `SyntheticBlock([head, Expr(tail)])`.
// Cost: O(1).
pub(crate) fn expand(head: Stmt, tail: Expr) -> Stmt {
    Stmt::SyntheticBlock(vec![head, Stmt::Expr(tail)])
}

/// Whether `head` is a declaration or assignment RakuAST can take as the
/// leftmost operand of a loose tail: a variable declaration or assignment, a
/// compound assignment of a variable, or a grouped declaration.
// Cost: O(1).
pub(crate) fn is_head(head: &Stmt) -> bool {
    matches!(head, Stmt::VarDecl { .. } | Stmt::Assign { .. })
        || matches!(head, Stmt::Expr(Expr::CompoundAssign { target, .. }) if variable_name(target).is_some())
        || signature_declaration(head).is_some()
}

/// The name an [`is_head`] head declares or assigns.
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
