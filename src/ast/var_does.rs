//! `my %h does Role`, `my @a does Role = 1, 2`: a declaration whose container
//! is mixed with a role.
//!
//! The parser expands it into the declaration, the in-place mixin on the fresh
//! variable and, last, the initializer: mixing in first would lose the mixin
//! when the assignment replaces the value (`SyntheticBlock([VarDecl, Expr(var
//! does Role), Assign])`). RakuAST keeps one `VarDeclaration` with a
//! `Trait::Does` and the initializer. [`expand`] builds the parser's shape,
//! [`recognize`] takes it apart again.

use super::{AssignOp, Expr, Stmt};
use crate::token_kind::TokenKind;

/// `SyntheticBlock([decl, Expr(VAR OP ROLE), init?])`, with `OP` `does` or `but`;
/// `name` is the declared variable as the parser spells it (`x`, `@a`, `%h`).
// Cost: O(1).
pub(crate) fn expand(name: &str, decl: Stmt, op: &str, role: Expr, init: Option<Stmt>) -> Stmt {
    let mixin = Expr::Binary {
        left: Box::new(Expr::Var(name.to_string())),
        op: TokenKind::Ident(op.to_string()),
        right: Box::new(role),
        form: Default::default(),
    };
    let mut stmts = vec![decl, Stmt::Expr(mixin)];
    stmts.extend(init);
    Stmt::SyntheticBlock(stmts)
}

/// The `(declaration, role, initializer)` of a `does` statement [`expand`]
/// could have built.
// Cost: O(1).
pub(crate) fn recognize(stmt: &Stmt) -> Option<(&Stmt, &Expr, Option<&Expr>)> {
    let Stmt::SyntheticBlock(stmts) = stmt else {
        return None;
    };
    let (decl, mixin, init) = match stmts.as_slice() {
        [decl, mixin] => (decl, mixin, None),
        [decl, mixin, init] => (decl, mixin, Some(init)),
        _ => return None,
    };
    let Stmt::VarDecl { name, .. } = decl else {
        return None;
    };
    let Stmt::Expr(Expr::Binary {
        left,
        op: TokenKind::Ident(op),
        right,
        ..
    }) = mixin
    else {
        return None;
    };
    if op != "does" || !matches!(left.as_ref(), Expr::Var(var) if var == name) {
        return None;
    }
    let init = match init {
        None => None,
        Some(Stmt::Assign {
            name: assigned,
            expr,
            op: AssignOp::Assign,
            ..
        }) if assigned == name => Some(expr),
        Some(_) => return None,
    };
    Some((decl, right, init))
}
