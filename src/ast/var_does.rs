//! `my %h does Role`, `my @a does Role = 1, 2`: a declaration whose container
//! is mixed with a role.
//!
//! The parser expands it into the declaration, the in-place mixin on the fresh
//! variable and, last, the initializer: mixing in first would lose the mixin
//! when the assignment replaces the value (`SyntheticBlock([VarDecl, Expr(var
//! does Role), Assign])`). RakuAST keeps one `VarDeclaration` with a
//! `Trait::Does` and the initializer. [`expand`] builds that shape from the
//! parser's `ParsedStmt::DoesDecl`.

use super::{Expr, Stmt};
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
