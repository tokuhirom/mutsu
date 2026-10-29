//! `my %h does Role`, `my @a does Role = 1, 2`: a declaration whose container
//! is mixed with a role.
//!
//! The parser expands it into the declaration, the in-place mixin on the fresh
//! variable and, last, the initializer: mixing in first would lose the mixin
//! when the assignment replaces the value (`SyntheticBlock([VarDecl, Expr(var
//! does Role), Assign])`). RakuAST keeps one `VarDeclaration` with a
//! `Trait::Does` and the initializer. [`expand`] builds that shape from the
//! parser's `ParsedStmt::DoesDecl`.

use super::{AssignOp, Expr, Stmt};
use crate::token_kind::TokenKind;

/// `SyntheticBlock([decl, Expr(VAR OP ROLE), init?])`, with `OP` `does` or `but`;
/// `name` is the declared variable as the parser spells it (`x`, `@a`, `%h`).
// Cost: O(1).
pub(crate) const DECL_INIT: &str = "__mutsu_decl_init";
pub(crate) const DECL_INIT_DONE: &str = "__mutsu_decl_init_done";

pub(crate) fn expand(name: &str, decl: Stmt, op: &str, role: Expr, init: Option<Stmt>) -> Stmt {
    let is_does = op == "does";
    let mixin = Expr::Binary {
        left: Box::new(Expr::Var(name.to_string())),
        op: TokenKind::Ident(op.to_string()),
        right: Box::new(role),
        form: Default::default(),
    };
    let mut stmts = vec![decl, Stmt::Expr(mixin)];
    match init {
        // `my %h does R = LIST` is the declaration's own initializer, which a
        // composed `STORE` receives with `:INITIALIZE` (a later `%h = LIST`
        // does not). The assignment is bracketed by two internal calls: the
        // first marks the initializer (read by the tied-store dispatch), the
        // second drops the mark when no `STORE` consumed it.
        Some(Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
            target_is_sigilless,
        }) if is_does => {
            stmts.push(Stmt::Assign {
                name,
                expr: Expr::Call {
                    name: crate::symbol::Symbol::intern(DECL_INIT),
                    args: vec![expr],
                    listop: false,
                },
                op: AssignOp::Assign,
                target_is_sigilless,
            });
            stmts.push(Stmt::Expr(Expr::Call {
                name: crate::symbol::Symbol::intern(DECL_INIT_DONE),
                args: Vec::new(),
                listop: false,
            }));
        }
        init => stmts.extend(init),
    }
    Stmt::SyntheticBlock(stmts)
}
