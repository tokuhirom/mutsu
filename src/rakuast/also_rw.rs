//! `also is rw;` in a class body (#18598). The parser lifts it into the
//! class's `class_is_rw` flag and marks that it was written so
//! (`PackageMarks::rw_also`); Rakudo keeps it as a `Statement::Also` holding
//! `Trait::Is(name => rw)` in the body. The view puts that statement back, and
//! the lowering turns it into the statement the parser reads `also is rw` as,
//! which the class lowering lifts the same way.

use super::{RakuAstNode, Value};
use crate::ast::{Expr, Stmt};
use crate::token_kind::TokenKind;

/// The `Statement::Also` of a class written with `also is rw;`.
// Cost: O(1).
pub(super) fn statement(rw_also: bool) -> Option<RakuAstNode> {
    rw_also.then(|| {
        super::role::also_is_statement(Value::rakuast(Box::new(
            super::routine_traits::trait_is("rw", None),
        )))
    })
}

/// The statement the parser reads `also is rw;` as.
// Cost: O(1).
pub(super) fn lower() -> Stmt {
    Stmt::Expr(Expr::Binary {
        left: Box::new(Expr::BareWord("also".to_string())),
        op: TokenKind::Ident("is".to_string()),
        right: Box::new(Expr::BareWord("rw".to_string())),
        form: Default::default(),
    })
}

/// Take the `also is rw;` statements out of a lowered class `body`: whether
/// there were any.
// Cost: O(s), s = statements of the body.
pub(super) fn take(body: &mut Vec<Stmt>) -> bool {
    let before = body.len();
    body.retain(|stmt| !crate::parser::stmt_is_also_is_rw(stmt));
    body.len() != before
}
