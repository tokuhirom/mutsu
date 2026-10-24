//! `sub foo($n) { … }(21)`: a named routine declaration called on the spot.
//!
//! The parser expands it into the declaration followed by a call of the
//! declared name, which is all the compiler needs. Rakudo's tree keeps the
//! source form instead: an `ApplyPostfix` whose operand is the `Sub` and whose
//! postfix is a `Call::Term` (measured on 2026.09). The parser keeps that form
//! (`ParsedStmt::ImmediateCall`); [`expand`] is its one lowering.

use super::{Expr, Stmt};

/// The declaration `sub_decl` followed by a call of its name with `args`.
// Cost: O(a), a = number of arguments.
pub(crate) fn expand(sub_decl: Stmt, args: Vec<Expr>) -> Stmt {
    let Stmt::SubDecl { name, .. } = &sub_decl else {
        unreachable!("an immediate call follows a named sub declaration");
    };
    let call = Stmt::Expr(Expr::Call {
        name: *name,
        args,
        listop: false,
    });
    Stmt::SyntheticBlock(vec![sub_decl, call])
}
