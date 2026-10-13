//! `sub foo($n) { … }(21)`: a named routine declaration called on the spot.
//!
//! The parser expands it into the declaration followed by a call of the
//! declared name, which is all the compiler needs. Rakudo's tree keeps the
//! source form instead: an `ApplyPostfix` whose operand is the `Sub` and whose
//! postfix is a `Call::Term` (measured on 2026.09). A spelling-keeping parse
//! therefore opens the expansion with a [`SourceForm::ImmediateCall`] record,
//! and [`expand`] is the one builder the parser and the RakuAST lowering share.

use super::{Expr, SourceForm, Stmt};

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
    let mut stmts = Vec::with_capacity(3);
    if super::spelled::keeping() {
        stmts.push(Stmt::SourceForm(Box::new(SourceForm::ImmediateCall)));
    }
    stmts.push(sub_decl);
    stmts.push(call);
    Stmt::SyntheticBlock(stmts)
}

/// The declaration and the call arguments of an expansion [`expand`] built
/// with its record, or `None` for any other statement.
// Cost: O(1).
pub(crate) fn parts(stmt: &Stmt) -> Option<(&Stmt, &[Expr])> {
    let Stmt::SyntheticBlock(stmts) = stmt else {
        return None;
    };
    match stmts.as_slice() {
        [
            Stmt::SourceForm(form),
            sub_decl @ Stmt::SubDecl { .. },
            Stmt::Expr(Expr::Call { args, .. }),
        ] if matches!(form.as_ref(), SourceForm::ImmediateCall) => Some((sub_decl, args)),
        _ => None,
    }
}
