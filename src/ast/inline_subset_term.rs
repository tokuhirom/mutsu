//! Shared parser expansion for an inline `subset` used as a term.

use super::{Expr, SourceForm, Stmt};

/// Expand a subset term to its declaration and the value of its generated name.
pub(crate) fn expand(declaration: Stmt) -> Expr {
    let Stmt::SubsetDecl { name, .. } = &declaration else {
        unreachable!("inline subset term must contain a subset declaration")
    };
    let name = name.resolve();
    Expr::desugar_block(vec![
        Stmt::SourceForm(Box::new(SourceForm::InlineSubsetTerm(Box::new(
            declaration.clone(),
        )))),
        declaration,
        Stmt::Expr(Expr::BareWord(name)),
    ])
}

/// Recover the written declaration from an inline subset expansion.
pub(crate) fn inline_subset_declaration(stmts: &[Stmt]) -> Option<&Stmt> {
    let Some(Stmt::SourceForm(form)) = stmts.first() else {
        return None;
    };
    match form.as_ref() {
        SourceForm::InlineSubsetTerm(declaration) => Some(declaration),
        _ => None,
    }
}
