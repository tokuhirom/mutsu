//! `my $x *= 5`: a declaration whose initializer is a compound assignment.
//!
//! The parser expands it into the declaration (with its default value) and an
//! assignment of `$x * 5` to it. Rakudo's tree keeps the source form: an
//! `ApplyInfix` whose `left` is the `VarDeclaration` and whose infix is
//! `MetaInfix::Assign` (measured on 2026.09). When the written operator and
//! right-hand side are known (a spelling-keeping parse, or the RakuAST
//! lowering), [`expand`] opens the expansion with a
//! [`SourceForm::DeclarationCompoundAssign`] record and keeps them in the
//! assignment's `Expr::CompoundAssign` marker; [`parts`] reads them back.

use super::{AssignOp, Expr, SourceForm, Stmt};

/// The written operator (`"*="`) and right-hand side of the compound
/// assignment.
pub(crate) struct Source {
    pub(crate) op: String,
    pub(crate) rhs: Expr,
}

/// The declaration `decl` of `name`, then the assignment of `value` (the
/// compound operator applied to the declared variable) to it.
// Cost: O(1).
pub(crate) fn expand(decl: Stmt, name: String, value: Expr, source: Option<Source>) -> Stmt {
    let Some(Source { op, rhs }) = source else {
        let assign = Stmt::Assign {
            name,
            expr: value,
            op: AssignOp::Assign,
            target_is_sigilless: false,
        };
        return Stmt::SyntheticBlock(vec![decl, assign]);
    };
    let assign = Stmt::Assign {
        expr: Expr::CompoundAssign {
            target: Box::new(Expr::Var(name.clone())),
            op,
            rhs: Box::new(rhs),
            expanded: Box::new(value),
        },
        name,
        op: AssignOp::Assign,
        target_is_sigilless: false,
    };
    Stmt::SyntheticBlock(vec![
        Stmt::SourceForm(Box::new(SourceForm::DeclarationCompoundAssign)),
        decl,
        assign,
    ])
}

/// The declaration, operator and right-hand side of an expansion [`expand`]
/// built with its record, or `None` for any other statement. The declaration
/// may also be an attribute's (`has $!g //= 5`, built by the parser's
/// `expand_attribute_compound_assign`).
// Cost: O(1).
pub(crate) fn parts(stmt: &Stmt) -> Option<(&Stmt, &str, &Expr)> {
    let Stmt::SyntheticBlock(stmts) = stmt else {
        return None;
    };
    match stmts.as_slice() {
        [
            Stmt::SourceForm(form),
            decl @ (Stmt::VarDecl { .. } | Stmt::HasDecl { .. }),
            Stmt::Assign {
                expr: Expr::CompoundAssign { op, rhs, .. },
                ..
            },
        ] if matches!(form.as_ref(), SourceForm::DeclarationCompoundAssign) => {
            Some((decl, op.as_str(), rhs.as_ref()))
        }
        _ => None,
    }
}

/// Split an expansion [`expand`] built with its record for a postfix `for`:
/// the record and the declaration run once, before the loop, and the
/// assignment is the loop body (`my $product *= $_ for @values`). Any other
/// statement comes back unchanged, with nothing hoisted.
// Cost: O(1).
pub(crate) fn split_for_modifier(stmt: Stmt) -> (Option<Vec<Stmt>>, Stmt) {
    if parts(&stmt).is_none() {
        return (None, stmt);
    }
    let Stmt::SyntheticBlock(mut stmts) = stmt else {
        unreachable!("parts matched a synthetic block");
    };
    let assign = stmts.pop().expect("parts matched three statements");
    (Some(stmts), assign)
}

/// The declaration and the loop of a postfix `for` over a split expansion
/// ([`split_for_modifier`]): `[record, declaration, for]`.
// Cost: O(1).
pub(crate) fn for_modifier_parts(stmt: &Stmt) -> Option<(&Stmt, &Stmt)> {
    let Stmt::SyntheticBlock(stmts) = stmt else {
        return None;
    };
    match stmts.as_slice() {
        [
            Stmt::SourceForm(form),
            decl @ Stmt::VarDecl { .. },
            loop_stmt @ Stmt::For {
                is_statement_modifier: true,
                body,
                ..
            },
        ] if matches!(form.as_ref(), SourceForm::DeclarationCompoundAssign)
            && matches!(
                body.as_slice(),
                [Stmt::Assign {
                    expr: Expr::CompoundAssign { .. },
                    ..
                }]
            ) =>
        {
            Some((decl, loop_stmt))
        }
        _ => None,
    }
}
