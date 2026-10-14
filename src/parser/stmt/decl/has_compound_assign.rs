//! `has $!g OP= EXPR` (#8441 gap 2): a declaration followed by a compound
//! assignment to the declared attribute.
//!
//! Rakudo's `has` initializers are `=`, `:=`, `::=` and `.=`; any other
//! compound operator leaves the declaration without an initializer and applies
//! to the declared variable as an ordinary compound assignment written right
//! after it. Its tree keeps the source form, as for `my $x OP= EXPR`:
//! `ApplyInfix(left => VarDeclaration::Simple(scope => "has"), infix =>
//! MetaInfix::Assign(Infix(OP)), right => EXPR)` (measured on rakudo 2026.09).
//! The parser and the RakuAST lowering share this expansion.

use super::super::assign::{CompoundAssignOp, compound_assigned_value_expr};
use crate::ast::{AssignOp, Expr, Stmt};

/// The assignment of `name OP rhs` back to the attribute: through the
/// accessor (`Expr::AssignExpr`) for a public `$.g`, so a non-`rw` one dies
/// as rakudo's does; as a `Stmt::Assign` of `$!g` (or of the alias `$g`)
/// otherwise. With `keep_source` a private one also keeps the written
/// operator and right-hand side, which the RakuAST converter reads back
/// (`ast::decl_compound_assign::parts`).
// Cost: O(n), n = size of `rhs` (cloned when the source is kept).
pub(crate) fn attribute_compound_assign(
    name: &str,
    is_public: bool,
    is_alias: bool,
    op: CompoundAssignOp,
    rhs: Expr,
    keep_source: bool,
) -> Stmt {
    if is_public {
        let var_name = format!(".{name}");
        let read = Expr::Var(var_name.clone());
        return Stmt::Expr(Expr::AssignExpr {
            name: var_name,
            expr: Box::new(compound_assigned_value_expr(read, op, rhs)),
            is_bind: false,
        });
    }
    let var_name = if is_alias {
        name.to_string()
    } else {
        format!("!{name}")
    };
    let read = Expr::Var(var_name.clone());
    let expr = if keep_source {
        Expr::CompoundAssign {
            target: Box::new(read.clone()),
            op: op.symbol().to_string(),
            rhs: Box::new(rhs.clone()),
            expanded: Box::new(compound_assigned_value_expr(read, op, rhs)),
        }
    } else {
        compound_assigned_value_expr(read, op, rhs)
    };
    Stmt::Assign {
        name: var_name,
        expr,
        op: AssignOp::Assign,
        target_is_sigilless: false,
    }
}

/// The declaration `decl` followed by its compound assignment, as one flat
/// block (the class-body attribute discovery flattens only one level). One
/// that kept its source opens with the record `my $x OP= EXPR` uses.
// Cost: O(1).
pub(crate) fn expand_attribute_compound_assign(decl: Stmt, assign: Stmt) -> Stmt {
    let kept = matches!(
        &assign,
        Stmt::Assign {
            expr: Expr::CompoundAssign { .. },
            ..
        }
    );
    if kept {
        Stmt::SyntheticBlock(vec![
            Stmt::SourceForm(Box::new(crate::ast::SourceForm::DeclarationCompoundAssign)),
            decl,
            assign,
        ])
    } else {
        Stmt::SyntheticBlock(vec![decl, assign])
    }
}
