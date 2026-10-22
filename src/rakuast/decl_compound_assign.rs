//! `my $x OP= EXPR` ↔ `ApplyInfix(left => VarDeclaration::Simple, infix =>
//! MetaInfix::Assign(Infix(OP)), right => EXPR)` (measured on rakudo 2026.09).
//!
//! Lowering hands the node back to the parser's own expansion
//! (`ast::decl_compound_assign`): the declaration with its default value, then
//! one assignment of the operator applied to it. Lowering it as an ordinary
//! compound assignment over a declaration term instead re-ran the declaration
//! with every iteration of a postfix `for`.

use super::lower::{lower_expr, lower_stmt, named_child, named_child_or_positional, unsupported};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Stmt;
use crate::value::{RuntimeError, ValueView};

/// Whether `node` is a compound assignment whose left side is a variable
/// declaration.
// Cost: O(f), f = fields of the node.
pub(super) fn is_declaration_compound_assign(node: &RakuAstNode) -> bool {
    node.class == RakuAstClass::ApplyInfix
        && named_child(node, "left").is_ok_and(|l| l.class == RakuAstClass::VarDeclarationSimple)
        && named_child(node, "infix").is_ok_and(|i| {
            i.class == RakuAstClass::MetaInfixAssign
                && named_child_or_positional(i).is_ok_and(|op| op.class == RakuAstClass::Infix)
        })
}

/// The parser's expansion of an [`is_declaration_compound_assign`] node.
// Cost: O(n), n = size of the declaration and the right-hand side.
pub(super) fn lower(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let decl = lower_stmt(named_child(node, "left")?)?;
    let infix = named_child_or_positional(named_child(node, "infix")?)?;
    let op = match super::lower::positional_leaf(infix)?.view() {
        ValueView::Str(op) => op.to_string(),
        _ => return Err(unsupported(node)),
    };
    let rhs = lower_expr(named_child(node, "right")?)?;
    // A user-declared operator (`my $x foo= 6`) is the parser's `InfixFunc`
    // over the declared variable; only the built-in operators have a compound
    // assignment kind.
    let Some(compound) = crate::parser::compound_assign_op_from_name(&op) else {
        let Stmt::VarDecl { name, .. } = &decl else {
            return Err(unsupported(node));
        };
        let name = name.clone();
        let value = crate::ast::Expr::InfixFunc {
            name: op.clone(),
            left: Box::new(crate::ast::Expr::Var(name.clone())),
            right: vec![rhs.clone()],
            modifier: None,
        };
        return Ok(crate::ast::decl_compound_assign::expand(
            decl,
            name,
            value,
            Some(crate::ast::decl_compound_assign::Source {
                op: format!("{op}="),
                rhs,
            }),
        ));
    };
    // `has $!g //= 5`: the attribute's own expansion. A public `$.g` keeps no
    // source in the parser, so the converter never renders one.
    if let Stmt::HasDecl {
        name,
        is_public: false,
        is_alias,
        default: None,
        ..
    } = &decl
    {
        let assign = crate::parser::attribute_compound_assign(
            &name.resolve(),
            false,
            *is_alias,
            compound,
            rhs,
            true,
        );
        return Ok(crate::parser::expand_attribute_compound_assign(
            decl, assign,
        ));
    }
    let Stmt::VarDecl { name, .. } = &decl else {
        return Err(unsupported(node));
    };
    let name = name.clone();
    let value = crate::parser::compound_assigned_value_expr(
        crate::ast::Expr::Var(name.clone()),
        compound,
        rhs.clone(),
    );
    Ok(crate::ast::decl_compound_assign::expand(
        decl,
        name,
        value,
        Some(crate::ast::decl_compound_assign::Source {
            op: format!("{op}="),
            rhs,
        }),
    ))
}
