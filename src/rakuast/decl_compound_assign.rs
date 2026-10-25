//! `my $x OP= EXPR` ↔ `ApplyInfix(left => VarDeclaration::Simple, infix =>
//! MetaInfix::Assign(Infix(OP)), right => EXPR)` (measured on rakudo 2026.09).
//!
//! The parser keeps it as `ParsedStmt::DeclCompound` and [`lower_parsed`] is
//! its one lowering: the declaration with its default value, then one
//! assignment of the operator applied to it. Lowering it as an ordinary
//! compound assignment over a declaration term instead re-ran the declaration
//! with every iteration of a postfix `for`.

use super::lower::{lower_expr, lower_stmt, named_child, named_child_or_positional, unsupported};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::{Expr, Stmt};
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

/// An [`is_declaration_compound_assign`] node as the parser's
/// `ParsedStmt::DeclCompound`, lowered.
// Cost: O(n), n = size of the declaration and the right-hand side.
pub(super) fn lower(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let decl = lower_stmt(named_child(node, "left")?)?;
    let infix = named_child_or_positional(named_child(node, "infix")?)?;
    let op = match super::lower::positional_leaf(infix)?.view() {
        ValueView::Str(op) => op.to_string(),
        _ => return Err(unsupported(node)),
    };
    let source = lower_expr(named_child(node, "right")?)?;
    lower_parsed(decl, op, source)
}

/// The `op` of a `ParsedStmt::DeclCompound` that is a zip metaoperator
/// assignment (`my @a Z= EXPR`) rather than a compound assignment.
pub(crate) const ZIP_ASSIGN: &str = "Z=";

/// The expansion of `ParsedStmt::DeclCompound`: `decl`, then the assignment
/// of `op` applied to the declared variable and `source`.
// Cost: O(n), n = size of the declaration and the right-hand side.
pub(crate) fn lower_parsed(decl: Stmt, op: String, source: Expr) -> Result<Stmt, RuntimeError> {
    // A user-declared or word operator (`my $x foo= 6`, `my $n mod= 5`) is
    // the parser's `InfixFunc` over the declared variable; only the built-in
    // operators have a compound assignment kind.
    // `my @a Z= rhs` element-wise assigns into the fresh declaration, which is
    // the only way to flat-fill a shaped array (plain `=` to one is
    // `X::Assignment::ToShaped`).
    if op == ZIP_ASSIGN {
        let Stmt::VarDecl { name, .. } = &decl else {
            return Err(RuntimeError::new("a zip assignment of this declaration"));
        };
        let name = name.clone();
        let value = crate::parser::zip_assign_call(Expr::Var(name.clone()), source);
        return Ok(crate::ast::decl_compound_assign::expand(decl, name, value));
    }
    let Some(compound) = crate::parser::compound_assign_op_from_name(&op) else {
        let Stmt::VarDecl { name, .. } = &decl else {
            return Err(RuntimeError::new("a compound declaration of this target"));
        };
        let name = name.clone();
        let value = Expr::InfixFunc {
            name: op,
            left: Box::new(Expr::Var(name.clone())),
            right: vec![source],
            modifier: None,
        };
        return Ok(crate::ast::decl_compound_assign::expand(decl, name, value));
    };
    // `has $!g //= 5`: the attribute's own expansion. A public `$.g` keeps no
    // node in the parser, so this never sees one.
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
            source,
        );
        return Ok(crate::parser::expand_attribute_compound_assign(
            decl, assign,
        ));
    }
    let Stmt::VarDecl { name, .. } = &decl else {
        return Err(RuntimeError::new("a compound declaration of this target"));
    };
    let name = name.clone();
    let value =
        crate::parser::compound_assigned_value_expr(Expr::Var(name.clone()), compound, source);
    Ok(crate::ast::decl_compound_assign::expand(decl, name, value))
}
