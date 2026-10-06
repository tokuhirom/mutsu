//! Symbolic dereference `$::($name)` / `@::($name)` and `::($name) = v` across
//! the RakuAST boundary.
//!
//! Measured against rakudo 2026.09:
//!
//! ```text
//! $::($n)         Var::Package(name => Name(Part::Empty, Part::Expression($n)), sigil => "$")
//! $::($n) = 5     ApplyInfix(Var::Package(…), Assignment(:item), 5)
//! ::($n) = 5      ApplyInfix(Term::Name(Name(Part::Empty, Part::Expression($n))), Assignment, 5)
//! ```
//!
//! The parser keeps the variable forms as [`Expr::SymbolicDeref`] /
//! [`Expr::SymbolicDerefAssign`] (the sigil and the name expression) and the
//! type form as [`Expr::IndirectTypeLookupAssign`] (the lookup itself, without
//! assignment, is `Expr::IndirectTypeLookup`, handled in `convert.rs`).

use super::convert::{assignment_around, convert_expr, leaf_field, node_field};
use super::lower::{lower_expr, named_child};
use super::name_parts::{self, NameShape};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::value::{RuntimeError, Value};

/// The `Name(Part::Empty, Part::Expression(EXPR))` of a dynamic lookup.
fn indirect_name(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let part = RakuAstNode {
        class: RakuAstClass::NamePartExpression,
        fields: vec![node_field(None, convert_expr(expr)?)],
    };
    Ok(name_parts::name_from_parts(vec![
        name_parts::leading_empty(),
        Value::rakuast(Box::new(part)),
    ]))
}

/// `$::($n)` as `Var::Package`.
// Cost: O(n), n = nodes of the name expression.
pub(super) fn convert(sigil: &str, expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::VarPackage,
        fields: vec![
            node_field(Some("name"), indirect_name(expr)?),
            leaf_field(Some("sigil"), Value::str(sigil.to_string())),
        ],
    })
}

/// `$::($n) = v`.
// Cost: O(n), n = nodes of the expression and the value.
pub(super) fn convert_assign(
    sigil: &str,
    expr: &Expr,
    value: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    assignment_around(convert(sigil, expr)?, sigil == "$", value)
}

/// `::($n) = v`.
// Cost: O(n), n = nodes of the expression and the value.
pub(super) fn convert_type_assign(expr: &Expr, value: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let lookup = RakuAstNode {
        class: RakuAstClass::TermName,
        fields: vec![node_field(None, indirect_name(expr)?)],
    };
    assignment_around(lookup, false, value)
}

/// The name expression of a plain `::(EXPR)` name (no static tail).
fn plain_indirect(name: &RakuAstNode) -> Option<&RakuAstNode> {
    match name_parts::name_shape(name)? {
        NameShape::Indirect {
            expr,
            tail,
            trailing: false,
        } if tail.is_empty() => Some(expr),
        _ => None,
    }
}

/// A `Var::Package` over a dynamic name as the parser's [`Expr::SymbolicDeref`],
/// or `None` when `node` is not one.
// Cost: O(n), n = nodes of the name expression.
pub(super) fn lower(node: &RakuAstNode) -> Option<Result<Expr, RuntimeError>> {
    if node.class != RakuAstClass::VarPackage {
        return None;
    }
    let expr = plain_indirect(named_child(node, "name").ok()?)?;
    let sigil = super::lower::leaf_str(node, "sigil").ok()?;
    if !matches!(sigil.as_str(), "$" | "@" | "%" | "&") {
        return None;
    }
    Some(lower_expr(expr).map(|expr| Expr::SymbolicDeref {
        sigil,
        expr: Box::new(expr),
    }))
}

/// `ApplyInfix(Var::Package(dynamic) | Term::Name(dynamic), Assignment, v)` as
/// the parser's assignment node, or `None` for any other assignment.
// Cost: O(n), n = nodes of the name expression and the value.
pub(super) fn lower_assign(node: &RakuAstNode) -> Result<Option<Expr>, RuntimeError> {
    let left = named_child(node, "left")?;
    let value = || lower_expr(named_child(node, "right")?).map(Box::new);
    match left.class {
        RakuAstClass::VarPackage => {
            let Some(Ok(Expr::SymbolicDeref { sigil, expr })) = lower(left) else {
                return Ok(None);
            };
            Ok(Some(Expr::SymbolicDerefAssign {
                sigil,
                expr,
                value: value()?,
            }))
        }
        RakuAstClass::TermName => {
            let Some(name) = left.fields.first().and_then(|f| match &f.value {
                super::RakuAstFieldValue::Node(v) => super::lower::rakuast_node_of(v),
                _ => None,
            }) else {
                return Ok(None);
            };
            let Some(expr) = plain_indirect(name) else {
                return Ok(None);
            };
            Ok(Some(Expr::IndirectTypeLookupAssign {
                expr: Box::new(lower_expr(expr)?),
                value: value()?,
            }))
        }
        _ => Ok(None),
    }
}
