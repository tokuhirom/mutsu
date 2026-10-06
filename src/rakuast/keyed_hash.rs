//! A key-typed hash declaration (`my %h{Str}`, `my Int %h{Str}`) across the
//! RakuAST boundary.
//!
//! Measured on rakudo 2026.09, the key type is the declaration's `shape`, a
//! `SemiList` holding the type, and a written value type is its `type`:
//!
//! ```text
//! VarDeclaration::Simple(type => Type::Simple(Int),
//!   shape => SemiList(Statement::Expression(Type::Simple(Str))),
//!   sigil => "%", desigilname => Name.from-identifier("h"))
//! ```
//!
//! The parser folds both into one type-constraint string (`"Int{Str}"`), and
//! records `crate::ast::keyed_hash::IMPLICIT_VALUE_TYPE` when it supplied the
//! `Any` value type itself, so `my %h{Str}` and `my Any %h{Str}` stay apart.

use super::convert::{build_type_node, node_field, statement_expression};
use super::lower::{named_child_or_positional, unsupported};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::ast::keyed_hash::{IMPLICIT_VALUE_TYPE, join};
use crate::value::RuntimeError;

/// The `(value type, key type)` a `%` declaration renders: `None` for a
/// declaration that is not a keyed hash, and a `None` value type when the
/// parser supplied it.
// Cost: O(k + t), k = length of the type string, t = custom traits.
pub(super) fn split<'a>(
    name: &str,
    type_constraint: Option<&'a str>,
    custom_traits: &[(String, Option<Expr>)],
) -> Option<(Option<&'a str>, &'a str)> {
    if !name.starts_with('%') {
        return None;
    }
    let (value, key) = crate::ast::keyed_hash::split(type_constraint?)?;
    let implicit = custom_traits.iter().any(|(n, _)| n == IMPLICIT_VALUE_TYPE);
    Some(((!implicit).then_some(value), key))
}

/// Put the key type `key` into `decl` as its `shape`, after its `type` and
/// ahead of its `sigil`.
// Cost: O(f), f = fields of `decl`.
pub(super) fn insert_shape(decl: &mut RakuAstNode, key: &str) -> Result<(), RuntimeError> {
    let shape = RakuAstNode {
        class: RakuAstClass::SemiList,
        fields: vec![node_field(
            None,
            statement_expression(build_type_node(key)?),
        )],
    };
    let at = decl
        .fields
        .iter()
        .position(|f| f.name == Some("sigil"))
        .unwrap_or(decl.fields.len());
    decl.fields.insert(at, node_field(Some("shape"), shape));
    Ok(())
}

/// The `shape` of a shaped array declaration: one statement per dimension.
// Cost: O(d), d = size of the dimensions.
pub(super) fn insert_dimensions(
    decl: &mut RakuAstNode,
    dims: &[crate::ast::Expr],
) -> Result<(), RuntimeError> {
    let statements = dims
        .iter()
        .map(|dim| {
            Ok(node_field(
                None,
                statement_expression(super::convert::convert_expr(dim)?),
            ))
        })
        .collect::<Result<Vec<_>, RuntimeError>>()?;
    let shape = RakuAstNode {
        class: RakuAstClass::SemiList,
        fields: statements,
    };
    let at = decl
        .fields
        .iter()
        .position(|f| f.name == Some("sigil"))
        .unwrap_or(decl.fields.len());
    decl.fields.insert(at, node_field(Some("shape"), shape));
    Ok(())
}

/// A declaration's `shape` folded back into its type constraint: the parser's
/// keyed-hash string, plus the implicit-value-type marker in `custom_traits`
/// when the declaration has no `type`. A declaration without a `shape` keeps
/// `type_constraint`; any shape other than one key type on a `%` declaration
/// stays the boundary.
// Cost: O(n), n = size of the shape.
pub(super) fn lower(
    node: &RakuAstNode,
    sigil: &str,
    type_constraint: Option<String>,
    custom_traits: &mut Vec<(String, Option<Expr>)>,
) -> Result<Option<String>, RuntimeError> {
    let Some(field) = node.fields.iter().find(|f| f.name == Some("shape")) else {
        return Ok(type_constraint);
    };
    // The shape of an array is its dimensions, read by `lower_dimensions`.
    if sigil == "@" {
        return Ok(type_constraint);
    }
    let RakuAstFieldValue::Node(shape) = &field.value else {
        return Err(unsupported(node));
    };
    let super::ValueView::RakuAst(shape) = shape.view() else {
        return Err(unsupported(node));
    };
    if sigil != "%" || shape.class != RakuAstClass::SemiList || shape.fields.len() != 1 {
        return Err(unsupported(node));
    }
    let statement = named_child_or_positional(shape)?;
    if statement.class != RakuAstClass::StatementExpression {
        return Err(unsupported(node));
    }
    let key_node = super::lower::named_child(statement, "expression")?;
    let key = super::type_lower::type_constraint(node, key_node)?;
    if type_constraint.is_none() {
        custom_traits.push((IMPLICIT_VALUE_TYPE.to_string(), None));
    }
    Ok(Some(join(type_constraint.as_deref(), &key)))
}

/// The dimensions of a shaped array declaration's `shape`, or `None` when the
/// declaration has no shape or is not an array.
// Cost: O(d), d = size of the dimensions.
pub(super) fn lower_dimensions(
    node: &RakuAstNode,
    sigil: &str,
) -> Result<Option<Vec<Expr>>, RuntimeError> {
    if sigil != "@" {
        return Ok(None);
    }
    let Some(field) = node.fields.iter().find(|f| f.name == Some("shape")) else {
        return Ok(None);
    };
    let RakuAstFieldValue::Node(shape) = &field.value else {
        return Err(unsupported(node));
    };
    let super::ValueView::RakuAst(shape) = shape.view() else {
        return Err(unsupported(node));
    };
    if shape.class != RakuAstClass::SemiList || shape.fields.is_empty() {
        return Err(unsupported(node));
    }
    let mut dims = Vec::with_capacity(shape.fields.len());
    for field in &shape.fields {
        let RakuAstFieldValue::Node(statement) = &field.value else {
            return Err(unsupported(node));
        };
        let super::ValueView::RakuAst(statement) = statement.view() else {
            return Err(unsupported(node));
        };
        if statement.class != RakuAstClass::StatementExpression {
            return Err(unsupported(node));
        }
        dims.push(super::lower::lower_expr(super::lower::named_child(
            statement,
            "expression",
        )?)?);
    }
    Ok(Some(dims))
}
