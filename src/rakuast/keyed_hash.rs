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

use super::convert::{
    build_type_node, convert_expr, node_field, statement_expression,
    unsupported as unsupported_source,
};
use super::lower::unsupported;
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::ast::keyed_hash::join;
use crate::ast::var_marks::VarMarks;
use crate::value::{RuntimeError, Value, ValueView};

/// The hidden source spelling retained on an inline anonymous subset key.
const INLINE_SUBSET_SOURCE: &str = "__mutsu_hash_subset_source";

pub(super) fn is_source_marker(field: &super::RakuAstField) -> bool {
    field.name == Some(INLINE_SUBSET_SOURCE)
}

fn subset_source(node: &RakuAstNode) -> Option<String> {
    let field = node.fields.iter().find(|field| is_source_marker(field))?;
    let super::RakuAstFieldValue::Node(value) = &field.value else {
        return None;
    };
    let ValueView::Str(source) = value.view() else {
        return None;
    };
    Some(source.to_string())
}

/// The `(value type, key type)` a `%` declaration renders: `None` for a
/// declaration that is not a keyed hash, and a `None` value type when the
/// parser supplied it.
// Cost: O(k), k = length of the type string.
pub(super) fn split<'a>(
    name: &str,
    type_constraint: Option<&'a str>,
    marks: &VarMarks,
) -> Option<(Option<&'a str>, &'a str)> {
    if !name.starts_with('%') {
        return None;
    }
    let (value, key) = crate::ast::keyed_hash::split(type_constraint?)?;
    Some(((!marks.implicit_value_type).then_some(value), key))
}

/// Put the key type `key` into `decl` as its `shape`, after its `type` and
/// ahead of its `sigil`.
// Cost: O(f), f = fields of `decl`.
pub(super) fn insert_shape(decl: &mut RakuAstNode, key: &str) -> Result<(), RuntimeError> {
    let shape = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::SemiList,
        fields: super::type_args::type_sources(key, ';')
            .map(|key| {
                Ok(node_field(
                    None,
                    statement_expression(build_type_node(key)?),
                ))
            })
            .collect::<Result<Vec<_>, RuntimeError>>()?,
    };
    let at = decl
        .fields
        .iter()
        .position(|f| f.name == Some("sigil"))
        .unwrap_or(decl.fields.len());
    decl.fields.insert(at, node_field(Some("shape"), shape));
    Ok(())
}

/// Preserve an anonymous subset's structured `Type::Subset` tree in a hash
/// key shape, retaining the parser spelling for the existing key-type binder.
// Cost: O(n), n = size of the subset expression and source spelling.
pub(super) fn insert_subset_shape(
    decl: &mut RakuAstNode,
    expression: &Expr,
    source: &str,
) -> Result<(), RuntimeError> {
    let mut subset = convert_expr(expression)?;
    if subset.class != RakuAstClass::TypeSubset {
        return Err(unsupported_source("inline subset hash key type"));
    }
    let declaration = match expression {
        Expr::Ast(node) => match node.as_ref() {
            crate::ast::parsed::ParsedExpr::InlineSubset(declaration) => Some(declaration.as_ref()),
            _ => None,
        },
        _ => None,
    }
    .ok_or_else(|| unsupported_source("inline subset hash key type"))?;
    let crate::ast::Stmt::SubsetDecl {
        base,
        base_is_explicit,
        ..
    } = declaration
    else {
        return Err(unsupported_source("inline subset hash key type"));
    };
    if !*base_is_explicit || !super::type_build::is_simple_type(base) {
        return Err(unsupported_source(
            "anonymous subset with a non-simple base type",
        ));
    }
    let name = super::name_parts::name_from_parts(vec![
        super::name_parts::leading_empty(),
        super::name_parts::leading_empty(),
    ]);
    let name = Value::rakuast(Box::new(name));
    let name_field = subset
        .fields
        .iter_mut()
        .find(|field| field.name == Some("name"))
        .ok_or_else(|| unsupported_source("inline subset without a name field"))?;
    name_field.value = super::RakuAstFieldValue::Node(name);
    subset.fields.push(super::RakuAstField {
        name: Some(INLINE_SUBSET_SOURCE),
        value: super::RakuAstFieldValue::Node(Value::str(source.to_string())),
    });
    let shape = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::SemiList,
        fields: vec![node_field(None, statement_expression(subset))],
    };
    let at = decl
        .fields
        .iter()
        .position(|field| field.name == Some("sigil"))
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
        origin: Default::default(),
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
/// keyed-hash string, plus `marks.implicit_value_type` when the declaration
/// has no `type`. A declaration without a `shape` keeps
/// `type_constraint`; each type statement on a `%` shape names one key dimension.
// Cost: O(n), n = size of the shape.
pub(super) fn lower(
    node: &RakuAstNode,
    sigil: &str,
    type_constraint: Option<String>,
    marks: &mut VarMarks,
) -> Result<Option<String>, RuntimeError> {
    let Some(key) = lower_key_type(node, sigil)? else {
        return Ok(type_constraint);
    };
    marks.implicit_value_type = type_constraint.is_none();
    Ok(Some(join(type_constraint.as_deref(), &key)))
}

/// The source key type shared by variable and attribute declarations.
// Cost: O(n), n = size of the shape.
pub(super) fn lower_key_type(
    node: &RakuAstNode,
    sigil: &str,
) -> Result<Option<String>, RuntimeError> {
    let Some(field) = node.fields.iter().find(|f| f.name == Some("shape")) else {
        return Ok(None);
    };
    // The shape of an array is its dimensions, read by `lower_dimensions`.
    if sigil == "@" {
        return Ok(None);
    }
    let RakuAstFieldValue::Node(shape) = &field.value else {
        return Err(unsupported(node));
    };
    let super::ValueView::RakuAst(shape) = shape.view() else {
        return Err(unsupported(node));
    };
    if sigil != "%" || shape.class != RakuAstClass::SemiList || shape.fields.is_empty() {
        return Err(unsupported(node));
    }
    let mut keys = Vec::with_capacity(shape.fields.len());
    for field in &shape.fields {
        if field.name.is_some() {
            return Err(unsupported(node));
        }
        let statement = super::lower::child_node(&field.value)?;
        if statement.class != RakuAstClass::StatementExpression {
            return Err(unsupported(node));
        }
        let key_node = super::lower::named_child(statement, "expression")?;
        if key_node.class == RakuAstClass::TypeSubset
            && let Some(source) = subset_source(key_node)
        {
            keys.push(source);
        } else {
            keys.push(super::type_lower::type_constraint(node, key_node)?);
        }
    }
    Ok(Some(keys.join("; ")))
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
