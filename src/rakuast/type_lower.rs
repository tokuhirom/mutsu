//! Lowering a RakuAST type node back to the parser's type-constraint spelling.
//!
//! The inverse of `convert::build_type_node`: the parser keeps a type
//! constraint as one string (`Int`, `Str:D`, `Int()`, `Hash[Str, Int]`), and
//! the converter renders that string as `Type::Simple`, `Type::Definedness`,
//! `Type::Coercion` or `Type::Parameterized`. Every lowering site that reads a
//! `type`, a `returns` or a trait's type goes through [`type_constraint`], so
//! a type spelling the converter can render is one every site can lower.

use super::lower::{
    lower_expr, named_child, named_child_or_positional, regex_subrule_argument_source, unsupported,
};
use super::name_parts::{self, NameShape};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::value::{RuntimeError, ValueView};

/// The parser's spelling of `type_node`; `owner` names the node a refusal
/// reports.
pub(super) fn type_constraint(
    owner: &RakuAstNode,
    type_node: &RakuAstNode,
) -> Result<String, RuntimeError> {
    match type_node.class {
        RakuAstClass::TypeSimple => {
            let name_node = named_child_or_positional(type_node)?;
            match name_parts::name_shape(name_node) {
                Some(NameShape::Identifier(name)) => Ok(name),
                _ => Err(unsupported(owner)),
            }
        }
        // `Int:D` / `Int:U`.
        RakuAstClass::TypeDefinedness => {
            let base = simple_base(owner, type_node)?;
            let definite = match named_bool(type_node, "definite") {
                Some(definite) => definite,
                None => return Err(unsupported(owner)),
            };
            Ok(format!("{base}{}", if definite { ":D" } else { ":U" }))
        }
        // `Int:_`.
        RakuAstClass::TypeAnyDefinedness => {
            let base = simple_base(owner, type_node)?;
            Ok(format!("{base}:_"))
        }
        // `Int()` / `Int(Cool)`, spelled the way the parser records them.
        RakuAstClass::TypeCoercion => {
            let base = simple_base(owner, type_node)?;
            let constraint = match type_node.fields.as_slice() {
                [_] => String::new(),
                [_, constraint] if constraint.name == Some("constraint") => {
                    let RakuAstFieldValue::Node(value) = &constraint.value else {
                        return Err(unsupported(owner));
                    };
                    let ValueView::RakuAst(constraint) = value.view() else {
                        return Err(unsupported(owner));
                    };
                    type_constraint(owner, constraint)?
                }
                _ => return Err(unsupported(owner)),
            };
            Ok(format!("{base}({constraint})"))
        }
        // `Array[Int]` / `Hash[Str, Int]`, args joined the way the parser
        // spells them.
        RakuAstClass::TypeParameterized => {
            let base = simple_base(owner, type_node)?;
            let args = named_child(type_node, "args")?;
            if args.class != RakuAstClass::ArgList || args.fields.is_empty() {
                return Err(unsupported(owner));
            }
            let mut parts = Vec::with_capacity(args.fields.len());
            for field in &args.fields {
                let RakuAstFieldValue::Node(value) = &field.value else {
                    return Err(unsupported(owner));
                };
                let ValueView::RakuAst(arg) = value.view() else {
                    return Err(unsupported(owner));
                };
                if field.name.is_some() {
                    return Err(unsupported(owner));
                }
                parts.push(match arg.class {
                    RakuAstClass::TypeSimple
                    | RakuAstClass::TypeDefinedness
                    | RakuAstClass::TypeAnyDefinedness
                    | RakuAstClass::TypeCoercion
                    | RakuAstClass::TypeParameterized => argument_type_constraint(owner, arg)?,
                    _ => regex_subrule_argument_source(arg)?,
                });
            }
            Ok(format!("{base}[{}]", parts.join(", ")))
        }
        _ => Err(unsupported(owner)),
    }
}

/// Preserve argument expressions for `is`/`does` role applications when
/// lowering a parameterized type from RakuAST.
pub(super) fn type_application_args(
    owner: &RakuAstNode,
    type_node: &RakuAstNode,
) -> Result<Option<Vec<Expr>>, RuntimeError> {
    if type_node.class != RakuAstClass::TypeParameterized {
        return Ok(None);
    }
    let args = named_child(type_node, "args")?;
    if args.class != RakuAstClass::ArgList {
        return Err(unsupported(owner));
    }
    args.fields
        .iter()
        .map(|field| {
            if field.name.is_some() {
                return Err(unsupported(owner));
            }
            let RakuAstFieldValue::Node(value) = &field.value else {
                return Err(unsupported(owner));
            };
            let ValueView::RakuAst(node) = value.view() else {
                return Err(unsupported(owner));
            };
            if node.class == RakuAstClass::TypeSimple {
                Ok(Expr::BareWord(argument_type_constraint(owner, node)?))
            } else {
                lower_expr(node)
            }
        })
        .collect::<Result<Vec<_>, _>>()
        .map(Some)
}

/// A leading empty edge denotes a capture only in a type argument. Ordinary
/// type references such as `::Int` still resolve the plain identifier.
fn argument_type_constraint(
    owner: &RakuAstNode,
    node: &RakuAstNode,
) -> Result<String, RuntimeError> {
    let name = type_constraint(owner, node)?;
    if node.class == RakuAstClass::TypeSimple
        && name_parts::has_leading_empty(named_child_or_positional(node)?)
    {
        Ok(format!("::{name}"))
    } else {
        Ok(name)
    }
}

/// The `base-type` of a definedness / coercion / parameterized type, which the
/// converter only ever renders as a `Type::Simple`.
fn simple_base(owner: &RakuAstNode, type_node: &RakuAstNode) -> Result<String, RuntimeError> {
    let base = named_child(type_node, "base-type")?;
    if base.class != RakuAstClass::TypeSimple {
        return Err(unsupported(owner));
    }
    type_constraint(owner, base)
}

fn named_bool(node: &RakuAstNode, name: &str) -> Option<bool> {
    let field = node.fields.iter().find(|f| f.name == Some(name))?;
    let RakuAstFieldValue::Node(value) = &field.value else {
        return None;
    };
    match value.view() {
        ValueView::Bool(b) => Some(b),
        _ => None,
    }
}
