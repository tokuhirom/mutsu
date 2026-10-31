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
use crate::scope::NameScope;
use crate::value::{RuntimeError, ValueView};

/// Return-value literals retain their expression form instead of becoming names.
// Cost: O(n), n = constraint spelling length.
pub(super) fn return_constraint(
    names: &NameScope,
    owner: &RakuAstNode,
    node: &RakuAstNode,
) -> Result<String, RuntimeError> {
    match node.class {
        RakuAstClass::IntLiteral | RakuAstClass::NumLiteral | RakuAstClass::RatLiteral => {
            regex_subrule_argument_source(names, node)
        }
        // `--> "foo"`: a string literal. A plain one keeps its double
        // quotes; any other is spelled single-quoted with `\\` and `\'`
        // escaped, which interpolates nothing.
        RakuAstClass::QuotedString => match lower_expr(names, node)? {
            Expr::Literal(value) if matches!(value.view(), ValueView::Str(_)) => {
                let text = value.to_string_value();
                let spelling =
                    if super::type_build::plain_string_literal(&format!("\"{text}\"")).is_some() {
                        format!("\"{text}\"")
                    } else {
                        format!("'{}'", text.replace('\\', "\\\\").replace('\'', "\\'"))
                    };
                if super::type_build::string_literal_value(&spelling).as_deref()
                    == Some(text.as_str())
                {
                    Ok(spelling)
                } else {
                    Err(unsupported(owner))
                }
            }
            _ => Err(unsupported(owner)),
        },
        _ => type_constraint(names, owner, node),
    }
}

/// The parser's spelling of `type_node`; `owner` names the node a refusal
/// reports.
pub(super) fn type_constraint(
    names: &NameScope,
    owner: &RakuAstNode,
    type_node: &RakuAstNode,
) -> Result<String, RuntimeError> {
    type_constraint_at(names, owner, type_node, 0)
}

/// Parent names retain the absolute edge; parameter captures and ordinary
/// type-object expressions use their existing, context-specific spelling.
// Cost: O(n), n = type-node size; composition depth is bounded.
pub(super) fn parent_type_constraint(
    names: &NameScope,
    owner: &RakuAstNode,
    node: &RakuAstNode,
) -> Result<String, RuntimeError> {
    let name = type_constraint(names, owner, node)?;
    let mut base = node;
    for _ in 0..super::type_build::MAX_TYPE_DEPTH {
        if base.class == RakuAstClass::TypeSimple {
            return if name_parts::has_leading_empty(named_child_or_positional(base)?) {
                Ok(format!("::{name}"))
            } else {
                Ok(name)
            };
        }
        base = named_child(base, "base-type")?;
    }
    Err(unsupported(owner))
}

fn type_constraint_at(
    names: &NameScope,
    owner: &RakuAstNode,
    type_node: &RakuAstNode,
    depth: usize,
) -> Result<String, RuntimeError> {
    if depth >= super::type_build::MAX_TYPE_DEPTH {
        return Err(RuntimeError::new(
            "RakuAST: type composition exceeds 128 levels",
        ));
    }
    match type_node.class {
        RakuAstClass::TypeCapture => {
            let name_node = named_child_or_positional(type_node)?;
            match name_parts::name_shape(name_node) {
                Some(NameShape::Identifier(name)) => Ok(format!("::{name}")),
                _ => Err(unsupported(owner)),
            }
        }
        RakuAstClass::TypeSimple => {
            let name_node = named_child_or_positional(type_node)?;
            match name_parts::name_shape(name_node) {
                Some(NameShape::Identifier(name)) => Ok(name),
                _ => Err(unsupported(owner)),
            }
        }
        // `Int:D` / `Int:U`.
        RakuAstClass::TypeDefinedness => {
            let base = composed_base(names, owner, type_node, depth + 1)?;
            let definite = match named_bool(type_node, "definite") {
                Some(definite) => definite,
                None => return Err(unsupported(owner)),
            };
            Ok(format!("{base}{}", if definite { ":D" } else { ":U" }))
        }
        // `Int:_`.
        RakuAstClass::TypeAnyDefinedness => {
            let base = composed_base(names, owner, type_node, depth + 1)?;
            Ok(format!("{base}:_"))
        }
        // `Int()` / `Int(Cool)`, spelled the way the parser records them.
        RakuAstClass::TypeCoercion => {
            let base = composed_base(names, owner, type_node, depth + 1)?;
            let constraint = match type_node.fields.as_slice() {
                [_] => String::new(),
                [_, constraint] if constraint.name == Some("constraint") => {
                    let RakuAstFieldValue::Node(value) = &constraint.value else {
                        return Err(unsupported(owner));
                    };
                    let ValueView::RakuAst(constraint) = value.view() else {
                        return Err(unsupported(owner));
                    };
                    type_constraint_at(names, owner, constraint, depth + 1)?
                }
                _ => return Err(unsupported(owner)),
            };
            Ok(format!("{base}({constraint})"))
        }
        // `Array[Int]` / `Hash[Str, Int]`, args joined the way the parser
        // spells them.
        RakuAstClass::TypeParameterized => {
            let base = composed_base(names, owner, type_node, depth + 1)?;
            // No `args` field is an empty application (`R[]`).
            let Ok(args) = named_child(type_node, "args") else {
                return Ok(format!("{base}[]"));
            };
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
                    | RakuAstClass::TypeParameterized => {
                        argument_type_constraint_at(names, owner, arg, depth + 1)?
                    }
                    _ => regex_subrule_argument_source(names, arg)?,
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
    names: &NameScope,
    owner: &RakuAstNode,
    type_node: &RakuAstNode,
) -> Result<Option<Vec<Expr>>, RuntimeError> {
    if type_node.class != RakuAstClass::TypeParameterized {
        return Ok(None);
    }
    let Ok(args) = named_child(type_node, "args") else {
        return Ok(Some(Vec::new()));
    };
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
                Ok(Expr::BareWord(argument_type_constraint(
                    names, owner, node,
                )?))
            } else {
                lower_expr(names, node)
            }
        })
        .collect::<Result<Vec<_>, _>>()
        .map(Some)
}

/// A leading empty edge denotes a capture only in a type argument. Ordinary
/// type references such as `::Int` still resolve the plain identifier.
fn argument_type_constraint(
    names: &NameScope,
    owner: &RakuAstNode,
    node: &RakuAstNode,
) -> Result<String, RuntimeError> {
    argument_type_constraint_at(names, owner, node, 0)
}

fn argument_type_constraint_at(
    names: &NameScope,
    owner: &RakuAstNode,
    node: &RakuAstNode,
    depth: usize,
) -> Result<String, RuntimeError> {
    let name = type_constraint_at(names, owner, node, depth)?;
    if node.class == RakuAstClass::TypeSimple
        && name_parts::has_leading_empty(named_child_or_positional(node)?)
    {
        Ok(format!("::{name}"))
    } else {
        Ok(name)
    }
}

/// Composed bases use the same bounded recursion as type arguments.
fn composed_base(
    names: &NameScope,
    owner: &RakuAstNode,
    type_node: &RakuAstNode,
    depth: usize,
) -> Result<String, RuntimeError> {
    let base = named_child(type_node, "base-type")?;
    type_constraint_at(names, owner, base, depth)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn composed_type_lowering_shares_one_depth_budget() {
        let names = &crate::scope::NameScope::default();
        let mut node = super::super::bareword::simple_type_node("Int");
        for _ in 0..super::super::type_build::MAX_TYPE_DEPTH {
            node = RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::TypeCoercion,
                fields: vec![super::super::convert::node_field(Some("base-type"), node)],
            };
        }
        assert!(type_constraint(names, &node, &node).is_err());
        let node = super::super::type_build::build_type_node(names, "Int:D(Str)").unwrap();
        assert_eq!(type_constraint(names, &node, &node).unwrap(), "Int:D(Str)");
        let node = super::super::type_build::build_type_node(names, "Hash[Int:D]:D").unwrap();
        assert_eq!(
            type_constraint(names, &node, &node).unwrap(),
            "Hash[Int:D]:D"
        );
    }
}
