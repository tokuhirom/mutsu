//! Routine traits in written order, independently of their execution fields.

use super::convert::{ReturnSpelling, blockoid, name_from_identifier, signature};
use super::convert::{build_type_node, node_field, unsupported};
use super::lower::{list_field, named_child, named_child_or_positional, positional_leaf};
use super::name_parts;
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::routine_trait::{RoutineTrait, TraitArgument};
use crate::ast::{Expr, ParamDef, Stmt};
use crate::value::{RuntimeError, Value, ValueView};

// Cost: O(b + t + a), b = body statements, t = traits, a = argument size.
pub(super) fn apply(node: &mut RakuAstNode, body: &[Stmt]) -> Result<bool, RuntimeError> {
    let Some(traits) = crate::ast::routine_trait::get(body) else {
        return Ok(false);
    };
    let mut nodes = Vec::with_capacity(traits.len());
    for item in traits {
        let trait_node = match item {
            RoutineTrait::Is { name, argument } => {
                let argument = match argument {
                    None => None,
                    Some(TraitArgument::Words(text)) => {
                        Some(super::package_header::words_value(text))
                    }
                    Some(TraitArgument::ExportTags(tags)) => {
                        super::routine_traits::explicit_export_argument(tags)
                    }
                    Some(TraitArgument::Operator(reference)) => {
                        Some(super::attribute::paren_argument(&Expr::CodeVar(
                            crate::op_prec::trait_target_name(reference),
                        ))?)
                    }
                    Some(TraitArgument::Parentheses(expr)) => {
                        let expr = match expr {
                            Expr::Grouped(inner) => inner,
                            other => other,
                        };
                        Some(super::attribute::paren_argument(expr)?)
                    }
                };
                super::routine_traits::trait_is(name, argument)
            }
            RoutineTrait::Returns(ty) | RoutineTrait::Of(ty) => RakuAstNode {
                class: if matches!(item, RoutineTrait::Returns(_)) {
                    RakuAstClass::TraitReturns
                } else {
                    RakuAstClass::TraitOf
                },
                fields: vec![node_field(None, build_type_node(ty)?)],
            },
            RoutineTrait::Unsupported(name) => return Err(unsupported(name)),
        };
        nodes.push(Value::rakuast(Box::new(trait_node)));
    }
    node.fields.retain(|field| field.name != Some("traits"));
    let at = node
        .fields
        .iter()
        .position(|field| field.name == Some("body"))
        .unwrap_or(node.fields.len());
    node.fields.insert(
        at,
        RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(nodes),
        },
    );
    // Anonymous routine builders receive the folded return constraint too;
    // its written trait must not turn into an additional signature arrow.
    if traits
        .iter()
        .any(|item| matches!(item, RoutineTrait::Returns(_) | RoutineTrait::Of(_)))
        && let Some(field) = node
            .fields
            .iter_mut()
            .find(|field| field.name == Some("signature"))
        && let RakuAstFieldValue::Node(value) = &mut field.value
        && let ValueView::RakuAst(signature) = value.view()
    {
        let mut signature = signature.clone();
        signature
            .fields
            .retain(|field| field.name != Some("returns"));
        *value = Value::rakuast(Box::new(signature));
    }
    node.fields.retain(|field| {
        if field.name != Some("signature") { return true }
        match &field.value {
            RakuAstFieldValue::Node(value) => match value.view() {
                ValueView::RakuAst(signature) => !signature.fields.iter().all(|field| {
                    field.name == Some("parameters") && matches!(&field.value, RakuAstFieldValue::List(values) if values.is_empty())
                }),
                _ => true,
            },
            _ => true,
        }
    });
    Ok(true)
}

/// Retain a constructed node's traits when lowering it to the execution AST.
// Cost: O(b + t + a), b = body statements, t = traits, a = argument size.
pub(super) fn retain(node: &RakuAstNode, body: &mut Vec<Stmt>) -> Result<(), RuntimeError> {
    if !node.fields.iter().any(|field| field.name == Some("traits")) {
        return Ok(());
    }
    let mut traits = Vec::new();
    for item in list_field(node, "traits")? {
        let ValueView::RakuAst(item) = item.view() else {
            return Err(super::lower::unsupported(node));
        };
        let source = match item.class {
            RakuAstClass::TraitReturns | RakuAstClass::TraitOf => {
                let ty = super::lower::simple_type_name(node, named_child_or_positional(item)?)?;
                if item.class == RakuAstClass::TraitReturns {
                    RoutineTrait::Returns(ty)
                } else {
                    RoutineTrait::Of(ty)
                }
            }
            RakuAstClass::TraitIs => {
                let name = positional_leaf(named_child(item, "name")?)?;
                let ValueView::Str(name) = name.view() else {
                    return Err(super::lower::unsupported(node));
                };
                let argument = match named_child(item, "argument") {
                    Err(_) => None,
                    Ok(argument) if name.as_str() == "export" => {
                        let tags = super::routine_traits::export_tags(argument)?
                            .ok_or_else(|| super::lower::unsupported(node))?;
                        Some(TraitArgument::ExportTags(tags))
                    }
                    Ok(argument) if argument.class == RakuAstClass::QuotedString => {
                        let segments = list_field(argument, "segments")?;
                        let [segment] = segments else {
                            return Err(super::lower::unsupported(node));
                        };
                        let ValueView::RakuAst(segment) = segment.view() else {
                            return Err(super::lower::unsupported(node));
                        };
                        let value = positional_leaf(segment)?;
                        let ValueView::Str(text) = value.view() else {
                            return Err(super::lower::unsupported(node));
                        };
                        Some(TraitArgument::Words(text.to_string()))
                    }
                    Ok(argument) => Some(TraitArgument::Parentheses(
                        super::attribute::lower_paren_argument(node, argument)?,
                    )),
                };
                RoutineTrait::Is {
                    name: name.to_string(),
                    argument,
                }
            }
            _ => return Err(super::lower::unsupported(node)),
        };
        traits.push(source);
    }
    crate::ast::routine_trait::attach(body, traits);
    Ok(())
}

/// Which spelling a routine's return type used, read off the parser markers.
/// `returns X of Y` leaves both markers (mutsu folds them into one
/// `X[Y]` return type); raku models that as a single trait.
pub(super) fn return_type_spelling(
    custom_traits: &[(String, Option<Expr>)],
) -> Result<ReturnSpelling, RuntimeError> {
    let returns = custom_traits.iter().any(|(t, _)| t == "__return_via_trait");
    let of = custom_traits.iter().any(|(t, _)| t == "__return_via_of");
    match (returns, of) {
        (false, false) => Ok(ReturnSpelling::Arrow),
        (true, false) => Ok(ReturnSpelling::ReturnsTrait),
        (false, true) => Ok(ReturnSpelling::OfTrait),
        (true, true) => Ok(ReturnSpelling::ReturnsTrait),
    }
}

/// A named routine with its signature, return spelling and body.
// Cost: O(n), n = size of the declaration.
pub(super) fn routine_node(
    class: RakuAstClass,
    name: &str,
    param_defs: &[ParamDef],
    body: &[Stmt],
    return_type: Option<(&str, ReturnSpelling)>,
) -> Result<RakuAstNode, RuntimeError> {
    let name_node = name_parts::operator_name(name).unwrap_or_else(|| name_from_identifier(name));
    let mut fields = vec![node_field(Some("name"), name_node)];
    let arrow_returns = match return_type {
        Some((t, ReturnSpelling::Arrow)) => Some(t),
        _ => None,
    };
    if !param_defs.is_empty() || arrow_returns.is_some() {
        fields.push(node_field(
            Some("signature"),
            signature(param_defs, true, arrow_returns)?,
        ));
    }
    if let Some((t, spelling)) = return_type
        && spelling != ReturnSpelling::Arrow
    {
        let trait_class = match spelling {
            ReturnSpelling::OfTrait => RakuAstClass::TraitOf,
            _ => RakuAstClass::TraitReturns,
        };
        let trait_node = RakuAstNode {
            class: trait_class,
            fields: vec![node_field(None, build_type_node(t)?)],
        };
        fields.push(RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(trait_node))]),
        });
    }
    fields.push(node_field(Some("body"), blockoid(body)?));
    Ok(RakuAstNode { class, fields })
}
