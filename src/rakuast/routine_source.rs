//! Routine traits in written order, independently of their execution fields.

use super::convert::{blockoid, name_from_identifier, signature};
use super::convert::{leaf_field, node_field, unsupported};
use super::lower::{
    list_field, lower_signature_parameters, named_child, named_child_or_positional, positional_leaf,
};
use super::name_parts;
use super::type_build::return_constraint_node;
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::routine_trait::{ParentKind, RoutineTrait, TraitArgument};
use crate::ast::{Expr, ParamDef, Stmt};
use crate::value::{RuntimeError, Value, ValueView};

const SOURCE_SIGNATURE_ALTERNATES: &str = "source-signature-alternates";
const SOURCE_SUPERSEDE: &str = "source-supersede";
pub(super) type SignatureAlternates = Vec<(Vec<String>, Vec<ParamDef>)>;

/// The S13 alternate-signature and `supersede` forms are accepted by mutsu's
/// source parser but have no public Rakudo node shape. Carry their executable
/// source data without changing the public Sub constructor rendering.
// Cost: O(p), p = parameters in alternate signatures.
pub(super) fn mark_legacy_sub_forms(
    node: &mut RakuAstNode,
    alternates: &[(Vec<String>, Vec<ParamDef>)],
    supersede: bool,
) -> Result<(), RuntimeError> {
    if !alternates.is_empty() {
        let signatures = alternates
            .iter()
            .map(|(_, defs)| signature(defs, true, None).map(|sig| Value::rakuast(Box::new(sig))))
            .collect::<Result<Vec<_>, _>>()?;
        node.fields.push(RakuAstField {
            name: Some(SOURCE_SIGNATURE_ALTERNATES),
            value: RakuAstFieldValue::List(signatures),
        });
    }
    if supersede {
        node.fields
            .push(leaf_field(Some(SOURCE_SUPERSEDE), Value::TRUE));
    }
    Ok(())
}

/// The hidden source field for a package's superseded built-in HOW.
// Cost: O(1).
pub(super) fn package_supersede_field() -> RakuAstField {
    leaf_field(Some(SOURCE_SUPERSEDE), Value::TRUE)
}

// Cost: O(p), p = parameters in alternate signatures.
pub(super) fn lower_alternates(node: &RakuAstNode) -> Result<SignatureAlternates, RuntimeError> {
    let Some(field) = node
        .fields
        .iter()
        .find(|field| field.name == Some(SOURCE_SIGNATURE_ALTERNATES))
    else {
        return Ok(Vec::new());
    };
    let RakuAstFieldValue::List(signatures) = &field.value else {
        return Err(unsupported("alternate signature metadata"));
    };
    signatures
        .iter()
        .map(|value| {
            let ValueView::RakuAst(signature) = value.view() else {
                return Err(unsupported("alternate signature metadata"));
            };
            if signature.class != RakuAstClass::Signature {
                return Err(unsupported("alternate signature metadata"));
            }
            let defs = lower_signature_parameters(signature, node)?;
            let names = defs.iter().map(|def| def.name.clone()).collect();
            Ok((names, defs))
        })
        .collect()
}

// Cost: O(f), f = fields of the routine node.
pub(super) fn has_supersede(node: &RakuAstNode) -> bool {
    node.fields
        .iter()
        .any(|field| field.name == Some(SOURCE_SUPERSEDE))
}

// Cost: O(1).
pub(super) fn is_legacy_source_field(field: &RakuAstField) -> bool {
    matches!(
        field.name,
        Some(SOURCE_SIGNATURE_ALTERNATES | SOURCE_SUPERSEDE)
    )
}

/// The `Trait::*` node for one trait the source wrote.
// Cost: O(k + a), k = length of the trait's spelling, a = size of its argument.
pub(super) fn trait_node(item: &RoutineTrait) -> Result<RakuAstNode, RuntimeError> {
    Ok(match item {
        RoutineTrait::Is { name, argument } => {
            let argument = match argument {
                None => None,
                Some(TraitArgument::Words(text)) => Some(super::package_header::words_value(text)),
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
            // A bare lowercase `is NAME` names a type when NAME is one.
            if super::routine_traits::is_type_trait_name(name)
                || (argument.is_none() && super::bareword::names_type(name))
            {
                super::routine_traits::trait_is_type(name, argument)?
            } else {
                super::routine_traits::trait_is(name, argument)
            }
        }
        RoutineTrait::Returns(ty) | RoutineTrait::Of(ty) => RakuAstNode {
            origin: Default::default(),
            class: if matches!(item, RoutineTrait::Returns(_)) {
                RakuAstClass::TraitReturns
            } else {
                RakuAstClass::TraitOf
            },
            fields: vec![node_field(None, return_constraint_node(ty)?)],
        },
        RoutineTrait::Handles(term) => RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::TraitHandles,
            fields: vec![node_field(None, super::convert::convert_expr(term)?)],
        },
        RoutineTrait::Parent { kind, name, args } => {
            // `is NAME` names a parent only when NAME is a type; any other name
            // is a trait of the program's own, rendered by name.
            if *kind == ParentKind::Is
                && args.is_none()
                && !super::bareword::names_type(name)
                && !name.contains(['[', ':'])
            {
                return Ok(super::routine_traits::trait_is(name, None));
            }
            let type_node = match args {
                Some(args) => super::type_args::parameterized_type_node(name, Some(args))?,
                None => super::type_build::build_type_node(name)?,
            };
            match kind {
                ParentKind::Is => RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::TraitIs,
                    fields: vec![node_field(Some("type"), type_node)],
                },
                ParentKind::Does => RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::TraitDoes,
                    fields: vec![node_field(None, type_node)],
                },
                ParentKind::Hides => RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::TraitHides,
                    fields: vec![node_field(None, type_node)],
                },
            }
        }
        RoutineTrait::Unsupported(name) => return Err(unsupported(name)),
    })
}

/// The `traits` of a class, role or grammar header: the nodes of the traits
/// written in front of its block, in written order. A class with no record
/// wrote none.
// Cost: O(t + a), t = written traits, a = size of their arguments.
pub(super) fn header_traits(body: &[Stmt]) -> Result<Vec<Value>, RuntimeError> {
    crate::ast::routine_trait::get(body)
        .unwrap_or_default()
        .iter()
        .map(|item| trait_node(item).map(|node| Value::rakuast(Box::new(node))))
        .collect()
}

// Cost: O(b + t + a), b = body statements, t = traits, a = argument size.
pub(super) fn apply(node: &mut RakuAstNode, body: &[Stmt]) -> Result<bool, RuntimeError> {
    let Some(traits) = crate::ast::routine_trait::get(body) else {
        return Ok(false);
    };
    let mut nodes = Vec::with_capacity(traits.len());
    for item in traits {
        nodes.push(Value::rakuast(Box::new(trait_node(item)?)));
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

/// Whether a routine body keeps the written term of every `handles` clause,
/// so its `Trait::Handles` can be rendered as written.
// Cost: O(b + t), b = body statements, t = traits.
pub(super) fn keeps_handles(body: &[Stmt]) -> bool {
    crate::ast::routine_trait::get(body)
        .is_some_and(|traits| traits.iter().any(|t| matches!(t, RoutineTrait::Handles(_))))
}

/// The `handles` specifications of the retained `Trait::Handles` terms, as
/// the parser builds them from the same terms; `None` when one does not name
/// a delegation the parser knows.
// Cost: O(b + t + a), b = body statements, t = traits, a = size of the terms.
pub(super) fn handle_specs(body: &[Stmt]) -> Option<Vec<crate::ast::HandleSpec>> {
    let mut specs = Vec::new();
    for item in crate::ast::routine_trait::get(body).unwrap_or_default() {
        if let RoutineTrait::Handles(term) = item {
            specs.extend(crate::parser::handle_specs_from_term(term)?);
        }
    }
    Some(specs)
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
            RakuAstClass::TraitHandles => {
                RoutineTrait::Handles(super::lower::lower_expr(named_child_or_positional(item)?)?)
            }
            RakuAstClass::TraitIs => {
                if named_child(item, "type").is_ok() {
                    let (name, argument) = super::routine_traits::lower_custom(node, item)?
                        .ok_or_else(|| super::lower::unsupported(node))?;
                    RoutineTrait::Is {
                        name,
                        argument: argument.map(TraitArgument::Parentheses),
                    }
                } else {
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
            }
            _ => return Err(super::lower::unsupported(node)),
        };
        traits.push(source);
    }
    crate::ast::routine_trait::retain(body, traits);
    Ok(())
}

/// A named routine with its signature and body. The return type is rendered as
/// the signature's `-->`; a `returns` / `of` trait the source wrote replaces
/// that when [`apply`] puts the written traits on the node.
// Cost: O(n), n = size of the declaration.
pub(super) fn routine_node(
    class: RakuAstClass,
    name: &str,
    param_defs: &[ParamDef],
    body: &[Stmt],
    returns: Option<&str>,
) -> Result<RakuAstNode, RuntimeError> {
    let _underscore_scope = super::underscore_bindings::Scope::parameters(param_defs);
    let name_node = name_parts::operator_name(name).unwrap_or_else(|| name_from_identifier(name));
    let mut fields = vec![node_field(Some("name"), name_node)];
    if !param_defs.is_empty() || returns.is_some() {
        fields.push(node_field(
            Some("signature"),
            signature(param_defs, true, returns)?,
        ));
    }
    fields.push(node_field(Some("body"), blockoid(body)?));
    Ok(RakuAstNode {
        origin: Default::default(),
        class,
        fields,
    })
}
