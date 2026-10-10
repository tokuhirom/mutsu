//! Signature declarations (`my ($a, @b) = …`) across the RakuAST boundary.
//!
//! Measured against rakudo 2026.09, a declarator list is one
//! `RakuAST::VarDeclaration::Signature`:
//!
//! ```text
//! VarDeclaration::Signature(
//!   signature   => Signature(parameters => (Parameter(default-rw => True,
//!                    target => ParameterTarget::Var(name => "$a"), optional => False), …)),
//!   scope       => "our",                          # only when not `my`
//!   initializer => Initializer::Assign(EXPR))      # or Initializer::Bind; absent when bare
//! ```
//!
//! The parser expands the declaration (`parser::stmt::decl::destructure::desugar`)
//! and keeps the source form as the expansion's first statement (ADR-10723
//! Stage 1), so both directions go through that record: `convert` reads it,
//! and `lower` builds one and hands it to the same expansion. An element can
//! have a type (`Int $a`), a `where`, one of `is rw` / `raw` / `copy` /
//! `readonly`, a sigilless target (`\c`, a `ParameterTarget::Term`), a name
//! (`:$c`, `names => ("c",)`) or be slurpy (`*@r`, `slurpy => Flattened`); the
//! declaration's own type is the signature's `returns`. Optional/defaulted
//! elements and literal postconstraints retain their parameter fields. A group
//! `is default` is metadata omitted by Rakudo's renderer. Nested groups remain
//! refused until their parser representation retains the grouping.

use super::convert::{convert_expr, leaf_field, node_field, unsupported};
use super::lower::{list_field, lower_expr, named_child, named_child_or_positional};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{ParamTrait, SignatureDecl, SignatureInit, SignatureVar, Stmt};
use crate::value::{RuntimeError, Value, ValueView};

/// Construct the same declaring signature shape produced by the parser.
// Cost: O(a), a = number of constructor arguments (a fixed number of lookups).
pub(super) fn construct(args: &[Value]) -> Result<Value, RuntimeError> {
    let ctor = "RakuAST::VarDeclaration::Signature.new";
    let signature = super::named_arg(args, "signature")
        .ok_or_else(|| RuntimeError::new(format!("{ctor} requires `signature`")))?;
    super::require_rakuast_class(&signature, RakuAstClass::Signature, ctor)?;
    let mut fields = vec![RakuAstField {
        name: Some("signature"),
        value: RakuAstFieldValue::Node(signature),
    }];
    if let Some(scope) = super::named_arg(args, "scope") {
        if !matches!(scope.view(), ValueView::Str(_)) {
            return Err(RuntimeError::new(format!(
                "{ctor} expects `scope` to be Str"
            )));
        }
        fields.push(leaf_field(Some("scope"), scope));
    }
    if let Some(ty) = super::named_arg(args, "type") {
        super::require_rakuast_type(&ty, ctor)?;
        fields.push(RakuAstField {
            name: Some("type"),
            value: RakuAstFieldValue::Node(ty),
        });
    }
    if let Some(init) = super::named_arg(args, "initializer") {
        if !matches!(init.view(), ValueView::RakuAst(n) if matches!(n.class,
            RakuAstClass::InitializerAssign | RakuAstClass::InitializerBind))
        {
            return Err(RuntimeError::new(format!(
                "{ctor} expects an assignment or binding initializer"
            )));
        }
        fields.push(RakuAstField {
            name: Some("initializer"),
            value: RakuAstFieldValue::Node(init),
        });
    }
    Ok(Value::rakuast(Box::new(RakuAstNode {
        class: RakuAstClass::VarDeclarationSignature,
        fields,
    })))
}

/// The `VarDeclaration::Signature` an expansion's source-form record describes.
pub(super) fn convert(decl: &SignatureDecl) -> Result<RakuAstNode, RuntimeError> {
    if decl.has_nested_group {
        return Err(unsupported("signature declaration with a nested group"));
    }
    let mut parameters = Vec::with_capacity(decl.vars.len());
    for var in &decl.vars {
        parameters.push(Value::rakuast(Box::new(parameter(
            var,
            decl.init.as_ref().is_some_and(|init| init.is_binding),
        )?)));
    }
    let mut signature_fields = vec![RakuAstField {
        name: Some("parameters"),
        value: RakuAstFieldValue::List(parameters),
    }];
    if let Some(type_name) = &decl.type_constraint {
        signature_fields.push(node_field(
            Some("returns"),
            super::convert::build_type_node(type_name)?,
        ));
    }
    let signature = RakuAstNode {
        class: RakuAstClass::Signature,
        fields: signature_fields,
    };
    let mut fields = vec![node_field(Some("signature"), signature)];
    if let Some(default) = &decl.group_default {
        // Rakudo omits the group trait from .raku, but execution still needs it.
        fields.push(node_field(Some("group-default"), convert_expr(default)?));
    }
    if decl.is_our || decl.is_state {
        let scope = if decl.is_our { "our" } else { "state" };
        fields.push(leaf_field(Some("scope"), Value::str(scope.to_string())));
    }
    // The declaration's own type is also a field of the declaration.
    if let Some(type_name) = &decl.type_constraint {
        fields.push(node_field(
            Some("type"),
            super::convert::build_type_node(type_name)?,
        ));
    }
    if let Some(init) = &decl.init {
        let class = if init.is_binding {
            RakuAstClass::InitializerBind
        } else {
            RakuAstClass::InitializerAssign
        };
        fields.push(node_field(
            Some("initializer"),
            RakuAstNode {
                class,
                fields: vec![node_field(None, convert_expr(&init.rhs)?)],
            },
        ));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::VarDeclarationSignature,
        fields,
    })
}

/// `Parameter([type,] [names,] [default-rw => True,] target => …[, optional =>
/// False][, slurpy][, where][, traits])` -- a declarator-list element, which
/// unlike a routine parameter carries no implicit type and defaults to a
/// writable container (unless the declaration binds with `:=`). A named or
/// slurpy element has no `optional`.
// Cost: O(e), e = size of the element's `where` expression.
fn parameter(var: &SignatureVar, is_binding: bool) -> Result<RakuAstNode, RuntimeError> {
    if let Some(literal) = &var.literal_value {
        return super::signature_literal::convert(literal);
    }
    let target = if var.sigilless {
        RakuAstNode {
            class: RakuAstClass::ParameterTargetTerm,
            fields: vec![node_field(
                None,
                super::convert::name_from_identifier(&var.name),
            )],
        }
    } else {
        RakuAstNode {
            class: RakuAstClass::ParameterTargetVar,
            fields: vec![leaf_field(Some("name"), Value::str(var.spelling()))],
        }
    };
    let mut fields = Vec::new();
    if let Some(type_name) = &var.per_var_type_constraint {
        fields.push(node_field(
            Some("type"),
            super::convert::build_type_node(type_name)?,
        ));
    }
    if var.is_named {
        let name = var.name.trim_start_matches(['@', '%', '&']);
        fields.push(RakuAstField {
            name: Some("names"),
            value: RakuAstFieldValue::List(vec![Value::str(name.to_string())]),
        });
    }
    // A `:=` declaration binds the targets as they are: no writable default.
    if !is_binding {
        fields.push(leaf_field(Some("default-rw"), Value::truth(true)));
    }
    fields.push(node_field(Some("target"), target));
    if !var.is_named && !var.is_slurpy && (var.default.is_none() || var.is_optional) {
        fields.push(leaf_field(Some("optional"), Value::truth(var.is_optional)));
    }
    if let Some(default) = &var.default {
        fields.push(node_field(Some("default"), convert_expr(default)?));
    }
    if var.is_slurpy {
        fields.push(leaf_field(
            Some("slurpy"),
            super::slurpy_marker_value(RakuAstClass::ParameterSlurpyFlattened),
        ));
    }
    if let Some(constraint) = &var.where_constraint {
        fields.push(node_field(Some("where"), convert_expr(constraint)?));
    }
    if let Some(param_trait) = var.param_trait {
        fields.push(RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(
                super::routine_traits::trait_is(trait_name(param_trait), None),
            ))]),
        });
    }
    Ok(RakuAstNode {
        class: RakuAstClass::Parameter,
        fields,
    })
}

fn trait_name(param_trait: ParamTrait) -> &'static str {
    match param_trait {
        ParamTrait::Rw => "rw",
        ParamTrait::Raw => "raw",
        ParamTrait::Copy => "copy",
        ParamTrait::Readonly => "readonly",
    }
}

/// `VarDeclaration::Signature` -> the parser's expansion of the declaration.
pub(super) fn lower(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let signature = named_child(node, "signature")?;
    let type_constraint = match signature
        .fields
        .iter()
        .find(|f| f.name == Some("returns"))
        .or_else(|| node.fields.iter().find(|f| f.name == Some("type")))
    {
        None => None,
        Some(field) => match &field.value {
            RakuAstFieldValue::Node(v) => match v.view() {
                ValueView::RakuAst(type_node) => {
                    Some(super::lower::simple_type_name(node, type_node)?)
                }
                _ => return Err(super::lower::unsupported(node)),
            },
            _ => return Err(super::lower::unsupported(node)),
        },
    };
    let vars = list_field(signature, "parameters")?
        .iter()
        .map(|param| match param.view() {
            ValueView::RakuAst(param) => lower_parameter(param),
            _ => Err(super::lower::unsupported(node)),
        })
        .collect::<Result<Vec<_>, _>>()?;
    let scope = node
        .fields
        .iter()
        .find(|f| f.name == Some("scope"))
        .map(|f| match &f.value {
            RakuAstFieldValue::Node(v) => match v.view() {
                ValueView::Str(s) => Ok(s.to_string()),
                _ => Err(super::lower::unsupported(node)),
            },
            _ => Err(super::lower::unsupported(node)),
        })
        .transpose()?;
    let (is_our, is_state) = match scope.as_deref() {
        None | Some("my") => (false, false),
        Some("our") => (true, false),
        Some("state") => (false, true),
        Some(_) => return Err(super::lower::unsupported(node)),
    };
    let init = match named_child(node, "initializer") {
        Err(_) => None,
        Ok(init) => Some(SignatureInit {
            is_binding: match init.class {
                RakuAstClass::InitializerAssign => false,
                RakuAstClass::InitializerBind => true,
                _ => return Err(super::lower::unsupported(init)),
            },
            rhs: lower_expr(named_child_or_positional(init)?)?,
        }),
    };
    Ok(crate::parser::signature_decl_expansion(SignatureDecl {
        vars,
        is_state,
        is_our,
        type_constraint,
        group_default: node
            .fields
            .iter()
            .any(|f| f.name == Some("group-default"))
            .then(|| lower_expr(named_child(node, "group-default")?))
            .transpose()?,
        has_nested_group: false,
        init,
    }))
}

/// A declarator-list element, back from its `Parameter`.
fn lower_parameter(param: &RakuAstNode) -> Result<SignatureVar, RuntimeError> {
    let unsupported = || super::lower::unsupported(param);
    if param.class != RakuAstClass::Parameter {
        return Err(unsupported());
    }
    let mut var = SignatureVar::plain("$x");
    if param.fields.iter().any(|f| f.name == Some("value")) {
        return super::signature_literal::lower(param);
    }
    let mut named = false;
    let mut slurpy = false;
    for field in &param.fields {
        match field.name {
            Some("target") | Some("default-rw") => {}
            Some("optional") => {
                var.is_optional = super::lower::bool_field(param, "optional")?;
            }
            Some("default") => var.default = Some(lower_expr(named_child(param, "default")?)?),
            Some("type") => {
                let type_node = named_child(param, "type")?;
                var.per_var_type_constraint =
                    Some(super::lower::simple_type_name(param, type_node)?);
            }
            Some("names") => {
                let RakuAstFieldValue::List(names) = &field.value else {
                    return Err(unsupported());
                };
                let [name] = names.as_slice() else {
                    return Err(unsupported());
                };
                let ValueView::Str(name) = name.view() else {
                    return Err(unsupported());
                };
                named = true;
                var.name = name.to_string();
            }
            Some("slurpy") => {
                let RakuAstFieldValue::Node(marker) = &field.value else {
                    return Err(unsupported());
                };
                if super::slurpy_marker_class(marker)
                    != Some(RakuAstClass::ParameterSlurpyFlattened)
                {
                    return Err(unsupported());
                }
                slurpy = true;
            }
            Some("where") => {
                var.where_constraint = Some(lower_expr(named_child(param, "where")?)?);
            }
            Some("traits") => {
                let [item] = list_field(param, "traits")? else {
                    return Err(unsupported());
                };
                let ValueView::RakuAst(item) = item.view() else {
                    return Err(unsupported());
                };
                if item.class != RakuAstClass::TraitIs || item.fields.len() != 1 {
                    return Err(unsupported());
                }
                let name = super::lower::positional_leaf(named_child(item, "name")?)?;
                var.param_trait = Some(match name.view() {
                    ValueView::Str(s) if s.as_str() == "rw" => ParamTrait::Rw,
                    ValueView::Str(s) if s.as_str() == "raw" => ParamTrait::Raw,
                    ValueView::Str(s) if s.as_str() == "copy" => ParamTrait::Copy,
                    ValueView::Str(s) if s.as_str() == "readonly" => ParamTrait::Readonly,
                    _ => return Err(unsupported()),
                });
            }
            _ => return Err(unsupported()),
        }
    }
    let target = named_child(param, "target")?;
    let spelling = match target.class {
        RakuAstClass::ParameterTargetVar => {
            let name = super::lower::leaf_str(target, "name")?;
            if !name.starts_with(['$', '@', '%', '&']) {
                return Err(unsupported());
            }
            name
        }
        // `\c`: the term's name, with no sigil to strip.
        RakuAstClass::ParameterTargetTerm => {
            var.sigilless = true;
            match super::name_parts::name_shape(named_child_or_positional(target)?) {
                Some(super::name_parts::NameShape::Identifier(name)) => name,
                _ => return Err(unsupported()),
            }
        }
        _ => return Err(unsupported()),
    };
    if named {
        // `:$c` names the variable it binds.
        let bare = spelling.trim_start_matches(['$', '@', '%', '&']);
        if var.name != bare {
            return Err(unsupported());
        }
        var.is_named = true;
    }
    if var.sigilless {
        var.name = spelling;
    } else {
        var.name = SignatureVar::plain(&spelling).name;
    }
    var.is_slurpy = slurpy;
    Ok(var)
}
