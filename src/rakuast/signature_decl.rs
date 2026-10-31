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

use crate::scope::NameScope;
use super::convert::{convert_expr, leaf_field, node_field, unsupported};
use super::lower::{list_field, lower_expr, named_child, named_child_or_positional};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{ParamTrait, SignatureDecl, SignatureGroup, SignatureInit, SignatureVar, Stmt};
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
        origin: Default::default(),
        class: RakuAstClass::VarDeclarationSignature,
        fields,
    })))
}

/// The `VarDeclaration::Signature` an expansion's source-form record describes.
pub(super) fn convert(names: &NameScope, decl: &SignatureDecl) -> Result<RakuAstNode, RuntimeError> {
    let is_binding = decl.init.as_ref().is_some_and(|init| init.is_binding);
    let parameters = if let Some(shape) = &decl.group_shape {
        shape
            .iter()
            .map(|item| convert_group(item, &decl.vars, is_binding, 0))
            .collect::<Result<Vec<_>, _>>()?
    } else {
        decl.vars
            .iter()
            .map(|var| parameter(names, var, is_binding).map(|node| Value::rakuast(Box::new(node))))
            .collect::<Result<Vec<_>, _>>()?
    };
    let mut signature_fields = vec![RakuAstField {
        name: Some("parameters"),
        value: RakuAstFieldValue::List(parameters),
    }];
    if let Some(type_name) = &decl.type_constraint {
        signature_fields.push(node_field(
            Some("returns"),
            super::convert::build_type_node(names, type_name)?,
        ));
    }
    let signature = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Signature,
        fields: signature_fields,
    };
    let mut fields = vec![node_field(Some("signature"), signature)];
    if let Some(default) = &decl.group_default {
        // Rakudo omits the group trait from .raku, but execution still needs it.
        fields.push(node_field(Some("group-default"), convert_expr(names, default)?));
    }
    if decl.is_our || decl.is_state {
        let scope = if decl.is_our { "our" } else { "state" };
        fields.push(leaf_field(Some("scope"), Value::str(scope.to_string())));
    }
    // The declaration's own type is also a field of the declaration.
    if let Some(type_name) = &decl.type_constraint {
        fields.push(node_field(
            Some("type"),
            super::convert::build_type_node(names, type_name)?,
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
                origin: Default::default(),
                class,
                fields: vec![node_field(None, convert_expr(names, &init.rhs)?)],
            },
        ));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::VarDeclarationSignature,
        fields,
    })
}

/// A source group into a parameter with a nested sub-signature.
// Cost: O(n), n = leaves in the group, with nesting bounded at 128.
fn convert_group(
    item: &SignatureGroup,
    vars: &[SignatureVar],
    is_binding: bool,
    depth: usize,
) -> Result<Value, RuntimeError> {
    if depth > 128 {
        return Err(unsupported(
            "signature declaration group exceeds 128 levels",
        ));
    }
    let node = match item {
        SignatureGroup::Var(index) => parameter(
            vars.get(*index)
                .ok_or_else(|| unsupported("signature declaration group index"))?,
            is_binding,
        )?,
        SignatureGroup::Group(members) => {
            let parameters = members
                .iter()
                .map(|member| convert_group(member, vars, is_binding, depth + 1))
                .collect::<Result<Vec<_>, _>>()?;
            RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::Parameter,
                fields: vec![
                    leaf_field(Some("optional"), Value::truth(false)),
                    node_field(
                        Some("sub-signature"),
                        RakuAstNode {
                            origin: Default::default(),
                            class: RakuAstClass::Signature,
                            fields: vec![RakuAstField {
                                name: Some("parameters"),
                                value: RakuAstFieldValue::List(parameters),
                            }],
                        },
                    ),
                ],
            }
        }
    };
    Ok(Value::rakuast(Box::new(node)))
}

/// `Parameter([type,] [names,] [default-rw => True,] target => …[, optional =>
/// False][, slurpy][, where][, traits])` -- a declarator-list element, which
/// unlike a routine parameter carries no implicit type and defaults to a
/// writable container (unless the declaration binds with `:=`). A named or
/// slurpy element has no `optional`.
// Cost: O(e), e = size of the element's `where` expression.
fn parameter(names: &NameScope, var: &SignatureVar, is_binding: bool) -> Result<RakuAstNode, RuntimeError> {
    if let Some(literal) = &var.literal_value {
        return super::signature_literal::convert(names, literal);
    }
    let target = if var.sigilless {
        RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::ParameterTargetTerm,
            fields: vec![node_field(
                None,
                super::convert::name_from_identifier(&var.name),
            )],
        }
    } else {
        RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::ParameterTargetVar,
            fields: vec![leaf_field(
                Some("name"),
                Value::str(match anonymous_sigil(&var.name) {
                    Some(sigil) => sigil.to_string(),
                    None => var.spelling(),
                }),
            )],
        }
    };
    let mut fields = Vec::new();
    if let Some(type_name) = &var.per_var_type_constraint {
        fields.push(node_field(
            Some("type"),
            super::type_build::declaration_type_node(names, type_name)?,
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
        fields.push(node_field(Some("default"), convert_expr(names, default)?));
    }
    if var.is_slurpy {
        fields.push(leaf_field(
            Some("slurpy"),
            super::slurpy_marker_value(RakuAstClass::ParameterSlurpyFlattened),
        ));
    }
    if let Some(constraint) = &var.where_constraint {
        fields.push(node_field(Some("where"), convert_expr(names, constraint)?));
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
        origin: Default::default(),
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

/// `VarDeclaration::Signature` -> the declaration as the parser holds it
/// (`Stmt::Ast`), which the caller lowers.
pub(super) fn lower(names: &NameScope, node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
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
                    Some(super::lower::simple_type_name(names, node, type_node)?)
                }
                _ => return Err(super::lower::unsupported(node)),
            },
            _ => return Err(super::lower::unsupported(node)),
        },
    };
    let mut vars = Vec::new();
    let mut shape = Vec::new();
    for param in list_field(signature, "parameters")? {
        let ValueView::RakuAst(param) = param.view() else {
            return Err(super::lower::unsupported(node));
        };
        shape.push(lower_group_parameter(param, &mut vars, 0)?);
    }
    let has_nested_group = shape
        .iter()
        .any(|item| matches!(item, SignatureGroup::Group(_)));
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
    super::signature_expand::pending(SignatureDecl {
        vars,
        group_shape: has_nested_group.then_some(shape),
        is_state,
        is_our,
        type_constraint,
        group_default: node
            .fields
            .iter()
            .any(|f| f.name == Some("group-default"))
            .then(|| lower_expr(named_child(node, "group-default")?))
            .transpose()?,
        has_nested_group,
        init,
    })
}

/// Collect flattened declaration leaves while retaining sub-signature groups.
// Cost: O(n), n = parameter nodes, with nesting bounded at 128.
fn lower_group_parameter(
    param: &RakuAstNode,
    vars: &mut Vec<SignatureVar>,
    depth: usize,
) -> Result<SignatureGroup, RuntimeError> {
    if depth > 128 {
        return Err(super::lower::unsupported(param));
    }
    if let Ok(sub_signature) = named_child(param, "sub-signature") {
        if param.class != RakuAstClass::Parameter || sub_signature.class != RakuAstClass::Signature
        {
            return Err(super::lower::unsupported(param));
        }
        let mut members = Vec::new();
        for value in list_field(sub_signature, "parameters")? {
            let ValueView::RakuAst(member) = value.view() else {
                return Err(super::lower::unsupported(param));
            };
            members.push(lower_group_parameter(member, vars, depth + 1)?);
        }
        return Ok(SignatureGroup::Group(members));
    }
    let index = vars.len();
    vars.push(lower_parameter(names, param)?);
    Ok(SignatureGroup::Var(index))
}

/// A declarator-list element, back from its `Parameter`.
fn lower_parameter(names: &NameScope, param: &RakuAstNode) -> Result<SignatureVar, RuntimeError> {
    let unsupported = || super::lower::unsupported(param);
    if param.class != RakuAstClass::Parameter {
        return Err(unsupported());
    }
    let mut var = SignatureVar::plain("$x");
    if param.fields.iter().any(|f| f.name == Some("value")) {
        return super::signature_literal::lower(names, param);
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
                    Some(super::lower::simple_type_name(names, param, type_node)?);
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
    } else if let Some(internal) = anonymous_name(&spelling) {
        var.name = internal.to_string();
    } else {
        var.name = SignatureVar::plain(&spelling).name;
    }
    var.is_slurpy = slurpy;
    Ok(var)
}

/// The bare sigil an anonymous element (`my ($, @) = ...`) is written as,
/// for the parser's internal name of it.
// Cost: O(1).
fn anonymous_sigil(name: &str) -> Option<&'static str> {
    match name {
        super::convert::ANONYMOUS_SCALAR_PARAM => Some("$"),
        super::convert::ANONYMOUS_ARRAY_PARAM => Some("@"),
        super::convert::ANONYMOUS_HASH_PARAM => Some("%"),
        _ => None,
    }
}

/// The parser's internal name of the anonymous element written as `sigil`.
// Cost: O(1).
fn anonymous_name(sigil: &str) -> Option<&'static str> {
    match sigil {
        "$" => Some(super::convert::ANONYMOUS_SCALAR_PARAM),
        "@" => Some(super::convert::ANONYMOUS_ARRAY_PARAM),
        "%" => Some(super::convert::ANONYMOUS_HASH_PARAM),
        _ => None,
    }
}
