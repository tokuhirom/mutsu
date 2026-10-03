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
//! and `lower` builds one and hands it to the same expansion. Elements with a
//! type, default, constraint, trait, sigilless or literal spelling, a nested
//! group, a named or slurpy element and a group `is default` are deferred.

use super::convert::{convert_expr, leaf_field, node_field, unsupported};
use super::lower::{list_field, lower_expr, named_child, named_child_or_positional};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{SignatureDecl, SignatureInit, SignatureVar, Stmt};
use crate::value::{RuntimeError, Value, ValueView};

/// The `VarDeclaration::Signature` an expansion's source-form record describes.
pub(super) fn convert(decl: &SignatureDecl) -> Result<RakuAstNode, RuntimeError> {
    if decl.type_constraint.is_some()
        || decl.group_default.is_some()
        || decl.has_nested_group
        || !decl.vars.iter().all(SignatureVar::is_plain)
    {
        return Err(unsupported(
            "signature declaration with a typed, defaulted, constrained or non-positional element",
        ));
    }
    let parameters = decl
        .vars
        .iter()
        .map(|var| Value::rakuast(Box::new(parameter(&var.spelling()))))
        .collect();
    let signature = RakuAstNode {
        class: RakuAstClass::Signature,
        fields: vec![RakuAstField {
            name: Some("parameters"),
            value: RakuAstFieldValue::List(parameters),
        }],
    };
    let mut fields = vec![node_field(Some("signature"), signature)];
    if decl.is_our || decl.is_state {
        let scope = if decl.is_our { "our" } else { "state" };
        fields.push(leaf_field(Some("scope"), Value::str(scope.to_string())));
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

/// `Parameter(default-rw => True, target => ParameterTarget::Var(name => …),
/// optional => False)` -- a declarator-list element, which unlike a routine
/// parameter carries no implicit type and defaults to a writable container.
fn parameter(spelling: &str) -> RakuAstNode {
    let target = RakuAstNode {
        class: RakuAstClass::ParameterTargetVar,
        fields: vec![leaf_field(Some("name"), Value::str(spelling.to_string()))],
    };
    RakuAstNode {
        class: RakuAstClass::Parameter,
        fields: vec![
            leaf_field(Some("default-rw"), Value::truth(true)),
            node_field(Some("target"), target),
            leaf_field(Some("optional"), Value::truth(false)),
        ],
    }
}

/// `VarDeclaration::Signature` -> the parser's expansion of the declaration.
pub(super) fn lower(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let signature = named_child(node, "signature")?;
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
        type_constraint: None,
        group_default: None,
        has_nested_group: false,
        init,
    }))
}

/// A plain declarator-list element: only a variable target.
fn lower_parameter(param: &RakuAstNode) -> Result<SignatureVar, RuntimeError> {
    let unsupported = || super::lower::unsupported(param);
    if param.class != RakuAstClass::Parameter {
        return Err(unsupported());
    }
    for field in &param.fields {
        match field.name {
            Some("target") | Some("default-rw") => {}
            Some("optional") => {
                if matches!(&field.value, RakuAstFieldValue::Node(v) if v.truthy()) {
                    return Err(unsupported());
                }
            }
            _ => return Err(unsupported()),
        }
    }
    let target = named_child(param, "target")?;
    if target.class != RakuAstClass::ParameterTargetVar {
        return Err(unsupported());
    }
    let name = super::lower::leaf_str(target, "name")?;
    if !name.starts_with(['$', '@', '%', '&']) {
        return Err(unsupported());
    }
    Ok(SignatureVar::plain(&name))
}
