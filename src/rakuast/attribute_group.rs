//! Parenthesized `has` declarations as one source RakuAST signature.

use super::view::{build_type_node, leaf_field, node_field, unsupported};
use super::{RakuAstClass as C, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Stmt;
use crate::scope::NameScope;
use crate::value::{RuntimeError, Value, ValueView};

const ATTRIBUTES: &str = "attribute-group-members";

/// Build Rakudo's `VarDeclaration::Signature` shape and retain the individual
/// declarations as hidden semantic metadata, since traits on a grouped `has`
/// are not printed in Rakudo's constructor-form gist.
// Cost: O(a), a = total size of the attribute declarations and their expressions.
pub(super) fn to_signature(
    names: &NameScope,
    attributes: &[Stmt],
) -> Result<RakuAstNode, RuntimeError> {
    let mut parameters = Vec::with_capacity(attributes.len());
    let mut members = Vec::with_capacity(attributes.len());
    let mut common_type: Option<&str> = None;
    for attribute in attributes {
        let Stmt::HasDecl {
            name,
            sigil,
            is_public,
            is_alias,
            type_constraint,
            ..
        } = attribute
        else {
            return Err(unsupported("grouped attribute declaration"));
        };
        if let Some(ty) = type_constraint.as_deref() {
            if common_type.is_some_and(|common| common != ty) {
                return Err(unsupported("grouped attributes with different types"));
            }
            common_type = Some(ty);
        }
        let spelling = if *is_alias {
            format!("{sigil}{}", name.resolve())
        } else {
            format!(
                "{sigil}{}{}",
                if *is_public { "." } else { "!" },
                name.resolve()
            )
        };
        let target = RakuAstNode {
            origin: Default::default(),
            class: C::ParameterTargetVar,
            fields: vec![leaf_field(Some("name"), Value::str(spelling))],
        };
        parameters.push(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class: C::Parameter,
            fields: vec![
                node_field(Some("target"), target),
                leaf_field(Some("optional"), Value::truth(false)),
            ],
        })));
        let Some(member) = super::attribute_decl::view_node(names, attribute)? else {
            return Err(unsupported("grouped attribute declaration"));
        };
        let expression = super::lower::named_child(&member, "expression")?;
        members.push(Value::rakuast(Box::new(expression.clone())));
    }

    let mut signature_fields = vec![RakuAstField {
        name: Some("parameters"),
        value: RakuAstFieldValue::List(parameters),
    }];
    if let Some(ty) = common_type {
        signature_fields.push(node_field(Some("returns"), build_type_node(names, ty)?));
    }
    let mut fields = vec![node_field(
        Some("signature"),
        RakuAstNode {
            origin: Default::default(),
            class: C::Signature,
            fields: signature_fields,
        },
    )];
    fields.push(leaf_field(Some("scope"), Value::str("has".to_string())));
    if let Some(ty) = common_type {
        fields.push(node_field(Some("type"), build_type_node(names, ty)?));
    }
    fields.push(RakuAstField {
        name: Some(ATTRIBUTES),
        value: RakuAstFieldValue::List(members),
    });
    Ok(RakuAstNode {
        origin: Default::default(),
        class: C::VarDeclarationSignature,
        fields,
    })
}

/// Rakudo displays the declaration before a colon argument list and hides
/// declarations parsed from that list. Preserve those declarations as hidden
/// model metadata so evaluating the RakuAST still installs them. The result is
/// the statement of the visible declaration: a modifier written after the
/// whole statement (`has Int $.a: has Str $.b = 1 if 1`) is on it.
// Cost: O(a), a = total size of the declarations and expressions in the colon argument list.
pub(super) fn view_colon(
    names: &NameScope,
    attributes: &[Stmt],
) -> Result<RakuAstNode, RuntimeError> {
    let Some(visible) = attributes.last() else {
        return Err(unsupported("empty attribute colon argument list"));
    };
    let mut statement = member_statement(names, visible)?;
    let mut members = Vec::with_capacity(attributes.len());
    for attribute in attributes {
        let member = member_statement(names, attribute)?;
        members.push(Value::rakuast(Box::new(
            super::lower::named_child(&member, "expression")?.clone(),
        )));
    }
    let Some(field) = statement
        .fields
        .iter_mut()
        .find(|field| field.name == Some("expression"))
    else {
        return Err(unsupported("attribute colon argument list"));
    };
    let RakuAstFieldValue::Node(value) = &field.value else {
        return Err(unsupported("attribute colon argument list"));
    };
    let ValueView::RakuAst(declaration) = value.view() else {
        return Err(unsupported("attribute colon argument list"));
    };
    let mut declaration = declaration.clone();
    declaration.fields.push(RakuAstField {
        name: Some(ATTRIBUTES),
        value: RakuAstFieldValue::List(members),
    });
    field.value = RakuAstFieldValue::Node(Value::rakuast(Box::new(declaration)));
    Ok(statement)
}

/// The statement a member of a colon argument list is: a plain `has`, or one
/// that carries statement modifiers.
fn member_statement(names: &NameScope, member: &Stmt) -> Result<RakuAstNode, RuntimeError> {
    match member {
        Stmt::HasDecl { .. } => super::attribute_decl::view_node(names, member)?,
        _ => super::view::view_stmt(names, member)?,
    }
    .ok_or_else(|| unsupported("attribute colon argument list"))
}

/// Lower the hidden members back through the parser's shared class-body
/// expansion, retaining traits and attribute-specific runtime behavior.
// Cost: O(a), a = total size of attribute declarations and their expressions.
pub(super) fn lower(names: &NameScope, node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let Some(field) = node
        .fields
        .iter()
        .find(|field| field.name == Some(ATTRIBUTES))
    else {
        return Err(unsupported(
            "signature declaration without grouped attributes",
        ));
    };
    let RakuAstFieldValue::List(members) = &field.value else {
        return Err(unsupported("grouped attribute metadata"));
    };
    let mut attributes = Vec::with_capacity(members.len());
    for member in members {
        let ValueView::RakuAst(member) = member.view() else {
            return Err(unsupported("grouped attribute metadata"));
        };
        attributes.push(super::attribute_decl::lower(names, member)?);
    }
    super::conditional_lower::lower_parsed(crate::ast::parsed::ParsedStmt::AttributeGroup(
        attributes,
    ))
}

/// Lower an attribute declaration and its hidden colon-argument declarations.
// Cost: O(a), a = total size of declarations and expressions in the colon argument list.
pub(super) fn lower_colon(names: &NameScope, node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let Some(field) = node
        .fields
        .iter()
        .find(|field| field.name == Some(ATTRIBUTES))
    else {
        return Err(unsupported("attribute without colon metadata"));
    };
    let RakuAstFieldValue::List(members) = &field.value else {
        return Err(unsupported("attribute colon metadata"));
    };
    let mut attributes = Vec::with_capacity(members.len());
    for member in members {
        let ValueView::RakuAst(member) = member.view() else {
            return Err(unsupported("attribute colon metadata"));
        };
        attributes.push(super::attribute_decl::lower(names, member)?);
    }
    super::conditional_lower::lower_parsed(crate::ast::parsed::ParsedStmt::AttributeColonArglist(
        attributes,
    ))
}

// Cost: O(1).
pub(super) fn is_metadata(field: &RakuAstField) -> bool {
    field.name == Some(ATTRIBUTES)
}
