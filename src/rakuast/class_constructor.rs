//! Class source nodes provide the enclosing scope for constructed attributes.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use super::{named_arg, rakuast_node_list, require_rakuast_class};
use crate::value::{RuntimeError, Value};

// Cost: O(a + t + s), a = arguments, t = traits, s = stringified scope length.
pub(super) fn construct(args: &[Value]) -> Result<Value, RuntimeError> {
    construct_as(args, RakuAstClass::Class)
}

pub(super) fn construct_native(args: &[Value]) -> Result<Value, RuntimeError> {
    construct_as(args, RakuAstClass::Native)
}

// Cost: O(a + t + s), a = arguments, t = traits, s = stringified scope length.
pub(super) fn construct_grammar(args: &[Value]) -> Result<Value, RuntimeError> {
    construct_as(args, RakuAstClass::Grammar)
}

// Cost: O(a + t + s), a = arguments, t = traits, s = stringified scope length.
pub(super) fn construct_role(args: &[Value]) -> Result<Value, RuntimeError> {
    construct_as(args, RakuAstClass::Role)
}

// Cost: O(a), a = arguments.
pub(super) fn construct_role_body(args: &[Value]) -> Result<Value, RuntimeError> {
    let body = named_arg(args, "body").unwrap_or_else(super::empty_blockoid);
    require_rakuast_class(&body, RakuAstClass::Blockoid, "RakuAST::RoleBody.new")?;
    Ok(Value::rakuast(Box::new(super::role_implicit::add(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::RoleBody,
        fields: vec![field("body", body)],
    }))))
}

fn construct_as(args: &[Value], class: RakuAstClass) -> Result<Value, RuntimeError> {
    let constructor = format!("{}.new", class.printed_name());
    let mut fields = Vec::new();
    if let Some(scope) = named_arg(args, "scope")
        && scope.truthy()
    {
        fields.push(field("scope", Value::str(scope.to_string_value())));
    }
    if let Some(name) = named_arg(args, "name")
        && !name.is_nil()
    {
        require_rakuast_class(&name, RakuAstClass::Name, &constructor)?;
        fields.push(field("name", name));
    }
    // `class C is repr('CStruct')` keeps its repr, as a `native` does.
    if let Some(repr) = named_arg(args, "repr")
        && !repr.is_nil()
    {
        fields.push(field("repr", Value::str(repr.to_string_value())));
    }
    if let Some(traits) = named_arg(args, "traits")
        && !traits.is_nil()
    {
        fields.push(RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(rakuast_node_list(&traits, &constructor, "traits")?),
        });
    }
    let body = match named_arg(args, "body") {
        Some(body) => {
            let expected = if class == RakuAstClass::Role {
                RakuAstClass::RoleBody
            } else {
                RakuAstClass::Block
            };
            require_rakuast_class(&body, expected, &constructor)?;
            body
        }
        None if class == RakuAstClass::Role => construct_role_body(&[])?,
        None => super::empty_block(),
    };
    fields.push(field("body", body));
    if class == RakuAstClass::Role
        && let Some(signature) = named_arg(args, "parameterization")
        && !signature.is_nil()
    {
        require_rakuast_class(&signature, RakuAstClass::Signature, "RakuAST::Role.new")?;
        fields.push(field("parameterization", signature));
    }
    Ok(Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class,
        fields,
    })))
}

fn field(name: &'static str, value: Value) -> RakuAstField {
    RakuAstField {
        name: Some(name),
        value: RakuAstFieldValue::Node(value),
    }
}
