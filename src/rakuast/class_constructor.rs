//! Class source nodes provide the enclosing scope for constructed attributes.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use super::{named_arg, rakuast_node_list, require_rakuast_class};
use crate::value::{RuntimeError, Value};

const CONSTRUCTOR: &str = "RakuAST::Class.new";

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

fn construct_as(args: &[Value], class: RakuAstClass) -> Result<Value, RuntimeError> {
    let mut fields = Vec::new();
    if let Some(scope) = named_arg(args, "scope")
        && scope.truthy()
    {
        fields.push(field("scope", Value::str(scope.to_string_value())));
    }
    if let Some(name) = named_arg(args, "name")
        && !name.is_nil()
    {
        require_rakuast_class(&name, RakuAstClass::Name, CONSTRUCTOR)?;
        fields.push(field("name", name));
    }
    if class == RakuAstClass::Native
        && let Some(repr) = named_arg(args, "repr")
        && !repr.is_nil()
    {
        fields.push(field("repr", Value::str(repr.to_string_value())));
    }
    if let Some(traits) = named_arg(args, "traits")
        && !traits.is_nil()
    {
        fields.push(RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(rakuast_node_list(&traits, CONSTRUCTOR, "traits")?),
        });
    }
    let body = match named_arg(args, "body") {
        Some(body) => {
            require_rakuast_class(&body, RakuAstClass::Block, CONSTRUCTOR)?;
            body
        }
        None => super::empty_block(),
    };
    fields.push(field("body", body));
    Ok(Value::rakuast(Box::new(RakuAstNode { class, fields })))
}

fn field(name: &'static str, value: Value) -> RakuAstField {
    RakuAstField {
        name: Some(name),
        value: RakuAstFieldValue::Node(value),
    }
}
