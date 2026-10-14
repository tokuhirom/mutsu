//! Class source nodes provide the enclosing scope for constructed attributes.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use super::{named_arg, rakuast_node_list, require_rakuast_class};
use crate::value::{RuntimeError, Value};

const CONSTRUCTOR: &str = "RakuAST::Class.new";

// Cost: O(a + t + s), a = arguments, t = traits, s = stringified scope length.
pub(super) fn construct(args: &[Value]) -> Result<Value, RuntimeError> {
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
        None => Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::Block,
            fields: vec![field(
                "body",
                Value::rakuast(Box::new(RakuAstNode {
                    class: RakuAstClass::Blockoid,
                    fields: vec![RakuAstField {
                        name: None,
                        value: RakuAstFieldValue::Node(Value::rakuast(Box::new(RakuAstNode {
                            class: RakuAstClass::StatementList,
                            fields: Vec::new(),
                        }))),
                    }],
                })),
            )],
        })),
    };
    fields.push(field("body", body));
    Ok(Value::rakuast(Box::new(RakuAstNode {
        class: RakuAstClass::Class,
        fields,
    })))
}

fn field(name: &'static str, value: Value) -> RakuAstField {
    RakuAstField {
        name: Some(name),
        value: RakuAstFieldValue::Node(value),
    }
}
