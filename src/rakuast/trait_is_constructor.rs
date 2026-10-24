//! `RakuAST::Trait::Is.new(name => ..., type => ..., argument => ...)`.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use super::{named_arg, require_any_rakuast, require_rakuast_class};
use crate::value::{RuntimeError, Value};

const CONSTRUCTOR: &str = "RakuAST::Trait::Is.new";

// Cost: O(a), a = arguments.
pub(super) fn construct(args: &[Value]) -> Result<Value, RuntimeError> {
    let mut fields = Vec::new();
    if let Some(name) = named_arg(args, "name")
        && !name.is_nil()
    {
        require_rakuast_class(&name, RakuAstClass::Name, CONSTRUCTOR)?;
        fields.push(field("name", name));
    }
    // `class C is Parent` / `my @a is Buf`: the trait names a type.
    if let Some(type_node) = named_arg(args, "type")
        && !type_node.is_nil()
    {
        require_any_rakuast(&type_node, CONSTRUCTOR, "type")?;
        fields.push(field("type", type_node));
    }
    if let Some(argument) = named_arg(args, "argument")
        && !argument.is_nil()
    {
        require_any_rakuast(&argument, CONSTRUCTOR, "argument")?;
        fields.push(field("argument", argument));
    }
    Ok(Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::TraitIs,
        fields,
    })))
}

fn field(name: &'static str, value: Value) -> RakuAstField {
    RakuAstField {
        name: Some(name),
        value: RakuAstFieldValue::Node(value),
    }
}
