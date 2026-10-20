//! The shared variable/attribute declaration constructor retains source fields.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use super::{named_arg, rakuast_node_list, require_any_rakuast, require_rakuast_class};
use crate::value::{RuntimeError, Value, ValueView};

const CONSTRUCTOR: &str = "RakuAST::VarDeclaration::Simple.new";

// Cost: O(a + t + s), a = arguments, t = traits, s = stringified leaf length.
pub(super) fn construct(args: &[Value]) -> Result<Value, RuntimeError> {
    let sigil = named_arg(args, "sigil")
        .ok_or_else(|| RuntimeError::new(format!("{CONSTRUCTOR} requires a `sigil` argument")))?;
    let desigilname = named_arg(args, "desigilname").ok_or_else(|| {
        RuntimeError::new(format!("{CONSTRUCTOR} requires a `desigilname` argument"))
    })?;
    require_rakuast_class(&desigilname, RakuAstClass::Name, CONSTRUCTOR)?;
    let mut fields = Vec::new();
    for name in ["scope", "type", "shape"] {
        if let Some(value) = named_arg(args, name)
            && !value.is_nil()
        {
            match name {
                "scope" => {
                    let scope = value.to_string_value();
                    if !scope.is_empty() && scope != "my" {
                        fields.push(field(name, Value::str(scope)));
                    }
                    continue;
                }
                "type" => require_any_rakuast(&value, CONSTRUCTOR, name)?,
                "shape" => require_rakuast_class(&value, RakuAstClass::SemiList, CONSTRUCTOR)?,
                _ => {}
            }
            fields.push(field(name, value));
        }
    }
    fields.push(field("sigil", sigil));
    if let Some(twigil) = named_arg(args, "twigil")
        && twigil.truthy()
    {
        if !matches!(twigil.to_string_value().as_str(), "*" | "." | "!") {
            return Err(RuntimeError::new(format!(
                "{CONSTRUCTOR} does not support twigil '{}'",
                twigil.to_string_value()
            )));
        }
        fields.push(field("twigil", twigil));
    }
    fields.push(field("desigilname", desigilname));
    if let Some(traits) = named_arg(args, "traits")
        && !traits.is_nil()
    {
        fields.push(RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(rakuast_node_list(&traits, CONSTRUCTOR, "traits")?),
        });
    }
    push_initializer(&mut fields, args, CONSTRUCTOR)?;
    if let Some(constraint) = named_arg(args, "where")
        && !constraint.is_nil()
    {
        require_any_rakuast(&constraint, CONSTRUCTOR, "where")?;
        fields.push(field("where", constraint));
    }
    Ok(Value::rakuast(Box::new(RakuAstNode {
        class: RakuAstClass::VarDeclarationSimple,
        fields,
    })))
}

// Cost: O(a + s), a = arguments, s = scope spelling length.
pub(super) fn construct_anonymous(args: &[Value]) -> Result<Value, RuntimeError> {
    const CONSTRUCTOR: &str = "RakuAST::VarDeclaration::Anonymous.new";
    let mut fields = Vec::new();
    if let Some(scope) = named_arg(args, "scope") {
        let scope = scope.to_string_value();
        if !scope.is_empty() && scope != "my" {
            fields.push(field("scope", Value::str(scope)));
        }
    }
    let sigil = named_arg(args, "sigil")
        .ok_or_else(|| RuntimeError::new(format!("{CONSTRUCTOR} requires a `sigil` argument")))?;
    fields.push(field("sigil", sigil));
    if let Some(constraint) = named_arg(args, "type")
        && !constraint.is_nil()
    {
        require_any_rakuast(&constraint, CONSTRUCTOR, "type")?;
        fields.push(field("type", constraint));
    }
    push_initializer(&mut fields, args, CONSTRUCTOR)?;
    Ok(Value::rakuast(Box::new(RakuAstNode {
        class: RakuAstClass::VarDeclarationAnonymous,
        fields,
    })))
}

fn push_initializer(
    fields: &mut Vec<RakuAstField>,
    args: &[Value],
    constructor: &str,
) -> Result<(), RuntimeError> {
    if let Some(initializer) = named_arg(args, "initializer") {
        if !matches!(initializer.view(), ValueView::RakuAst(n) if matches!(n.class,
            RakuAstClass::InitializerAssign | RakuAstClass::InitializerBind | RakuAstClass::InitializerCallAssign))
        {
            return Err(RuntimeError::new(format!(
                "{constructor} expects a RakuAST::Initializer node"
            )));
        }
        fields.push(field("initializer", initializer));
    }
    Ok(())
}

fn field(name: &'static str, value: Value) -> RakuAstField {
    RakuAstField {
        name: Some(name),
        value: RakuAstFieldValue::Node(value),
    }
}
