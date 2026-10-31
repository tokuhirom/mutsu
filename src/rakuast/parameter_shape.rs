//! RakuAST's synthesized predicate block for shaped array parameters.

use crate::scope::NameScope;
use super::convert::{
    convert_expr, leaf_field, name_from_identifier, node_field, plain_infix, statement_expression,
    unsupported,
};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, ParamDef};
use crate::value::{RuntimeError, Value, ValueView};

/// Build Rakudo's `array-shape` predicate block from the parser's dimensions.
// Cost: O(d), d = number of dimensions and their expression trees.
pub(super) fn convert(names: &NameScope, pd: &ParamDef) -> Result<Option<RakuAstNode>, RuntimeError> {
    let Some(dimensions) = &pd.shape_constraints else {
        return Ok(None);
    };
    let dims = if dimensions.len() == 1 {
        convert_expr(names, &dimensions[0])?
    } else {
        let mut operands = Vec::with_capacity(dimensions.len());
        for dim in dimensions {
            operands.push(Value::rakuast(Box::new(convert_expr(names, dim)?)));
        }
        RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::ApplyListInfix,
            fields: vec![
                node_field(Some("infix"), plain_infix(",")),
                RakuAstField {
                    name: Some("operands"),
                    value: RakuAstFieldValue::List(operands),
                },
            ],
        }
    };
    let list = apply_method(dims, "list", None);
    let topic_shape = apply_method(
        RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::VarLexical,
            fields: vec![leaf_field(Some("name"), Value::str("$_".to_string()))],
        },
        "shape",
        None,
    );
    let accepts = apply_method(list, "ACCEPTS", Some(topic_shape));
    let predicate = apply_method(accepts, "Bool", None);
    let statement = statement_expression(predicate);
    let statements = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::StatementList,
        fields: vec![node_field(None, statement)],
    };
    let body = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Blockoid,
        fields: vec![node_field(None, statements)],
    };
    Ok(Some(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Block,
        fields: vec![node_field(Some("body"), body)],
    }))
}

fn apply_method(operand: RakuAstNode, name: &str, arg: Option<RakuAstNode>) -> RakuAstNode {
    let mut args = Vec::new();
    if let Some(arg) = arg {
        args.push(node_field(None, arg));
    }
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyPostfix,
        fields: vec![
            node_field(Some("operand"), operand),
            node_field(
                Some("postfix"),
                RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::CallMethod,
                    fields: vec![
                        node_field(Some("name"), name_from_identifier(name)),
                        node_field(
                            Some("args"),
                            RakuAstNode {
                                origin: Default::default(),
                                class: RakuAstClass::ArgList,
                                fields: args,
                            },
                        ),
                    ],
                },
            ),
        ],
    }
}

/// Decode the generated `.list.ACCEPTS($_.shape).Bool` predicate back to dimensions.
// Cost: O(d), d = number of dimensions and their expression trees.
pub(super) fn lower(names: &NameScope, block: &RakuAstNode) -> Result<Vec<Expr>, RuntimeError> {
    let blockoid = named(block, "body")?;
    let statement_list = nested_value(
        &blockoid
            .fields
            .first()
            .ok_or_else(|| unsupported("array-shape block body"))?
            .value,
        "blockoid body",
    )?;
    let statement = statement_list
        .fields
        .first()
        .ok_or_else(|| unsupported("array-shape block"))?;
    let expression = named(nested_value(&statement.value, "statement")?, "expression")?;
    let accepts = method_operand(expression, "Bool")?;
    let list = method_operand(accepts, "ACCEPTS")?;
    let args = named(named(accepts, "postfix")?, "args")?;
    let topic_shape = args
        .fields
        .first()
        .ok_or_else(|| unsupported("array-shape ACCEPTS"))?;
    let topic_shape = nested_value(&topic_shape.value, "ACCEPTS argument")?;
    let topic = method_operand(topic_shape, "shape")?;
    if topic.class != RakuAstClass::VarLexical || string_field(topic, "name")? != "$_" {
        return Err(unsupported("array-shape topic"));
    }
    if !method_name(list, "list")? {
        return Err(unsupported("array-shape list"));
    }
    let dimensions = named(list, "operand")?;
    if dimensions.class == RakuAstClass::ApplyListInfix {
        let infix = named(dimensions, "infix")?;
        let ValueView::Str(op) = positional(infix)?.view() else {
            return Err(unsupported("array-shape comma"));
        };
        if op.as_str() != "," {
            return Err(unsupported("array-shape comma"));
        }
        let field = dimensions
            .fields
            .iter()
            .find(|f| f.name == Some("operands"))
            .ok_or_else(|| unsupported("array-shape operands"))?;
        let RakuAstFieldValue::List(values) = &field.value else {
            return Err(unsupported("array-shape operands"));
        };
        return values
            .iter()
            .map(|v| lower_expr(nested_value_field(v)?))
            .collect();
    }
    Ok(vec![lower_expr(names, dimensions)?])
}

fn lower_expr(names: &NameScope, node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    super::lower::lower_expr(names, node)
}
fn named<'a>(node: &'a RakuAstNode, name: &str) -> Result<&'a RakuAstNode, RuntimeError> {
    let field = node
        .fields
        .iter()
        .find(|f| f.name == Some(name))
        .ok_or_else(|| unsupported(&format!("array-shape field {name} on {:?}", node.class)))?;
    nested_value(&field.value, name)
}
fn nested_value<'a>(
    value: &'a RakuAstFieldValue,
    context: &str,
) -> Result<&'a RakuAstNode, RuntimeError> {
    let RakuAstFieldValue::Node(value) = value else {
        return Err(unsupported(&format!(
            "array-shape {context} is not a node field"
        )));
    };
    let ValueView::RakuAst(node) = value.view() else {
        return Err(unsupported(&format!(
            "array-shape {context} value is not RakuAST"
        )));
    };
    Ok(node)
}
fn nested_value_field(value: &Value) -> Result<&RakuAstNode, RuntimeError> {
    let ValueView::RakuAst(node) = value.view() else {
        return Err(unsupported("array-shape operand"));
    };
    Ok(node)
}
fn method_operand<'a>(
    node: &'a RakuAstNode,
    expected: &str,
) -> Result<&'a RakuAstNode, RuntimeError> {
    if node.class != RakuAstClass::ApplyPostfix || !method_name(node, expected)? {
        return Err(unsupported("array-shape method"));
    }
    named(node, "operand")
}
fn method_name(node: &RakuAstNode, expected: &str) -> Result<bool, RuntimeError> {
    let postfix = named(node, "postfix")?;
    if postfix.class != RakuAstClass::CallMethod {
        return Ok(false);
    }
    Ok(name(named(postfix, "name")?)? == expected)
}
fn name(node: &RakuAstNode) -> Result<String, RuntimeError> {
    if let Some(field) = node.fields.first()
        && field.name.is_none()
        && let RakuAstFieldValue::Node(value) = &field.value
        && let ValueView::Str(value) = value.view()
    {
        return Ok(value.to_string());
    }
    let parts = node
        .fields
        .iter()
        .find(|f| f.name == Some("parts"))
        .ok_or_else(|| unsupported("array-shape method name"))?;
    let RakuAstFieldValue::List(parts) = &parts.value else {
        return Err(unsupported("array-shape method name"));
    };
    let part = parts
        .first()
        .ok_or_else(|| unsupported("array-shape method name"))?;
    let ValueView::RakuAst(part) = part.view() else {
        return Err(unsupported("array-shape method name"));
    };
    let part_name = if part.class == RakuAstClass::NamePartSimple {
        named(part, "name")?
    } else {
        return Err(unsupported("array-shape method name"));
    };
    let value = positional(part_name)?;
    let ValueView::Str(value) = value.view() else {
        return Err(unsupported("array-shape method name"));
    };
    Ok(value.to_string())
}
fn positional(node: &RakuAstNode) -> Result<&Value, RuntimeError> {
    node.fields
        .first()
        .map(|f| match &f.value {
            RakuAstFieldValue::Node(v) => Ok(v),
            _ => Err(unsupported("array-shape leaf")),
        })
        .transpose()?
        .ok_or_else(|| unsupported("array-shape leaf"))
}
fn string_field(node: &RakuAstNode, field: &str) -> Result<String, RuntimeError> {
    let value = node
        .fields
        .iter()
        .find(|f| f.name == Some(field))
        .ok_or_else(|| unsupported("array-shape name"))?;
    let RakuAstFieldValue::Node(value) = &value.value else {
        return Err(unsupported("array-shape name"));
    };
    let ValueView::Str(s) = value.view() else {
        return Err(unsupported("array-shape name"));
    };
    Ok(s.to_string())
}
