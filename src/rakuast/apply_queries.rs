//! The questions rakudo 2026.09's operator applications (`RakuAST::ApplyInfix`,
//! `ApplyPrefix`, `ApplyPostfix`, `ApplyListInfix`) answer about themselves
//! (`src/Raku/ast/operators.rakumod`): `operator` (the infix, prefix or postfix
//! node), `operands` (the left and right of an infix, the one operand of a
//! prefix or postfix, the arguments of a list infix, as a List) and
//! `return-type` (the `Mu` type object: none is declared).

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::Value;

// Cost: O(f), f = fields of the node.
fn field<'a>(node: &'a RakuAstNode, name: &str) -> Option<&'a RakuAstFieldValue> {
    node.fields
        .iter()
        .find(|f| f.name == Some(name))
        .map(|f| &f.value)
}

// Cost: O(f), f = fields of the node.
fn node_value(node: &RakuAstNode, name: &str) -> Option<Value> {
    match field(node, name)? {
        RakuAstFieldValue::Node(v) => Some(v.clone()),
        _ => None,
    }
}

/// `operator`: the `infix`, `prefix` or `postfix` node.
// Cost: O(f), f = fields of the node.
pub(crate) fn operator(node: &RakuAstNode) -> Value {
    ["infix", "prefix", "postfix"]
        .iter()
        .find_map(|name| node_value(node, name))
        .unwrap_or(Value::NIL)
}

/// `operands`: the operands, in the order they were written.
// Cost: O(f + a), f = fields of the node, a = operands.
pub(crate) fn operands(node: &RakuAstNode) -> Value {
    let items: Vec<Value> = if let Some(RakuAstFieldValue::List(args)) = field(node, "operands") {
        args.clone()
    } else if let Some(operand) = node_value(node, "operand") {
        vec![operand]
    } else {
        ["left", "right"]
            .iter()
            .filter_map(|name| node_value(node, name))
            .collect()
    };
    Value::array(items)
}

/// `args` (`ApplyInfix` only): the operands as a `RakuAST::ArgList`.
// Cost: O(f), f = fields of the node.
pub(crate) fn args(node: &RakuAstNode) -> Value {
    let fields = ["left", "right"]
        .iter()
        .filter_map(|name| node_value(node, name))
        .map(|value| RakuAstField {
            name: None,
            value: RakuAstFieldValue::Node(value),
        })
        .collect();
    Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ArgList,
        fields,
    }))
}

/// `return-type`: no type is declared, so `Mu`.
// Cost: O(1).
pub(crate) fn return_type(_node: &RakuAstNode) -> Value {
    Value::package(crate::symbol::Symbol::intern("Mu"))
}
