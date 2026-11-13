//! `RakuAST::ApplyPostfix.set-operands(@operands)` and its `ApplyInfix` /
//! `ApplyPrefix` twins: the operator application's operands become the given
//! nodes, in place, in the node every holder shares. A CHECK-phase rewrite of
//! `$*CU` (Red's `red-compile`, #17501) wraps an operand this way.

use super::super::{Handler, MethodRow, RowFlags};
use crate::rakuast::{RakuAstClass, RakuAstField, RakuAstFieldValue};
use crate::value::{RuntimeError, Value, ValueView};

/// The operand fields of an operator application, in operand order.
// Cost: O(1).
fn operand_fields(class: RakuAstClass) -> Option<&'static [&'static str]> {
    match class {
        RakuAstClass::ApplyPostfix | RakuAstClass::ApplyPrefix => Some(&["operand"]),
        RakuAstClass::ApplyInfix => Some(&["left", "right"]),
        _ => None,
    }
}

// Cost: O(f + o), f = fields of the node, o = operands.
fn set_operands(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    let ValueView::RakuAst(node) = target.view() else {
        return Err(RuntimeError::new("set-operands needs a RakuAST node"));
    };
    let Some(fields) = operand_fields(node.class) else {
        return Err(RuntimeError::new(
            "set-operands is only available on an operator application",
        ));
    };
    let operands = args.first().map(crate::value::to_list::value_to_list).unwrap_or_default();
    if operands
        .iter()
        .any(|operand| !matches!(operand.view(), ValueView::RakuAst(_)))
    {
        return Err(RuntimeError::new("set-operands expects RakuAST nodes"));
    }
    let mut updated = crate::rakuast::RakuAstNode::clone(node);
    for (name, operand) in fields.iter().zip(operands.iter()) {
        let value = RakuAstFieldValue::Node(operand.clone());
        match updated.fields.iter_mut().find(|f| f.name == Some(*name)) {
            Some(slot) => slot.value = value,
            None => updated.fields.insert(
                0,
                RakuAstField {
                    name: Some(*name),
                    value,
                },
            ),
        }
    }
    target.replace_rakuast_node(updated);
    Ok(Value::NIL)
}

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "RakuAST::ApplyPostfix",
        name: "set-operands",
        arity: 1,
        handler: Handler::Pure(set_operands),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::ApplyInfix",
        name: "set-operands",
        arity: 1,
        handler: Handler::Pure(set_operands),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::ApplyPrefix",
        name: "set-operands",
        arity: 1,
        handler: Handler::Pure(set_operands),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
];
