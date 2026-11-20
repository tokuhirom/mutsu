//! `is-pure` and `short-circuit` of the operator nodes, answered by
//! [`crate::rakuast::operator_purity`]; this file only registers the rows on
//! the owners rakudo declares them on.

use super::super::{Handler, MethodRow, RowFlags};
use super::rakuast_visit::native_node;
use crate::rakuast::RakuAstNode;
use crate::rakuast::operator_purity;
use crate::value::{RuntimeError, Value, ValueView};

fn answer(target: &Value, query: fn(&RakuAstNode) -> Option<bool>) -> Value {
    native_node(target)
        .and_then(|base| match base.view() {
            ValueView::RakuAst(node) => query(node),
            _ => None,
        })
        .map_or(Value::NIL, Value::truth)
}

// Cost: O(f), f = fields of the operator node.
fn is_pure(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(answer(target, operator_purity::is_pure))
}

// Cost: O(f), f = fields of the operator node.
fn short_circuit(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(answer(target, operator_purity::short_circuit))
}

// `Assignment.item`: the `:item` flag of an assignment node (`False` for a list
// assignment). A row of its own, so it wins over `Mu.item` (itemize).
// Cost: O(f), f = fields of the assignment node.
fn assignment_item(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let item = native_node(target).is_some_and(|base| match base.view() {
        ValueView::RakuAst(node) => node
            .fields
            .iter()
            .any(|f| matches!(f.value, crate::rakuast::RakuAstFieldValue::Adverb("item"))),
        _ => false,
    });
    Ok(Value::truth(item))
}

macro_rules! row {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Pure($handler),
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("RakuAST::Infixish", "is-pure", is_pure),
    row!("RakuAST::Infixish", "short-circuit", short_circuit),
    row!("RakuAST::Infix", "is-pure", is_pure),
    row!("RakuAST::Infix", "short-circuit", short_circuit),
    row!("RakuAST::MetaInfix", "is-pure", is_pure),
    row!("RakuAST::MetaInfix::Assign", "is-pure", is_pure),
    row!("RakuAST::Assignment", "is-pure", is_pure),
    row!("RakuAST::Assignment", "item", assignment_item),
    row!("RakuAST::Prefixish", "is-pure", is_pure),
    row!("RakuAST::Prefix", "is-pure", is_pure),
];
