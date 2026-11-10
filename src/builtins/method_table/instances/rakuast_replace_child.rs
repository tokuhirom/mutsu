//! `RakuAST::Node.IMPL-REPLACE-CHILD($old, $new)`: the child of the node that
//! is `$old` (the same node, not an equal one) becomes `$new`, in place, in the
//! node every holder shares. A CHECK-phase rewrite of `$*CU` (Qwiratry's
//! `BodyRewrite`) swaps nodes this way.

use super::super::{Handler, MethodRow, RowFlags};
use crate::rakuast::{RakuAstFieldValue, RakuAstNode};
use crate::value::{RuntimeError, Value, ValueView};

// Cost: O(1).
fn same_node(a: &Value, b: &Value) -> bool {
    matches!((a.view(), b.view()), (ValueView::RakuAst(x), ValueView::RakuAst(y)) if std::ptr::eq(x, y))
}

// Cost: O(f + l), f = fields of the node, l = the items of its list fields.
fn replace_child(
    node: &mut RakuAstNode,
    old: &Value,
    new: &Value,
) -> Result<Value, RuntimeError> {
    if !matches!(new.view(), ValueView::RakuAst(_)) {
        return Err(RuntimeError::new(
            "RakuAST::Node.IMPL-REPLACE-CHILD expects a RakuAST node",
        ));
    }
    for field in &mut node.fields {
        match &mut field.value {
            RakuAstFieldValue::Node(value) if same_node(value, old) => {
                *value = new.clone();
                return Ok(new.clone());
            }
            RakuAstFieldValue::List(items) => {
                if let Some(slot) = items.iter_mut().find(|item| same_node(item, old)) {
                    *slot = new.clone();
                    return Ok(new.clone());
                }
            }
            _ => {}
        }
    }
    Ok(Value::NIL)
}

// Cost: O(f + l), see `replace_child`.
fn handler(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target.rakuast_edit(|node| replace_child(node, &args[0], &args[1]))
}

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "RakuAST::Node",
    name: "IMPL-REPLACE-CHILD",
    arity: 2,
    handler: Handler::Narrow(handler),
    flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
    named: &[],
}];
