//! `WHY` and `set-WHY` of `RakuAST::Doc::DeclaratorTarget`, the role every
//! documentable declaration does; answered by [`crate::rakuast::why_doc`],
//! this file only registers the rows.

use super::super::{Handler, MethodRow, RowFlags};
use super::rakuast_visit::native_node;
use crate::value::{RuntimeError, Value, ValueView};

// Cost: O(f), f = fields of the node.
fn why(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => Some(crate::rakuast::why_doc::why(node)),
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

// Cost: O(f), f = fields of the node.
fn set_why(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target.rakuast_edit(|node| crate::rakuast::why_doc::set_why(node, &args[0]))
}

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "RakuAST::Doc::DeclaratorTarget",
        name: "WHY",
        arity: 0,
        handler: Handler::Pure(why),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Doc::DeclaratorTarget",
        name: "set-WHY",
        arity: 1,
        handler: Handler::Narrow(set_why),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
];
