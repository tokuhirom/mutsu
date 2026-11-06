//! `operator`, `operands`, `args` (infix only) and `return-type` of the operator applications
//! (`RakuAST::ApplyInfix`, `ApplyPrefix`, `ApplyPostfix`, `ApplyListInfix`),
//! answered by [`crate::rakuast::apply_queries`]; this file only registers
//! the rows.

use super::super::{Handler, MethodRow, RowFlags};
use super::rakuast_visit::native_node;
use crate::value::{RuntimeError, Value, ValueView};

/// A handler for one query.
macro_rules! apply_query {
    ($handler:ident, $query:ident) => {
        // Cost: O(f + a), f = fields of the application, a = operands.
        fn $handler(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
            let answer = native_node(target).and_then(|base| match base.view() {
                ValueView::RakuAst(node) => Some(crate::rakuast::apply_queries::$query(node)),
                _ => None,
            });
            Ok(answer.unwrap_or(Value::NIL))
        }
    };
}

apply_query!(operator, operator);
apply_query!(operands, operands);
apply_query!(args, args);
apply_query!(return_type, return_type);

/// One row.
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

// Only what rakudo declares on each owner: `return-type` on the infix and
// prefix applications (the postfix and list ones inherit it), and `operands`
// on a list infix is the accessor its `operands` field already has.
pub(super) static ROWS: &[MethodRow] = &[
    row!("RakuAST::ApplyInfix", "operator", operator),
    row!("RakuAST::ApplyInfix", "operands", operands),
    row!("RakuAST::ApplyInfix", "args", args),
    row!("RakuAST::ApplyInfix", "return-type", return_type),
    row!("RakuAST::ApplyPrefix", "operator", operator),
    row!("RakuAST::ApplyPrefix", "operands", operands),
    row!("RakuAST::ApplyPrefix", "return-type", return_type),
    row!("RakuAST::ApplyPostfix", "operator", operator),
    row!("RakuAST::ApplyPostfix", "operands", operands),
    row!("RakuAST::ApplyListInfix", "operator", operator),
];
