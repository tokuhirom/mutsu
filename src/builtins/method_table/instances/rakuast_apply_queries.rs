//! `operator`, `operands` and `return-type` of the operator applications
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
apply_query!(return_type, return_type);

/// The three rows of one owner.
macro_rules! rows_of {
    ($($owner:literal),* $(,)?) => {
        pub(super) static ROWS: &[MethodRow] = &[
            $(
                MethodRow {
                    owner: $owner,
                    name: "operator",
                    arity: 0,
                    handler: Handler::Pure(operator),
                    flags: RowFlags::OWNER_ONLY,
                    named: &[],
                },
                MethodRow {
                    owner: $owner,
                    name: "operands",
                    arity: 0,
                    handler: Handler::Pure(operands),
                    flags: RowFlags::OWNER_ONLY,
                    named: &[],
                },
                MethodRow {
                    owner: $owner,
                    name: "return-type",
                    arity: 0,
                    handler: Handler::Pure(return_type),
                    flags: RowFlags::OWNER_ONLY,
                    named: &[],
                },
            )*
        ];
    };
}

rows_of!(
    "RakuAST::ApplyInfix",
    "RakuAST::ApplyPrefix",
    "RakuAST::ApplyPostfix",
    "RakuAST::ApplyListInfix",
);
