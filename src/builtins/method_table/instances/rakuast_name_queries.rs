//! `RakuAST::Name`'s questions about its parts (`is-identifier`,
//! `is-multi-part`, `first-part`, ...). The answers are computed by
//! [`crate::rakuast::name_queries`]; this file only registers the rows.

use super::super::{Handler, MethodRow, RowFlags};
use super::rakuast_visit::native_node;
use crate::value::{RuntimeError, Value, ValueView};

/// A row and its handler for one query.
macro_rules! name_queries {
    ($($method:literal => $handler:ident),* $(,)?) => {
        $(
            // Cost: O(p), p = parts of the name.
            fn $handler(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
                let answer = native_node(target).and_then(|base| match base.view() {
                    ValueView::RakuAst(node) => crate::rakuast::name_queries::call(node, $method),
                    _ => None,
                });
                Ok(answer.unwrap_or(Value::NIL))
            }
        )*

        pub(super) static ROWS: &[MethodRow] = &[
            $(MethodRow {
                owner: "RakuAST::Name",
                name: $method,
                arity: 0,
                handler: Handler::Pure($handler),
                flags: RowFlags::OWNER_ONLY,
                named: &[],
            },)*
            MethodRow {
                owner: "RakuAST::Name",
                name: "qualified-with",
                arity: 1,
                handler: Handler::Narrow(qualified_with),
                // The qualifier is a node, not a plain scalar.
                flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
                named: &[],
            },
        ];
    };
}

// Cost: O(p + q), p = parts of the name, q = parts of the qualifier.
fn qualified_with(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let node_of = |value: &Value| {
        native_node(value).and_then(|base| match base.view() {
            ValueView::RakuAst(node) => Some(node.clone()),
            _ => None,
        })
    };
    let (name, qualifier) = (node_of(target)?, args.first().and_then(node_of));
    Some(match qualifier {
        Some(qualifier) => Ok(crate::rakuast::name_queries::qualified_with(
            &name, &qualifier,
        )),
        None => Err(RuntimeError::new(
            "Type check failed in binding to parameter '$qualifier'; expected RakuAST::Name",
        )),
    })
}
