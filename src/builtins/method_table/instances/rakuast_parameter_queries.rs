//! `RakuAST::Parameter`'s `named`, `is-positional`, `is-optional` and
//! `multi-invocant`, answered by [`crate::rakuast::parameter_queries`]; this
//! file only registers the rows.

use super::super::{Handler, MethodRow, RowFlags};
use super::rakuast_visit::native_node;
use crate::value::{RuntimeError, Value, ValueView};

/// A row and its handler for one query.
macro_rules! parameter_queries {
    ($($method:literal => $handler:ident => $query:ident),* $(,)?) => {
        $(
            // Cost: O(f), f = fields of the parameter.
            fn $handler(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
                let answer = native_node(target).and_then(|base| match base.view() {
                    ValueView::RakuAst(node) => {
                        Some(crate::rakuast::parameter_queries::$query(node))
                    }
                    _ => None,
                });
                Ok(answer.unwrap_or(Value::NIL))
            }
        )*

        pub(super) static ROWS: &[MethodRow] = &[
            $(MethodRow {
                owner: "RakuAST::Parameter",
                name: $method,
                arity: 0,
                handler: Handler::Pure($handler),
                flags: RowFlags::OWNER_ONLY,
                named: &[],
            },)*
        ];
    };
}

parameter_queries! {
    "named" => named => named,
    "is-positional" => is_positional => is_positional,
    "is-optional" => is_optional => is_optional,
    "multi-invocant" => multi_invocant => multi_invocant,
}
