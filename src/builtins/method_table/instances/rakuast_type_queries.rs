//! `RakuAST::Type::*`'s introspection rows (`is-native`, `is-coercive`,
//! `Definedness.name`, `Capture.definite`). The answers are computed by
//! [`crate::rakuast::type_queries`]; this file only registers the rows.

use super::super::{Handler, MethodRow, RowFlags};
use super::rakuast_visit::native_node;
use crate::value::{RuntimeError, Value, ValueView};

/// A row and its handler for one type query.
macro_rules! type_queries {
    ($($owner:literal, $method:literal => $handler:ident),* $(,)?) => {
        $(
            // Cost: O(f), f = fields of the type node.
            fn $handler(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
                let answer = native_node(target).and_then(|base| match base.view() {
                    ValueView::RakuAst(node) => {
                        crate::rakuast::type_queries::call(node, $method)
                    }
                    _ => None,
                });
                Ok(answer.unwrap_or(Value::NIL))
            }
        )*

        pub(super) static ROWS: &[MethodRow] = &[
            $(MethodRow {
                owner: $owner,
                name: $method,
                arity: 0,
                handler: Handler::Pure($handler),
                flags: RowFlags::OWNER_ONLY,
                named: &[],
            },)*
        ];
    };
}

type_queries! {
    "RakuAST::Type::Simple", "is-native" => simple_is_native,
    "RakuAST::Type::Setting", "is-native" => setting_is_native,
    "RakuAST::Type::Simple", "is-coercive" => simple_is_coercive,
    "RakuAST::Type::Setting", "is-coercive" => setting_is_coercive,
    // Declared where rakudo declares them: `Type::Derived` (Definedness,
    // Parameterized) and `Type` itself (Capture).
    "RakuAST::Type::Derived", "is-coercive" => derived_is_coercive,
    "RakuAST::Type::Subset", "is-coercive" => subset_is_coercive,
    "RakuAST::Type::Coercion", "is-coercive" => coercion_is_coercive,
    "RakuAST::Type", "is-coercive" => type_is_coercive,
    "RakuAST::Type::Definedness", "name" => definedness_name,
    "RakuAST::Type::Capture", "definite" => capture_definite,
}
