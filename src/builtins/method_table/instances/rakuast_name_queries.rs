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
        ];
    };
}

name_queries! {
    "is-identifier" => is_identifier,
    "is-simple" => is_simple,
    "is-multi-part" => is_multi_part,
    "is-global-lookup" => is_global_lookup,
    "is-package-lookup" => is_package_lookup,
    "is-pseudo-package" => is_pseudo_package,
    "is-installable" => is_installable,
    "has-colonpairs" => has_colonpairs,
    "first-part" => first_part,
    "last-part" => last_part,
    "root-part" => root_part,
    "simple-identifier" => simple_identifier,
    "base-name" => base_name,
    "without-colonpairs" => without_colonpairs,
    "without-first-part" => without_first_part,
}

// Cost: O(p + q), p, q = parts of the name and of the target.
fn qualified_with(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let base = native_node(target)?;
    let ValueView::RakuAst(node) = base.view() else {
        return None;
    };
    Some(crate::rakuast::name_queries::qualified_with(node, &args[0]))
}

pub(super) static ONE_ARG_ROWS: &[MethodRow] = &[MethodRow {
    owner: "RakuAST::Name",
    name: "qualified-with",
    arity: 1,
    handler: Handler::Narrow(qualified_with),
    flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
    named: &[],
}];
