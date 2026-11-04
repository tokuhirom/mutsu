//! The questions rakudo 2026.09's `RakuAST::ArgList` answers about its
//! arguments (`src/Raku/ast/call.rakumod`): `arity` (an Int), `has-args` (a
//! Bool) and `arg-at-pos`, whose index counts from the end when negative and
//! which answers the `Mu` type object past either end.

use super::{RakuAstNode, node_accessor};
use crate::value::Value;

// Cost: O(a), a = arguments of the list.
fn args(node: &RakuAstNode) -> Vec<Value> {
    node_accessor(node, "args")
        .and_then(|v| v.as_list_items().map(<[Value]>::to_vec))
        .unwrap_or_default()
}

/// `arity`: the number of arguments.
// Cost: O(a), a = arguments of the list.
pub(crate) fn arity(node: &RakuAstNode) -> Value {
    Value::int(i64::try_from(args(node).len()).unwrap_or(i64::MAX))
}

/// `has-args`: whether there is at least one argument.
// Cost: O(a), a = arguments of the list.
pub(crate) fn has_args(node: &RakuAstNode) -> Value {
    Value::truth(!args(node).is_empty())
}

/// `arg-at-pos(index)`: the argument at `index`.
// Cost: O(a), a = arguments of the list.
pub(crate) fn arg_at_pos(node: &RakuAstNode, index: i64) -> Value {
    let args = args(node);
    let at = if index < 0 {
        i64::try_from(args.len()).ok().map(|len| len + index)
    } else {
        Some(index)
    };
    at.and_then(|i| usize::try_from(i).ok())
        .and_then(|i| args.get(i).cloned())
        .unwrap_or_else(|| Value::package(crate::symbol::Symbol::intern("Mu")))
}
