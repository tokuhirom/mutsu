//! The questions rakudo 2026.09's `RakuAST::ArgList` answers about its
//! arguments (`src/Raku/ast/call.rakumod`): `arity` (an Int), `has-args` (a
//! Bool) and `arg-at-pos`, whose index counts from the end when negative and
//! which answers the `Mu` type object past either end.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode, node_accessor};
use crate::value::{RuntimeError, Value, ValueView};

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

/// `ArgList.from-comma-list($comma-apply)`: an `ArgList` over the operands of
/// an `ApplyListInfix`, whatever its infix is (rakudo only type-checks the
/// parameter). The class method of `src/Raku/ast/call.rakumod`.
// Cost: O(o), o = operands of the list infix.
pub(crate) fn from_comma_list(args: &[Value]) -> Result<Value, RuntimeError> {
    let [apply] = args else {
        return Err(RuntimeError::new(format!(
            "Too few positionals passed; expected 2 arguments but got {}",
            args.len() + 1
        )));
    };
    let operands = match apply.view() {
        ValueView::RakuAst(node) if node.class == RakuAstClass::ApplyListInfix => {
            node_accessor(node, "operands")
                .and_then(|v| v.as_list_items().map(<[Value]>::to_vec))
                .unwrap_or_default()
        }
        _ => {
            return Err(RuntimeError::new(format!(
                "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '$comma-apply'; expected \
                 RakuAST::ApplyListInfix but got {}",
                crate::value::type_name::value_type_name(apply)
            )));
        }
    };
    Ok(Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ArgList,
        fields: operands
            .into_iter()
            .map(|value| RakuAstField {
                name: None,
                value: RakuAstFieldValue::Node(value),
            })
            .collect(),
    })))
}
