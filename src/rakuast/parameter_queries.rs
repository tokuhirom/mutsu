//! The questions rakudo 2026.09's `RakuAST::Parameter` answers about itself
//! (`src/Raku/ast/parameter.rakumod`): `named`, `is-positional`,
//! `is-optional` and `multi-invocant`, all Bools. The rules were measured on
//! rakudo over scalars, optionals, defaults, named parameters (with `?`, `!`,
//! `is required`, a default), slurpies, a capture, a sigilless parameter and a
//! parameter after `;;`.

use super::{RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

// Cost: O(f), f = fields of the parameter.
fn field<'a>(node: &'a RakuAstNode, name: &str) -> Option<&'a RakuAstFieldValue> {
    node.fields
        .iter()
        .find(|f| f.name == Some(name))
        .map(|f| &f.value)
}

// Cost: O(f), f = fields of the parameter.
fn has_names(node: &RakuAstNode) -> bool {
    match field(node, "names") {
        Some(RakuAstFieldValue::List(names)) => !names.is_empty(),
        Some(RakuAstFieldValue::Node(_)) => true,
        _ => false,
    }
}

// Cost: O(f), f = fields of the parameter.
fn is_slurpy(node: &RakuAstNode) -> bool {
    field(node, "slurpy").is_some()
}

/// `named`: written with a name (`:$x`); a slurpy hash is not.
// Cost: O(f), f = fields of the parameter.
pub(crate) fn named(node: &RakuAstNode) -> Value {
    Value::truth(has_names(node))
}

/// `is-positional`: neither named nor slurpy.
// Cost: O(f), f = fields of the parameter.
pub(crate) fn is_positional(node: &RakuAstNode) -> Value {
    Value::truth(!has_names(node) && !is_slurpy(node))
}

/// `is-optional`: an explicit `optional` wins; otherwise a slurpy is not, a
/// named parameter is, and a positional one is when it has a default.
// Cost: O(f), f = fields of the parameter.
pub(crate) fn is_optional(node: &RakuAstNode) -> Value {
    if let Some(RakuAstFieldValue::Node(v)) = field(node, "optional")
        && let ValueView::Bool(b) = v.view()
    {
        return Value::truth(b);
    }
    if is_slurpy(node) {
        return Value::truth(false);
    }
    Value::truth(has_names(node) || field(node, "default").is_some())
}

/// `multi-invocant`: true unless the parameter was written after `;;`.
// Cost: O(f), f = fields of the parameter.
pub(crate) fn multi_invocant(node: &RakuAstNode) -> Value {
    if let Some(RakuAstFieldValue::Node(v)) = field(node, "multi-invocant")
        && let ValueView::Bool(b) = v.view()
    {
        return Value::truth(b);
    }
    Value::truth(true)
}
