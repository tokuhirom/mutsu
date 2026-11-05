//! `return-type` of the literal nodes, as rakudo 2026.09 answers it
//! (`src/Raku/ast/literals.rakumod`): a `Literal` has the type of the value it
//! holds (`1` is `Int`, `1e0` `Num`, `1.5` `Rat`, `v1.2` `Version`), and a
//! `QuotedString` or a `QuoteWordsAtom` is a `Str`.

use super::{RakuAstNode, node_accessor};
use crate::value::Value;

/// `Literal.return-type`: the type object of the node's value.
// Cost: O(1).
pub(crate) fn literal_return_type(node: &RakuAstNode) -> Value {
    let name = node_accessor(node, "value")
        .map_or("Mu", |value| crate::value::type_name::value_type_name(&value));
    Value::package(crate::symbol::Symbol::intern(name))
}

/// `QuotedString.return-type` / `QuoteWordsAtom.return-type`: `Str`.
// Cost: O(1).
pub(crate) fn string_return_type() -> Value {
    Value::package(crate::symbol::Symbol::intern("Str"))
}
