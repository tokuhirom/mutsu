//! `return-type` of the literal nodes, as rakudo 2026.09 answers it
//! (`src/Raku/ast/literals.rakumod`): a `Literal` has the type of the value it
//! holds (`1` is `Int`, `1e0` `Num`, `1.5` `Rat`, `v1.2` `Version`), and a
//! `QuotedString` or a `QuoteWordsAtom` is a `Str`.

use super::{RakuAstNode, node_accessor};
use crate::value::Value;

/// `Literal.return-type`: the type object of the node's value.
// Cost: O(1).
pub(crate) fn literal_return_type(node: &RakuAstNode) -> Value {
    let name = node_accessor(node, "value").map_or("Mu", |value| {
        crate::value::type_name::value_type_name(&value)
    });
    Value::package(crate::symbol::Symbol::intern(name))
}

/// `QuotedString.return-type` / `QuoteWordsAtom.return-type`: `Str`.
// Cost: O(1).
pub(crate) fn string_return_type() -> Value {
    Value::package(crate::symbol::Symbol::intern("Str"))
}

/// `Literal.compile-time-value`: the value the literal holds.
// Cost: O(1).
pub(crate) fn literal_compile_time_value(node: &RakuAstNode) -> Value {
    node_accessor(node, "value").unwrap_or(Value::NIL)
}

/// `QuotedString.maybe-compile-time-value`: the string its segments spell when
/// every one is a `StrLiteral` (no interpolation, no processor), else `Nil`.
// Cost: O(s), s = total length of the segments.
pub(crate) fn quoted_string_maybe_compile_time_value(node: &RakuAstNode) -> Value {
    use super::RakuAstFieldValue;
    if node
        .fields
        .iter()
        .any(|f| matches!(f.name, Some("processors")))
    {
        return Value::NIL;
    }
    let Some(segments) = node.fields.iter().find(|f| f.name == Some("segments")) else {
        return Value::NIL;
    };
    let items: Vec<Value> = match &segments.value {
        RakuAstFieldValue::List(items) => items.clone(),
        RakuAstFieldValue::Node(v) => vec![v.clone()],
        RakuAstFieldValue::Adverb(_) => return Value::NIL,
    };
    let mut out = String::new();
    for item in &items {
        match item.view() {
            crate::value::ValueView::RakuAst(seg)
                if seg.class.printed_name() == "RakuAST::StrLiteral" =>
            {
                out.push_str(&literal_compile_time_value(seg).to_string_value());
            }
            _ => return Value::NIL,
        }
    }
    Value::str(out)
}
