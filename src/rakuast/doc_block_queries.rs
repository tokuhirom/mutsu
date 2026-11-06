//! `RakuAST::Doc::Block.allowed-markup` and `.visual-table`, as rakudo 2026.09
//! answers them (measured with the bare `raku` over every block type):
//!
//! - `allowed-markup` is a type object, `RakuAST::Doc::Block::NOK` for the
//!   verbatim types `code`, `implicit-code` and `defn`, and
//!   `RakuAST::Doc::Block::OK` for every other type. Both are `Map`
//!   subclasses; `OK<X>` is `True` and `NOK<X>` is `False` for any key (the
//!   classes are declared by [`crate::runtime`]'s Doc::Block markup prelude,
//!   ADR-14916).
//! - `visual-table` is `True` for a `table` block holding a
//!   `RakuAST::Doc::LegacyRow` paragraph, `False` for any other `table`
//!   block, and the junction `any(False, False)` for a block of any other type.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::{JunctionKind, Value, ValueView};

fn type_of(node: &RakuAstNode) -> String {
    node.fields
        .iter()
        .find(|f| f.name == Some("type"))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::Node(v) => Some(v.to_string_value()),
            _ => None,
        })
        .unwrap_or_default()
}

/// The name of the type object `allowed-markup` answers for `node`.
// Cost: O(f), f = fields of the node.
pub(crate) fn allowed_markup(node: &RakuAstNode) -> Value {
    let verbatim = matches!(type_of(node).as_str(), "code" | "implicit-code" | "defn");
    Value::package(crate::symbol::Symbol::intern(if verbatim {
        "RakuAST::Doc::Block::NOK"
    } else {
        "RakuAST::Doc::Block::OK"
    }))
}

/// `visual-table`.
// Cost: O(f + p), f = fields of the node, p = paragraphs.
pub(crate) fn visual_table(node: &RakuAstNode) -> Value {
    if node.class != RakuAstClass::DocBlock || type_of(node) != "table" {
        return Value::junction(
            JunctionKind::Any,
            vec![Value::truth(false), Value::truth(false)],
        );
    }
    let rows = node
        .fields
        .iter()
        .find(|f| f.name == Some("paragraphs"))
        .is_some_and(|f| {
            match &f.value {
            RakuAstFieldValue::List(items) => items.iter().any(|p| {
                matches!(p.view(), ValueView::RakuAst(n) if n.class == RakuAstClass::DocLegacyRow)
            }),
            _ => false,
        }
        });
    Value::truth(rows)
}
