//! `CompUnit.finish-content`: the text after the `=finish` line of the source
//! a compilation unit was read from (rakudo's `$=finish` data, which its
//! `RakuAST::Deparse::Highlight` prints after the code), or the `Mu` type
//! object when the source has none.
//!
//! Kept in the hidden field [`FIELD`], which no walk, render or lowering reads.

use super::{RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::Value;

const FIELD: &str = "finish-content";

/// Whether `field` is the hidden `=finish` text.
// Cost: O(1).
pub(super) fn is_hidden(_node: &RakuAstNode, field: &RakuAstField) -> bool {
    field.name == Some(FIELD)
}

/// Record the `=finish` text of `source`, if it has any.
// Cost: O(n), n = length of the source.
pub(super) fn record(fields: &mut Vec<RakuAstField>, source: &str) {
    if let (_, Some(content)) = crate::parser::split_finish_section(source) {
        fields.push(RakuAstField {
            name: Some(FIELD),
            value: RakuAstFieldValue::Node(Value::str(content)),
        });
    }
}

/// `finish-content`.
// Cost: O(f), f = fields of the node.
pub(crate) fn read(node: &RakuAstNode) -> Value {
    node.fields
        .iter()
        .find_map(|f| match (&f.name, &f.value) {
            (Some(FIELD), RakuAstFieldValue::Node(v)) => Some(v.clone()),
            _ => None,
        })
        .unwrap_or_else(|| Value::package(crate::symbol::Symbol::intern("Mu")))
}
