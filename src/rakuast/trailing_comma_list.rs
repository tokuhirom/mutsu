//! A hash composer or `%(...)` holding one entry and a trailing comma
//! (`{:k,}`, `{a => 1,}`, `%(:k,)`) (#18444).
//!
//! The parser keeps a hash literal's entries as a list of pairs and does not
//! record a trailing comma, so the view gives a lone entry by itself. Rakudo
//! parses `:k,` as a comma list of one: `HashComposer(ApplyListInfix(",",
//! (ColonPair::True("k"),)))`. Once the composer is placed, its source says
//! whether the comma was written, and the entry is put back inside the list.

use super::view::{node_field, plain_infix};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::Value;

/// Wrap `contents`, a lone entry of a composer whose text between the
/// brackets is `inner`, in the comma list a trailing comma makes of it.
/// Whether it did.
// Cost: O(n), n = length of `inner`.
pub(super) fn wrap(contents: &mut RakuAstNode, inner: &str) -> bool {
    if contents.class == RakuAstClass::ApplyListInfix || !inner.trim_end().ends_with(',') {
        return false;
    }
    let entry = std::mem::replace(
        contents,
        RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::ApplyListInfix,
            fields: Vec::new(),
        },
    );
    contents.fields = vec![
        node_field(Some("infix"), plain_infix(",")),
        RakuAstField {
            name: Some("operands"),
            value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(entry))]),
        },
    ];
    true
}
