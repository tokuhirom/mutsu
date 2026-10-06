//! The source text of a regex code block across the RakuAST boundary.
//!
//! A regex is executed from its pattern *text*: a token's pattern is spliced
//! into the patterns that call it, and the engine parses the text again, code
//! blocks included. The tree a code block lowers to (`Regex::Block` over a
//! `Block`) holds only the statements, and mutsu has no deparser to turn them
//! back into text, so a declaration lowered from a tree would run its blocks as
//! `{}`. Rakudo keeps an `origin` on every node, which is how it can name the
//! source a node was parsed from; the converter therefore keeps the block's
//! spelling in a hidden `source` field, the way a statement keeps its line
//! (`origin`) and a named capture its sigil (`array`), and `lower` puts it back.
//! A hand-built node has none, so its block lowers to an empty spelling as it
//! always did. A regex statement (`:my $x = 1;`) is kept the same way.
//!
//! The field is part of the model but not of the constructor form: Rakudo's
//! `.raku` shows no source text either.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// The hidden field's name.
const FIELD: &str = "source";

/// The `source` field for a code block spelled `code`.
// Cost: O(|code|).
pub(super) fn source_field(code: &str) -> RakuAstField {
    RakuAstField {
        name: Some(FIELD),
        value: RakuAstFieldValue::Node(Value::str(code.to_string())),
    }
}

/// Whether `field` is the hidden source of a regex code block `node`.
// Cost: O(1).
pub(super) fn is_source(node: &RakuAstNode, field: &RakuAstField) -> bool {
    field.name == Some(FIELD)
        && matches!(
            node.class,
            RakuAstClass::RegexBlock
                | RakuAstClass::RegexAssertionPredicateBlock
                | RakuAstClass::RegexAssertionInterpolatedBlock
                | RakuAstClass::RegexStatement
                | RakuAstClass::RegexQuantifierBlockRange
                | RakuAstClass::RegexQuote
        )
}

/// The spelling `node` was converted from, or an empty one for a hand-built
/// node.
// Cost: O(f + |code|), f = fields of `node`.
pub(super) fn source_of(node: &RakuAstNode) -> String {
    node.fields
        .iter()
        .find(|f| is_source(node, f))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::Node(v) => match v.view() {
                ValueView::Str(s) => Some(s.to_string()),
                _ => None,
            },
            _ => None,
        })
        .unwrap_or_default()
}
