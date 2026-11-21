//! The source text of a regex code block across the RakuAST boundary.
//!
//! A regex is executed from its pattern *text*: a token's pattern is spliced
//! into the patterns that call it, and the engine parses the text again, code
//! blocks included. The tree a code block lowers to (`Regex::Block` over a
//! `Block`) holds only the statements, and mutsu has no deparser to turn them
//! back into text, so a declaration lowered from a tree would run its blocks as
//! `{}`. Rakudo keeps an `origin` on every node, which is how it can name the
//! source a node was parsed from; the view therefore keeps the block's
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
                | RakuAstClass::RegexInterpolation
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

/// The source of the code node `node` (see [`source_of`]) as the engine runs
/// it: the regex engine parses a code block's text again when it matches, so
/// `$?LINE` in it would read the line within that text. The view keeps the
/// written `$?LINE` (`Var::Compiler::Line`, #18291), whose value is the line it
/// was written on; the first one found in the block fixes the block's first
/// line, and every `$?LINE` in the text is folded to its line, as the parser
/// folds an anonymous regex body (`fold_line_in_code_blocks`).
// Cost: O(n + |code|), n = nodes in the block.
pub(super) fn execution_source_of(node: &RakuAstNode) -> String {
    let code = source_of(node);
    let Some(first) = first_compiler_line(node) else {
        return code;
    };
    let Some(at) = code.find("$?LINE") else {
        return code;
    };
    let first_line = first - code[..at].matches('\n').count() as i64;
    let folded = crate::parser::fold_line_in_code_blocks(&format!("{{{code}}}"), first_line);
    folded
        .strip_prefix('{')
        .and_then(|s| s.strip_suffix('}'))
        .map_or(code, str::to_string)
}

/// The line the first `Var::Compiler::Line` under `node` holds, in source
/// order.
// Cost: O(n), n = nodes under `node`.
fn first_compiler_line(node: &RakuAstNode) -> Option<i64> {
    if node.class == RakuAstClass::VarCompilerLine {
        return match node.fields.first().map(|f| &f.value) {
            Some(RakuAstFieldValue::Node(v)) => match v.view() {
                ValueView::Int(line) => Some(line),
                _ => None,
            },
            _ => None,
        };
    }
    node.fields.iter().find_map(|f| match &f.value {
        RakuAstFieldValue::Node(v) => {
            super::lower::rakuast_node_of(v).and_then(first_compiler_line)
        }
        RakuAstFieldValue::List(items) => items
            .iter()
            .filter_map(super::lower::rakuast_node_of)
            .find_map(first_compiler_line),
        RakuAstFieldValue::Adverb(_) => None,
    })
}
