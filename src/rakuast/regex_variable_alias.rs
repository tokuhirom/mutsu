//! `$name=ATOM` in a regex: an alias to a variable rather than to a capture.
//!
//! Rakudo 2026.09 parses the alias and then keeps only the interpolation of
//! `$name`: its `.AST` is `Regex::Interpolation(var => Var::Lexical("$name"))`
//! and the aliased atom is gone, from the tree and from the match. Mutsu's
//! matcher runs the pattern from its text, which keeps the atom, so dropping it
//! at the boundary would change what the round-tripped regex matches. The
//! converter therefore renders the same public node as rakudo and carries the
//! aliased atom in a hidden `source-alias` field (the way a code block keeps
//! its `source`); `lower` restores the alias from it. A hand-built
//! `Regex::Interpolation` has no such field and stays a plain interpolation.
//!
//! Whitespace written after the alias belongs to the aliased atom in rakudo's
//! grammar, so its `WithWhitespace` disappears with the atom: the public node
//! is the bare interpolation and the hidden atom carries the wrapper.

use super::convert::{leaf_field, node_field, regex_node};
use super::lower::{lower_regex_node, named_child};
use super::{RakuAstClass, RakuAstField, RakuAstNode};
use crate::regex_tree::{RegexExtension, RegexNode};
use crate::value::{RuntimeError, Value};

/// The hidden field's name.
const FIELD: &str = "source-alias";
/// The hidden whitespace written before / after the `=` (`$name = ATOM`).
const GAP_BEFORE: &str = "source-alias-gap-before";
const GAP_AFTER: &str = "source-alias-gap-after";

/// `$name=regex` as the node rakudo renders, with the aliased atom hidden.
// Cost: O(n), n = size of the aliased atom.
pub(super) fn convert(
    (name, sequential, array): (&str, bool, bool),
    gap: (&str, &str),
    regex: &RegexNode,
) -> Result<RakuAstNode, RuntimeError> {
    let (name, sequential) = (name.to_string(), sequential);
    let mut node = regex_node(&if array {
        RegexNode::ArrayInterpolation { name, sequential }
    } else {
        RegexNode::Interpolation { name, sequential }
    })?;
    node.fields
        .push(node_field(Some(FIELD), regex_node(regex)?));
    for (field, text) in [(GAP_BEFORE, gap.0), (GAP_AFTER, gap.1)] {
        if !text.is_empty() {
            node.fields
                .push(leaf_field(Some(field), Value::str(text.to_string())));
        }
    }
    Ok(node)
}

/// `WithWhitespace(alias)`: the wrapper moves onto the hidden atom.
// Cost: O(n), n = size of the aliased atom.
pub(super) fn convert_spaced(alias: &RegexNode) -> Result<RakuAstNode, RuntimeError> {
    let RegexNode::Extension(RegexExtension::VariableAlias {
        name,
        sequential,
        array,
        gap_before,
        gap_after,
        regex,
    }) = alias
    else {
        return Err(super::convert::unsupported("spaced regex variable alias"));
    };
    convert(
        (name, *sequential, *array),
        (gap_before, gap_after),
        &RegexNode::WithWhitespace(regex.clone()),
    )
}

/// Whether `field` is the hidden aliased atom of a regex interpolation.
// Cost: O(1).
pub(super) fn is_source_field(node: &RakuAstNode, field: &RakuAstField) -> bool {
    node.class == RakuAstClass::RegexInterpolation
        && matches!(field.name, Some(name) if name == FIELD || name == GAP_BEFORE || name == GAP_AFTER)
}

/// A `Regex::Interpolation` carrying an aliased atom, as the alias again;
/// `None` for any other node.
// Cost: O(n), n = size of the aliased atom.
pub(super) fn lower(node: &RakuAstNode) -> Option<Result<RegexNode, RuntimeError>> {
    if node.class != RakuAstClass::RegexInterpolation
        || !node.fields.iter().any(|field| field.name == Some(FIELD))
    {
        return None;
    }
    Some((|| {
        let regex = lower_regex_node(named_child(node, FIELD)?)?;
        let interpolation = RakuAstNode {
            class: node.class,
            fields: node
                .fields
                .iter()
                .filter(|field| !is_source_field(node, field))
                .cloned()
                .collect(),
        };
        let (name, sequential, array) = match lower_regex_node(&interpolation)? {
            RegexNode::Interpolation { name, sequential } => (name, sequential, false),
            RegexNode::ArrayInterpolation { name, sequential } => (name, sequential, true),
            _ => return Err(super::lower::unsupported(node)),
        };
        {
            {
                let (regex, spaced) = match regex {
                    RegexNode::WithWhitespace(regex) => (regex, true),
                    regex => (Box::new(regex), false),
                };
                let gap = |wanted| super::lower::leaf_str(node, wanted).unwrap_or_default();
                let alias = RegexNode::Extension(RegexExtension::VariableAlias {
                    name,
                    sequential,
                    array,
                    gap_before: gap(GAP_BEFORE),
                    gap_after: gap(GAP_AFTER),
                    regex,
                });
                Ok(if spaced {
                    RegexNode::WithWhitespace(Box::new(alias))
                } else {
                    alias
                })
            }
        }
    })())
}
