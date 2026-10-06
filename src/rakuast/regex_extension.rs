//! The regex constructs of [`RegexExtension`] across the RakuAST boundary
//! (the shapes are in that type's documentation).
//!
//! A statement keeps its spelling in the hidden `source` field of
//! [`regex_code`](super::regex_code), as a code block does, because there is no
//! deparser to turn the statement back into the text the matcher reads.

use super::convert::{
    block_statements, leaf_field, node_field, quoted_string, regex_node, unsupported,
};
use super::lower::{lower_regex_node, lower_stmt, positional_leaf, rakuast_node_of};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::regex_tree::{RegexExtension, RegexNode};
use crate::value::{RuntimeError, Value, ValueView};

/// `extension` as its RakuAST node.
// Cost: O(n), n = size of the construct.
pub(super) fn convert(extension: &RegexExtension) -> Result<RakuAstNode, RuntimeError> {
    Ok(match extension {
        RegexExtension::Lookahead { negated, assertion } => {
            let mut fields = Vec::new();
            if *negated {
                fields.push(leaf_field(Some("negated"), Value::truth(true)));
            }
            fields.push(node_field(Some("assertion"), regex_node(assertion)?));
            RakuAstNode {
                class: RakuAstClass::RegexAssertionLookahead,
                fields,
            }
        }
        RegexExtension::Words(text) => {
            let mut quoted = quoted_string(Value::str(text.clone()));
            quoted.fields.insert(
                0,
                RakuAstField {
                    name: Some("processors"),
                    value: RakuAstFieldValue::List(vec![Value::str("words".to_string())]),
                },
            );
            RakuAstNode {
                class: RakuAstClass::RegexQuote,
                fields: vec![node_field(None, quoted)],
            }
        }
        RegexExtension::Tilde { goal, expr } => RakuAstNode {
            class: RakuAstClass::RegexNested,
            fields: vec![
                node_field(None, regex_node(goal)?),
                node_field(None, regex_node(expr)?),
            ],
        },
        RegexExtension::Statement { code, body } => {
            let mut statements = block_statements(body)?;
            let (Some(statement), None) = (statements.pop(), statements.pop()) else {
                return Err(unsupported("regex statement"));
            };
            RakuAstNode {
                class: RakuAstClass::RegexStatement,
                fields: vec![
                    node_field(None, statement),
                    super::regex_code::source_field(code),
                ],
            }
        }
        RegexExtension::BackReferenceNamed(name) => RakuAstNode {
            class: RakuAstClass::RegexBackReferenceNamed,
            fields: vec![leaf_field(None, Value::str(name.clone()))],
        },
        RegexExtension::BackReferencePositional(index) => RakuAstNode {
            class: RakuAstClass::RegexBackReferencePositional,
            fields: vec![leaf_field(None, Value::int(i64::from(*index)))],
        },
        RegexExtension::Recurse => RakuAstNode {
            class: RakuAstClass::RegexAssertionRecurse,
            fields: Vec::new(),
        },
    })
}

/// The node of `class` as a [`RegexNode`], or `None` when it is not one of
/// these constructs.
// Cost: O(n), n = size of the construct.
pub(super) fn lower(node: &RakuAstNode) -> Option<Result<RegexNode, RuntimeError>> {
    let extension = match node.class {
        RakuAstClass::RegexAssertionLookahead => return lower_lookahead(node),
        RakuAstClass::RegexQuote => return lower_words(node),
        RakuAstClass::RegexNested => lower_tilde(node),
        RakuAstClass::RegexStatement => lower_statement(node),
        RakuAstClass::RegexBackReferenceNamed => {
            positional_leaf(node).and_then(|name| match name.view() {
                ValueView::Str(name) => Ok(RegexExtension::BackReferenceNamed(name.to_string())),
                _ => Err(super::lower::unsupported(node)),
            })
        }
        RakuAstClass::RegexBackReferencePositional => {
            positional_leaf(node).and_then(|index| match index.view() {
                ValueView::Int(n) if n >= 0 => u32::try_from(n)
                    .map(RegexExtension::BackReferencePositional)
                    .map_err(|_| super::lower::unsupported(node)),
                _ => Err(super::lower::unsupported(node)),
            })
        }
        RakuAstClass::RegexAssertionRecurse => Ok(RegexExtension::Recurse),
        _ => return None,
    };
    Some(extension.map(RegexNode::Extension))
}

/// `< a b >`: a quote with the `words` processor.
fn lower_words(node: &RakuAstNode) -> Option<Result<RegexNode, RuntimeError>> {
    let quoted = super::lower::named_child_or_positional(node).ok()?;
    let processors = quoted
        .fields
        .iter()
        .find(|f| f.name == Some("processors"))?;
    let RakuAstFieldValue::List(items) = &processors.value else {
        return None;
    };
    if !matches!(items.as_slice(), [only] if matches!(only.view(), ValueView::Str(s) if s.as_str() == "words"))
    {
        return None;
    }
    Some(
        super::lower::list_field(quoted, "segments").and_then(|segments| {
            let [segment] = segments else {
                return Err(super::lower::unsupported(node));
            };
            let segment =
                rakuast_node_of(segment).ok_or_else(|| super::lower::unsupported(node))?;
            match positional_leaf(segment)?.view() {
                ValueView::Str(text) => Ok(RegexNode::Extension(RegexExtension::Words(
                    text.to_string(),
                ))),
                _ => Err(super::lower::unsupported(node)),
            }
        }),
    )
}

/// `<?name>` / `<!name>` / `<?[x]>`: a lookahead whose assertion is not the
/// `before` / `after` keyword form, which the tree models on its own.
fn lower_lookahead(node: &RakuAstNode) -> Option<Result<RegexNode, RuntimeError>> {
    let assertion = super::lower::named_child(node, "assertion").ok()?;
    let keyword_form = assertion.class == RakuAstClass::RegexAssertionNamedRegexArg
        || assertion.class == RakuAstClass::RegexAssertionInterpolatedVar;
    if keyword_form {
        return None;
    }
    Some(
        super::lower::bool_field(node, "negated").and_then(|negated| {
            Ok(RegexNode::Extension(RegexExtension::Lookahead {
                negated,
                assertion: Box::new(lower_regex_node(assertion)?),
            }))
        }),
    )
}

fn lower_tilde(node: &RakuAstNode) -> Result<RegexExtension, RuntimeError> {
    let [goal, expr] = node.fields.as_slice() else {
        return Err(super::lower::unsupported(node));
    };
    let child = |field: &RakuAstField| -> Result<Box<RegexNode>, RuntimeError> {
        let RakuAstFieldValue::Node(value) = &field.value else {
            return Err(super::lower::unsupported(node));
        };
        let child = rakuast_node_of(value).ok_or_else(|| super::lower::unsupported(node))?;
        lower_regex_node(child).map(Box::new)
    };
    Ok(RegexExtension::Tilde {
        goal: child(goal)?,
        expr: child(expr)?,
    })
}

fn lower_statement(node: &RakuAstNode) -> Result<RegexExtension, RuntimeError> {
    let statement = node
        .fields
        .iter()
        .find(|f| f.name.is_none())
        .and_then(|f| match &f.value {
            RakuAstFieldValue::Node(value) => rakuast_node_of(value),
            _ => None,
        })
        .ok_or_else(|| super::lower::unsupported(node))?;
    Ok(RegexExtension::Statement {
        code: super::regex_code::source_of(node),
        body: vec![lower_stmt(statement)?],
    })
}

/// The accessors of `Regex::Nested`, whose two positional children are
/// `goal` and `expr`.
// Cost: O(1).
pub(super) fn nested_accessor(node: &RakuAstNode, method: &str) -> Option<Value> {
    if node.class != RakuAstClass::RegexNested {
        return None;
    }
    let at = match method {
        "goal" => 0,
        "expr" => 1,
        _ => return None,
    };
    match &node.fields.get(at)?.value {
        RakuAstFieldValue::Node(value) => Some(value.clone()),
        _ => None,
    }
}
