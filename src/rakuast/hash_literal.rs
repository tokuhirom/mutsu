//! Hash literals across the RakuAST boundary.
//!
//! Measured against rakudo 2026.09, the two spellings of a literal-keyed hash
//! are different nodes:
//!
//! ```text
//! {a => 1, b => 2}  Circumfix::HashComposer(ApplyListInfix(",", FatArrow…))
//! {a => 1}          Circumfix::HashComposer(FatArrow)
//! {}                Circumfix::HashComposer()
//! %(a => 1, b => 2) Contextualizer::Hash(StatementSequence(
//!                       Statement::Expression(ApplyListInfix(",", FatArrow…))))
//! %()               Contextualizer::Hash(StatementSequence())
//! ```
//!
//! The parser builds both as one [`Expr::Hash`] and records which spelling the
//! source wrote as its [`HashSpelling`]; both directions read and restore it.

use super::convert::{convert_expr, leaf_field, node_field, plain_infix, statement_expression};
use super::lower::{lower_expr, named_child, named_child_or_positional, unsupported};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, HashSpelling};
use crate::token_kind::TokenKind;
use crate::value::{RuntimeError, Value, ValueView};

/// The node for a hash literal with literal keys.
// Cost: O(p), p = pairs of the literal (plus converting their values).
pub(super) fn convert(
    pairs: &[(String, Option<Expr>)],
    spelling: HashSpelling,
) -> Result<RakuAstNode, RuntimeError> {
    let contents = contents_node(pairs)?;
    Ok(match spelling {
        HashSpelling::Composer => RakuAstNode {
            class: RakuAstClass::CircumfixHashComposer,
            fields: contents.map(|c| node_field(None, c)).into_iter().collect(),
        },
        HashSpelling::Contextualizer => {
            let sequence = RakuAstNode {
                class: RakuAstClass::StatementSequence,
                fields: contents
                    .map(|c| node_field(None, statement_expression(c)))
                    .into_iter()
                    .collect(),
            };
            RakuAstNode {
                class: RakuAstClass::ContextualizerHash,
                fields: vec![node_field(None, sequence)],
            }
        }
    })
}

/// The pairs as one expression: nothing for an empty literal, the lone
/// `FatArrow`, or a comma list of them.
fn contents_node(pairs: &[(String, Option<Expr>)]) -> Result<Option<RakuAstNode>, RuntimeError> {
    let mut fatarrows = Vec::with_capacity(pairs.len());
    for (key, value) in pairs {
        let value = value.as_ref().ok_or_else(|| {
            RuntimeError::new(
                "RakuAST: `.AST` does not yet support this construct: value-less hash key",
            )
        })?;
        fatarrows.push(RakuAstNode {
            class: RakuAstClass::FatArrow,
            fields: vec![
                leaf_field(Some("key"), Value::str(key.clone())),
                node_field(Some("value"), convert_expr(value)?),
            ],
        });
    }
    if fatarrows.len() <= 1 {
        return Ok(fatarrows.pop());
    }
    let operands = fatarrows
        .into_iter()
        .map(|n| Value::rakuast(Box::new(n)))
        .collect();
    Ok(Some(RakuAstNode {
        class: RakuAstClass::ApplyListInfix,
        fields: vec![
            node_field(Some("infix"), plain_infix(",")),
            RakuAstField {
                name: Some("operands"),
                value: RakuAstFieldValue::List(operands),
            },
        ],
    }))
}

/// `Circumfix::HashComposer` -> a composer-spelled [`Expr::Hash`].
// Cost: O(p), p = pairs of the composer (plus lowering their values).
pub(super) fn lower_composer(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let pairs = match node.fields.as_slice() {
        [] => Vec::new(),
        [_] => lower_pairs(node, named_child_or_positional(node)?)?,
        _ => return Err(unsupported(node)),
    };
    Ok(Expr::Hash(pairs, HashSpelling::Composer))
}

/// `Contextualizer::Hash` -> a contextualizer-spelled [`Expr::Hash`].
// Cost: O(p), p = pairs of the contextualizer (plus lowering their values).
pub(super) fn lower_contextualizer(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let sequence = named_child_or_positional(node)?;
    if sequence.class != RakuAstClass::StatementSequence {
        return Err(unsupported(node));
    }
    let pairs = match sequence.fields.as_slice() {
        [] => Vec::new(),
        [_] => {
            let statement = named_child_or_positional(sequence)?;
            if statement.class != RakuAstClass::StatementExpression {
                return Err(unsupported(node));
            }
            lower_pairs(node, named_child(statement, "expression")?)?
        }
        // `%(a => 1; b => 2)`: more than one statement is a different hash
        // (the parser keeps no such literal), so it stays the boundary.
        _ => return Err(unsupported(node)),
    };
    Ok(Expr::Hash(pairs, HashSpelling::Contextualizer))
}

/// The literal-keyed pairs of a hash literal's contents. Anything else -- a
/// computed key, a slipped hash, a positional value -- is the `hash(…)` call
/// the parser builds for such a composer, which these nodes never carry, so
/// it is refused rather than lowered to a different hash.
fn lower_pairs(
    node: &RakuAstNode,
    contents: &RakuAstNode,
) -> Result<Vec<(String, Option<Expr>)>, RuntimeError> {
    let items = match lower_expr(contents)? {
        Expr::ArrayLiteral(items) if contents.class == RakuAstClass::ApplyListInfix => items,
        single => vec![single],
    };
    items
        .into_iter()
        .map(|item| literal_pair(item).ok_or_else(|| unsupported(node)))
        .collect()
}

/// A `key => value` pair with a literal string key.
fn literal_pair(item: Expr) -> Option<(String, Option<Expr>)> {
    let item = match item {
        Expr::PositionalPair(inner) => *inner,
        other => other,
    };
    let Expr::Binary {
        left,
        op: TokenKind::FatArrow,
        right,
    } = item
    else {
        return None;
    };
    let (Expr::Literal(key) | Expr::LiteralSrc(key, _)) = *left else {
        return None;
    };
    let ValueView::Str(key) = key.view() else {
        return None;
    };
    Some((key.to_string(), Some(*right)))
}
