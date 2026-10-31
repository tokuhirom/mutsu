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

use crate::scope::NameScope;
use super::convert::{convert_expr, leaf_field, node_field, plain_infix, statement_expression};
use super::lower::{lower_expr, named_child, named_child_or_positional, unsupported};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, HashSpelling};
use crate::token_kind::TokenKind;
use crate::value::{RuntimeError, Value, ValueView};

/// The node for a hash literal with literal keys.
// Cost: O(p), p = pairs of the literal (plus converting their values).
pub(super) fn convert(names: &NameScope, 
    pairs: &[(String, Option<Expr>)],
    spelling: HashSpelling,
) -> Result<RakuAstNode, RuntimeError> {
    let contents = contents_node(names, pairs)?;
    Ok(match spelling {
        HashSpelling::Composer => RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::CircumfixHashComposer,
            fields: contents.map(|c| node_field(None, c)).into_iter().collect(),
        },
        HashSpelling::Contextualizer => {
            let sequence = RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::StatementSequence,
                fields: contents
                    .map(|c| node_field(None, statement_expression(c)))
                    .into_iter()
                    .collect(),
            };
            RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::ContextualizerHash,
                fields: vec![node_field(None, sequence)],
            }
        }
    })
}

/// The pairs as one expression: nothing for an empty literal, the lone
/// `FatArrow`, or a comma list of them.
fn contents_node(names: &NameScope, pairs: &[(String, Option<Expr>)]) -> Result<Option<RakuAstNode>, RuntimeError> {
    let mut fatarrows = Vec::with_capacity(pairs.len());
    for (key, value) in pairs {
        // `{:x}`: the parser keeps a bare colonpair as a pair with no value,
        // which is `x => True`; rakudo spells it `ColonPair::True`.
        let Some(value) = value.as_ref() else {
            fatarrows.push(RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::ColonPairTrue,
                fields: vec![leaf_field(None, Value::str(key.clone()))],
            });
            continue;
        };
        let mut pair = RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::FatArrow,
            fields: vec![
                leaf_field(Some("key"), Value::str(key.clone())),
                node_field(Some("value"), convert_expr(names, value)?),
            ],
        };
        super::infix_origins::derive(names, &mut pair);
        fatarrows.push(pair);
    }
    if fatarrows.len() <= 1 {
        return Ok(fatarrows.pop());
    }
    let operands = fatarrows
        .into_iter()
        .map(|n| Value::rakuast(Box::new(n)))
        .collect();
    let mut list = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyListInfix,
        fields: vec![
            node_field(Some("infix"), plain_infix(",")),
            RakuAstField {
                name: Some("operands"),
                value: RakuAstFieldValue::List(operands),
            },
        ],
    };
    super::infix_origins::derive(names, &mut list);
    Ok(Some(list))
}

/// The typed node the parser builds for an object hash `:{ ... }`.
// Cost: O(1).
pub(crate) fn object_hash_expr(entries: Vec<Expr>) -> Expr {
    Expr::Ast(Box::new(crate::ast::parsed::ParsedExpr::ObjectHash(
        entries,
    )))
}

/// The `Circumfix::HashComposer(:object-hash)` of an object hash's entries.
// Cost: O(n), n = size of the entries.
pub(super) fn object_hash_node(names: &NameScope, args: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = Vec::with_capacity(2);
    if args.len() == 1 {
        fields.push(node_field(None, convert_expr(names, &args[0])?));
    } else if !args.is_empty() {
        fields.push(node_field(None, super::convert::comma_list_node(names, args)?));
    }
    fields.push(leaf_field(Some("object-hash"), Value::truth(true)));
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::CircumfixHashComposer,
        fields,
    })
}

/// `Circumfix::HashComposer` -> a composer-spelled [`Expr::Hash`] or an
/// object-hash constructor call.
// Cost: O(p), p = pairs of the composer (plus lowering their values).
pub(super) fn lower_composer(names: &NameScope, node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    if super::lower::bool_field(node, "object-hash")? {
        let args = match node.fields.iter().find(|field| field.name.is_none()) {
            None => Vec::new(),
            Some(field) => {
                let expression = match &field.value {
                    RakuAstFieldValue::Node(value) => match value.view() {
                        ValueView::RakuAst(child) => lower_expr(names, child)?,
                        _ => return Err(unsupported(node)),
                    },
                    _ => return Err(unsupported(node)),
                };
                match expression {
                    Expr::ArrayLiteral(items) if node_expression_is_comma_list(field) => items,
                    other => vec![other],
                }
            }
        };
        return Ok(Expr::Call {
            name: crate::symbol::Symbol::intern("__object_hash"),
            args,
            listop: false,
        });
    }
    let pairs = match node.fields.as_slice() {
        [] => Vec::new(),
        [_] => lower_pairs(names, node, named_child_or_positional(node)?)?,
        _ => return Err(unsupported(node)),
    };
    Ok(Expr::Hash(pairs, HashSpelling::Composer))
}

/// Whether a HashComposer's expression child is a comma-list node.
// Cost: O(1), one field lookup and node-kind check.
fn node_expression_is_comma_list(field: &RakuAstField) -> bool {
    matches!(&field.value, RakuAstFieldValue::Node(value)
        if matches!(value.view(), ValueView::RakuAst(child)
            if child.class == RakuAstClass::ApplyListInfix))
}

/// `Contextualizer::Hash` -> a contextualizer-spelled [`Expr::Hash`].
// Cost: O(p), p = pairs of the contextualizer (plus lowering their values).
pub(super) fn lower_contextualizer(names: &NameScope, node: &RakuAstNode) -> Result<Expr, RuntimeError> {
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
            lower_pairs(names, node, named_child(statement, "expression")?)?
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
fn lower_pairs(names: &NameScope, 
    node: &RakuAstNode,
    contents: &RakuAstNode,
) -> Result<Vec<(String, Option<Expr>)>, RuntimeError> {
    let items = match lower_expr(names, contents)? {
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
        ..
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
