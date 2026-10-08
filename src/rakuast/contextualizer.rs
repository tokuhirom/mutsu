//! `$(...)`, `@(...)` and `%(...)` across the RakuAST boundary.
//!
//! Measured against rakudo 2026.09, a contextualizer holds a `StatementSequence`
//! of its contents, except that the `$` of `$@(...)` / `$%(...)` holds the inner
//! contextualizer directly:
//!
//! ```text
//! $(1, 2)   Contextualizer::Item(StatementSequence(Statement::Expression(…)))
//! @(1, 2)   Contextualizer::List(StatementSequence(Statement::Expression(…)))
//! %(1, 2)   Contextualizer::Hash(StatementSequence(Statement::Expression(…)))
//! $@(1, 2)  Contextualizer::Item(Contextualizer::List(StatementSequence(…)))
//! ```
//!
//! The parser keeps them as [`Expr::Contextualizer`] (which compiles to the
//! `.item` / `.list` / `.hash` call), so a user-written call of that name still
//! renders as a call. A `%(...)` of literal pairs is an [`Expr::Hash`] instead
//! (see `hash_literal`).

use super::convert::{convert_expr, node_field, statement_expression};
use super::lower::{lower_expr, named_child_or_positional, unsupported};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::{ContextKind, Expr};
use crate::value::RuntimeError;

fn class_of(kind: ContextKind) -> RakuAstClass {
    match kind {
        ContextKind::Item => RakuAstClass::ContextualizerItem,
        ContextKind::List => RakuAstClass::ContextualizerList,
        ContextKind::Hash => RakuAstClass::ContextualizerHash,
    }
}

/// An [`Expr::Contextualizer`] as its `Contextualizer::*` node.
// Cost: O(n), n = nodes of the contents.
pub(super) fn convert(kind: ContextKind, inner: &Expr) -> Result<RakuAstNode, RuntimeError> {
    // `$@(...)` / `$%(...)`: the inner contextualizer is the child itself;
    // otherwise the child is a `StatementSequence` of the parenthesized contents.
    let contents = match inner {
        Expr::Grouped(contents) => contents.as_ref(),
        // `%(a => 1)`: the parser marks the parenthesized pair positional.
        Expr::PositionalPair(pair) => match pair.as_ref() {
            Expr::Grouped(contents) => contents.as_ref(),
            other => other,
        },
        other => other,
    };
    let target = match contents {
        Expr::Contextualizer { .. } if matches!(inner, Expr::Contextualizer { .. }) => {
            convert_expr(contents)?
        }
        Expr::ArrayLiteral(items) if items.is_empty() => sequence(None),
        other => sequence(Some(statement_expression(convert_expr(other)?))),
    };
    Ok(RakuAstNode {
        class: class_of(kind),
        fields: vec![node_field(None, target)],
    })
}

fn sequence(statement: Option<RakuAstNode>) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::StatementSequence,
        fields: statement.map(|s| node_field(None, s)).into_iter().collect(),
    }
}

/// A `Contextualizer::*` node -> an [`Expr::Contextualizer`]. A `%(...)` of
/// literal pairs is the [`Expr::Hash`] the parser builds for it instead.
// Cost: O(n), n = nodes of the contents.
pub(super) fn lower(node: &RakuAstNode, kind: ContextKind) -> Result<Expr, RuntimeError> {
    if kind == ContextKind::Hash
        && let Ok(hash) = super::hash_literal::lower_contextualizer(node)
    {
        return Ok(hash);
    }
    let target = named_child_or_positional(node)?;
    let inner = match target.class {
        RakuAstClass::ContextualizerItem => lower(target, ContextKind::Item)?,
        RakuAstClass::ContextualizerList => lower(target, ContextKind::List)?,
        RakuAstClass::ContextualizerHash => lower(target, ContextKind::Hash)?,
        // `$@a` / `$%h` / `$[1, 2]`: the item contextualizer over the term.
        RakuAstClass::CircumfixArrayComposer | RakuAstClass::CircumfixHashComposer
            if kind == ContextKind::Item =>
        {
            return Ok(Expr::MethodCall {
                target: Box::new(lower_expr(target)?),
                name: crate::symbol::Symbol::intern("item"),
                args: Vec::new(),
                modifier: None,
                quoted: false,
                sugar: true,
            });
        }
        RakuAstClass::VarLexical if kind == ContextKind::Item => {
            let term = lower_expr(target)?;
            if !matches!(
                term,
                Expr::ArrayVar(_) | Expr::HashVar(_) | Expr::BracketArray(..)
            ) {
                return Err(unsupported(node));
            }
            return Ok(Expr::Itemize(Box::new(term)));
        }
        RakuAstClass::StatementSequence => match target.fields.as_slice() {
            [] => Expr::Grouped(Box::new(Expr::ArrayLiteral(Vec::new()))),
            [_] => {
                let statement = named_child_or_positional(target)?;
                if statement.class != RakuAstClass::StatementExpression {
                    return Err(unsupported(node));
                }
                let expression = super::lower::named_child(statement, "expression")?;
                Expr::Grouped(Box::new(lower_expr(expression)?))
            }
            _ => return Err(unsupported(node)),
        },
        // `@$s` / `@.m(...)`: the parser's sugared `.list` call, converted by
        // [`convert_list_call`], holds the term itself rather than a
        // `StatementSequence`.
        // The sugared call is rebuilt as the parser spelled it: `@$h` keeps
        // the `Grouped` target the `for` lowering keys on, and `sugar: true`
        // keeps it the re-reading contextualizer.
        _ if kind == ContextKind::List => {
            let grouped = target.class == RakuAstClass::CircumfixParentheses;
            let term = match lower_expr(target)? {
                Expr::Grouped(inner) => *inner,
                other => other,
            };
            return Ok(Expr::MethodCall {
                target: Box::new(if grouped {
                    Expr::Grouped(Box::new(term))
                } else {
                    term
                }),
                name: crate::symbol::Symbol::intern("list"),
                args: Vec::new(),
                modifier: None,
                quoted: false,
                sugar: true,
            });
        }
        _ => return Err(unsupported(node)),
    };
    Ok(Expr::Contextualizer {
        kind,
        inner: Box::new(inner),
    })
}

/// An [`Expr::Itemize`] (`$@a`, `$%h`, `$[1, 2]`) as `Contextualizer::Item`
/// over the term itself, which holds no `StatementSequence`.
// Cost: O(n), n = nodes of the term.
pub(super) fn convert_itemize(inner: &Expr) -> Result<RakuAstNode, RuntimeError> {
    if !matches!(
        inner,
        Expr::ArrayVar(_) | Expr::HashVar(_) | Expr::BracketArray(..)
    ) {
        return Err(super::convert::unsupported(&format!("Itemize({inner:?})")));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::ContextualizerItem,
        fields: vec![node_field(None, convert_expr(inner)?)],
    })
}

/// The item contextualizer of a brace or bracket composer (`${ a => 1 }`,
/// `$[1, 2]`) as `Contextualizer::Item` over the composer, which holds no
/// `StatementSequence`; the parser spells it as an `.item` call.
// Cost: O(n), n = nodes of the composer.
pub(super) fn convert_item_call(inner: &Expr) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::ContextualizerItem,
        fields: vec![node_field(None, convert_expr(inner)?)],
    })
}

/// The array contextualizer of a term (`@$s`), which the parser spells as a
/// sugared `.list` call, as `Contextualizer::List` over the term itself. Keeping
/// the node (rather than an `ApplyPostfix` `.list`) is what lets the lowering
/// rebuild the re-reading contextualizer instead of an explicit `.list` call,
/// which consumes a `Seq` (#9930).
// Cost: O(n), n = nodes of the term.
pub(super) fn convert_list_call(inner: &Expr) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::ContextualizerList,
        fields: vec![node_field(None, convert_expr(inner)?)],
    })
}
