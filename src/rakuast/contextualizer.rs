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

use super::convert::{node_field, parenthesized_statement};
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
    let target = match inner {
        // `$@(...)` / `$%(...)`: the inner contextualizer is the child itself.
        Expr::Contextualizer { kind, inner } => convert(*kind, inner)?,
        // `(...)` written inside the sigil's own parentheses.
        Expr::Grouped(contents) => sequence(match contents.as_ref() {
            Expr::ArrayLiteral(items) if items.is_empty() => None,
            other => Some(parenthesized_statement(other)?),
        }),
        other => sequence(Some(parenthesized_statement(other)?)),
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
        _ => return Err(unsupported(node)),
    };
    Ok(Expr::Contextualizer {
        kind,
        inner: Box::new(inner),
    })
}
