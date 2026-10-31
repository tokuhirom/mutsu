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
use crate::ast::{ContextKind, Expr, Stmt};
use crate::scope::NameScope;
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
pub(super) fn convert(
    names: &NameScope,
    kind: ContextKind,
    inner: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
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
            convert_expr(names, contents)?
        }
        Expr::ArrayLiteral(items) if items.is_empty() => sequence(None),
        other => sequence(Some(statement_expression(convert_expr(names, other)?))),
    };
    Ok(RakuAstNode {
        origin: Default::default(),
        class: class_of(kind),
        fields: vec![node_field(None, target)],
    })
}

/// The `$(EXPR)` of an interpolated string: the parser spells it
/// `DoStmt(Expr(EXPR))`, raku `Contextualizer::Item(StatementSequence(..))`.
// Cost: O(n), n = nodes of the contents.
pub(super) fn convert_segment(
    names: &NameScope,
    contents: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ContextualizerItem,
        fields: vec![node_field(
            None,
            sequence(Some(statement_expression(convert_expr(names, contents)?))),
        )],
    })
}

/// The inverse of [`convert_segment`]: the contents of a one-expression
/// `Contextualizer::Item` segment, `None` for any other shape.
// Cost: O(n), n = nodes of the contents.
pub(super) fn lower_segment(
    names: &NameScope,
    node: &RakuAstNode,
) -> Result<Option<Expr>, RuntimeError> {
    let target = named_child_or_positional(node)?;
    if target.class != RakuAstClass::StatementSequence {
        return Ok(None);
    }
    let [_] = target.fields.as_slice() else {
        return Ok(None);
    };
    let statement = named_child_or_positional(target)?;
    if statement.class != RakuAstClass::StatementExpression {
        return Ok(None);
    }
    let expression = super::lower::named_child(statement, "expression")?;
    Ok(Some(lower_expr(names, expression)?))
}

/// `$(STMT; STMT; ...)`: the parser's [`crate::parser::item_statements_expr`] as
/// `Contextualizer::Item(StatementSequence(Statement::*, ...))`.
// Cost: O(n), n = nodes of the statements.
pub(super) fn convert_statements(
    names: &NameScope,
    stmts: &[Stmt],
) -> Result<RakuAstNode, RuntimeError> {
    let fields = super::convert::block_statements(names, stmts)?
        .into_iter()
        .map(|statement| node_field(None, statement))
        .collect();
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ContextualizerItem,
        fields: vec![node_field(
            None,
            RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::StatementSequence,
                fields,
            },
        )],
    })
}

fn sequence(statement: Option<RakuAstNode>) -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::StatementSequence,
        fields: statement.map(|s| node_field(None, s)).into_iter().collect(),
    }
}

/// A `Contextualizer::*` node -> an [`Expr::Contextualizer`]. A `%(...)` of
/// literal pairs is the [`Expr::Hash`] the parser builds for it instead.
// Cost: O(n), n = nodes of the contents.
pub(super) fn lower(
    names: &NameScope,
    node: &RakuAstNode,
    kind: ContextKind,
) -> Result<Expr, RuntimeError> {
    if kind == ContextKind::Hash
        && let Ok(hash) = super::hash_literal::lower_contextualizer(names, node)
    {
        return Ok(hash);
    }
    let target = named_child_or_positional(node)?;
    let inner = match target.class {
        RakuAstClass::ContextualizerItem => lower(names, target, ContextKind::Item)?,
        RakuAstClass::ContextualizerList => lower(names, target, ContextKind::List)?,
        RakuAstClass::ContextualizerHash => lower(names, target, ContextKind::Hash)?,
        // `$@a` / `$%h` / `$[1, 2]`: the item contextualizer over the term.
        RakuAstClass::CircumfixArrayComposer | RakuAstClass::CircumfixHashComposer
            if kind == ContextKind::Item =>
        {
            return Ok(Expr::MethodCall {
                target: Box::new(lower_expr(names, target)?),
                name: crate::symbol::Symbol::intern("item"),
                args: Vec::new(),
                modifier: None,
                quoted: false,
                sugar: true,
            });
        }
        // `$@` / `$%`: the item contextualizer over an anonymous `@` / `%`.
        RakuAstClass::VarLexical
        | RakuAstClass::VarDynamic
        | RakuAstClass::VarPackage
        | RakuAstClass::VarAttribute
        | RakuAstClass::VarAttributePublic
        | RakuAstClass::VarDeclarationAnonymous
            if kind == ContextKind::Item =>
        {
            let term = lower_expr(names, target)?;
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
                Expr::Grouped(Box::new(lower_expr(names, expression)?))
            }
            // `$(a; b)`: several statements.
            _ if kind == ContextKind::Item => {
                return Ok(crate::parser::item_statements_expr(
                    super::lower::lower_statement_sequence(names, target)?,
                ));
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
            let term = match lower_expr(names, target)? {
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
pub(super) fn convert_itemize(
    names: &NameScope,
    inner: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    if !matches!(
        inner,
        Expr::ArrayVar(_) | Expr::HashVar(_) | Expr::BracketArray(..)
    ) {
        return Err(super::convert::unsupported(&format!("Itemize({inner:?})")));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ContextualizerItem,
        fields: vec![node_field(None, convert_expr(names, inner)?)],
    })
}

/// The item contextualizer of a brace or bracket composer (`${ a => 1 }`,
/// `$[1, 2]`) as `Contextualizer::Item` over the composer, which holds no
/// `StatementSequence`; the parser spells it as an `.item` call.
// Cost: O(n), n = nodes of the composer.
pub(super) fn convert_item_call(
    names: &NameScope,
    inner: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ContextualizerItem,
        fields: vec![node_field(None, convert_expr(names, inner)?)],
    })
}

/// The array contextualizer of a term (`@$s`), which the parser spells as a
/// sugared `.list` call, as `Contextualizer::List` over the term itself. Keeping
/// the node (rather than an `ApplyPostfix` `.list`) is what lets the lowering
/// rebuild the re-reading contextualizer instead of an explicit `.list` call,
/// which consumes a `Seq` (#9930).
// Cost: O(n), n = nodes of the term.
pub(super) fn convert_list_call(
    names: &NameScope,
    inner: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ContextualizerList,
        fields: vec![node_field(None, convert_expr(names, inner)?)],
    })
}
