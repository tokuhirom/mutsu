//! The statements of a compilation unit or a block, each as its RakuAST node
//! (`RakuAST::StatementList`), with the carried ones as leaves.

use super::view::{node_field, unit_package_taking, view_stmt};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode, bareword, origin};
use crate::ast::Stmt;
use crate::scope::NameScope;
use crate::value::{RuntimeError, ValueView};

/// Top-level: a parsed program becomes a `RakuAST::StatementList`.
pub(super) fn statement_list(
    names: &NameScope,
    stmts: &[Stmt],
) -> Result<RakuAstNode, RuntimeError> {
    let _underscore_scope = super::underscore_bindings::Scope::reset();
    let _scope = bareword::DeclaredNames::collect(names, stmts);
    statement_list_inner(names, stmts)
}

/// The statements of `stmts`, each as its node.
// Cost: O(n), n = size of the statements.
pub(super) fn block_statements(
    names: &NameScope,
    stmts: &[Stmt],
) -> Result<Vec<RakuAstNode>, RuntimeError> {
    Ok(statement_list_inner(names, stmts)?
        .fields
        .into_iter()
        .filter_map(|field| match field.value {
            RakuAstFieldValue::Node(value) => match value.view() {
                ValueView::RakuAst(node) => Some(node.clone()),
                _ => None,
            },
            _ => None,
        })
        .collect())
}

pub(super) fn statement_list_inner(
    names: &NameScope,
    stmts: &[Stmt],
) -> Result<RakuAstNode, RuntimeError> {
    let _underscore_scope = super::underscore_bindings::Scope::block(stmts);
    let _block = bareword::BlockScope::enter(names, stmts);
    let mut fields = Vec::new();
    // The line of the statement about to be converted: the `SetLine` marker
    // in front of it becomes the node's hidden origin (see `origin`).
    let mut line = None;
    let mut trace = None;
    for (at, stmt) in stmts.iter().enumerate() {
        if let Stmt::SetLine(n, span) = stmt {
            line = Some((*n, *span));
            continue;
        }
        if matches!(stmt, Stmt::Trace { .. }) {
            trace = Some(stmt.clone());
            continue;
        }
        // `unit module M;` / `unit package P;`: rakudo holds the rest of the
        // unit in the declaration's body; the parser leaves it beside it.
        // A `unit` declaration's statement runs over the rest of the unit: a
        // term of the rest (a `;` that ends no statement) belongs to it.
        let unit = unit_package_taking(stmt, &stmts[at + 1..]);
        let held = unit.as_ref().and(super::unit_span::rest_span(&stmts[at + 1..]));
        let _statement = super::expr_origins::InStatement::enter(line.map(|(_, span)| {
            match held {
                Some(rest) if span.is_some() => {
                    crate::ast::SrcSpan::new(span.from as usize, rest.to as usize)
                }
                _ => super::unit_span::widen_statement(span, stmt),
            }
        }));
        if let Some(unit) = unit {
            if let Some(mut node) = view_stmt(names, &unit)? {
                super::routine_origins::package_origins(&mut node, &unit);
                if let Some(field) = trace.take().as_ref().and_then(origin::trace_field) {
                    node.fields.push(field);
                }
                if let Some((n, mut span)) = line.take() {
                    // The declaration runs over the rest of the unit it holds.
                    if let Some(rest) = super::unit_span::rest_span(&stmts[at + 1..])
                        && span.is_some()
                    {
                        span = crate::ast::SrcSpan::new(span.from as usize, rest.to as usize);
                    }
                    origin::set(names, &mut node, n, span);
                }
                fields.push(node_field(None, node));
            }
            break;
        }
        // The `state` declaration the parser puts at the top of a block for
        // each bare `$` it contains: rakudo's node is the `$` itself.
        if crate::ast::anon_state::is_implicit_decl(stmt) && !super::finalize::executing() {
            continue;
        }
        if let Some(mut node) = view_stmt(names, stmt)? {
            super::routine_origins::package_origins(&mut node, stmt);
            if let Some(field) = trace.take().as_ref().and_then(origin::trace_field) {
                node.fields.push(field);
            }
            if let Some((n, span)) = line.take() {
                let span = super::unit_span::widen_statement(span, stmt);
                origin::set(names, &mut node, n, span);
            }
            fields.push(node_field(None, node));
        }
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::StatementList,
        fields,
    })
}
