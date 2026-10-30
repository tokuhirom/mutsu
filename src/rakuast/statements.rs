//! The statements of a compilation unit or a block, each as its RakuAST node
//! (`RakuAST::StatementList`), with the carried ones as leaves.

use super::convert::{convert_stmt, node_field, unit_package_taking};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode, bareword, origin};
use crate::ast::Stmt;
use crate::value::{RuntimeError, ValueView};

/// Top-level: a parsed program becomes a `RakuAST::StatementList`.
pub(super) fn statement_list(stmts: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    let _underscore_scope = super::underscore_bindings::Scope::reset();
    let _scope = bareword::DeclaredNames::collect(stmts);
    statement_list_inner(Statements::Shared(stmts))
}

/// [`statement_list`] for the executing round trip, which owns the unit: a
/// statement the round trip carries moves into its leaf instead of being
/// copied, and the unit is not read again afterwards.
// Cost: O(n), n = size of the statements.
pub(super) fn statement_list_owned(stmts: &mut [Stmt]) -> Result<RakuAstNode, RuntimeError> {
    let _underscore_scope = super::underscore_bindings::Scope::reset();
    let _scope = bareword::DeclaredNames::collect(stmts);
    statement_list_inner(Statements::Owned(stmts))
}

/// The statements of `stmts`, each as its node.
// Cost: O(n), n = size of the statements.
pub(super) fn block_statements(stmts: &[Stmt]) -> Result<Vec<RakuAstNode>, RuntimeError> {
    Ok(statement_list_inner(Statements::Shared(stmts))?
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

/// The statements a list is converted from: shared with the caller, or owned by
/// an executing round trip.
pub(super) enum Statements<'a> {
    Shared(&'a [Stmt]),
    Owned(&'a mut [Stmt]),
}

impl Statements<'_> {
    fn all(&self) -> &[Stmt] {
        match self {
            Statements::Shared(stmts) => stmts,
            Statements::Owned(stmts) => stmts,
        }
    }

    /// The leaf carrying statement `at`, which an owned list gives up.
    fn carry(&mut self, at: usize, line: Option<i64>) -> Option<RakuAstNode> {
        match self {
            Statements::Shared(stmts) => super::passthrough::stmt_leaf(&stmts[at], line),
            Statements::Owned(stmts) => super::passthrough::stmt_leaf_taking(&mut stmts[at], line),
        }
    }
}

pub(super) fn statement_list_inner(mut stmts: Statements<'_>) -> Result<RakuAstNode, RuntimeError> {
    let _statements = super::passthrough::Block::enter();
    let _underscore_scope = super::underscore_bindings::Scope::block(stmts.all());
    let _block = bareword::BlockScope::enter(stmts.all());
    let mut fields = Vec::new();
    // The line of the statement about to be converted: the `SetLine` marker
    // in front of it becomes the node's hidden origin (see `origin`).
    let mut line = None;
    let mut trace = None;
    for at in 0..stmts.all().len() {
        let stmt = &stmts.all()[at];
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
        let _statement = super::expr_origins::InStatement::enter(line.map(|(_, span)| span));
        if let Some(unit) = unit_package_taking(stmt, &stmts.all()[at + 1..]) {
            if let Some(mut node) = convert_stmt(&unit)? {
                super::routine_origins::package_origins(&mut node, &unit);
                if let Some(field) = trace.take().as_ref().and_then(origin::trace_field) {
                    node.fields.push(field);
                }
                if let Some((n, span)) = line.take() {
                    origin::set(&mut node, n, span);
                }
                fields.push(node_field(None, node));
            }
            break;
        }
        // The `state` declaration the parser puts at the top of a block for
        // each bare `$` it contains: rakudo's node is the `$` itself.
        if crate::ast::anon_state::is_implicit_decl(stmt) && !super::passthrough::executing() {
            continue;
        }
        if trace.is_none()
            && let Some(leaf) = stmts.carry(at, line.map(|(n, _)| n))
        {
            line = None;
            fields.push(node_field(None, leaf));
            continue;
        }
        let stmt = &stmts.all()[at];
        if let Some(mut node) = convert_stmt(stmt)? {
            super::routine_origins::package_origins(&mut node, stmt);
            if let Some(field) = trace.take().as_ref().and_then(origin::trace_field) {
                node.fields.push(field);
            }
            if let Some((n, span)) = line.take() {
                origin::set(&mut node, n, span);
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

