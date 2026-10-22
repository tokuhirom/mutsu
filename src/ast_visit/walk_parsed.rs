//! The recursion over [`ParsedStmt`], the parser's RakuAST-shaped nodes
//! (ADR-10723 Stage 2). A parse that keeps them still runs the whole-program
//! passes, so every child is visited exactly as the lowered statement's would
//! be.

use super::{Visit, VisitMut, params, params_mut};
use crate::ast::parsed::{BlockParams, ParsedBlock, ParsedStmt};

/// Visits every child of `node`.
// Cost: O(n), n = size of `node`'s subtree.
pub(crate) fn walk_parsed<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, node: &'ast ParsedStmt) {
    match node {
        ParsedStmt::Conditional(c) => {
            v.visit_expr(&c.condition);
            block(v, &c.then);
            for clause in &c.clauses {
                v.visit_expr(&clause.condition);
                block(v, &clause.then);
            }
            if let Some(b) = &c.else_block {
                block(v, b);
            }
        }
    }
}

// Cost: O(n), n = size of the block.
fn block<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, b: &'ast ParsedBlock) {
    match &b.params {
        BlockParams::None => {}
        BlockParams::List(defs) => params(v, defs),
        BlockParams::Topic { name: _, def } => {
            if let Some(def) = def {
                v.visit_param(def);
            }
        }
    }
    super::walk_stmts(v, &b.body);
}

/// Visits every child of `node` mutably.
// Cost: O(n), n = size of `node`'s subtree.
pub(crate) fn walk_parsed_mut<V: VisitMut + ?Sized>(v: &mut V, node: &mut ParsedStmt) {
    match node {
        ParsedStmt::Conditional(c) => {
            v.visit_expr_mut(&mut c.condition);
            block_mut(v, &mut c.then);
            for clause in &mut c.clauses {
                v.visit_expr_mut(&mut clause.condition);
                block_mut(v, &mut clause.then);
            }
            if let Some(b) = &mut c.else_block {
                block_mut(v, b);
            }
        }
    }
}

// Cost: O(n), n = size of the block.
fn block_mut<V: VisitMut + ?Sized>(v: &mut V, b: &mut ParsedBlock) {
    match &mut b.params {
        BlockParams::None => {}
        BlockParams::List(defs) => params_mut(v, defs),
        BlockParams::Topic { name: _, def } => {
            if let Some(def) = def {
                v.visit_param_mut(def);
            }
        }
    }
    super::walk_stmts_mut(v, &mut b.body);
}
