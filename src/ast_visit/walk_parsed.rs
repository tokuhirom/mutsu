//! The recursion over [`ParsedStmt`], the parser's RakuAST-shaped nodes
//! (ADR-10723 Stage 2). A parse that keeps them still runs the whole-program
//! passes, so every child is visited exactly as the lowered statement's would
//! be.

use super::{Visit, VisitMut, params, params_mut};
use crate::ast::parsed::{BlockParams, ParsedBlock, ParsedStmt};

/// Visits every child of `node`.
// Cost: O(n), n = size of `node`'s subtree.
pub(crate) fn walk_parsed<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, node: &'ast ParsedStmt) {
    if let ParsedStmt::Conditional(c) = node {
        // Each clause's condition comes before its block.
        v.visit_expr(&c.condition);
        block(v, &c.then);
        for clause in &c.clauses {
            v.visit_expr(&clause.condition);
            block(v, &clause.then);
        }
        if let Some(b) = &c.else_block {
            block(v, b);
        }
        return;
    }
    if let Some(stmt) = node.inner_stmt() {
        super::walk_stmt(v, stmt);
        return;
    }
    for e in node.headers() {
        v.visit_expr(e);
    }
    match node {
        ParsedStmt::WhileLoop(w) => super::names(v, w.label.iter(), super::NameKind::Label),
        ParsedStmt::Loop(l) => {
            if let Some(init) = &l.init {
                v.visit_stmt(init);
            }
            super::names(v, l.label.iter(), super::NameKind::Label);
        }
        _ => {}
    }
    for b in node.blocks() {
        block(v, b);
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
        ParsedStmt::Given { topic: e, block }
        | ParsedStmt::When {
            condition: e,
            block,
        } => {
            v.visit_expr_mut(e);
            block_mut(v, block);
        }
        ParsedStmt::WhileLoop(w) => {
            v.visit_expr_mut(&mut w.condition);
            block_mut(v, &mut w.block);
        }
        ParsedStmt::Default(block) | ParsedStmt::BareBlock(block) => block_mut(v, block),
        ParsedStmt::For { stmt, .. } | ParsedStmt::BindDecl(stmt) => super::walk_stmt_mut(v, stmt),
        ParsedStmt::Loop(l) => {
            if let Some(init) = &mut l.init {
                v.visit_stmt_mut(init);
            }
            for e in [&mut l.condition, &mut l.step].into_iter().flatten() {
                v.visit_expr_mut(e);
            }
            block_mut(v, &mut l.block);
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
