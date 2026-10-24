//! The recursion over [`ParsedStmt`], the parser's RakuAST-shaped nodes
//! (ADR-10723 Stage 2). A parse that keeps them still runs the whole-program
//! passes, so every child is visited exactly as the lowered statement's would
//! be.

use super::{Visit, VisitMut, params, params_mut};
use crate::ast::parsed::{BlockParams, ParsedBlock, ParsedExpr, ParsedStmt};

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
    if let ParsedStmt::PackageHeader { decl, header, .. } = node {
        // The adverb values come before the declaration.
        for (_, value) in &header.adverbs {
            v.visit_expr(value);
        }
        super::walk_stmt(v, decl);
        return;
    }
    if let ParsedStmt::ImmediateCall { decl, args } = node {
        super::walk_stmt(v, decl);
        for arg in args {
            v.visit_expr(arg);
        }
        return;
    }
    if let ParsedStmt::AttributeModifier {
        decl, condition, ..
    } = node
    {
        super::walk_stmt(v, decl);
        v.visit_expr(condition);
        return;
    }
    if let ParsedStmt::TakeLogicalTail { value, right, .. } = node {
        v.visit_expr(value);
        v.visit_expr(right);
        return;
    }
    if let ParsedStmt::CommaDecls(declarations) = node {
        super::walk_stmts(v, declarations);
        return;
    }
    if let Some(stmt) = node.inner_stmt() {
        super::walk_stmt(v, stmt);
        return;
    }
    if let ParsedStmt::DoesDecl { decl, role, init } = node {
        super::walk_stmt(v, decl);
        v.visit_expr(role);
        if let Some(init) = init {
            v.visit_expr(init);
        }
        return;
    }
    if let ParsedStmt::SigillessDecl(d) = node {
        v.visit_expr(&d.expr);
        return;
    }
    if let ParsedStmt::DeclTail { head, tail } = node {
        super::walk_stmt(v, head);
        v.visit_expr(tail);
        return;
    }
    if let ParsedStmt::WillDecl { decl, phasers } = node {
        super::walk_stmt(v, decl);
        for (_, body) in phasers {
            super::walk_stmts(v, body);
        }
        return;
    }
    // The modified statement comes before its modifiers.
    if let ParsedStmt::Modified(m) = node {
        v.visit_stmt(&m.stmt);
        for modifier in m.modifiers() {
            v.visit_expr(&modifier.expr);
        }
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
        ParsedStmt::For { stmt, .. }
        | ParsedStmt::BindDecl(stmt)
        | ParsedStmt::TempDecl(stmt)
        | ParsedStmt::BarePhaser(stmt) => super::walk_stmt_mut(v, stmt),
        ParsedStmt::TakeLogicalTail { value, right, .. } => {
            v.visit_expr_mut(value);
            v.visit_expr_mut(right);
        }
        ParsedStmt::CommaDecls(declarations) => super::walk_stmts_mut(v, declarations),
        ParsedStmt::ImmediateCall { decl, args } => {
            super::walk_stmt_mut(v, decl);
            for arg in args {
                v.visit_expr_mut(arg);
            }
        }
        ParsedStmt::AttributeModifier {
            decl, condition, ..
        } => {
            super::walk_stmt_mut(v, decl);
            v.visit_expr_mut(condition);
        }
        ParsedStmt::DoesDecl { decl, role, init } => {
            super::walk_stmt_mut(v, decl);
            v.visit_expr_mut(role);
            if let Some(init) = init {
                v.visit_expr_mut(init);
            }
        }
        ParsedStmt::SigillessDecl(d) => v.visit_expr_mut(&mut d.expr),
        ParsedStmt::PackageHeader { decl, header, .. } => {
            for (_, value) in &mut header.adverbs {
                v.visit_expr_mut(value);
            }
            super::walk_stmt_mut(v, decl);
        }
        ParsedStmt::DeclTail { head, tail } => {
            super::walk_stmt_mut(v, head);
            v.visit_expr_mut(tail);
        }
        ParsedStmt::WillDecl { decl, phasers } => {
            super::walk_stmt_mut(v, decl);
            for (_, body) in phasers {
                v.visit_stmts_mut(body);
            }
        }
        ParsedStmt::Modified(m) => {
            v.visit_stmt_mut(&mut m.stmt);
            for modifier in [&mut m.condition, &mut m.loop_].into_iter().flatten() {
                v.visit_expr_mut(&mut modifier.expr);
            }
        }
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

/// Visits every child of `node`, an expression kept in its RakuAST shape.
// Cost: O(n), n = size of `node`'s subtree.
pub(crate) fn walk_parsed_expr<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, node: &'ast ParsedExpr) {
    match node {
        ParsedExpr::Assign(a) => {
            v.visit_expr(&a.target);
            v.visit_expr(&a.source);
        }
        ParsedExpr::CompoundAssign(a) => {
            v.visit_expr(&a.target);
            v.visit_expr(&a.source);
        }
    }
}

/// Visits every child of `node` mutably.
// Cost: O(n), n = size of `node`'s subtree.
pub(crate) fn walk_parsed_expr_mut<V: VisitMut + ?Sized>(v: &mut V, node: &mut ParsedExpr) {
    match node {
        ParsedExpr::Assign(a) => {
            v.visit_expr_mut(&mut a.target);
            v.visit_expr_mut(&mut a.source);
        }
        ParsedExpr::CompoundAssign(a) => {
            v.visit_expr_mut(&mut a.target);
            v.visit_expr_mut(&mut a.source);
        }
    }
}
