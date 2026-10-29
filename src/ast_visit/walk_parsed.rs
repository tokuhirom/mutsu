//! The recursion over [`ParsedStmt`], the parser's RakuAST-shaped nodes
//! (ADR-10723 Stage 2). A parse that keeps them still runs the whole-program
//! passes, so every child is visited exactly as the lowered statement's would
//! be.

use super::{Visit, VisitMut, params, params_mut};
use crate::ast::parsed::{BlockParams, ParsedBlock, ParsedExpr, ParsedStmt};

/// Visits every child of `node`.
// Cost: O(n), n = size of `node`'s subtree.
pub(crate) fn walk_parsed<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, node: &'ast ParsedStmt) {
    if let ParsedStmt::RoutineTraits(traits) = node {
        // Not code: the execution fields hold the same expressions.
        v.visit_routine_traits(traits);
        return;
    }
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
        v.visit_stmt(decl);
        return;
    }
    if let ParsedStmt::ImmediateCall { decl, args } = node {
        v.visit_stmt(decl);
        for arg in args {
            v.visit_expr(arg);
        }
        return;
    }
    if let ParsedStmt::AttributeModifier {
        decl, condition, ..
    } = node
    {
        v.visit_stmt(decl);
        v.visit_expr(condition);
        return;
    }
    if let ParsedStmt::TakeLogicalTail { value, right, .. } = node {
        v.visit_expr(value);
        v.visit_expr(right);
        return;
    }
    if let ParsedStmt::InlineProtoUse { proto, use_stmt } = node {
        v.visit_stmt(proto);
        v.visit_stmt(use_stmt);
        return;
    }
    if let ParsedStmt::DoneValue(value) = node {
        v.visit_expr(value);
        return;
    }
    if let ParsedStmt::CallAssignList { head, items } = node {
        v.visit_stmt(head);
        for item in items {
            v.visit_expr(item);
        }
        return;
    }
    if let ParsedStmt::CommaDecls(declarations)
    | ParsedStmt::AttributeGroup(declarations)
    | ParsedStmt::AttributeColonArglist(declarations) = node
    {
        super::walk_stmts(v, declarations);
        return;
    }
    if let ParsedStmt::SignatureDecl(decl) = node {
        v.visit_signature_decl(decl);
        return;
    }
    if let ParsedStmt::Temporize(t) = node {
        if let crate::ast::temporize::Target::Element(element) = &t.target {
            v.visit_expr(element);
        }
        if let crate::ast::temporize::Assigned::Plain(e)
        | crate::ast::temporize::Assigned::Compound(e) = &t.assigned
        {
            v.visit_expr(e);
        }
        return;
    }
    if let ParsedStmt::SignatureBind { signature, rhs } = node {
        v.visit_expr(signature);
        v.visit_expr(rhs);
        return;
    }
    if let ParsedStmt::MethodAssignDecl(form) = node {
        super::walk_source::walk_method_assign_decl(v, form);
        return;
    }
    if let Some(stmt) = node.inner_stmt() {
        v.visit_stmt(stmt);
        return;
    }
    if let ParsedStmt::DoesDecl { decl, role, init } = node {
        v.visit_stmt(decl);
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
        v.visit_stmt(head);
        v.visit_expr(tail);
        return;
    }
    if let ParsedStmt::DeclCompound { decl, source, .. } = node {
        v.visit_stmt(decl);
        v.visit_expr(source);
        return;
    }
    if let ParsedStmt::WillDecl { decl, phasers, .. } = node {
        v.visit_stmt(decl);
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
        ParsedStmt::For { stmt, .. } | ParsedStmt::BindDecl(stmt) | ParsedStmt::TempDecl(stmt) => {
            super::walk_stmt_mut(v, stmt)
        }
        ParsedStmt::RoutineTraits(_) => {}
        ParsedStmt::TakeLogicalTail { value, right, .. } => {
            v.visit_expr_mut(value);
            v.visit_expr_mut(right);
        }
        ParsedStmt::InlineProtoUse { proto, use_stmt } => {
            v.visit_stmt_mut(proto);
            v.visit_stmt_mut(use_stmt);
        }
        ParsedStmt::DoneValue(value) => v.visit_expr_mut(value),
        ParsedStmt::CallAssignList { head, items } => {
            v.visit_stmt_mut(head);
            for item in items {
                v.visit_expr_mut(item);
            }
        }
        ParsedStmt::CommaDecls(declarations)
        | ParsedStmt::AttributeGroup(declarations)
        | ParsedStmt::AttributeColonArglist(declarations) => super::walk_stmts_mut(v, declarations),
        ParsedStmt::SignatureDecl(decl) => walk_signature_decl_mut(v, decl),
        ParsedStmt::Temporize(t) => {
            if let crate::ast::temporize::Target::Element(element) = &mut t.target {
                v.visit_expr_mut(element);
            }
            if let crate::ast::temporize::Assigned::Plain(e)
            | crate::ast::temporize::Assigned::Compound(e) = &mut t.assigned
            {
                v.visit_expr_mut(e);
            }
        }
        ParsedStmt::SignatureBind { signature, rhs } => {
            v.visit_expr_mut(signature);
            v.visit_expr_mut(rhs);
        }
        ParsedStmt::MethodAssignDecl(form) => walk_method_assign_decl_mut(v, form),
        ParsedStmt::ImmediateCall { decl, args } => {
            v.visit_stmt_mut(decl);
            for arg in args {
                v.visit_expr_mut(arg);
            }
        }
        ParsedStmt::AttributeModifier {
            decl, condition, ..
        } => {
            v.visit_stmt_mut(decl);
            v.visit_expr_mut(condition);
        }
        ParsedStmt::DoesDecl { decl, role, init } => {
            v.visit_stmt_mut(decl);
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
            v.visit_stmt_mut(decl);
        }
        ParsedStmt::DeclTail { head, tail } => {
            v.visit_stmt_mut(head);
            v.visit_expr_mut(tail);
        }
        ParsedStmt::DeclCompound { decl, source, .. } => {
            v.visit_stmt_mut(decl);
            v.visit_expr_mut(source);
        }
        ParsedStmt::WillDecl { decl, phasers, .. } => {
            v.visit_stmt_mut(decl);
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
        ParsedExpr::Supply(body) | ParsedExpr::StartBody(body) => super::walk_stmts(v, body),
        ParsedExpr::HyperAssign(a) => {
            v.visit_expr(&a.target);
            v.visit_expr(&a.value);
        }
        ParsedExpr::InlineSubset(declaration) => v.visit_stmt(declaration),
        ParsedExpr::HyperPrefix(p) => v.visit_expr(&p.operand),
        ParsedExpr::AtomicVar(a) => {
            if let crate::ast::parsed::AtomicVar::Store { value, .. } = a {
                v.visit_expr(value);
            }
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
        ParsedExpr::Supply(body) | ParsedExpr::StartBody(body) => super::walk_stmts_mut(v, body),
        ParsedExpr::HyperAssign(a) => {
            v.visit_expr_mut(&mut a.target);
            v.visit_expr_mut(&mut a.value);
        }
        ParsedExpr::InlineSubset(declaration) => v.visit_stmt_mut(declaration),
        ParsedExpr::HyperPrefix(p) => v.visit_expr_mut(&mut p.operand),
        ParsedExpr::AtomicVar(a) => {
            if let crate::ast::parsed::AtomicVar::Store { value, .. } = a {
                v.visit_expr_mut(value);
            }
        }
    }
}

/// [`super::walk_source::walk_method_assign_decl`], mutably.
// Cost: O(n), n = nodes of the declaration's call.
fn walk_method_assign_decl_mut<V: VisitMut + ?Sized>(
    v: &mut V,
    form: &mut crate::ast::method_assign_decl::MethodAssignDecl,
) {
    super::traits_mut(v, &mut form.custom_traits);
    if let Some(expr) = &mut form.where_constraint {
        v.visit_expr_mut(expr);
    }
    if let Some(callee) = &mut form.callee {
        v.visit_expr_mut(callee);
    }
    super::exprs_mut(v, &mut form.args);
    if let Some(chain) = &mut form.chain {
        v.visit_expr_mut(chain);
    }
    if let Some(dims) = &mut form.shape {
        super::exprs_mut(v, dims);
    }
}

/// [`super::walk_signature_decl`], mutably: the expressions the declaration
/// holds.
// Cost: O(n), n = nodes of the declaration.
fn walk_signature_decl_mut<V: VisitMut + ?Sized>(v: &mut V, decl: &mut crate::ast::SignatureDecl) {
    for var in &mut decl.vars {
        for expr in [
            &mut var.default,
            &mut var.where_constraint,
            &mut var.literal_value,
        ]
        .into_iter()
        .flatten()
        {
            v.visit_expr_mut(expr);
        }
    }
    if let Some(expr) = &mut decl.group_default {
        v.visit_expr_mut(expr);
    }
    if let Some(init) = &mut decl.init {
        v.visit_expr_mut(&mut init.rhs);
    }
}
