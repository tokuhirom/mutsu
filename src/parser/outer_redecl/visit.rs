//! The scope walk itself (see the parent module): a [`VisitMut`] that opens a
//! scope at every lexical boundary and records references and declarations
//! in source order. Its only mutation is the `__init_sees_self` trait
//! [`super::walk_var_decl`] adds.

use super::{Ctx, Scope, decl_key, dynamic_key, param_keys, ref_key, walk_list, walk_var_decl};
use crate::ast::{Expr, ParamDef, Stmt};
use crate::ast_visit::{
    VisitMut, walk_expr_mut, walk_param_mut, walk_regex_node_mut, walk_stmt_mut,
};
use crate::regex_tree::RegexNode;

impl Ctx {
    /// Runs `f` in a fresh nested scope seeded with the declared names
    /// `seeds` (see [`param_keys`]).
    fn scoped(&mut self, seeds: Vec<String>, f: impl FnOnce(&mut Ctx)) {
        self.scopes.push(Scope::new());
        for key in seeds {
            self.seed(key);
        }
        f(self);
        self.scopes.pop();
    }

    /// Registers a read of the variable `sigil` + `name`.
    fn reference_var(&mut self, sigil: char, name: &str) {
        if let Some(k) = ref_key(sigil, name) {
            self.reference(k);
        } else if let Some(k) = dynamic_key(sigil, name) {
            self.reference_dynamic(&k);
        }
    }

    /// Registers a write through the declared-form name `name` (`"y"`, `"@a"`).
    fn reference_decl_name(&mut self, name: &str) {
        if let Some(k) = decl_key(name) {
            self.reference(k);
        }
    }
}

impl VisitMut for Ctx {
    fn visit_stmts_mut(&mut self, body: &mut Vec<Stmt>) {
        walk_list(body, self);
    }

    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::SetLine(n) => self.line = *n,
            Stmt::VarDecl { .. } => walk_var_decl(stmt, true, self),
            // Writes read the binding they write to: `temp $y`, `$y = ...`.
            Stmt::Assign { name, .. }
            | Stmt::Let { name, .. }
            | Stmt::TempMethodAssign { var_name: name, .. } => {
                let key = name.clone();
                self.reference_decl_name(&key);
                walk_stmt_mut(self, stmt);
            }
            // Routine boundaries: a fresh scope seeded with the parameters,
            // whose defaults and `where` clauses are in that scope. Closures
            // still see outer lexicals, so this is a normal nested scope.
            Stmt::SubDecl {
                params, param_defs, ..
            }
            | Stmt::MethodDecl {
                params, param_defs, ..
            }
            | Stmt::ProtoDecl {
                params, param_defs, ..
            } => {
                let seeds = param_keys(params, param_defs);
                self.scoped(seeds, |c| walk_stmt_mut(c, stmt));
            }
            // The supply is evaluated outside the block; the block's
            // parameters are its own lexicals.
            Stmt::Whenever {
                supply,
                params,
                param_defs,
                body,
            } => {
                self.visit_expr_mut(supply);
                let seeds = param_keys(params, param_defs);
                self.scoped(seeds, |c| {
                    for p in param_defs.iter_mut() {
                        c.visit_param_mut(p);
                    }
                    c.visit_stmts_mut(body);
                });
            }
            // Bodies that open a new lexical scope but preserve outer
            // visibility. An attribute's default is a thunk with its own
            // scope (rakudo accepts `has $.a = $y; my $y`).
            Stmt::Phaser { .. }
            | Stmt::TokenDecl { .. }
            | Stmt::RuleDecl { .. }
            | Stmt::ClassDecl { .. }
            | Stmt::RoleDecl { .. }
            | Stmt::Package { .. }
            | Stmt::PackageRuntimeBody { .. }
            | Stmt::AugmentClass { .. }
            | Stmt::HasDecl { .. }
            | Stmt::Block(_)
            | Stmt::Default(_)
            | Stmt::Catch(_)
            | Stmt::Control(_)
            | Stmt::React { .. }
            // A C-style loop's `init` declarations share the loop's scope.
            | Stmt::Loop { .. } => self.scoped(Vec::new(), |c| walk_stmt_mut(c, stmt)),
            Stmt::Given { topic: cond, body, .. }
            | Stmt::When { cond, body, .. }
            | Stmt::While { cond, body, .. } => {
                self.visit_expr_mut(cond);
                self.scoped(Vec::new(), |c| c.visit_stmts_mut(body));
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                ..
            } => {
                self.visit_expr_mut(cond);
                self.scoped(Vec::new(), |c| c.visit_stmts_mut(then_branch));
                // `elsif` chains are a nested `If` inside `else_branch`.
                self.scoped(Vec::new(), |c| c.visit_stmts_mut(else_branch));
            }
            Stmt::For {
                iterable,
                param,
                param_def,
                params,
                params_def,
                body,
                ..
            } => {
                self.visit_expr_mut(iterable);
                let mut seeds = param_keys(params, params_def);
                let single = param.iter().chain(param_def.as_ref().as_ref().map(|d| &d.name));
                seeds.extend(single.filter_map(|n| decl_key(n)));
                self.scoped(seeds, |c| {
                    if let Some(p) = param_def.as_mut() {
                        c.visit_param_mut(p);
                    }
                    for p in params_def.iter_mut() {
                        c.visit_param_mut(p);
                    }
                    c.visit_stmts_mut(body);
                });
            }
            // A `SyntheticBlock` (e.g. the lowering of `my ($a, $b) = ...`)
            // declares into the enclosing scope, as does everything else.
            _ => walk_stmt_mut(self, stmt),
        }
    }

    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Var(n) => self.reference_var('$', n),
            Expr::ArrayVar(n) => self.reference_var('@', n),
            Expr::HashVar(n) => self.reference_var('%', n),
            Expr::AssignExpr { name, .. } => {
                let key = name.clone();
                self.reference_decl_name(&key);
                walk_expr_mut(self, expr);
            }
            // Body-bearing expressions open a new nested lexical scope.
            Expr::Block(_)
            | Expr::Gather(_)
            | Expr::DoBlock { .. }
            | Expr::Once { .. }
            | Expr::PhaserExpr { .. }
            | Expr::AnonSub { .. } => self.scoped(Vec::new(), |c| walk_expr_mut(c, expr)),
            Expr::AnonSubParams {
                params, param_defs, ..
            } => {
                let seeds = param_keys(params, param_defs);
                self.scoped(seeds, |c| walk_expr_mut(c, expr));
            }
            Expr::Lambda { param, .. } => {
                let seeds = decl_key(param).into_iter().collect();
                self.scoped(seeds, |c| walk_expr_mut(c, expr));
            }
            Expr::Try { body, catch } => {
                self.scoped(Vec::new(), |c| c.visit_stmts_mut(body));
                if let Some(catch) = catch {
                    self.scoped(Vec::new(), |c| c.visit_stmts_mut(catch));
                }
            }
            // `do STMT` shares the enclosing scope (`do my $x = 5` declares
            // here), and an un-expanded WhateverCurry introduces no names of
            // its own yet (ADR-0033), so both are walked transparently, like
            // every other same-scope compound expression.
            _ => walk_expr_mut(self, expr),
        }
    }

    fn visit_param_mut(&mut self, param: &mut ParamDef) {
        // A code parameter's signature (`&c:(Int $ = $y)`) is a type
        // constraint that is never run, so its defaults read nothing
        // (rakudo accepts `sub f(&c:(Int $ = $y)) { my $y }`).
        let code_signature = param.code_signature.take();
        walk_param_mut(self, param);
        param.code_signature = code_signature;
    }

    fn visit_regex_node_mut(&mut self, node: &mut RegexNode) {
        match node {
            // A regex code block is a nested code object.
            RegexNode::CodeAssertion { .. }
            | RegexNode::CodeBlock { .. }
            | RegexNode::InterpolatedBlock { .. } => {
                self.scoped(Vec::new(), |c| walk_regex_node_mut(c, node))
            }
            _ => walk_regex_node_mut(self, node),
        }
    }
}
