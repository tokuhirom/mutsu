//! The bounded env-sync set of a lazily-registered declaration: a named sub's
//! body (#10960) or a class's registration (#10999).
//!
//! A named sub is installed by a `RegisterDecl` op and has no runtime
//! closure-creation op, so the declaring frame's `compute_needs_env_sync`
//! cannot see which of its lexicals the body reads by name. It used to fold
//! EVERY local of such a frame into `needs_env_sync`, which made each store in
//! a top-level loop pay an env mirror write as soon as the script declared
//! any sub. The body is compiled right at its declaration, though, so its
//! free-variable set is known there: this module resolves it against the
//! declaring frame's slots and records it in
//! [`crate::opcode::CompiledCode::lazy_body_env_sync_slots`], marking the plan
//! bounded so the frame-wide fold skips it.
//!
//! The bar is the one the nested-closure fold in `compute_needs_env_sync`
//! already meets: every name the body (or anything nested in it) can read by
//! name from this frame's env must keep its mirror. A body that reaches names
//! no scan can bound stays unbounded and keeps the conservative fold.
//!
//! A class is registered the same way and reads outer lexicals through more
//! channels — its compiled method bodies, their signatures' declaration-time
//! expressions, attribute descriptors, trait and parent-argument chunks, the
//! class-body statement chunks run at registration, and the type names it
//! mentions. [`Compiler::note_class_decl_env_sync`] bounds the plan only when
//! every one of them is enumerable. A role (#11078) adds its type-parameter
//! signatures and the deferred body statements each composition runs; see
//! [`Compiler::note_role_decl_env_sync`]. Expressions still evaluated from
//! raw AST at registration (a computed method name, a fallback trait
//! argument) are enumerated from an analysis compile of the same expression,
//! a static `token`/`rule` body from its pattern text, and a `__hoisted`
//! shell is bounded together with its source-order declaration (#11116).

use super::Compiler;
use super::lazy_body_reads::{
    chunk_reads, collect_by_name_reads, lazy_body_reads_bounded, push_type_name_tokens,
    push_unique, token_body_reads,
};
use crate::ast::{Expr, ParamDef, Stmt};
use crate::opcode::{
    ClassBodyOp, CompiledAttrDecl, CompiledClassDeclPlan, CompiledDeclExpr, CompiledMethodDecl,
    CompiledRoleDeclPlan, DeclTraitArg, DeferredBodyOpKind,
};
use crate::symbol::Symbol;
use std::collections::{HashMap, HashSet};

impl Compiler {
    /// Record the env-sync slots of a named sub's compiled bodies `keys` and
    /// mark sub-declaration plans `plan_idxs` (`decl_plans` indices) bounded.
    /// Leaves the plans unbounded (the frame keeps its every-local fold) when
    /// a body failed to compile (`expected` bodies but fewer `keys`) or reads
    /// names by a mechanism no op scan can bound.
    // Cost: O(b), b = total ops and constants of the sub's compiled bodies,
    // nested closures included.
    pub(super) fn note_sub_decl_env_sync(
        &mut self,
        plan_idxs: &[u32],
        keys: &[Symbol],
        expected: usize,
    ) {
        if keys.len() != expected {
            return;
        }
        let mut names: Vec<Symbol> = Vec::new();
        for key in keys {
            let Some(cf) = self.compiled_functions.get(key) else {
                return;
            };
            if !lazy_body_reads_bounded(&cf.code) {
                return;
            }
            collect_by_name_reads(&cf.code, &mut names);
        }
        self.record_bounded_lazy_decl(plan_idxs, names);
    }

    /// The class counterpart of [`Self::note_sub_decl_env_sync`] (#10999):
    /// mark the class-declaration plan `decl_idx` (a `decl_plans` index)
    /// bounded when every channel its registration reads outer lexicals
    /// through by name is enumerable, folding those names into this frame's
    /// env-sync slots. The channels are the compiled method bodies, their
    /// signatures' declaration-time expressions, the attribute descriptors,
    /// the trait and parent-argument chunks, the class-body statement chunks,
    /// and the type names the header and signatures mention. A `token`/`rule`
    /// body that is not static leaves the plan unbounded.
    // Cost: O(b), b = total ops and constants of the class's compiled method
    // bodies and declaration chunks, nested closures included.
    pub(super) fn note_class_decl_env_sync(&mut self, decl_idx: u32) {
        let Some(crate::opcode::CompiledDeclPlanRef::Class(plan_idx)) =
            self.code.decl_plans.get(decl_idx as usize)
        else {
            return;
        };
        let Some(plan) = self.code.class_decl_plans.get(*plan_idx as usize) else {
            return;
        };
        let decl_id = plan.decl_id;
        let Some(names) = self.class_plan_by_name_reads(plan) else {
            return;
        };
        self.record_bounded_type_decl(decl_idx, decl_id, names);
    }

    /// Every name the registration of `plan` (and each method it installs)
    /// may resolve by name in the declaring frame's env, or `None` when one
    /// of them is beyond enumeration.
    // Cost: O(b), b as in `note_class_decl_env_sync`.
    fn class_plan_by_name_reads(&self, plan: &CompiledClassDeclPlan) -> Option<Vec<Symbol>> {
        let mut names: Vec<Symbol> = Vec::new();
        if let Some(chunk) = &plan.name_chunk {
            chunk_reads(chunk, &mut names)?;
        }
        for type_name in plan
            .parents
            .iter()
            .chain(&plan.does_parents)
            .chain(&plan.hidden_parents)
            .chain(&plan.body_parents)
        {
            push_type_name_tokens(type_name, &mut names);
        }
        for sym in &plan.trusts {
            sym.with_str(|s| push_type_name_tokens(s, &mut names));
        }
        for (_, arg) in &plan.custom_traits {
            self.decl_arg_reads(arg.as_ref(), &mut names)?;
        }
        for (_, args) in &plan.parent_arg_chunks {
            for arg in args {
                self.decl_arg_reads(Some(arg), &mut names)?;
            }
        }
        self.attr_decls_by_name_reads(&plan.attr_decls, &mut names)?;
        self.methods_by_name_reads(&plan.method_name_chunks, &plan.method_decls, &mut names)?;
        for op in &plan.body_plan {
            match op {
                ClassBodyOp::Attr { .. } | ClassBodyOp::Method => {}
                ClassBodyOp::Does { name, args } => {
                    name.with_str(|s| push_type_name_tokens(s, &mut names));
                    for arg in args.iter().flatten() {
                        self.decl_arg_reads(Some(arg), &mut names)?;
                    }
                }
                ClassBodyOp::ClassSub {
                    chunk,
                    hoist_chunk,
                    raw,
                    ..
                } => {
                    self.stmt_chunk_reads(chunk.as_ref(), raw, &mut names)?;
                    if let Some(hoist) = hoist_chunk {
                        chunk_reads(hoist, &mut names)?;
                    }
                }
                ClassBodyOp::CodeAlias { chunk, raw, .. }
                | ClassBodyOp::ProtoMethod { chunk, raw, .. }
                | ClassBodyOp::LeavePhaser { chunk, raw, .. }
                | ClassBodyOp::Other { chunk, raw, .. } => {
                    self.stmt_chunk_reads(chunk.as_ref(), raw, &mut names)?
                }
                ClassBodyOp::TokenRule { plan } => self.token_decl_reads(
                    &plan.param_defs,
                    &plan.raw_body,
                    &plan.qq_thunk_chunks,
                    &mut names,
                )?,
            }
        }
        Some(names)
    }

    /// The role counterpart of [`Self::note_class_decl_env_sync`] (#11078):
    /// mark the role-declaration plan `decl_idx` bounded when every channel
    /// its registration and compositions read outer lexicals through by name
    /// is enumerable. Beyond the class channels, a role has type-parameter
    /// signatures (defaults evaluated per parameterization) and deferred
    /// body statements run at each composition: a `Plain` one is recompiled
    /// from raw AST under the composition's ambient package, so its reads
    /// are enumerated from an analysis compile of the same statement — the
    /// package changes how a name qualifies, not which lexicals it reads.
    /// A `token`/`rule` statement (interpreter-executed, ADR-0009) leaves
    /// the plan unbounded.
    // Cost: O(b), b = total ops and constants of the role's compiled method
    // bodies, declaration chunks and analysis-compiled deferred statements.
    pub(super) fn note_role_decl_env_sync(&mut self, decl_idx: u32) {
        let Some(crate::opcode::CompiledDeclPlanRef::Role(plan_idx)) =
            self.code.decl_plans.get(decl_idx as usize)
        else {
            return;
        };
        let Some(plan) = self.code.role_decl_plans.get(*plan_idx as usize) else {
            return;
        };
        let decl_id = plan.decl_id;
        let Some(names) = self.role_plan_by_name_reads(plan) else {
            return;
        };
        self.record_bounded_type_decl(decl_idx, decl_id, names);
    }

    /// Mark the class/role plan `decl_idx` bounded with its reads `names`,
    /// together with any `__hoisted` shell of the same declaration
    /// (`decl_id`) this compiler emitted: the shell registers a subset of the
    /// same body, so it reads no name the full declaration does not. The
    /// shell's own plan is never bounded on its own (its methods are compiled
    /// only on demand, so there are no bodies to scan). The names are also
    /// kept in `lazy_decl_reads` for an enclosing declaration's bound.
    // Cost: O(n * s + h), n = names.len(), s = slots already recorded,
    // h = hoisted shells of this compiler.
    fn record_bounded_type_decl(&mut self, decl_idx: u32, decl_id: u64, names: Vec<Symbol>) {
        let mut idxs = vec![decl_idx];
        idxs.extend(
            self.hoisted_type_shells
                .iter()
                .filter(|(id, idx)| *id == decl_id && *idx != decl_idx)
                .map(|(_, idx)| *idx),
        );
        for sym in &names {
            push_unique(&mut self.code.lazy_decl_reads, *sym);
        }
        self.record_bounded_lazy_decl(&idxs, names);
    }

    /// Every name the registration and compositions of the role `plan` may
    /// resolve by name in the declaring frame's env, or `None` when one of
    /// them is beyond enumeration.
    // Cost: O(b), b as in `note_role_decl_env_sync`.
    fn role_plan_by_name_reads(&self, plan: &CompiledRoleDeclPlan) -> Option<Vec<Symbol>> {
        let mut names: Vec<Symbol> = Vec::new();
        self.param_defs_by_name_reads(&plan.type_param_defs, &mut names)?;
        for (_, arg) in &plan.custom_traits {
            self.decl_arg_reads(arg.as_ref(), &mut names)?;
        }
        for parent in &plan.parent_ops {
            // A bracketed parent whose arguments did not parse as an
            // expression list is evaluated from its spelling.
            if parent.args.is_none() && parent.name.with_str(|s| s.contains('[')) {
                return None;
            }
            parent
                .name
                .with_str(|s| push_type_name_tokens(s, &mut names));
            for arg in parent.args.iter().flatten() {
                self.decl_arg_reads(Some(arg), &mut names)?;
            }
        }
        self.attr_decls_by_name_reads(&plan.attr_decls, &mut names)?;
        self.methods_by_name_reads(&plan.method_name_chunks, &plan.method_decls, &mut names)?;
        let package = self.qualified_role_decl_name(&plan.name.resolve());
        for op in &plan.deferred_body_ops {
            match (op.kind, &op.chunk) {
                (DeferredBodyOpKind::TokenRule, _) => {
                    let (Stmt::TokenDecl {
                        param_defs, body, ..
                    }
                    | Stmt::RuleDecl {
                        param_defs, body, ..
                    }) = &op.raw
                    else {
                        return None;
                    };
                    self.token_decl_reads(param_defs, body, &op.qq_thunk_chunks, &mut names)?;
                }
                (_, Some(chunk)) => chunk_reads(chunk, &mut names)?,
                (_, None) => self.raw_stmt_reads(&op.raw, &package, &mut names)?,
            }
        }
        Some(names)
    }

    /// Fold the by-name reads of a type's top-level `method`/`submethod`
    /// declarations: each computed name, trait argument, compiled body, its
    /// signature's declaration-time expressions and its return type.
    // Cost: O(b), b = total ops and constants of the method bodies and
    // their declaration-time expressions.
    fn methods_by_name_reads(
        &self,
        name_chunks: &[Option<CompiledDeclExpr>],
        methods: &[CompiledMethodDecl],
        out: &mut Vec<Symbol>,
    ) -> Option<()> {
        for chunk in name_chunks.iter().flatten() {
            chunk_reads(chunk, out)?;
        }
        for method in methods {
            let exprs = method.name_expr.iter().chain(
                method
                    .custom_traits
                    .iter()
                    .filter_map(|(_, arg)| arg.as_ref()),
            );
            for expr in exprs {
                self.ast_expr_reads(expr, out)?;
            }
            // A method of a computed class name, or one with a computed name
            // itself, is compiled only at registration: enumerate its reads
            // from an analysis compile of the same body instead.
            let analysis;
            let (code, param_defs) = match method.compiled_routine_key {
                Some(key) => {
                    let cf = self.compiled_functions.get(&key)?;
                    (&*cf.code, &cf.param_defs)
                }
                None => {
                    let params: Vec<String> =
                        method.param_defs.iter().map(|p| p.name.clone()).collect();
                    analysis = self.new_decl_chunk_compiler().compile_closure_body(
                        &params,
                        &method.param_defs,
                        &method.body,
                    );
                    (&analysis, &method.param_defs)
                }
            };
            if !lazy_body_reads_bounded(code) {
                return None;
            }
            collect_by_name_reads(code, out);
            self.param_defs_by_name_reads(param_defs, out)?;
            if let Some(ret) = &method.return_type {
                push_type_name_tokens(ret, out);
            }
        }
        Some(())
    }

    /// Fold the by-name reads of a type's attribute descriptors: their
    /// `default`/`where`/`is default` arguments, unknown-trait arguments and
    /// type names.
    // Cost: O(b), b = total ops and constants of the attribute expressions.
    fn attr_decls_by_name_reads(
        &self,
        attrs: &[(Symbol, CompiledAttrDecl)],
        out: &mut Vec<Symbol>,
    ) -> Option<()> {
        for (_, attr) in attrs {
            for (_, _, arg) in &attr.unknown_traits {
                if let Some(expr) = arg {
                    self.ast_expr_reads(expr, out)?;
                }
            }
            for arg in [&attr.default, &attr.where_constraint, &attr.is_default] {
                self.decl_arg_reads(arg.as_ref(), out)?;
            }
            for type_name in attr.type_constraint.iter().chain(&attr.is_type) {
                push_type_name_tokens(type_name, out);
            }
        }
        Some(())
    }

    /// Fold the by-name reads of a `token`/`rule` declaration: its
    /// signature, its `"..."` thunk chunks and its (static) pattern.
    // Cost: O(p + b), p = pattern length, b = signature and thunk size.
    fn token_decl_reads(
        &self,
        params: &[ParamDef],
        body: &[Stmt],
        qq_thunk_chunks: &[(Symbol, CompiledDeclExpr)],
        out: &mut Vec<Symbol>,
    ) -> Option<()> {
        self.param_defs_by_name_reads(params, out)?;
        for (_, chunk) in qq_thunk_chunks {
            chunk_reads(chunk, out)?;
        }
        token_body_reads(body, out)
    }

    /// Fold the by-name reads of a class-body statement: its precompiled
    /// chunk, or — when the statement still runs from raw AST at
    /// registration (a computed class name has no package to compile it
    /// against) — an analysis compile of the same statement.
    // Cost: O(b), b = size of the statement's compiled form.
    fn stmt_chunk_reads(
        &self,
        chunk: Option<&CompiledDeclExpr>,
        raw: &Stmt,
        out: &mut Vec<Symbol>,
    ) -> Option<()> {
        match chunk {
            Some(chunk) => chunk_reads(chunk, out),
            None => self.raw_stmt_reads(raw, &self.current_package, out),
        }
    }

    /// Fold the by-name reads of a statement run from raw AST, through an
    /// analysis compile of it under `package`: the package changes how a
    /// name qualifies, not which lexicals it reads.
    // Cost: O(b), b = size of the statement's compiled form.
    fn raw_stmt_reads(&self, stmt: &Stmt, package: &str, out: &mut Vec<Symbol>) -> Option<()> {
        let chunk = self.compile_decl_stmts_chunk_in_package(
            std::slice::from_ref(stmt),
            package,
            &HashSet::new(),
            &HashMap::new(),
            &HashSet::new(),
            None,
        );
        chunk_reads(&chunk, out)
    }

    /// Fold the by-name reads of one declaration-time argument.
    // Cost: O(b), b = ops and constants of the argument's chunk.
    fn decl_arg_reads(&self, arg: Option<&DeclTraitArg>, out: &mut Vec<Symbol>) -> Option<()> {
        match arg {
            None | Some(DeclTraitArg::Literal(_)) => Some(()),
            Some(DeclTraitArg::Compiled(chunk)) => chunk_reads(chunk, out),
            Some(DeclTraitArg::Ast(expr)) => self.ast_expr_reads(expr, out),
        }
    }

    /// Fold the by-name reads of an expression evaluated from raw AST at
    /// registration, through an analysis compile of the same expression:
    /// evaluating it reads exactly the names its compiled form reads.
    // Cost: O(e), e = size of `expr`.
    fn ast_expr_reads(&self, expr: &Expr, out: &mut Vec<Symbol>) -> Option<()> {
        chunk_reads(&self.compile_decl_expr(expr), out)
    }

    /// Fold the by-name reads of a signature's declaration-time expressions
    /// (defaults, `where` constraints, trait arguments, shape constraints,
    /// nested signatures), which are evaluated from the `ParamDef` AST at
    /// call time and never reach a compiled body's ops, plus the type names
    /// it constrains its parameters with.
    // Cost: O(p), p = total size of the signature's expressions.
    fn param_defs_by_name_reads(&self, params: &[ParamDef], out: &mut Vec<Symbol>) -> Option<()> {
        for pd in params {
            if let Some(ty) = &pd.type_constraint {
                push_type_name_tokens(ty, out);
            }
            let exprs = pd
                .default
                .iter()
                .chain(pd.where_constraint.as_deref())
                .chain(pd.trait_args.iter().map(|(_, e)| e))
                .chain(pd.shape_constraints.iter().flatten());
            for expr in exprs {
                self.ast_expr_reads(expr, out)?;
            }
            for nested in pd
                .sub_signature
                .iter()
                .chain(&pd.outer_sub_signature)
                .chain(pd.code_signature.iter().map(|(sig, _)| sig))
            {
                self.param_defs_by_name_reads(nested, out)?;
            }
        }
        Some(())
    }

    /// Resolve `names` against this frame's slots into
    /// `lazy_body_env_sync_slots` and mark the declaration plans `decl_idxs`
    /// bounded.
    // Cost: O(n * s), n = names.len(), s = slots already recorded.
    fn record_bounded_lazy_decl(&mut self, decl_idxs: &[u32], names: Vec<Symbol>) {
        for sym in names {
            let slot = sym.with_str(|s| {
                self.local_map.get(s).copied().or_else(|| {
                    // `@$x` / `%$x` record `@x` / `%x`, but the lexical is the
                    // scalar `x` (see the closure fold this mirrors).
                    s.strip_prefix(['@', '%', '&'])
                        .and_then(|bare| self.local_map.get(bare).copied())
                })
            });
            if let Some(slot) = slot
                && !self.code.lazy_body_env_sync_slots.contains(&slot)
            {
                self.code.lazy_body_env_sync_slots.push(slot);
            }
        }
        for &idx in decl_idxs {
            if !self.code.bounded_lazy_decl_plans.contains(&idx) {
                self.code.bounded_lazy_decl_plans.push(idx);
            }
        }
    }
}
