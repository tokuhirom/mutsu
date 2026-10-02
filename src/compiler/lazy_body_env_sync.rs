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
//! [`Compiler::note_role_decl_env_sync`].

use super::Compiler;
use crate::ast::ParamDef;
use crate::opcode::{
    ClassBodyOp, CompiledAttrDecl, CompiledClassDeclPlan, CompiledCode, CompiledDeclExpr,
    CompiledMethodDecl, CompiledRoleDeclPlan, DeclTraitArg, DeferredBodyOpKind, OpCode,
};
use std::collections::{HashMap, HashSet};
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

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
    /// and the type names the header and signatures mention. Anything
    /// evaluated from raw AST at registration, a computed class or method
    /// name, and a `token`/`rule` body leave the plan unbounded.
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
        let Some(names) = self.class_plan_by_name_reads(plan) else {
            return;
        };
        self.record_bounded_lazy_decl(&[decl_idx], names);
    }

    /// Every name the registration of `plan` (and each method it installs)
    /// may resolve by name in the declaring frame's env, or `None` when one
    /// of them is beyond enumeration.
    // Cost: O(b), b as in `note_class_decl_env_sync`.
    fn class_plan_by_name_reads(&self, plan: &CompiledClassDeclPlan) -> Option<Vec<Symbol>> {
        if plan.name_chunk.is_some() {
            return None;
        }
        let mut names: Vec<Symbol> = Vec::new();
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
            decl_arg_reads(arg.as_ref(), &mut names)?;
        }
        for (_, args) in &plan.parent_arg_chunks {
            for arg in args {
                decl_arg_reads(Some(arg), &mut names)?;
            }
        }
        attr_decls_by_name_reads(&plan.attr_decls, &mut names)?;
        self.methods_by_name_reads(&plan.method_name_chunks, &plan.method_decls, &mut names)?;
        for op in &plan.body_plan {
            match op {
                ClassBodyOp::Attr { .. } | ClassBodyOp::Method => {}
                ClassBodyOp::Does { name, args } => {
                    name.with_str(|s| push_type_name_tokens(s, &mut names));
                    for arg in args.iter().flatten() {
                        decl_arg_reads(Some(arg), &mut names)?;
                    }
                }
                ClassBodyOp::ClassSub {
                    chunk, hoist_chunk, ..
                } => {
                    chunk_reads(chunk.as_ref()?, &mut names)?;
                    if let Some(hoist) = hoist_chunk {
                        chunk_reads(hoist, &mut names)?;
                    }
                }
                ClassBodyOp::CodeAlias { chunk, .. }
                | ClassBodyOp::ProtoMethod { chunk, .. }
                | ClassBodyOp::LeavePhaser { chunk, .. }
                | ClassBodyOp::Other { chunk, .. } => chunk_reads(chunk.as_ref()?, &mut names)?,
                ClassBodyOp::TokenRule { .. } => return None,
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
        let Some(names) = self.role_plan_by_name_reads(plan) else {
            return;
        };
        self.record_bounded_lazy_decl(&[decl_idx], names);
    }

    /// Every name the registration and compositions of the role `plan` may
    /// resolve by name in the declaring frame's env, or `None` when one of
    /// them is beyond enumeration.
    // Cost: O(b), b as in `note_role_decl_env_sync`.
    fn role_plan_by_name_reads(&self, plan: &CompiledRoleDeclPlan) -> Option<Vec<Symbol>> {
        let mut names: Vec<Symbol> = Vec::new();
        self.param_defs_by_name_reads(&plan.type_param_defs, &mut names)?;
        for (_, arg) in &plan.custom_traits {
            decl_arg_reads(arg.as_ref(), &mut names)?;
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
                decl_arg_reads(Some(arg), &mut names)?;
            }
        }
        attr_decls_by_name_reads(&plan.attr_decls, &mut names)?;
        self.methods_by_name_reads(&plan.method_name_chunks, &plan.method_decls, &mut names)?;
        let package = self.qualified_role_decl_name(&plan.name.resolve());
        for op in &plan.deferred_body_ops {
            match (op.kind, &op.chunk) {
                (DeferredBodyOpKind::TokenRule, _) => return None,
                (_, Some(chunk)) => chunk_reads(chunk, &mut names)?,
                (_, None) => {
                    let chunk = self.compile_decl_stmts_chunk_in_package(
                        std::slice::from_ref(&op.raw),
                        &package,
                        &HashSet::new(),
                        &HashMap::new(),
                        &HashSet::new(),
                    );
                    chunk_reads(&chunk, &mut names)?;
                }
            }
        }
        Some(names)
    }

    /// Fold the by-name reads of a type's top-level `method`/`submethod`
    /// declarations: each compiled body, its signature's declaration-time
    /// expressions and its return type. `None` for a computed method name,
    /// a trait argument, or a body that was not compiled.
    // Cost: O(b), b = total ops and constants of the method bodies.
    fn methods_by_name_reads(
        &self,
        name_chunks: &[Option<CompiledDeclExpr>],
        methods: &[CompiledMethodDecl],
        out: &mut Vec<Symbol>,
    ) -> Option<()> {
        if name_chunks.iter().any(Option::is_some) {
            return None;
        }
        for method in methods {
            if method.name_expr.is_some()
                || method.custom_traits.iter().any(|(_, arg)| arg.is_some())
            {
                return None;
            }
            let cf = self.compiled_functions.get(&method.compiled_routine_key?)?;
            if !lazy_body_reads_bounded(&cf.code) {
                return None;
            }
            collect_by_name_reads(&cf.code, out);
            self.param_defs_by_name_reads(&cf.param_defs, out)?;
            if let Some(ret) = &method.return_type {
                push_type_name_tokens(ret, out);
            }
        }
        Some(())
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
                let chunk = self.compile_decl_expr(expr);
                chunk_reads(&chunk, out)?;
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

/// Fold the by-name reads of a type's attribute descriptors: their
/// `default`/`where`/`is default` chunks and type names. `None` for an
/// unknown trait with an argument (evaluated from raw AST).
// Cost: O(b), b = total ops and constants of the attribute chunks.
fn attr_decls_by_name_reads(
    attrs: &[(Symbol, CompiledAttrDecl)],
    out: &mut Vec<Symbol>,
) -> Option<()> {
    for (_, attr) in attrs {
        if attr.unknown_traits.iter().any(|(_, _, arg)| arg.is_some()) {
            return None;
        }
        for arg in [&attr.default, &attr.where_constraint, &attr.is_default] {
            decl_arg_reads(arg.as_ref(), out)?;
        }
        for type_name in attr.type_constraint.iter().chain(&attr.is_type) {
            push_type_name_tokens(type_name, out);
        }
    }
    Some(())
}

/// Fold the by-name reads of one declaration-time argument; `None` when it is
/// evaluated from raw AST (no compiled body to scan).
// Cost: O(b), b = ops and constants of the argument's chunk.
fn decl_arg_reads(arg: Option<&DeclTraitArg>, out: &mut Vec<Symbol>) -> Option<()> {
    match arg {
        None | Some(DeclTraitArg::Literal(_)) => Some(()),
        Some(DeclTraitArg::Compiled(chunk)) => chunk_reads(chunk, out),
        Some(DeclTraitArg::Ast(_)) => None,
    }
}

/// Fold the by-name reads of a compiled declaration chunk; `None` when it
/// reads names no op scan can bound.
// Cost: O(b), b = ops and constants of `chunk` and its nested closures.
fn chunk_reads(chunk: &CompiledDeclExpr, out: &mut Vec<Symbol>) -> Option<()> {
    if !lazy_body_reads_bounded(&chunk.code) {
        return None;
    }
    collect_by_name_reads(&chunk.code, out);
    Some(())
}

/// Push every identifier in a type-name spelling (`Foo::Bar[Int]:D`, a
/// parent string with bracketed arguments) as a by-name read: a lexical type
/// or constant the declaration names (`my constant T = Int; class C is T`)
/// is looked up by name at registration. Over-approximating with unrelated
/// tokens only keeps an extra mirror live.
// Cost: O(n), n = type_name.len().
fn push_type_name_tokens(type_name: &str, out: &mut Vec<Symbol>) {
    let mut push = |token: &str| {
        if token.is_empty() {
            return;
        }
        let sym = Symbol::intern(token);
        if !out.contains(&sym) {
            out.push(sym);
        }
    };
    let is_name_char = |c: char| c.is_alphanumeric() || matches!(c, '_' | '-' | '\'' | ':');
    for raw in type_name.split(|c: char| !is_name_char(c)) {
        push(raw.trim_matches(':'));
        for part in raw.split(':') {
            push(part);
        }
    }
}

/// Whether every by-name read of `code` (and of each closure nested in it)
/// is visible to [`collect_by_name_reads`]. An interpolating or indirect
/// regex, a dynamic substitution replacement, a deferred phaser, and a
/// nested declaration that is not itself a bounded sub all resolve names the
/// op scan cannot enumerate.
// Cost: O(b), b = total ops and constants of `code` and its nested closures.
fn lazy_body_reads_bounded(code: &CompiledCode) -> bool {
    let own = code.ops.iter().all(|op| match op {
        // Only a bounded SUB: a nested class's body-statement and type-name
        // reads are not folded into the enclosing body's `free_var_syms`, so
        // they would be lost on the way out.
        OpCode::RegisterDecl(idx) => {
            matches!(
                code.decl_plans.get(*idx as usize),
                Some(crate::opcode::CompiledDeclPlanRef::Sub(_))
            ) && code.bounded_lazy_decl_plans.contains(idx)
        }
        OpCode::PhaserEnd { .. } | OpCode::CheckPhaser { .. } => false,
        _ => true,
    }) && !code.holds_interpolating_regex()
        && !code.holds_dynamic_substitution()
        && !code.holds_indirect_regex_lookup();
    own && code
        .closure_compiled_codes
        .iter()
        .all(|c| lazy_body_reads_bounded(c))
}

/// Every name `code` may resolve by name in an enclosing frame's env: its
/// free reads and writes (which already include its nested closures', nested
/// routines' and `gather`/`whenever` bodies'), the rw-arg-sink targets, and —
/// at any closure depth — the scalars it mutates in place and the bare
/// callee names (a call `e()` colliding with an outer `my $e` reads `env[e]`).
// Cost: O(b), b = total ops of `code` and its nested closures.
fn collect_by_name_reads(code: &CompiledCode, out: &mut Vec<Symbol>) {
    let mut push = |sym: Symbol| {
        if !out.contains(&sym) {
            out.push(sym);
        }
    };
    for sym in code
        .free_var_syms
        .iter()
        .chain(&code.free_var_writes)
        .chain(&code.free_var_container_writes)
        .chain(&code.rw_arg_env_sync_syms)
    {
        push(*sym);
    }
    for op in &code.ops {
        let idx = code
            .op_container_mutate_const_idx(op)
            .or_else(|| CompiledCode::op_callee_name_const_idx(op));
        if let Some(idx) = idx
            && let Some(ValueView::Str(name)) = code.constants.get(idx as usize).map(Value::view)
            && !code.locals.iter().any(|l| l.as_str() == name.as_str())
        {
            push(Symbol::intern(name.as_str()));
        }
    }
    for nested in &code.closure_compiled_codes {
        collect_by_name_reads(nested, out);
    }
}
