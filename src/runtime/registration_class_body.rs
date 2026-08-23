//! Named phases of `register_class_decl` (ADR-0019 D0): the class-body
//! statement walk — attribute pre-scan, the per-statement dispatch, and the
//! small statement arms (code aliases, proto methods, generic statements).
//! Pure mechanical extraction from `registration_class_decl.rs` — no
//! behavior change.

use super::registration_class::AttrValidationCtx;
use super::*;
use crate::symbol::Symbol;

/// Shared state of the class-body walk, passed by `&mut` to the per-statement
/// arms instead of threading the individual locals (ADR-0019 D0).
pub(super) struct ClassBodyCx<'a> {
    pub(super) name: &'a str,
    pub(super) is_hidden: bool,
    pub(super) class_is_rw: bool,
    pub(super) class_lang_rev: &'a str,
    pub(super) saved_package: String,
    pub(super) saved_env: crate::env::Env,
    pub(super) class_def: ClassDef,
    /// Attribute names declared directly in this class body combined with
    /// role-composed attributes: the full set of attributes valid for
    /// `$!attr` access.
    pub(super) class_own_attrs: HashSet<String>,
    /// Precompiled typed descriptor for each of this class body's own
    /// attributes (ADR-0019 D2b remainder), keyed by attribute name; see
    /// `class_body_has_decl`.
    pub(super) attr_decls: &'a [(Symbol, crate::opcode::CompiledAttrDecl)],
    /// Precompiled runtime-resolved-name chunk for each top-level `method`/
    /// `submethod` declaration in the body (ADR-0019 D3-1), read by position
    /// via `method_name_chunk_idx`; see `class_body_method_decl`.
    pub(super) method_name_chunks: &'a [Option<crate::opcode::CompiledDeclExpr>],
    /// Precompiled typed mirror of each top-level `method`/`submethod`
    /// declaration in the body (ADR-0019 D3-7), read by position via
    /// `method_name_chunk_idx`; see `class_body_method_decl`.
    pub(super) method_decls: &'a [crate::opcode::CompiledMethodDecl],
    /// Cursor into `method_name_chunks`/`method_decls`, advanced by one on
    /// every method statement `class_body_method_decl` processes.
    pub(super) method_name_chunk_idx: usize,
    /// The ambient program-wide compiled-function pool (ADR-0019 D3-8b); see
    /// `ClassDeclModifiers::compiled_fns`.
    pub(super) compiled_fns: &'a crate::opcode::CompiledFns,
    /// See [`super::registration_class::ClassDeclModifiers::is_hoisted_shell`].
    pub(super) is_hoisted_shell: bool,
}

impl ClassBodyCx<'_> {
    pub(super) fn attr_ctx(&self) -> AttrValidationCtx<'_> {
        AttrValidationCtx {
            attrs: &self.class_own_attrs,
            pkg_name: self.name,
            pkg_kind: "class",
        }
    }
}

/// Whether a body-walk arm falls through to the per-statement registry
/// re-publication tail or skips it (the arm executed a `continue` in the
/// original single-function walk).
pub(super) enum ClassBodyFlow {
    RunTail,
    SkipTail,
}

/// A collected class-body-scoped LEAVE phaser (`will leave { ... }`),
/// carrying both its raw inner body (the pre-D6-3d execution source) and its
/// precompiled chunk (ADR-0019 D6-3c/d) — see `run_class_body_leave_phasers`.
pub(super) struct ClassBodyLeavePhaser {
    pub(super) body: Vec<Stmt>,
    pub(super) chunk: Option<crate::opcode::CompiledDeclExpr>,
}

impl Interpreter {
    /// Execute the class body: make the class package current, walk every
    /// op in `body_plan` through the per-statement arms, then fire LEAVE
    /// phasers, persist class-body statics, and restore the enclosing
    /// package/lexical scope. Returns the final `ClassDef`.
    ///
    /// `body_plan` (ADR-0019 D6-3a-d) is the sole driver of this walk as of
    /// D6-4 — there is no separate raw `Vec<Stmt>` to zip it against any
    /// more. It is already `SyntheticBlock`-flattened and carries nested-sub
    /// `has` declarations as trailing `Attr` ops (`crate::opcode::class_body_plan`),
    /// so it needs no further preprocessing here. A registration path with
    /// no compiled plan (role-pun/mixin synthesis, `augment class`) passes
    /// an empty `body_plan`, and the loop below simply does nothing.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn run_class_body(
        &mut self,
        name: &str,
        class_def: ClassDef,
        is_hidden: bool,
        class_is_rw: bool,
        class_lang_rev: &str,
        own_attribute_names: &[Symbol],
        attr_decls: &[(Symbol, crate::opcode::CompiledAttrDecl)],
        method_name_chunks: &[Option<crate::opcode::CompiledDeclExpr>],
        method_decls: &[crate::opcode::CompiledMethodDecl],
        declared_static_names: &[Symbol],
        compiled_fns: &crate::opcode::CompiledFns,
        body_plan: &[crate::opcode::ClassBodyOp],
        is_hoisted_shell: bool,
    ) -> Result<ClassDef, RuntimeError> {
        let saved_package = self.current_package();
        let saved_env = self.env.clone();
        self.set_current_package(name.to_string());
        self.env
            .insert("?CLASS".to_string(), Value::package(Symbol::intern(name)));
        // The set of attributes valid for $!attr access: names declared
        // directly in this class body (precomputed by the compiler at plan
        // lowering, ADR-0019 D2a — `own_attribute_names` already covers the
        // flattened top level plus any `has` nested inside a body `sub`)
        // combined with role-composed attributes already in class_def.attributes.
        let mut class_own_attrs: HashSet<String> = class_def
            .attributes
            .iter()
            .map(|a| a.name.clone())
            .collect();
        class_own_attrs.extend(own_attribute_names.iter().map(|s| s.resolve()));
        let mut cx = ClassBodyCx {
            name,
            is_hidden,
            class_is_rw,
            class_lang_rev,
            saved_package,
            saved_env,
            class_def,
            class_own_attrs,
            attr_decls,
            method_name_chunks,
            method_decls,
            method_name_chunk_idx: 0,
            compiled_fns,
            is_hoisted_shell,
        };
        let saved_functions_keys: HashSet<String> = self
            .registry()
            .functions
            .keys()
            .map(|k| k.resolve())
            .collect();
        // LEAVE phasers declared at class-body scope (e.g. via
        // `my $x will leave { ... }`) must fire when the class body is left,
        // i.e. once all body statements have been processed. Collect them here
        // and run them (LIFO) after the loop instead of executing them inline.
        let mut class_leave_phasers: Vec<ClassBodyLeavePhaser> = Vec::new();
        for op in body_plan {
            match op {
                crate::opcode::ClassBodyOp::LeavePhaser { chunk, raw } => {
                    let Stmt::Phaser { body, .. } = raw else {
                        unreachable!("LeavePhaser op's raw statement must be Stmt::Phaser");
                    };
                    class_leave_phasers.push(ClassBodyLeavePhaser {
                        body: body.clone(),
                        chunk: chunk.clone(),
                    });
                    continue;
                }
                crate::opcode::ClassBodyOp::Attr { name } => {
                    if matches!(
                        self.class_body_has_decl(&mut cx, *name)?,
                        ClassBodyFlow::SkipTail
                    ) {
                        continue;
                    }
                }
                crate::opcode::ClassBodyOp::Method => {
                    self.class_body_method_decl(&mut cx)?;
                }
                crate::opcode::ClassBodyOp::Does {
                    name: role_name,
                    args,
                } => {
                    if matches!(
                        self.class_body_does_decl(&mut cx, *role_name, args.as_deref())?,
                        ClassBodyFlow::SkipTail
                    ) {
                        continue;
                    }
                }
                // our &baz ::= &bar  — alias a method under a new name
                crate::opcode::ClassBodyOp::CodeAlias { chunk, raw } => {
                    self.class_body_code_alias(&mut cx, raw, chunk.as_ref())?;
                }
                crate::opcode::ClassBodyOp::ProtoMethod { raw, .. } => {
                    self.class_body_proto_method_decl(&mut cx, raw)?;
                }
                crate::opcode::ClassBodyOp::TokenRule { plan } => {
                    self.register_token_decl(
                        &plan.name.resolve(),
                        &plan.params,
                        &plan.param_defs,
                        &plan.raw_body,
                        plan.multi,
                    );
                }
                crate::opcode::ClassBodyOp::ClassSub {
                    chunk,
                    raw,
                    is_swallowable,
                    is_compile_time_phaser,
                    ..
                }
                | crate::opcode::ClassBodyOp::Other {
                    chunk,
                    raw,
                    is_swallowable,
                    is_compile_time_phaser,
                } => {
                    self.class_body_other_stmt(
                        &mut cx,
                        raw,
                        chunk.as_ref(),
                        *is_swallowable,
                        *is_compile_time_phaser,
                    )?;
                }
            }
            // Check if any new functions were registered under the class package
            // during body processing (e.g., class-scoped subs).
            // Only count functions that are actual subs (not methods, which are
            // registered via MethodDecl and stored in the class methods table).
            if let crate::opcode::ClassBodyOp::ClassSub { name: sub_name, .. } = op {
                let fq = format!("{}::{}", cx.name, sub_name);
                if self.registry().functions.contains_key(&Symbol::intern(&fq))
                    && !saved_functions_keys.contains(&fq)
                {
                    self.registry_mut()
                        .class_subs
                        .entry(cx.name.to_string())
                        .or_default()
                        .insert(fq, Value::TRUE);
                }
            }
            self.registry_mut()
                .classes
                .insert(cx.name.to_string(), cx.class_def.clone());
            self.registry_mut()
                .sync_accessor_entries(Symbol::intern(cx.name));
        }
        self.run_class_body_leave_phasers(&cx, &class_leave_phasers)?;
        self.persist_class_body_statics(&cx, declared_static_names);
        self.restore_nested_type_short_names(&cx);
        self.set_current_package(cx.saved_package.clone());
        Ok(cx.class_def)
    }

    /// Run a class-body statement's side effects, using its precompiled
    /// `body_plan` chunk (ADR-0019 D6-3d) when available instead of
    /// on-the-fly compiling `stmts` via `run_block_raw` on every
    /// registration. Falls back to `run_block_raw(stmts)` for a statement
    /// kind D6-3b/c never compiles a chunk for (`token`/`rule`, the
    /// ADR-0009 carve-out) or a registration path with no compiled plan at
    /// all (role-pun/mixin synthesis, `augment class`).
    pub(super) fn run_class_body_chunk_or_raw(
        &mut self,
        chunk: Option<&crate::opcode::CompiledDeclExpr>,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        if let Some(chunk) = chunk {
            return self.run_compiled_block_raw(&chunk.code, &chunk.fns);
        }
        self.run_block_raw(stmts)
    }

    /// `our &baz ::= &bar` in a class body — alias a method under a new name.
    fn class_body_code_alias(
        &mut self,
        cx: &mut ClassBodyCx<'_>,
        stmt: &Stmt,
        chunk: Option<&crate::opcode::CompiledDeclExpr>,
    ) -> Result<(), RuntimeError> {
        let Stmt::VarDecl {
            name: var_name,
            expr: Expr::CodeVar(source_name),
            ..
        } = stmt
        else {
            unreachable!("class_body_code_alias called on a non-CodeVar VarDecl");
        };
        let alias = var_name.trim_start_matches('&').to_string();
        // The `Option<Vec<MethodDef>>` must be bound to an owned local before
        // the `if let` so the `self.registry()` read guard drops before
        // `self.registry_mut()` below -- otherwise this is a same-thread
        // recursive `RwLock` acquisition (the R8 hazard: `if let Some(x) =
        // self.registry()....cloned() { ... self.registry_mut() ... }`).
        let overloads = self.registry().user_method_overloads(cx.name, source_name);
        if let Some(overloads) = overloads {
            self.registry_mut().set_user_methods(
                Symbol::intern(cx.name),
                Symbol::intern(&alias),
                overloads,
            );
        }
        // Also execute the statement so the code variable is set
        self.registry_mut()
            .classes
            .insert(cx.name.to_string(), cx.class_def.clone());
        self.registry_mut()
            .sync_accessor_entries(Symbol::intern(cx.name));
        self.run_class_body_chunk_or_raw(chunk, std::slice::from_ref(stmt))?;
        for outer_name in cx.saved_env.keys() {
            let class_scoped_name = format!("{}::{}", cx.name, outer_name);
            if let Some(updated) = self.env.get(&class_scoped_name).cloned() {
                self.env.insert_sym(*outer_name, updated);
            }
        }
        if let Some(updated) = self.registry().classes.get(cx.name).cloned() {
            cx.class_def = updated;
        }
        Ok(())
    }

    /// `proto method` declaration in a class body.
    fn class_body_proto_method_decl(
        &mut self,
        cx: &mut ClassBodyCx<'_>,
        stmt: &Stmt,
    ) -> Result<(), RuntimeError> {
        let Stmt::ProtoDecl {
            name: proto_name,
            param_defs,
            return_type,
            body: proto_body,
            is_method: true,
            ..
        } = stmt
        else {
            unreachable!("class_body_proto_method_decl called on a non-proto-method statement");
        };
        let method_name = proto_name.resolve();
        let effective_param_defs =
            crate::method_signature_shared::effective_method_param_defs(param_defs, false);
        let effective_params: Vec<String> = effective_param_defs
            .iter()
            .map(|p| p.name.clone())
            .collect();
        let fdef = FunctionDef {
            package: Symbol::intern(cx.name),
            name: *proto_name,
            params: effective_params,
            param_defs: effective_param_defs,
            body: proto_body.clone(),
            is_test_assertion: false,
            is_rw: false,
            is_raw: false,
            is_method: true,
            empty_sig: false,
            is_stub: Self::is_stub_routine_body(proto_body),
            return_type: return_type.clone(),
            is_default: false,
            deprecated_message: None,
            source_file: self.current_source_file(),
            decl_order: crate::runtime::resolution::next_decl_order(),
            compiled: None,
            body_fp_cache: std::sync::OnceLock::new(),
            body_facts_cache: std::sync::OnceLock::new(),
            rw_tail_expr: None,
        };
        self.registry_mut()
            .set_proto_method(cx.name, &method_name, fdef);
        Ok(())
    }

    /// Any other class-body statement: validate anonymous-method attribute
    /// references, then execute the statement with the class visible in the
    /// registry (swallowing BEGIN/EVAL failures so the class still registers).
    fn class_body_other_stmt(
        &mut self,
        cx: &mut ClassBodyCx<'_>,
        stmt: &Stmt,
        chunk: Option<&crate::opcode::CompiledDeclExpr>,
        is_swallowable: bool,
        is_compile_time_phaser: bool,
    ) -> Result<(), RuntimeError> {
        // An anonymous method (`method { $!x }`) parses as an
        // `AnonSubParams` expression statement (with an implicit
        // `self` param), NOT a `MethodDecl`, so it bypasses the
        // named-method validation above. Validate its body too so an
        // undeclared attribute inside it still raises
        // X::Attribute::Undeclared at composition time.
        if let Stmt::Expr(Expr::AnonSubParams { params, body, .. }) = stmt
            && params.first().map(String::as_str) == Some("self")
        {
            Self::validate_attr_declared_in_class(&cx.attr_ctx(), body)?;
        }
        // BEGIN phasers and EVAL calls in class bodies may fail
        // (e.g. `BEGIN EVAL q[has $.x]` or `EVAL q[has $.x]`).
        // Swallow errors from these so the class still registers.
        // `is_swallowable` is precomputed at plan-classification time
        // (`crate::opcode::is_swallowable_class_body_stmt`, ADR-0019 D10
        // follow-up) instead of re-matching `stmt`'s shape here.
        self.registry_mut()
            .classes
            .insert(cx.name.to_string(), cx.class_def.clone());
        self.registry_mut()
            .sync_accessor_entries(Symbol::intern(cx.name));
        // Mark this class as "being defined" so a `has`-attribute
        // declaration executed by a *compile-time* phaser
        // (`class Foo { BEGIN EVAL q[has $.x] }`) attaches to it
        // instead of throwing X::Attribute::NoPackage. Restored (not
        // cleared) to support nested class-in-BEGIN definitions.
        // Only BEGIN/CHECK phasers qualify: a *plain* runtime `EVAL
        // q[has $.x]` runs after the class is composed, so its
        // attribute must NOT take (roast S12-attributes/class.t
        // test 21 asserts `.x` then throws X::Method::NotFound).
        // `is_compile_time_phaser` is precomputed the same way
        // (`crate::opcode::is_compile_time_phaser_stmt`).
        let saved_defining = if is_compile_time_phaser {
            Some(self.defining_class.replace(cx.name.to_string()))
        } else {
            None
        };
        let result = self.run_class_body_chunk_or_raw(chunk, std::slice::from_ref(stmt));
        if let Some(saved) = saved_defining {
            self.defining_class = saved;
        }
        if let Err(e) = result {
            if !is_swallowable {
                return Err(e);
            }
        } else {
            for outer_name in cx.saved_env.keys() {
                let class_scoped_name = format!("{}::{}", cx.name, outer_name);
                if let Some(updated) = self.env.get(&class_scoped_name).cloned() {
                    self.env.insert_sym(*outer_name, updated);
                }
            }
        }
        if let Some(updated) = self.registry().classes.get(cx.name).cloned() {
            cx.class_def = updated;
        }
        Ok(())
    }
}
