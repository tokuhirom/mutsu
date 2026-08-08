//! Named phases of `register_class_decl` (ADR-0019 D0): the class-body
//! statement walk — attribute pre-scan, the per-statement dispatch, and the
//! small statement arms (code aliases, proto methods, generic statements).
//! Pure mechanical extraction from `registration_class_decl.rs` — no
//! behavior change.

use super::registration_class::AttrValidationCtx;
use super::*;
use crate::ast::PhaserKind;
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
    /// Precompiled `is default(...)` chunks for this class body's own
    /// attributes (ADR-0019 D2c), keyed by attribute name; see
    /// `class_body_has_decl`.
    pub(super) is_default_chunks: &'a [(Symbol, crate::opcode::DeclTraitArg)],
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

impl Interpreter {
    /// Recursively surface `has`-attribute declarations nested inside a sub/block
    /// within a class body (`class C { sub f { has $.x } }`). Descends into
    /// `sub`/block bodies but NOT into a nested `class`/`role` (which owns its own
    /// attribute scope). Collects only `HasDecl` statements.
    fn collect_nested_class_has_decls<'a>(stmts: &'a [Stmt], out: &mut Vec<&'a Stmt>) {
        for s in stmts {
            match s {
                // Own attribute scope / already collected at the top level.
                Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } | Stmt::HasDecl { .. } => {}
                Stmt::SubDecl { body, .. } => {
                    for inner in body {
                        if matches!(inner, Stmt::HasDecl { .. }) {
                            out.push(inner);
                        }
                    }
                    Self::collect_nested_class_has_decls(body, out);
                }
                _ => {}
            }
        }
    }

    /// Execute the class body: make the class package current, walk every
    /// (flattened) body statement through the per-statement arms, then fire
    /// LEAVE phasers, persist class-body statics, and restore the enclosing
    /// package/lexical scope. Returns the final `ClassDef`.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn run_class_body(
        &mut self,
        name: &str,
        body: &[Stmt],
        class_def: ClassDef,
        is_hidden: bool,
        class_is_rw: bool,
        class_lang_rev: &str,
        own_attribute_names: &[Symbol],
        is_default_chunks: &[(Symbol, crate::opcode::DeclTraitArg)],
        method_name_chunks: &[Option<crate::opcode::CompiledDeclExpr>],
        method_decls: &[crate::opcode::CompiledMethodDecl],
    ) -> Result<ClassDef, RuntimeError> {
        let saved_package = self.current_package();
        let saved_env = self.env.clone();
        self.set_current_package(name.to_string());
        self.env
            .insert("?CLASS".to_string(), Value::package(Symbol::intern(name)));
        // Flatten SyntheticBlock (from `has ($a, $b)` list form) so inner
        // HasDecl statements are processed at the top level.
        let mut flattened_body: Vec<&Stmt> = body
            .iter()
            .flat_map(|s| match s {
                Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
                other => vec![other],
            })
            .collect();
        // An attribute can also be declared inside a nested sub/block within the
        // class body (`class C { sub f { has $.x } }`) — Rakudo still registers it
        // on the class. Surface those `has` declarations so the main loop below
        // registers the attribute (the nested sub itself is still processed
        // normally; its body's `has` is a compile-time declaration).
        Self::collect_nested_class_has_decls(body, &mut flattened_body);
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
            is_default_chunks,
            method_name_chunks,
            method_decls,
            method_name_chunk_idx: 0,
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
        let mut class_leave_phasers: Vec<Vec<Stmt>> = Vec::new();
        for stmt in flattened_body {
            match stmt {
                Stmt::Phaser {
                    kind: PhaserKind::Leave,
                    body,
                } => {
                    class_leave_phasers.push(body.clone());
                }
                Stmt::HasDecl { .. } => {
                    if matches!(
                        self.class_body_has_decl(&mut cx, stmt)?,
                        ClassBodyFlow::SkipTail
                    ) {
                        continue;
                    }
                }
                Stmt::MethodDecl { .. } => {
                    self.class_body_method_decl(&mut cx)?;
                }
                Stmt::DoesDecl { .. } => {
                    if matches!(
                        self.class_body_does_decl(&mut cx, stmt)?,
                        ClassBodyFlow::SkipTail
                    ) {
                        continue;
                    }
                }
                Stmt::TrustsDecl {
                    name: trusted_class,
                } => {
                    self.registry_mut()
                        .class_trusts
                        .entry(cx.name.to_string())
                        .or_default()
                        .insert(trusted_class.resolve());
                }
                // our &baz ::= &bar  — alias a method under a new name
                Stmt::VarDecl {
                    name: var_name,
                    expr: Expr::CodeVar(_),
                    ..
                } if var_name.starts_with('&') => {
                    self.class_body_code_alias(&mut cx, stmt)?;
                }
                Stmt::ProtoDecl {
                    is_method: true, ..
                } => {
                    self.class_body_proto_method_decl(&mut cx, stmt)?;
                }
                _ => {
                    self.class_body_other_stmt(&mut cx, stmt)?;
                }
            }
            // Check if any new functions were registered under the class package
            // during body processing (e.g., class-scoped subs).
            // Only count functions that are actual subs (not methods, which are
            // registered via MethodDecl and stored in the class methods table).
            if let Stmt::SubDecl { name: sub_name, .. } = stmt {
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
            self.registry_mut().sync_user_method_entries(cx.name);
        }
        self.run_class_body_leave_phasers(&cx, &class_leave_phasers)?;
        self.persist_class_body_statics(&cx, body);
        self.restore_nested_type_short_names(&cx);
        self.set_current_package(cx.saved_package.clone());
        Ok(cx.class_def)
    }

    /// `our &baz ::= &bar` in a class body — alias a method under a new name.
    fn class_body_code_alias(
        &mut self,
        cx: &mut ClassBodyCx<'_>,
        stmt: &Stmt,
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
        if let Some(overloads) = cx.class_def.methods.get(source_name).cloned() {
            cx.class_def.methods.insert(alias, overloads);
        }
        // Also execute the statement so the code variable is set
        self.registry_mut()
            .classes
            .insert(cx.name.to_string(), cx.class_def.clone());
        self.registry_mut().sync_user_method_entries(cx.name);
        self.run_block_raw(std::slice::from_ref(stmt))?;
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
            body: proto_body,
            is_method: true,
            ..
        } = stmt
        else {
            unreachable!("class_body_proto_method_decl called on a non-proto-method statement");
        };
        let method_name = proto_name.resolve();
        let effective_param_defs = Self::effective_method_param_defs(param_defs, false);
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
            return_type: None,
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
            .proto_methods
            .insert((cx.name.to_string(), method_name), fdef);
        Ok(())
    }

    /// Any other class-body statement: validate anonymous-method attribute
    /// references, then execute the statement with the class visible in the
    /// registry (swallowing BEGIN/EVAL failures so the class still registers).
    fn class_body_other_stmt(
        &mut self,
        cx: &mut ClassBodyCx<'_>,
        stmt: &Stmt,
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
        let is_swallowable = matches!(
            stmt,
            Stmt::Phaser {
                kind: PhaserKind::Begin,
                ..
            }
        ) || matches!(
            stmt,
            Stmt::Call { name: fn_name, .. }
                if fn_name.resolve() == "EVAL"
        ) || matches!(
            stmt,
            Stmt::Expr(Expr::Call { name: fn_name, .. })
                if fn_name.resolve() == "EVAL"
        );
        self.registry_mut()
            .classes
            .insert(cx.name.to_string(), cx.class_def.clone());
        self.registry_mut().sync_user_method_entries(cx.name);
        // Mark this class as "being defined" so a `has`-attribute
        // declaration executed by a *compile-time* phaser
        // (`class Foo { BEGIN EVAL q[has $.x] }`) attaches to it
        // instead of throwing X::Attribute::NoPackage. Restored (not
        // cleared) to support nested class-in-BEGIN definitions.
        // Only BEGIN/CHECK phasers qualify: a *plain* runtime `EVAL
        // q[has $.x]` runs after the class is composed, so its
        // attribute must NOT take (roast S12-attributes/class.t
        // test 21 asserts `.x` then throws X::Method::NotFound).
        let is_compile_time_phaser = matches!(
            stmt,
            Stmt::Phaser {
                kind: PhaserKind::Begin | PhaserKind::Check,
                ..
            }
        );
        let saved_defining = if is_compile_time_phaser {
            Some(self.defining_class.replace(cx.name.to_string()))
        } else {
            None
        };
        let result = self.run_block_raw(std::slice::from_ref(stmt));
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
