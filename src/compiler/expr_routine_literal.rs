use super::*;
use crate::symbol::Symbol;

/// Find expression-position declarations inside a synthesized WhateverCode.
/// The generated callable is transparent for lexical scoping, while explicit
/// source closures/blocks remain boundaries of their own.
// Cost: O(n), n = size of `body` outside nested scopes.
pub(super) fn collect_whatever_expr_decls(
    body: &[Stmt],
    out: &mut std::collections::HashSet<String>,
) {
    struct DeclScan<'a> {
        out: &'a mut std::collections::HashSet<String>,
    }
    impl<'ast> crate::ast_visit::Visit<'ast> for DeclScan<'_> {
        fn visit_stmt(&mut self, stmt: &'ast Stmt) {
            if let Stmt::VarDecl { name, is_our, .. } = stmt
                && !*is_our
            {
                self.out.insert(name.clone());
            }
            crate::ast::scope_scan::walk_stmt_own_scope(self, stmt);
        }
        fn visit_expr(&mut self, expr: &'ast Expr) {
            if !crate::ast::scope_scan::opens_own_scope(expr) {
                crate::ast_visit::walk_expr(self, expr);
            }
        }
    }
    let mut scan = DeclScan { out };
    // A WhateverCode body is the curried expression; its statements are only
    // that expression and line markers.
    for stmt in body {
        if let Stmt::Expr(e) = stmt {
            crate::ast_visit::Visit::visit_expr(&mut scan, e);
        }
    }
}

impl Compiler {
    /// Whether `name` is an implicit-slurpy spelling (`@_` / `%_`) that an
    /// enclosing scope declares as a lexical -- `sub f(*%_) { { %_ } }` -- so a
    /// nested block reads that variable instead of taking it as a placeholder.
    fn implicit_slurpy_is_outer_lexical(&self, name: &str) -> bool {
        matches!(name, "@_" | "%_")
            && (self.local_map.contains_key(name) || self.enclosing_local_names.contains(name))
    }

    /// Compile AnonSubParams expression.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn compile_expr_anon_sub_params(
        &mut self,
        params: &[String],
        param_defs: &[crate::ast::ParamDef],
        return_type: &Option<String>,
        body: &[Stmt],
        is_rw: bool,
        is_raw: bool,
        custom_traits: &[(String, Option<crate::ast::Expr>)],
        is_whatever_code: bool,
        declarator: crate::ast::RoutineDeclarator,
        routine_name: Symbol,
        prototype: Option<Symbol>,
    ) {
        // A parameter type naming a `constant` alias is its target, as in a
        // sub's signature (`param_defs_with_type_aliases_resolved`, #13728).
        let resolved_defs = self.param_defs_with_type_aliases_resolved(param_defs);
        let param_defs = resolved_defs.as_deref().unwrap_or(param_defs);
        let method_literal = matches!(
            declarator,
            crate::ast::RoutineDeclarator::Method | crate::ast::RoutineDeclarator::Submethod
        );
        let method_defs;
        let method_params;
        let (params, param_defs) = if method_literal {
            method_defs = crate::method_signature_shared::effective_method_param_defs(
                param_defs,
                self.static_class_hidden,
            );
            method_params = method_defs
                .iter()
                .map(|pd| pd.name.clone())
                .collect::<Vec<_>>();
            (method_params.as_slice(), method_defs.as_slice())
        } else {
            (params, param_defs)
        };
        let declarator_doc = self.pending_declarator_doc.take().map(std::sync::Arc::new);
        // The parser synthesizes `*%_` for a signature-less block that reads
        // `%_`. Inside a method, however, an ordinary nested block captures
        // the method's implicit `%_`; it does not shadow it with a new empty
        // hash. RakuAST lowering resolves the same method binding; an unbound
        // placeholder keeps `block_param` false, while a read resolved to the
        // method's hash follows this inferred-parameter capture path.
        if declarator == crate::ast::RoutineDeclarator::Block
            && self.lexically_in_method
            && params.len() == 1
            && params[0] == "%_"
            && param_defs.len() == 1
            && param_defs[0].name == "%_"
            && param_defs[0].slurpy
            && param_defs[0].block_param
            && crate::ast::body_reads_args_hash(body)
        {
            let saved = self.capture_enclosing_method_named_args;
            self.capture_enclosing_method_named_args = true;
            self.compile_expr_anon_sub(body, false, true);
            self.capture_enclosing_method_named_args = saved;
            return;
        }
        // Likewise outside a method: the parser synthesizes `*@_`/`*%_` for a
        // block that reads `@_`/`%_`, but when an enclosing scope declares that
        // name (`sub f(@a, *%_) { @a.map({ to-json $_, |%_ }) }`) the block
        // reads the outer lexical and keeps the default `$_` signature, as in
        // rakudo -- it is not a placeholder at all (JSON::Fast::Hyper).
        if declarator == crate::ast::RoutineDeclarator::Block
            && !params.is_empty()
            && params.len() == param_defs.len()
            && param_defs.iter().all(|pd| {
                matches!(pd.name.as_str(), "@_" | "%_")
                    && pd.slurpy
                    && pd.block_param
                    && self.implicit_slurpy_is_outer_lexical(&pd.name)
            })
        {
            self.compile_expr_anon_sub(body, false, true);
            return;
        }
        // Validate for placeholder conflicts
        if let Some(err_val) = self.check_placeholder_conflicts(params, body, None) {
            let idx = self.code.add_constant(err_val);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die { user_throw: false });
            return;
        }
        // Placeholders cannot appear in blocks/subs with explicit signatures.
        // A WhateverCode is exempt: it was synthesized from `*` and owns only its
        // `*`-derived params, so a `$^name` in its body (e.g. the `$^namespace` in
        // `{ ... $^namespace ~ * => * }`) is a free variable belonging to the
        // enclosing explicit block, not an override of the WhateverCode's own
        // signature.
        if !is_whatever_code {
            let is_placeholder_param = |p: &str| {
                let name = p
                    .strip_prefix('&')
                    .or_else(|| p.strip_prefix('@'))
                    .or_else(|| p.strip_prefix('%'))
                    .unwrap_or(p);
                name.starts_with('^') || name.starts_with(':')
            };
            let has_explicit_sig =
                params.is_empty() || params.iter().all(|p| !is_placeholder_param(p));
            // Only placeholders belonging to THIS block conflict with its
            // signature. A nested block owns the placeholders written inside it
            // (`-> $b, $i { ({ $^a + $^b }, { $^a * $^b })[$i](1, 2) }` is legal),
            // so the scan must stop at every nested `{}` — the deep
            // `collect_placeholders` used to report the inner block's own `$^a`
            // as an override of the outer signature. This is the same collector
            // the `sub` declaration check uses (`placeholder_overrides_signature_error`),
            // so — as there — the implicit slurpies it reports (`@_` / `%_`) are
            // legal when explicitly declared as parameters (`-> @_ { … @_ }`).
            let declared: std::collections::HashSet<&str> =
                params.iter().map(String::as_str).collect();
            let body_placeholders: Vec<String> = crate::ast::collect_unattached_placeholders(body)
                .into_iter()
                .filter(|ph| !declared.contains(ph.as_str()))
                .filter(|ph| !self.implicit_slurpy_is_outer_lexical(ph))
                .collect();
            if has_explicit_sig && !body_placeholders.is_empty() {
                // `collect_unattached_placeholders` already returns display names
                // (`$^x`, `@_`).
                let display = body_placeholders[0].clone();
                let mut attrs = std::collections::HashMap::new();
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!(
                        "Placeholder variable '{}' cannot override existing signature",
                        display
                    )),
                );
                attrs.insert("placeholder".to_string(), Value::str(display));
                let err = Value::make_instance(
                    crate::symbol::Symbol::intern("X::Signature::Placeholder"),
                    attrs,
                );
                let idx = self.code.add_constant(err);
                self.code.emit(OpCode::LoadConst(idx));
                self.code.emit(OpCode::Die { user_throw: false });
                return;
            }
        }
        // Compile-time check: assignment to native-typed read-only params
        if let Some(err_val) = Self::check_native_readonly_param_assignment(param_defs, body) {
            let idx = self.code.add_constant(err_val);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die { user_throw: false });
            return;
        }
        // Check if this is a pointy block (-> { }) vs an anonymous routine.
        //
        // `declarator` is the source spelling the parser recorded: a routine
        // spelling only when `sub`, `method` or `submethod` was actually
        // written. Every other closure that lands on this node -- a pointy
        // block, a PLACEHOLDER block (`{ $^a }`) -- is a `Block` in raku, so it
        // must take the block path: `Mu`-typed params, no `return` boundary,
        // and a `Block` gist (`-> $a { ... }`, not `sub ($^a) { ... }`). A
        // `method (...) { }` literal is NOT one of them: raku makes it a
        // `Method`, which is a `Routine` — `return` returns from it and
        // `.^name` answers `Method`.
        //
        // Every construction site records the spelling (the synthesized
        // closures record `Block`), so no body-shape guess is needed: a
        // routine's body starts with a `SetLine` marker as any multi-line
        // block's does (#13627).
        let is_pointy = !declarator.is_routine();
        // Block params have Mu (not Any) as their implicit nominal type;
        // mark them here so an unpassed untyped optional seeds Mu.
        let marked_defs: Vec<crate::ast::ParamDef>;
        let param_defs: &[crate::ast::ParamDef] = if is_pointy {
            marked_defs = param_defs
                .iter()
                .map(|pd| {
                    let mut pd = pd.clone();
                    pd.mark_block_param();
                    pd
                })
                .collect();
            &marked_defs
        } else {
            param_defs
        };
        // Pointy blocks are NOT routine boundaries for `return`.
        let mut promoted_decls = std::collections::HashSet::new();
        if is_whatever_code {
            collect_whatever_expr_decls(body, &mut promoted_decls);
        }
        let mut promoted_decls: Vec<String> = promoted_decls
            .into_iter()
            .filter(|name| {
                self.local_map.contains_key(name) || self.enclosing_local_names.contains(name)
            })
            .collect();
        promoted_decls.sort();
        for name in &promoted_decls {
            let slot = self.declare_local(name);
            self.record_block_decl(name);
            self.code.emit(OpCode::LoadNil);
            self.code.emit(OpCode::MarkVarDeclContext);
            self.code.emit(OpCode::SetLocal(slot));
        }
        let saved_method_context = self.lexically_in_method;
        self.lexically_in_method |= method_literal;
        let mut compiled = if is_pointy {
            self.compile_closure_body(params, param_defs, body)
        } else if is_whatever_code {
            self.compile_closure_body_with_promoted_decls(params, param_defs, body, &promoted_decls)
        } else {
            // An anonymous `sub (...) is rw`/`is raw { ... }` hands out its
            // tail's container (ADR-0059 Slice 2); a pointy block's `is_rw` is
            // a loop-parameter trait and does not reach here.
            self.compile_routine_closure_body(params, param_defs, body, is_rw || is_raw)
        };
        self.lexically_in_method = saved_method_context;
        if is_pointy {
            compiled.is_pointy_block = true;
        }
        compiled.is_whatever_code = is_whatever_code;
        compiled.declarator_doc = declarator_doc;
        let esc = self.escaping_position;
        let cc_idx = self.add_closure_code_baked(compiled, esc);
        let idx = self.code.add_stmt(Stmt::SubDecl {
            spans: Default::default(),
            name: routine_name,
            name_expr: None,
            params: params.to_vec(),
            param_defs: param_defs.to_vec(),
            return_type: return_type.clone(),
            associativity: None,
            precedence_trait: None,
            signature_alternates: Vec::new(),
            body: body.to_vec(),
            multi: false,
            is_rw,
            is_raw,
            is_export: false,
            export_tags: Vec::new(),
            is_test_assertion: false,
            supersede: false,
            // A `method`/`submethod` literal is a Routine like a `sub`, but it
            // reports its own type. The declarator rides the pooled decl as a
            // `__mutsu_callable_type::<Type>` marker, which is what the
            // closure-building opcode installs in the captured environment —
            // the same marker a class-body method declaration sets.
            custom_traits: {
                let mut traits = custom_traits.to_vec();
                if let Some(marker) = declarator.literal_marker() {
                    traits.push((marker.to_string(), None));
                }
                traits
            },
        });
        self.code.emit(OpCode::MakeAnonSubParams(
            idx,
            Some(cc_idx),
            is_whatever_code,
            prototype,
        ));
    }
}
