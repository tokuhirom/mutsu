//! Named phases of `register_class_decl` (ADR-0019 D0): the `method` /
//! `submethod` arm of the class-body walk. Pure mechanical extraction from
//! `registration_class_decl.rs` — no behavior change.

use super::registration_class::make_delegation_method;
use super::registration_class_body::ClassBodyCx;
use super::registration_class_body_method_forms::method_sub_form_params;
use super::*;
use crate::ast::HandleSpec;
use crate::symbol::Symbol;

impl Interpreter {
    /// The `method` arm of the class-body walk: validate the declaration,
    /// build its `MethodDef`, install it in the method table, and register
    /// its exported / native / `our` / `my` side forms.
    pub(super) fn class_body_method_decl(
        &mut self,
        cx: &mut ClassBodyCx<'_>,
    ) -> Result<(), RuntimeError> {
        // ADR-0019 D3-7: `decl` is precompiled by the compiler at plan
        // lowering (`CompiledClassDeclPlan::method_decls`) and read here by
        // position via the same cursor `method_name_chunks` already uses
        // (D3-1) — `class_body_method_decl` no longer calls
        // `CompiledMethodDecl::from_stmt` on the raw statement itself.
        let chunk_idx = cx.method_name_chunk_idx;
        cx.method_name_chunk_idx += 1;
        let decl = cx
            .method_decls
            .get(chunk_idx)
            .cloned()
            .expect("method_decls misaligned with class body walk");
        self.validate_private_access_in_stmts(cx.name, &decl.body)?;
        Self::validate_attr_declared_in_class(&cx.attr_ctx(), &decl.body)?;
        // In BUILD/TWEAK submethods, :$!attr parameters must refer
        // to declared attributes; reject undeclared ones with
        // X::Attribute::Undeclared.
        {
            let mn = decl.name.resolve();
            if mn == "BUILD" || mn == "TWEAK" {
                for pd in &decl.param_defs {
                    if pd.name.starts_with('!') && pd.name != "!" {
                        let attr_name = &pd.name[1..]; // strip '!'
                        if !cx.class_own_attrs.contains(attr_name) {
                            let err = Self::undeclared_attr_error(&cx.attr_ctx(), attr_name, "!");
                            self.set_current_package(cx.saved_package.clone());
                            self.env = cx.saved_env.clone();
                            return Err(err);
                        }
                    }
                }
            }
        }
        // ADR-0019 D3-1: the chunk at this cursor position was compiled from
        // this exact statement's `name_expr` at plan-lowering time — the
        // compiler and this walk both flatten `SyntheticBlock` the same way,
        // so position, not name, is the shared key (see
        // `Compiler::compile_method_name_chunks`).
        let resolved_method_name = if decl.name_expr.is_some() {
            let chunk = cx
                .method_name_chunks
                .get(chunk_idx)
                .and_then(|c| c.as_ref())
                .expect("method_name_chunks misaligned with class body walk");
            self.run_decl_expr(chunk)?.to_string_value()
        } else {
            decl.name.resolve()
        };
        let mut effective_param_defs = crate::method_signature_shared::effective_method_param_defs(
            &decl.param_defs,
            cx.is_hidden,
        );
        // ADR-0019 D3-8b: the main-pass compiled bytecode this method body
        // may already have (`decl.compiled_routine_key`,
        // `Compiler::compile_method_body`, ADR-0019 D3-8a) was compiled from
        // `effective_param_defs` at THIS point in the pipeline — before the
        // ::?CLASS substitution below, which `compile_method_body`
        // deliberately never performs (design decision 3: a type-constraint
        // STRING is bind-time-only data, never baked into bytecode).
        // Snapshot it here so the install-by-key guard further down compares
        // against exactly what the compiler computed, not this function's
        // substituted copy.
        let mut raw_param_defs_for_key_check = effective_param_defs.clone();
        // Resolve the ::?CLASS pseudo-type in parameter type
        // constraints to the enclosing class (raku fixes ::?CLASS
        // at compile time to the declaring class), mirroring the
        // attribute-type resolution above. Without this, binding a
        // non-invocant `::?CLASS:U \t` param type-checks against
        // the literal string "::?CLASS:U" and always fails.
        for pd in effective_param_defs.iter_mut() {
            if let Some(tc) = &pd.type_constraint
                && tc.contains("::?CLASS")
            {
                pd.type_constraint = Some(tc.replace("::?CLASS", cx.name));
            }
        }
        // Auto-detect @_ usage in methods without explicit signatures.
        // ADR-0019 D3-9: `decl.uses_bare_positional_args` is precomputed at
        // plan-lowering time, so this reads a bool instead of re-scanning
        // `decl.body` on every registration.
        crate::method_signature_shared::apply_auto_positional_slurpy_from_flag(
            decl.param_defs.is_empty(),
            decl.uses_bare_positional_args,
            &mut effective_param_defs,
        );
        // Mirror the same auto-slurpy insertion onto the pre-substitution
        // snapshot: it depends only on names/slurpy-ness (never on
        // `type_constraint` content), so it commutes with the substitution
        // above and this stays byte-identical to what `compile_method_body`
        // computed.
        crate::method_signature_shared::apply_auto_positional_slurpy_from_flag(
            decl.param_defs.is_empty(),
            decl.uses_bare_positional_args,
            &mut raw_param_defs_for_key_check,
        );
        let effective_params: Vec<String> = effective_param_defs
            .iter()
            .map(|p| p.name.clone())
            .collect();
        // ADR-0019 D3-8b (design decision 4): install the main-pass compiled
        // bytecode by key when it resolves in the ambient compiled-function
        // pool AND its params/param_defs match what THIS registration walk
        // just computed. `ParamDef` has no `PartialEq` (it embeds `Expr`,
        // which does not derive it either — adding that is a much larger,
        // separate change), so structural equality is checked via `Debug`
        // formatting, matching the comparison already used to pin
        // main-pass/registration-time byte parity in the D3-8a test suite
        // (`compiler/helpers_method_body.rs`). This is exact, not a
        // heuristic: both sides are derived from the SAME cloned
        // `decl.param_defs`/`decl.body` in this single process, so there is
        // no cross-run Symbol-id or closure-ordinal divergence to normalize
        // away (unlike that test, which compares two separate compiles).
        // A mismatch (or missing key) leaves `compiled_code`/`compiled_fns`
        // `None`, falling back unchanged to the registration-time throwaway
        // compile (`compile_method_def_in_place_with_dist` via the bulk
        // `compile_class_methods` pass).
        let matched_compiled_fn = decl
            .compiled_routine_key
            .and_then(|key| cx.compiled_fns.get(&key))
            .filter(|cf| {
                let expected_full_params: Vec<String> =
                    ["self", "__ANON_STATE__", "?CLASS", "?ROLE"]
                        .iter()
                        .map(|s| s.to_string())
                        .chain(effective_params.iter().cloned())
                        .collect();
                cf.params == expected_full_params
                    && format!("{:?}", cf.param_defs) == format!("{raw_param_defs_for_key_check:?}")
            });
        let installed_compiled_code =
            matched_compiled_fn.map(|cf| std::sync::Arc::new(cf.code.clone()));
        let installed_compiled_fns = matched_compiled_fn.and_then(|cf| cf.compiled_fns.clone());
        let def = MethodDef {
            lexical_package: cx.saved_package.clone(),
            params: effective_params.clone(),
            param_defs: effective_param_defs.clone(),
            body: std::sync::Arc::new(decl.body.clone()),
            is_rw: decl.is_rw,
            is_private: decl.is_private,
            is_multi: decl.multi,
            // Use is_submethod for the MethodDef is_my flag, which
            // controls inheritance filtering (submethods not inherited).
            // `my method` and `our method` are NOT added to the method
            // table at all — they are only registered as functions.
            is_my: decl.is_submethod,
            role_origin: None,
            original_role: None,
            return_type: decl.return_type.clone(),
            compiled_code: installed_compiled_code,
            compiled_fns: installed_compiled_fns,
            delegation: None,
            is_default: decl.is_default_candidate,
            deprecated_message: decl.deprecated_message.clone(),
            is_submethod: decl.is_submethod,
            captured_env: None,
            source_file: self.current_source_file(),
        };
        // `my method` and `our method` are NOT part of the class
        // method table — they are only callable as functions.
        // Submethods (is_submethod=true) DO go in the table even
        // though they also have is_my=true from the parser.
        // `my method` and `our method` are NOT part of the class
        // method table — they are only callable as functions.
        // Submethods (is_submethod=true) DO go in the table even
        // though they also have is_my=true from the parser.
        // The `our &name = method name(...)` variable form
        // (our_variable_form=true) keeps the method in the table.
        let is_lexical_only = decl.is_my && !decl.is_submethod;
        let is_our_only = decl.is_our && !decl.our_variable_form;
        if !is_lexical_only && !is_our_only {
            if decl.multi {
                cx.class_def
                    .methods
                    .entry(resolved_method_name.clone())
                    .or_default()
                    .push(def);
            } else {
                // Check for duplicate non-multi method definition.
                // Only error if the existing method was defined in
                // this class (not composed from a role) AND shares the
                // same privacy: a private `method !foo` and a public
                // `method foo` live in separate namespaces and do not
                // collide (they are stored together but dispatch filters
                // on `is_private`).
                let new_is_private = def.is_private;
                if let Some(existing) = cx.class_def.methods.get(&resolved_method_name) {
                    let conflicts = existing
                        .iter()
                        .any(|m| m.role_origin.is_none() && m.is_private == new_is_private);
                    if conflicts {
                        return Err(RuntimeError::new(format!(
                            "Package '{}' already has a method '{}' (did you mean to declare a multi method?)",
                            cx.name, resolved_method_name
                        )));
                    }
                }
                // A non-multi method replaces prior same-privacy
                // candidates but must preserve methods of the OTHER
                // privacy stored under the same name.
                let entry = cx
                    .class_def
                    .methods
                    .entry(resolved_method_name.clone())
                    .or_default();
                entry.retain(|m| m.is_private != new_is_private);
                entry.push(def);
            }
        }
        // A method declared `is export` is importable as a *sub* whose
        // invocant becomes the first (typed) positional and whose body
        // dispatches back to the method — `import ClassName` then makes
        // both an operator method (`method infix:<as> is export`, so
        // `$obj as $x` resolves to it) and a plain-named one (`method
        // greet() is export`, so `greet($obj)` resolves to it) importable.
        // `register_exported_operator_method_sub`'s forwarding body is
        // name-agnostic despite the name — it dispatches on whatever
        // `resolved_method_name` is.
        if decl.is_export && !self.suppress_exports {
            let tags = if decl.export_tags.is_empty() {
                vec!["DEFAULT".to_string()]
            } else {
                decl.export_tags.clone()
            };
            self.register_exported_operator_method_sub(
                cx.name,
                &resolved_method_name,
                &effective_param_defs,
                tags,
            );
        }
        // An `is native(...)` method routes calls through NativeCall
        // instead of its `{ * }` body, exactly as an `is native` sub
        // does — with the invocant as the first C argument. This is
        // how a whole C API is usually bound (`DBDish::mysql::Native`
        // declares every one of its ~40 entry points this way).
        if decl.custom_traits.iter().any(|(t, _)| t == "native") {
            // Class/role method declarations still register from the
            // source declaration (ADR-0019 phase D), so their trait
            // arguments arrive as expressions.
            self.register_native_call_method(
                cx.name,
                &resolved_method_name,
                &decl.param_defs,
                decl.return_type.as_ref(),
                &crate::opcode::decl_traits_from_ast(&decl.custom_traits),
            )?;
        }
        // Apply custom trait_mod:<is> for each non-builtin trait on methods
        if !decl.custom_traits.is_empty() {
            let has_trait_mod =
                self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>");
            if has_trait_mod {
                for (trait_name, trait_arg) in &decl.custom_traits {
                    let mut trait_env = self.env.clone();
                    // Add method lookup markers so .wrap stores in
                    // method_wrap_chains (keyed by class+method).
                    trait_env.insert(
                        "__mutsu_lookup_class".to_string(),
                        Value::str(cx.name.to_string()),
                    );
                    trait_env.insert(
                        "__mutsu_lookup_method".to_string(),
                        Value::str(resolved_method_name.clone()),
                    );
                    trait_env.insert("__mutsu_lookup_candidate_idx".to_string(), Value::int(0));
                    // The code object passed to a user `trait_mod:<is>` candidate
                    // must report as a `Method`, not a `Sub`, the same way
                    // `sub_value_from_function_def` tags a real method's code
                    // object — otherwise a candidate typed `(Method $m, ...)`
                    // (the only form `raku` accepts for a method-level trait)
                    // never type-checks and the trait application silently does
                    // nothing.
                    trait_env.insert(
                        "__mutsu_callable_type".to_string(),
                        Value::str_from("Method"),
                    );
                    let sub_val = Value::make_sub(
                        Symbol::intern(cx.name),
                        Symbol::intern(&resolved_method_name),
                        effective_params.clone(),
                        effective_param_defs.clone(),
                        decl.body.clone(),
                        decl.is_rw,
                        trait_env,
                    );
                    let trait_arg_val = if let Some(arg_expr) = trait_arg {
                        Some(self.eval_block_value(&[crate::ast::Stmt::Expr(arg_expr.clone())])?)
                    } else {
                        None
                    };
                    let type_obj = self.resolve_type_object(trait_name);
                    let mut args = vec![sub_val];
                    if let Some(type_val) = type_obj {
                        args.push(type_val);
                        if let Some(arg_val) = trait_arg_val {
                            args.push(arg_val);
                        }
                        let _ = self.call_function("trait_mod:<is>", args);
                    } else {
                        let named_val = if let Some(arg_val) = trait_arg_val {
                            Value::pair(trait_name.clone(), arg_val)
                        } else {
                            Value::pair(trait_name.clone(), Value::TRUE)
                        };
                        args.push(named_val);
                        let _ = self.call_function("trait_mod:<is>", args);
                    }
                }
            }
        }
        // `handles` on a method: synthesize forwarder methods that
        // delegate to the return value of this method. E.g.
        //   method Str() handles 'uc' { 'x' }
        // registers a `uc` method that calls `self.Str.uc(|@_)`.
        if !decl.handles.is_empty() {
            // Encode "method-based delegation" by prefixing the
            // source method name with `&`; the delegation dispatch
            // sites recognize this prefix and invoke the named
            // method on self to obtain the delegate.
            let source_attr_marker = format!("&{}", resolved_method_name);
            for spec in &decl.handles {
                match spec {
                    HandleSpec::Name(target) => {
                        cx.class_def
                            .methods
                            .entry(target.clone())
                            .or_default()
                            .push(make_delegation_method(&source_attr_marker, target));
                    }
                    HandleSpec::Rename { exposed, target } => {
                        cx.class_def
                            .methods
                            .entry(exposed.clone())
                            .or_default()
                            .push(make_delegation_method(&source_attr_marker, target));
                    }
                    HandleSpec::Wildcard => {
                        cx.class_def
                            .wildcard_handles
                            .push(source_attr_marker.clone());
                    }
                    HandleSpec::Regex(pattern) => {
                        cx.class_def
                            .wildcard_handles
                            .push(format!("{}:regex:{}", source_attr_marker, pattern));
                    }
                    HandleSpec::Type(_) => {
                        // Method-based delegation via a type name
                        // is not yet supported; fall through.
                    }
                }
            }
        }
        // `our method` also registers as a package-scoped sub
        if decl.is_our {
            let qualified_name = format!("{}::{}", cx.name, resolved_method_name);
            let (our_params, our_param_defs) =
                method_sub_form_params(&effective_params, &effective_param_defs);
            let func_def = crate::ast::FunctionDef {
                package: Symbol::intern(cx.name),
                name: Symbol::intern(&resolved_method_name),
                params: our_params,
                param_defs: our_param_defs,
                body: decl.body.clone(),
                is_test_assertion: false,
                is_rw: decl.is_rw,
                is_raw: false,
                is_method: true,
                empty_sig: false,
                is_stub: Self::is_stub_routine_body(&decl.body),
                return_type: None,
                is_default: decl.is_default_candidate,
                deprecated_message: None,
                source_file: self.current_source_file(),
                decl_order: crate::runtime::resolution::next_decl_order(),
                compiled: None,
                body_fp_cache: std::sync::OnceLock::new(),
                body_facts_cache: std::sync::OnceLock::new(),
                rw_tail_expr: None,
            };
            self.registry_mut().functions.insert(
                Symbol::intern(&qualified_name),
                std::sync::Arc::new(func_def),
            );
            // Invalidate name-keyed resolution caches.
            self.fn_resolve_gen += 1;
        }
        // `my method` registers as a lexically-scoped function
        // (callable as `name(invocant)` inside the class body)
        if decl.is_my {
            let (my_params, my_param_defs) =
                method_sub_form_params(&effective_params, &effective_param_defs);
            let func_def = crate::ast::FunctionDef {
                package: Symbol::intern(cx.name),
                name: Symbol::intern(&resolved_method_name),
                params: my_params,
                param_defs: my_param_defs,
                body: decl.body.clone(),
                is_test_assertion: false,
                is_rw: decl.is_rw,
                is_raw: false,
                is_method: true,
                empty_sig: false,
                is_stub: Self::is_stub_routine_body(&decl.body),
                return_type: None,
                is_default: decl.is_default_candidate,
                deprecated_message: None,
                source_file: self.current_source_file(),
                decl_order: crate::runtime::resolution::next_decl_order(),
                compiled: None,
                body_fp_cache: std::sync::OnceLock::new(),
                body_facts_cache: std::sync::OnceLock::new(),
                rw_tail_expr: None,
            };
            // Register under the short name (lexical scope)
            self.registry_mut().functions.insert(
                Symbol::intern(&resolved_method_name),
                std::sync::Arc::new(func_def.clone()),
            );
            // Also register under the qualified name for consistency
            let qualified_name = format!("{}::{}", cx.name, resolved_method_name);
            self.registry_mut().functions.insert(
                Symbol::intern(&qualified_name),
                std::sync::Arc::new(func_def),
            );
            // Invalidate name-keyed resolution caches.
            self.fn_resolve_gen += 1;
            // Mark as my-scoped so it doesn't appear in the package stash
            self.mark_my_scoped_package_item(qualified_name);
        }
        Ok(())
    }
}
