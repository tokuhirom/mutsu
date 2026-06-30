//! Lambda/block-closure creation and sub/proto/token registration ops.
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn exec_make_lambda_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        cc_idx: Option<u32>,
        is_whatever_code: bool,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::SubDecl {
            params,
            param_defs,
            return_type,
            body,
            is_rw,
            is_raw,
            ..
        } = stmt
        {
            let compiled_code = Self::resolve_closure_code(code, cc_idx);
            self.box_captured_lexicals(code, &compiled_code);
            let owned_captures = self.compute_owned_captures(&compiled_code);
            let upvalues = self.capture_upvalues(code, &compiled_code);
            // Upvalue snapshot (single-store Slice E); see `capture_closure_env`.
            let mut env = self.capture_closure_env(code, &compiled_code);
            if let Some(rt) = return_type {
                env.insert("__mutsu_return_type".to_string(), Value::str(rt.clone()));
            }
            if is_whatever_code {
                env.insert(
                    "__mutsu_callable_type".to_string(),
                    Value::str_from("WhateverCode"),
                );
            }
            let cc_source_line = compiled_code
                .as_ref()
                .and_then(|cc| cc.source_line)
                .map(|l| l as u32)
                .or_else(|| self.current_source_line());
            let val = Value::Sub(std::sync::Arc::new(crate::value::SubData {
                package: Symbol::intern(&self.current_package()),
                name: Symbol::intern(""),
                params: params.clone(),
                param_defs: param_defs.clone(),
                body: body.clone(),
                is_rw: *is_rw,
                is_raw: *is_raw,
                env,
                assumed_positional: Vec::new(),
                assumed_named: std::collections::HashMap::new(),
                id: crate::value::next_instance_id(),
                empty_sig: params.is_empty() && param_defs.is_empty(),
                // A pointy block (`-> $x {...}`) is a `Block`, not a `Sub` — mark it
                // so `.WHAT`/`.^name`/smartmatch report `Block`. Named anonymous subs
                // (`sub {...}`) have `is_pointy_block == false` and stay `Sub`.
                is_bare_block: compiled_code.as_ref().is_some_and(|cc| cc.is_pointy_block),
                owned_captures,
                upvalues,
                compiled_code,
                deprecated_message: None,
                source_line: cc_source_line,
                source_file: self.current_source_file(),
            }));
            self.stack.push(val);
            Ok(())
        } else {
            Err(RuntimeError::new("MakeLambda expects SubDecl"))
        }
    }

    pub(super) fn exec_make_block_closure_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        cc_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::Block(body) = stmt {
            let compiled_code = Self::resolve_closure_code(code, cc_idx);
            self.box_captured_lexicals(code, &compiled_code);
            let owned_captures = self.compute_owned_captures(&compiled_code);
            let upvalues = self.capture_upvalues(code, &compiled_code);
            let cc_source_line = compiled_code
                .as_ref()
                .and_then(|cc| cc.source_line)
                .map(|l| l as u32)
                .or_else(|| self.current_source_line());
            let val = Value::Sub(std::sync::Arc::new(crate::value::SubData {
                package: Symbol::intern(&self.current_package()),
                name: Symbol::intern(""),
                params: vec![],
                param_defs: Vec::new(),
                body: body.clone(),
                is_rw: false,
                is_raw: false,
                // Upvalue snapshot (single-store Slice E); see capture_closure_env.
                env: self.capture_closure_env(code, &compiled_code),
                assumed_positional: Vec::new(),
                assumed_named: std::collections::HashMap::new(),
                id: crate::value::next_instance_id(),
                empty_sig: false,
                is_bare_block: true,
                owned_captures,
                upvalues,
                compiled_code,
                deprecated_message: None,
                source_line: cc_source_line,
                source_file: self.current_source_file(),
            }));
            self.stack.push(val);
            Ok(())
        } else {
            Err(RuntimeError::new("MakeBlockClosure expects Block"))
        }
    }

    pub(super) fn exec_register_sub_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::SubDecl {
            name,
            name_expr,
            params,
            param_defs,
            return_type,
            associativity,
            signature_alternates,
            body,
            multi,
            is_rw,
            is_raw,
            is_export,
            export_tags,
            is_test_assertion,
            supersede,
            custom_traits,
            ..
        } = stmt
        {
            let resolved_name = if let Some(expr) = name_expr {
                self.vm_eval_block_value(&[Stmt::Expr(expr.clone())])?
                    .to_string_value()
            } else {
                name.resolve()
            };
            // Compile-time declaration fingerprint for this site (absent for a
            // runtime-resolved `name_expr` sub), enabling the idempotent
            // re-registration fast path inside `register_sub_decl_fp`.
            let site_fp = code.sub_fingerprints.get(&idx).copied();
            let outcome = self.loan_env_for(|i| {
                i.register_sub_decl_fp(
                    &resolved_name,
                    params,
                    param_defs,
                    return_type.as_ref(),
                    associativity.as_ref(),
                    body,
                    *multi,
                    *is_rw,
                    *is_raw,
                    *is_test_assertion,
                    *supersede,
                    custom_traits,
                    site_fp,
                )
            })?;
            // An idempotent re-registration of an already-installed identical sub
            // leaves the registry untouched, so none of the install bookkeeping
            // below (cache invalidation, `&`-param shadow tracking, export, native
            // descriptor, signature alternates) needs to re-run — they were all
            // done by the first installation and persist.
            if outcome == crate::runtime::registration_sub::SubRegisterOutcome::Installed {
                // If this sub carries the `is native(...)` trait, record its C-FFI
                // descriptor so calls route through NativeCall instead of the body.
                if custom_traits.iter().any(|(t, _)| t == "native") {
                    self.register_native_call_sub(
                        &resolved_name,
                        param_defs,
                        return_type.as_ref(),
                        custom_traits,
                    )?;
                }
                self.fn_resolve_gen += 1;
                self.method_resolve_cache.clear();
                self.last_method_resolve = None;
                self.fast_method_cache.clear();
                self.multi_resolve_cache.clear();
                self.multi_type_cacheable.clear();
                self.func_multi_resolve_cache.clear();
                self.func_multi_type_cacheable.clear();
                self.dispatch_multi_candidate.clear();
                // Record `&`-sigil parameter names so calls to a same-named routine
                // inside this sub bypass the name-keyed light-call caches (the param
                // can shadow a package sub of the same name).
                for pd in param_defs {
                    if let Some(bare) = pd.name.strip_prefix('&')
                        && !bare.is_empty()
                    {
                        // Records both plain names (`foo`) and operator categories
                        // (`infix:<@@>`); both can shadow a same-named package routine.
                        self.amp_param_shadowed_names.insert(Symbol::intern(bare));
                    }
                }
                if *is_export && !self.suppress_exports {
                    let pkg = self.current_package().to_string();
                    self.register_exported_sub(
                        pkg.clone(),
                        resolved_name.clone(),
                        export_tags.clone(),
                    );
                    // If a custom `is` trait mixed a role into this routine, the
                    // resulting Mixin lives in the lexical env as `&name` but would
                    // be dropped when the module scope exits. Capture it so `import`
                    // can restore the trait-modified value.
                    let code_var_key = format!("&{}", resolved_name);
                    if let Some(val @ Value::Mixin(..)) = self.env().get(&code_var_key) {
                        self.record_exported_sub_value(pkg, resolved_name.clone(), val.clone());
                    }
                }
                for (alt_params, alt_param_defs) in signature_alternates {
                    self.loan_env_for(|i| {
                        i.register_sub_decl(
                            &resolved_name,
                            alt_params,
                            alt_param_defs,
                            return_type.as_ref(),
                            associativity.as_ref(),
                            body,
                            *multi,
                            *is_rw,
                            *is_raw,
                            *is_test_assertion,
                            *supersede,
                            custom_traits,
                        )
                    })?;
                }
            }
            // An `our sub` declared in a bare block closes over the block's `my`
            // lexicals, but a registry routine has no per-sub closure env and the
            // block scope is dropped on exit. When this `RegisterSub` runs in SOURCE
            // ORDER (after the `my $a = ...` that the sub captures — `RegisterSub` is
            // emitted both hoisted at block top AND in place), the captured local is
            // already a boxed shared cell in `env`. Persist those cells into
            // `escaped_our_lexical_cells` so a call made AFTER the block reads the
            // live value. Keyed to the sub's own declaration (not the box site), so
            // an unrelated sibling-block `my $a` cannot pollute the map; the hoisted
            // top-of-block registration runs before the box and finds no cell, so it
            // correctly persists nothing (a call before the block reads undefined).
            if custom_traits.iter().any(|(t, _)| t == "__our_scoped")
                && !self.escaping_our_lexical_names.is_empty()
            {
                let names: Vec<String> = self.escaping_our_lexical_names.iter().cloned().collect();
                for name in names {
                    if let Some(cell @ Value::ContainerRef(_)) = self.env().get(&name).cloned() {
                        self.escaped_our_lexical_cells.insert(name, cell);
                    }
                }
            }
            // Note: we intentionally do NOT push the Sub onto the stack or
            // store it in env here. The interpreter's trailing_sub_value
            // mechanism handles returning the Sub when it's the last statement
            // of a block. Pushing would interfere with stack depth tracking.
            Ok(())
        } else {
            Err(RuntimeError::new("RegisterSub expects SubDecl"))
        }
    }

    /// Build and store the NativeCall descriptor for an `is native(...)` sub.
    /// The library name comes from the `native` trait argument, the C symbol
    /// from an optional `is symbol('...')` trait (defaulting to the sub name),
    /// and the C signature from the parameter / return type constraints.
    fn register_native_call_sub(
        &mut self,
        name: &str,
        param_defs: &[crate::ast::ParamDef],
        return_type: Option<&String>,
        custom_traits: &[(String, Option<crate::ast::Expr>)],
    ) -> Result<(), RuntimeError> {
        use crate::runtime::nativecall::{CType, NativeCallSpec, ParamSpec};

        // Evaluate a trait's argument expression to a String, if present.
        let mut eval_trait_str = |trait_name: &str| -> Result<Option<String>, RuntimeError> {
            for (t, arg) in custom_traits {
                if t == trait_name {
                    return Ok(match arg {
                        Some(expr) => Some(
                            self.vm_eval_block_value(&[Stmt::Expr(expr.clone())])?
                                .to_string_value(),
                        ),
                        None => None,
                    });
                }
            }
            Ok(None)
        };

        let library = eval_trait_str("native")?;
        let symbol = eval_trait_str("symbol")?.unwrap_or_else(|| name.to_string());

        // Map each parameter's type constraint to a C type. An unmapped /
        // missing type means we cannot marshal it — skip native registration so
        // the failure surfaces clearly rather than mis-calling.
        let mut params = Vec::with_capacity(param_defs.len());
        for pd in param_defs {
            let Some(tc) = pd.type_constraint.as_deref() else {
                return Ok(());
            };
            let Some(ct) = CType::from_type_name(tc) else {
                return Ok(());
            };
            let is_rw = pd.traits.iter().any(|t| t == "rw");
            params.push(ParamSpec { ct, is_rw });
        }

        let ret = match return_type {
            None => CType::Void,
            Some(rt) => match CType::from_type_name(rt) {
                Some(ct) => ct,
                None => return Ok(()),
            },
        };

        self.native_call_specs.insert(
            name.to_string(),
            NativeCallSpec {
                library,
                symbol,
                params,
                ret,
            },
        );
        Ok(())
    }

    pub(super) fn exec_register_token_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        match stmt {
            Stmt::TokenDecl {
                name,
                params,
                param_defs,
                body,
                multi,
                ..
            }
            | Stmt::RuleDecl {
                name,
                params,
                param_defs,
                body,
                multi,
            } => {
                self.register_token_decl(&name.resolve(), params, param_defs, body, *multi);
                Ok(())
            }
            _ => Err(RuntimeError::new(
                "RegisterToken expects TokenDecl/RuleDecl",
            )),
        }
    }

    pub(super) fn exec_register_proto_sub_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::ProtoDecl {
            name,
            params,
            param_defs,
            body,
            is_export,
            custom_traits,
            ..
        } = stmt
        {
            let name_str = name.resolve();
            self.register_proto_decl(&name_str, params, param_defs, body)?;
            if *is_export {
                self.register_proto_decl_as_global(&name_str, params, param_defs, body)?;
                // Record the export so consumers/MAIN-dispatch see the whole multi
                // family. A `proto … is export` exports its candidates too (raku),
                // e.g. zef's `proto MAIN(|) is export` over `multi sub MAIN(…)`.
                if !self.suppress_exports {
                    let pkg = self.current_package().to_string();
                    self.register_exported_sub(pkg, name_str.clone(), Vec::new());
                }
            }
            // Apply custom trait_mod:<is> for each non-builtin trait (only if defined)
            if !custom_traits.is_empty() {
                let has_trait_mod =
                    self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>");
                for trait_name in custom_traits.iter().filter(|t| {
                    !t.starts_with("__")
                        && *t != "default"
                        && !t.starts_with("DEPRECATED")
                        && *t != "deep"
                }) {
                    if !has_trait_mod {
                        return Err(RuntimeError::new(format!(
                            "Can't use unknown trait 'is' -> '{}' in sub declaration.",
                            trait_name
                        )));
                    }
                    let sub_val = Value::make_sub(
                        Symbol::intern(&self.current_package()),
                        Symbol::intern(&name_str),
                        params.clone(),
                        param_defs.clone(),
                        body.clone(),
                        false,
                        self.clone_env(),
                    );
                    let named_arg = Value::Pair(trait_name.clone(), Box::new(Value::Bool(true)));
                    let result = loan_env!(
                        self,
                        call_function("trait_mod:<is>", vec![sub_val, named_arg])
                    )?;
                    // If the trait_mod returned a modified sub (e.g. with CALL-ME mixed in),
                    // store it in the env so function dispatch can find it.
                    if matches!(result, Value::Mixin(..)) {
                        self.env_mut().insert(format!("&{}", name), result);
                    }
                }
            }
            Ok(())
        } else {
            Err(RuntimeError::new("RegisterProtoSub expects ProtoDecl"))
        }
    }

    pub(super) fn exec_register_proto_token_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::ProtoToken { name } = stmt {
            self.register_proto_token_decl(&name.resolve());
            Ok(())
        } else {
            Err(RuntimeError::new("RegisterProtoToken expects ProtoToken"))
        }
    }
}
