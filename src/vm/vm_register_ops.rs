use super::*;
use crate::symbol::Symbol;

impl VM {
    pub(super) fn exec_make_gather_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::Block(body) = stmt {
            self.ensure_env_synced(code);
            let mut env = self.interpreter.env().clone();
            env.insert(
                "__mutsu_lazylist_from_gather".to_string(),
                Value::Bool(true),
            );
            let list = LazyList {
                body: body.clone(),
                env,
                cache: std::sync::Mutex::new(None),
            };
            let val = Value::LazyList(std::sync::Arc::new(list));
            self.stack.push(val);
            Ok(())
        } else {
            Err(RuntimeError::new("MakeGather expects Block"))
        }
    }

    fn resolve_closure_code(
        code: &CompiledCode,
        cc_idx: Option<u32>,
    ) -> Option<std::sync::Arc<CompiledCode>> {
        cc_idx.map(|i| code.closure_compiled_codes[i as usize].clone())
    }

    pub(super) fn exec_make_anon_sub_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        cc_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::Block(body) = stmt {
            self.ensure_env_synced(code);
            let params = crate::ast::collect_placeholders(body);
            let compiled_code = Self::resolve_closure_code(code, cc_idx);
            let val = Value::Sub(std::sync::Arc::new(crate::value::SubData {
                package: Symbol::intern(self.interpreter.current_package()),
                name: Symbol::intern(""),
                params,
                param_defs: Vec::new(),
                body: body.clone(),
                is_rw: false,
                is_raw: false,
                env: self.interpreter.env().clone(),
                assumed_positional: Vec::new(),
                assumed_named: std::collections::HashMap::new(),
                id: crate::value::next_instance_id(),
                empty_sig: false,
                compiled_code,
            }));
            self.stack.push(val);
            Ok(())
        } else {
            Err(RuntimeError::new("MakeAnonSub expects Block"))
        }
    }

    pub(super) fn exec_make_anon_sub_params_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        cc_idx: Option<u32>,
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
            self.ensure_env_synced(code);
            let mut env = self.interpreter.env().clone();
            if let Some(rt) = return_type {
                env.insert("__mutsu_return_type".to_string(), Value::str(rt.clone()));
            }
            let compiled_code = Self::resolve_closure_code(code, cc_idx);
            let val = Value::Sub(std::sync::Arc::new(crate::value::SubData {
                package: Symbol::intern(self.interpreter.current_package()),
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
                empty_sig: false,
                compiled_code,
            }));
            self.stack.push(val);
            Ok(())
        } else {
            Err(RuntimeError::new("MakeAnonSubParams expects SubDecl"))
        }
    }

    pub(super) fn exec_make_lambda_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        cc_idx: Option<u32>,
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
            self.ensure_env_synced(code);
            let mut env = self.interpreter.env().clone();
            if let Some(rt) = return_type {
                env.insert("__mutsu_return_type".to_string(), Value::str(rt.clone()));
            }
            let compiled_code = Self::resolve_closure_code(code, cc_idx);
            let val = Value::Sub(std::sync::Arc::new(crate::value::SubData {
                package: Symbol::intern(self.interpreter.current_package()),
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
                empty_sig: false,
                compiled_code,
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
            self.ensure_env_synced(code);
            let compiled_code = Self::resolve_closure_code(code, cc_idx);
            let val = Value::Sub(std::sync::Arc::new(crate::value::SubData {
                package: Symbol::intern(self.interpreter.current_package()),
                name: Symbol::intern(""),
                params: vec![],
                param_defs: Vec::new(),
                body: body.clone(),
                is_rw: false,
                is_raw: false,
                env: self.interpreter.env().clone(),
                assumed_positional: Vec::new(),
                assumed_named: std::collections::HashMap::new(),
                id: crate::value::next_instance_id(),
                empty_sig: false,
                compiled_code,
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
        self.ensure_env_synced(code);
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
        } = stmt
        {
            let resolved_name = if let Some(expr) = name_expr {
                self.interpreter
                    .eval_block_value(&[Stmt::Expr(expr.clone())])?
                    .to_string_value()
            } else {
                name.resolve()
            };
            self.interpreter.register_sub_decl(
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
            )?;
            if *is_export && !self.interpreter.suppress_exports {
                self.interpreter.register_exported_sub(
                    self.interpreter.current_package().to_string(),
                    resolved_name.clone(),
                    export_tags.clone(),
                );
            }
            for (alt_params, alt_param_defs) in signature_alternates {
                self.interpreter.register_sub_decl(
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
                )?;
            }
            Ok(())
        } else {
            Err(RuntimeError::new("RegisterSub expects SubDecl"))
        }
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
            }
            | Stmt::RuleDecl {
                name,
                params,
                param_defs,
                body,
                multi,
            } => {
                self.interpreter.register_token_decl(
                    &name.resolve(),
                    params,
                    param_defs,
                    body,
                    *multi,
                );
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
        } = stmt
        {
            let name_str = name.resolve();
            self.interpreter
                .register_proto_decl(&name_str, params, param_defs, body)?;
            if *is_export {
                self.interpreter
                    .register_proto_decl_as_global(&name_str, params, param_defs, body)?;
            }
            // Apply custom trait_mod:<is> for each non-builtin trait (only if defined)
            if !custom_traits.is_empty()
                && (self.interpreter.has_proto("trait_mod:<is>")
                    || self.interpreter.has_multi_candidates("trait_mod:<is>"))
            {
                for trait_name in custom_traits {
                    let sub_val = Value::make_sub(
                        Symbol::intern(self.interpreter.current_package()),
                        Symbol::intern(&name_str),
                        params.clone(),
                        param_defs.clone(),
                        body.clone(),
                        false,
                        self.interpreter.env().clone(),
                    );
                    let named_arg = Value::Pair(trait_name.clone(), Box::new(Value::Bool(true)));
                    let result = self
                        .interpreter
                        .call_function("trait_mod:<is>", vec![sub_val, named_arg])?;
                    // If the trait_mod returned a modified sub (e.g. with CALL-ME mixed in),
                    // store it in the env so function dispatch can find it.
                    if matches!(result, Value::Mixin(..)) {
                        self.interpreter
                            .env_mut()
                            .insert(format!("&{}", name), result);
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
            self.interpreter.register_proto_token_decl(&name.resolve());
            Ok(())
        } else {
            Err(RuntimeError::new("RegisterProtoToken expects ProtoToken"))
        }
    }

    pub(super) fn exec_use_module_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let module = Self::const_str(code, name_idx);
        self.interpreter.use_module(module)?;
        self.env_dirty = true;
        Ok(())
    }

    pub(super) fn exec_import_module_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        tags_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let module = Self::const_str(code, name_idx);
        let tags = tags_idx
            .and_then(|idx| code.constants.get(idx as usize))
            .and_then(|v| match v {
                Value::Array(items, ..) => Some(
                    items
                        .iter()
                        .map(|v| v.to_string_value())
                        .collect::<Vec<String>>(),
                ),
                _ => None,
            })
            .unwrap_or_default();
        self.interpreter.import_module(module, &tags)?;
        self.env_dirty = true;
        Ok(())
    }

    pub(super) fn exec_no_module_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let module = Self::const_str(code, name_idx);
        self.interpreter.no_module(module)?;
        self.env_dirty = true;
        Ok(())
    }

    pub(super) fn exec_need_module_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let module = Self::const_str(code, name_idx);
        self.interpreter.need_module(module)?;
        self.env_dirty = true;
        Ok(())
    }

    pub(super) fn exec_use_lib_path_op(
        &mut self,
        _code: &CompiledCode,
    ) -> Result<(), RuntimeError> {
        let value = self.stack.pop().unwrap_or(Value::Nil);
        let path = value.to_string_value();
        if path.is_empty() {
            return Err(RuntimeError::new(
                "X::LibEmpty: Repository specification can not be an empty string",
            ));
        }
        self.interpreter.add_lib_path(path);
        Ok(())
    }

    pub(super) fn exec_register_var_export_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        tags_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let tags = tags_idx
            .and_then(|idx| code.constants.get(idx as usize))
            .and_then(|v| match v {
                Value::Array(items, ..) => Some(
                    items
                        .iter()
                        .map(|v| v.to_string_value())
                        .collect::<Vec<String>>(),
                ),
                _ => None,
            })
            .unwrap_or_else(|| vec!["DEFAULT".to_string()]);
        self.interpreter.register_exported_var(
            self.interpreter.current_package().to_string(),
            name,
            tags,
        );
        Ok(())
    }

    pub(super) fn exec_apply_var_trait_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        trait_name_idx: u32,
        has_arg: bool,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        let trait_name = Self::const_str(code, trait_name_idx).to_string();

        // Handle `is default(...)` as a built-in variable trait.
        if trait_name == "default" {
            let default_value = if has_arg {
                self.stack.pop().unwrap_or(Value::Nil)
            } else {
                Value::Bool(true)
            };
            let name = name.to_string();
            self.interpreter
                .set_var_default(&name, default_value.clone());
            // For array/hash variables, also register the container default
            // so that element access on missing indices returns the default.
            if (name.starts_with('@') || name.starts_with('%'))
                && let Some(container) = self.locals_get_by_name(code, &name)
            {
                self.interpreter
                    .set_container_default(&container, default_value.clone());
            }
            // If the variable is currently Nil (uninitialized scalar), set it to the default.
            if !name.starts_with('@') && !name.starts_with('%') {
                let current = self.locals_get_by_name(code, &name);
                if matches!(current, Some(Value::Nil) | None) {
                    self.locals_set_by_name(code, &name, default_value.clone());
                    self.set_env_with_main_alias(&name, default_value);
                }
            }
            self.env_dirty = true;
            return Ok(());
        }

        // Handle `is Buf/Blob/buf8/...` trait for array variables:
        // converts the array variable into a native typed buffer.
        if name.starts_with('@') {
            if trait_name == "List" {
                if has_arg {
                    self.stack.pop(); // discard unsupported trait argument
                }
                self.interpreter.mark_readonly(name);
                return Ok(());
            }
            let is_buf_trait = matches!(
                trait_name.as_str(),
                "Buf"
                    | "Blob"
                    | "buf"
                    | "blob"
                    | "buf8"
                    | "buf16"
                    | "buf32"
                    | "buf64"
                    | "blob8"
                    | "blob16"
                    | "blob32"
                    | "blob64"
                    | "utf8"
            );
            if is_buf_trait {
                if has_arg {
                    self.stack.pop(); // discard unused arg
                }
                let buf_type = match trait_name.as_str() {
                    "buf" | "blob" => {
                        let resolved = self
                            .interpreter
                            .call_function("EVAL", vec![Value::str(trait_name.clone())])
                            .ok()
                            .or_else(|| self.get_env_with_main_alias(&trait_name))
                            .or_else(|| self.locals_get_by_name(code, &trait_name));
                        match resolved {
                            Some(Value::Package(sym)) => Value::Package(sym),
                            Some(Value::Scalar(inner)) => match *inner {
                                Value::Package(sym) => Value::Package(sym),
                                _ => Value::Package(crate::symbol::Symbol::intern(
                                    if trait_name == "buf" { "Buf" } else { "Blob" },
                                )),
                            },
                            _ => Value::Package(crate::symbol::Symbol::intern(
                                if trait_name == "buf" { "Buf" } else { "Blob" },
                            )),
                        }
                    }
                    _ => Value::Package(crate::symbol::Symbol::intern(&trait_name)),
                };
                // Get current value, convert to buf
                let current = self.locals_get_by_name(code, name).unwrap_or(Value::Nil);
                let items = match &current {
                    Value::Array(items, ..) => items
                        .iter()
                        .map(|v| Value::Int(crate::runtime::to_int(v)))
                        .collect(),
                    _ => Vec::new(),
                };
                let buf = self
                    .interpreter
                    .call_method_with_values(buf_type, "new", items)?;
                let name_str = name.to_string();
                self.locals_set_by_name(code, &name_str, buf.clone());
                self.set_env_with_main_alias(&name_str, buf);
                self.env_dirty = true;
                return Ok(());
            }
        }

        if !(self.interpreter.has_proto("trait_mod:<is>")
            || self.interpreter.has_multi_candidates("trait_mod:<is>"))
        {
            return Err(RuntimeError::new(format!(
                "X::Comp::Trait::Unknown: Unknown variable trait 'is {}'",
                trait_name
            )));
        }
        let trait_value = if has_arg {
            self.stack.pop().unwrap_or(Value::Nil)
        } else {
            Value::Bool(true)
        };
        let target = self
            .interpreter
            .env()
            .get(name)
            .cloned()
            .unwrap_or(Value::Nil);
        let var_obj = self
            .interpreter
            .call_method_mut_with_values(name, target, "VAR", vec![])?;
        let named_arg = Value::Pair(trait_name, Box::new(trait_value));
        self.interpreter
            .call_function("trait_mod:<is>", vec![var_obj, named_arg])?;
        self.env_dirty = true;
        Ok(())
    }

    pub(super) fn exec_register_enum_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::EnumDecl {
            name,
            variants,
            is_export,
        } = stmt
        {
            let result =
                self.interpreter
                    .register_enum_decl(&name.resolve(), variants, *is_export)?;
            // For anonymous enums, push the Map result onto the stack
            if name.resolve().is_empty() {
                self.stack.push(result);
            }
            self.env_dirty = true;
            Ok(())
        } else {
            Err(RuntimeError::new("RegisterEnum expects EnumDecl"))
        }
    }

    pub(super) fn exec_register_class_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::ClassDecl {
            name,
            name_expr,
            parents,
            class_is_rw,
            is_hidden,
            hidden_parents,
            does_parents,
            repr,
            body,
        } = stmt
        {
            let resolved_name = if let Some(expr) = name_expr {
                self.interpreter
                    .eval_block_value(&[Stmt::Expr(expr.clone())])?
                    .to_string_value()
            } else {
                name.resolve()
            };
            self.interpreter.register_class_decl(
                &resolved_name,
                parents,
                crate::runtime::ClassDeclModifiers {
                    class_is_rw: *class_is_rw,
                    is_hidden: *is_hidden,
                    hidden_parents,
                    does_parents,
                },
                body,
            )?;
            // Compile method bodies to bytecode for the fast path
            self.interpreter.compile_class_methods(&resolved_name);
            // Register CUnion repr if present
            if let Some(repr_name) = repr
                && repr_name == "CUnion"
            {
                self.interpreter.register_cunion_class(&resolved_name);
            }
            // Register the class name in the lexical env so that
            // ::("ClassName") indirect lookups can find it in the current scope.
            let env = self.interpreter.env_mut();
            env.insert(
                "_".to_string(),
                Value::Package(Symbol::intern(&resolved_name)),
            );
            env.entry(resolved_name.clone())
                .or_insert(Value::Package(Symbol::intern(&resolved_name)));
            self.env_dirty = true;
            Ok(())
        } else {
            Err(RuntimeError::new("RegisterClass expects ClassDecl"))
        }
    }

    pub(super) fn exec_augment_class_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::AugmentClass { name, body } = stmt {
            let name_str = name.resolve();
            // Check MONKEY-TYPING pragma: we check if `use MONKEY-TYPING` or `use MONKEY`
            // was issued. Since the compiler simply ignores these `use` statements,
            // we track them at the interpreter level.
            if !self.interpreter.monkey_typing_enabled() {
                return Err(RuntimeError::new(
                    "augment not allowed without 'use MONKEY-TYPING'",
                ));
            }
            self.interpreter.augment_class(&name_str, body)?;
            // Recompile augmented class methods for the fast path
            self.interpreter.compile_class_methods(&name_str);
            Ok(())
        } else {
            Err(RuntimeError::new("AugmentClass expects AugmentClass stmt"))
        }
    }

    pub(super) fn exec_register_role_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::RoleDecl {
            name,
            type_params,
            type_param_defs,
            body,
        } = stmt
        {
            let name_str = name.resolve();
            self.interpreter
                .register_role_decl(&name_str, type_params, type_param_defs, body)?;
            // Compile role method bodies to bytecode
            self.interpreter.compile_role_methods(&name_str);
            self.interpreter
                .env_mut()
                .insert("_".to_string(), Value::Package(Symbol::intern(&name_str)));
            self.env_dirty = true;
            Ok(())
        } else {
            Err(RuntimeError::new("RegisterRole expects RoleDecl"))
        }
    }

    pub(super) fn exec_register_subset_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::SubsetDecl {
            name,
            base,
            predicate,
            version,
        } = stmt
        {
            self.interpreter.register_subset_decl(
                &name.resolve(),
                base,
                predicate.as_ref(),
                version,
            );
            self.env_dirty = true;
            Ok(())
        } else {
            Err(RuntimeError::new("RegisterSubset expects SubsetDecl"))
        }
    }

    pub(super) fn exec_subtest_scope_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let end = body_end as usize;
        let body_start = *ip + 1;
        let label = self.stack.pop().unwrap_or(Value::Nil).to_string_value();
        let ctx = self.interpreter.begin_subtest();
        let saved_depth = self.stack.len();
        let run_result = self.run_range(code, body_start, end, compiled_fns);
        self.stack.truncate(saved_depth);
        self.interpreter.finish_subtest(ctx, &label, run_result)?;
        self.env_dirty = true;
        *ip = end;
        Ok(())
    }

    pub(super) fn exec_react_scope_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let end = body_end as usize;
        let body_start = *ip + 1;

        // React callbacks run through the interpreter path and capture from env.
        // Flush VM locals first so captured vars (e.g., @received in loop scopes)
        // are visible/mutable from whenever callbacks.
        self.ensure_env_synced(code);
        self.sync_env_from_locals(code);

        // Enter react mode: whenever blocks will register subscriptions
        self.interpreter.enter_react();
        let saved_depth = self.stack.len();
        let run_result = self.run_range(code, body_start, end, compiled_fns);
        self.stack.truncate(saved_depth);

        // Run the react event loop (processes all registered subscriptions)
        let event_result = self.interpreter.run_react_event_loop();
        self.sync_locals_from_env(code);
        self.env_dirty = true;

        *ip = end;
        if let Err(err) = run_result
            && !err.is_react_done
        {
            return Err(err);
        }
        if let Err(err) = event_result
            && !err.is_react_done
        {
            return Err(err);
        }
        Ok(())
    }

    pub(super) fn exec_whenever_scope_op(
        &mut self,
        code: &CompiledCode,
        body_idx: u32,
        param_idx: &Option<u32>,
        target_var_idx: &Option<u32>,
    ) -> Result<(), RuntimeError> {
        let supply_val = self.stack.pop().unwrap_or(Value::Nil);
        let param = param_idx.map(|idx| Self::const_str(code, idx).to_string());
        let target_var = target_var_idx.map(|idx| Self::const_str(code, idx));
        let stmt = &code.stmt_pool[body_idx as usize];
        if let Stmt::Block(body) = stmt {
            self.interpreter
                .run_whenever_with_value(supply_val, target_var, &param, body)?;
            self.env_dirty = true;
            Ok(())
        } else {
            Err(RuntimeError::new("WheneverScope expects Block body"))
        }
    }
}
