use super::*;

impl VM {
    pub(super) fn exec_make_gather_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::Block(body) = stmt {
            let list = LazyList {
                body: body.clone(),
                env: self.interpreter.env().clone(),
                cache: std::sync::Mutex::new(None),
            };
            let val = Value::LazyList(std::sync::Arc::new(list));
            self.stack.push(val);
            Ok(())
        } else {
            Err(RuntimeError::new("MakeGather expects Block"))
        }
    }

    pub(super) fn exec_make_anon_sub_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::Block(body) = stmt {
            let params = crate::ast::collect_placeholders(body);
            let val = Value::make_sub(
                self.interpreter.current_package().to_string(),
                String::new(),
                params,
                Vec::new(),
                body.clone(),
                false,
                self.interpreter.env().clone(),
            );
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
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::SubDecl {
            params,
            param_defs,
            return_type,
            body,
            is_rw,
            ..
        } = stmt
        {
            let mut env = self.interpreter.env().clone();
            if let Some(rt) = return_type {
                env.insert("__mutsu_return_type".to_string(), Value::Str(rt.clone()));
            }
            let val = Value::make_sub(
                self.interpreter.current_package().to_string(),
                String::new(),
                params.clone(),
                param_defs.clone(),
                body.clone(),
                *is_rw,
                env,
            );
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
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::SubDecl {
            params,
            param_defs,
            return_type,
            body,
            is_rw,
            ..
        } = stmt
        {
            let mut env = self.interpreter.env().clone();
            if let Some(rt) = return_type {
                env.insert("__mutsu_return_type".to_string(), Value::Str(rt.clone()));
            }
            let val = Value::make_sub(
                self.interpreter.current_package().to_string(),
                String::new(),
                params.clone(),
                param_defs.clone(),
                body.clone(),
                *is_rw,
                env,
            );
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
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::Block(body) = stmt {
            let val = Value::make_sub(
                self.interpreter.current_package().to_string(),
                String::new(),
                vec![],
                Vec::new(),
                body.clone(),
                false,
                self.interpreter.env().clone(),
            );
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
                        self.interpreter.current_package().to_string(),
                        name_str.clone(),
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
        self.sync_locals_from_env(code);
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
        self.sync_locals_from_env(code);
        Ok(())
    }

    pub(super) fn exec_no_module_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let module = Self::const_str(code, name_idx);
        self.interpreter.no_module(module)?;
        self.sync_locals_from_env(code);
        Ok(())
    }

    pub(super) fn exec_need_module_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let module = Self::const_str(code, name_idx);
        self.interpreter.need_module(module)?;
        self.sync_locals_from_env(code);
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
        self.sync_locals_from_env(code);
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
            self.sync_locals_from_env(code);
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
                *is_hidden,
                hidden_parents,
                does_parents,
                body,
            )?;
            // Register CUnion repr if present
            if let Some(repr_name) = repr
                && repr_name == "CUnion"
            {
                self.interpreter.register_cunion_class(&resolved_name);
            }
            // Register the class name in the lexical env so that
            // ::("ClassName") indirect lookups can find it in the current scope.
            let env = self.interpreter.env_mut();
            env.insert("_".to_string(), Value::Package(resolved_name.clone()));
            env.entry(resolved_name.clone())
                .or_insert(Value::Package(resolved_name));
            self.sync_locals_from_env(code);
            Ok(())
        } else {
            Err(RuntimeError::new("RegisterClass expects ClassDecl"))
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
            self.interpreter
                .env_mut()
                .insert("_".to_string(), Value::Package(name_str));
            self.sync_locals_from_env(code);
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
        } = stmt
        {
            self.interpreter
                .register_subset_decl(&name.resolve(), base, predicate.as_ref());
            self.sync_locals_from_env(code);
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
        self.sync_locals_from_env(code);
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

        // Enter react mode: whenever blocks will register subscriptions
        self.interpreter.enter_react();
        let saved_depth = self.stack.len();
        let run_result = self.run_range(code, body_start, end, compiled_fns);
        self.stack.truncate(saved_depth);

        // Run the react event loop (processes all registered subscriptions)
        let event_result = self.interpreter.run_react_event_loop();
        self.sync_locals_from_env(code);

        *ip = end;
        run_result?;
        event_result
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
            self.sync_locals_from_env(code);
            Ok(())
        } else {
            Err(RuntimeError::new("WheneverScope expects Block body"))
        }
    }
}
