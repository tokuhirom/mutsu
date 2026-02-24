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
            body,
            ..
        } = stmt
        {
            let val = Value::make_sub(
                self.interpreter.current_package().to_string(),
                String::new(),
                params.clone(),
                param_defs.clone(),
                body.clone(),
                self.interpreter.env().clone(),
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
            body,
            ..
        } = stmt
        {
            let val = Value::make_sub(
                self.interpreter.current_package().to_string(),
                String::new(),
                params.clone(),
                param_defs.clone(),
                body.clone(),
                self.interpreter.env().clone(),
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
            signature_alternates,
            body,
            multi,
            is_export,
            is_test_assertion,
            supersede,
        } = stmt
        {
            let resolved_name = if let Some(expr) = name_expr {
                self.interpreter
                    .eval_block_value(&[Stmt::Expr(expr.clone())])?
                    .to_string_value()
            } else {
                name.clone()
            };
            self.interpreter.register_sub_decl(
                &resolved_name,
                params,
                param_defs,
                body,
                *multi,
                *is_test_assertion,
                *supersede,
            )?;
            if *is_export && !self.interpreter.suppress_exports {
                self.interpreter.register_sub_decl_as_global(
                    &resolved_name,
                    params,
                    param_defs,
                    body,
                    *multi,
                    *is_test_assertion,
                    *supersede,
                )?;
            }
            for (alt_params, alt_param_defs) in signature_alternates {
                self.interpreter.register_sub_decl(
                    &resolved_name,
                    alt_params,
                    alt_param_defs,
                    body,
                    *multi,
                    *is_test_assertion,
                    *supersede,
                )?;
                if *is_export && !self.interpreter.suppress_exports {
                    self.interpreter.register_sub_decl_as_global(
                        &resolved_name,
                        alt_params,
                        alt_param_defs,
                        body,
                        *multi,
                        *is_test_assertion,
                        *supersede,
                    )?;
                }
            }
            self.sync_locals_from_env(code);
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
                self.interpreter
                    .register_token_decl(name, params, param_defs, body, *multi);
                self.sync_locals_from_env(code);
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
        } = stmt
        {
            self.interpreter
                .register_proto_decl(name, params, param_defs, body)?;
            if *is_export {
                self.interpreter
                    .register_proto_decl_as_global(name, params, param_defs, body)?;
            }
            self.sync_locals_from_env(code);
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
            self.interpreter.register_proto_token_decl(name);
            self.sync_locals_from_env(code);
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

    pub(super) fn exec_use_lib_path_op(&mut self, code: &CompiledCode) -> Result<(), RuntimeError> {
        let value = self.stack.pop().unwrap_or(Value::Nil);
        let path = value.to_string_value();
        if path.is_empty() {
            return Err(RuntimeError::new(
                "X::LibEmpty: Repository specification can not be an empty string",
            ));
        }
        self.interpreter.add_lib_path(path);
        self.sync_locals_from_env(code);
        Ok(())
    }

    pub(super) fn exec_register_enum_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::EnumDecl { name, variants } = stmt {
            let result = self.interpreter.register_enum_decl(name, variants)?;
            // For anonymous enums, push the Map result onto the stack
            if name.is_empty() {
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
            body,
        } = stmt
        {
            let resolved_name = if let Some(expr) = name_expr {
                self.interpreter
                    .eval_block_value(&[Stmt::Expr(expr.clone())])?
                    .to_string_value()
            } else {
                name.clone()
            };
            self.interpreter
                .register_class_decl(&resolved_name, parents, body)?;
            self.interpreter
                .env_mut()
                .insert("_".to_string(), Value::Package(resolved_name));
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
            body,
        } = stmt
        {
            self.interpreter
                .register_role_decl(name, type_params, body)?;
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
                .register_subset_decl(name, base, predicate.as_ref());
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
