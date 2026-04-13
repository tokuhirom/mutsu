use super::*;
use crate::compiler::Compiler;
use crate::symbol::Symbol;

impl VM {
    /// Get the current source line number from the interpreter env.
    fn current_source_line(&self) -> Option<u32> {
        self.interpreter.env().get("?LINE").and_then(|v| match v {
            Value::Int(n) => Some(*n as u32),
            _ => None,
        })
    }

    /// Get the current source file from the interpreter env.
    fn current_source_file(&self) -> Option<String> {
        self.interpreter.env().get("?FILE").and_then(|v| match v {
            Value::Str(s) => Some(s.to_string()),
            _ => None,
        })
    }

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
            // Compile the gather body to bytecode for VM-native forcing
            let compiler = Compiler::new();
            let (compiled_code, compiled_fns) = compiler.compile(body);
            let list = LazyList {
                body: body.clone(),
                env,
                cache: std::sync::Mutex::new(None),
                compiled_code: Some(std::sync::Arc::new(compiled_code)),
                compiled_fns: Some(std::sync::Arc::new(compiled_fns)),
                elems_count: None,
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
            let params = crate::ast::collect_placeholders_shallow(body);
            let compiled_code = Self::resolve_closure_code(code, cc_idx);
            let cc_source_line = compiled_code
                .as_ref()
                .and_then(|cc| cc.source_line)
                .map(|l| l as u32)
                .or_else(|| self.current_source_line());
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
                is_bare_block: true,
                compiled_code,
                deprecated_message: None,
                source_line: cc_source_line,
                source_file: self.current_source_file(),
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
            self.ensure_env_synced(code);
            let mut env = self.interpreter.env().clone();
            if let Some(rt) = return_type {
                env.insert("__mutsu_return_type".to_string(), Value::str(rt.clone()));
            }
            if is_whatever_code {
                env.insert(
                    "__mutsu_callable_type".to_string(),
                    Value::str_from("WhateverCode"),
                );
            }
            let compiled_code = Self::resolve_closure_code(code, cc_idx);
            let cc_source_line = compiled_code
                .as_ref()
                .and_then(|cc| cc.source_line)
                .map(|l| l as u32)
                .or_else(|| self.current_source_line());
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
                empty_sig: params.is_empty() && param_defs.is_empty(),
                is_bare_block: false,
                compiled_code,
                deprecated_message: None,
                source_line: cc_source_line,
                source_file: self.current_source_file(),
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
            self.ensure_env_synced(code);
            let mut env = self.interpreter.env().clone();
            if let Some(rt) = return_type {
                env.insert("__mutsu_return_type".to_string(), Value::str(rt.clone()));
            }
            if is_whatever_code {
                env.insert(
                    "__mutsu_callable_type".to_string(),
                    Value::str_from("WhateverCode"),
                );
            }
            let compiled_code = Self::resolve_closure_code(code, cc_idx);
            let cc_source_line = compiled_code
                .as_ref()
                .and_then(|cc| cc.source_line)
                .map(|l| l as u32)
                .or_else(|| self.current_source_line());
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
                empty_sig: params.is_empty() && param_defs.is_empty(),
                is_bare_block: false,
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
            self.ensure_env_synced(code);
            let compiled_code = Self::resolve_closure_code(code, cc_idx);
            let cc_source_line = compiled_code
                .as_ref()
                .and_then(|cc| cc.source_line)
                .map(|l| l as u32)
                .or_else(|| self.current_source_line());
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
                is_bare_block: true,
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
            ..
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
            self.fn_resolve_gen += 1;
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
        tags_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let module = Self::const_str(code, name_idx);
        let tags: Vec<String> = tags_idx
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
        self.interpreter.use_module_with_tags(module, &tags)?;
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
                // Replace existing Nil and uninitialized (Package("Any"))
                // elements with the default value (Raku container semantics:
                // Nil/uninitialized slots in a defaulted container become
                // the default).
                if name.starts_with('@')
                    && let Value::Array(ref items, kind) = container
                {
                    let is_hole = |v: &Value| {
                        matches!(v, Value::Nil) || matches!(v, Value::Package(n) if n == "Any")
                    };
                    let has_holes = items.iter().any(is_hole);
                    if has_holes {
                        let replaced: Vec<Value> = items
                            .iter()
                            .map(|v| {
                                if is_hole(v) {
                                    default_value.clone()
                                } else {
                                    v.clone()
                                }
                            })
                            .collect();
                        let new_arr = Value::Array(Arc::new(replaced), kind);
                        self.interpreter
                            .set_container_default(&new_arr, default_value.clone());
                        self.locals_set_by_name(code, &name, new_arr.clone());
                        self.set_env_with_main_alias(&name, new_arr);
                    }
                }
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
                // Get current value, convert to buf.
                // The value might be an Array (from the initializer) or already
                // a Buf/Blob Instance (if SetLocal coerced through an old Buf
                // container in the same slot, e.g. in a loop redeclaration).
                let current = self.locals_get_by_name(code, name).unwrap_or(Value::Nil);
                let items = match &current {
                    Value::Array(items, ..) => items
                        .iter()
                        .map(|v| Value::Int(crate::runtime::to_int(v)))
                        .collect(),
                    Value::Instance { attributes, .. } => {
                        // Extract items from an existing Buf/Blob instance
                        if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                            items
                                .iter()
                                .map(|v| Value::Int(crate::runtime::to_int(v)))
                                .collect()
                        } else {
                            Vec::new()
                        }
                    }
                    _ => Vec::new(),
                };
                let buf = self.try_compiled_method_or_interpret(buf_type, "new", items)?;
                let name_str = name.to_string();
                self.locals_set_by_name(code, &name_str, buf.clone());
                self.set_env_with_main_alias(&name_str, buf);
                self.env_dirty = true;
                return Ok(());
            }
        }

        // Handle `is Map` on hash variables: register the hash as a Map type
        // and mark it read-only (Maps are immutable in Raku).
        if name.starts_with('%') && trait_name == "Map" {
            if has_arg {
                self.stack.pop(); // discard unused trait argument
            }
            let name_str = name.to_string();
            // Register container type metadata with declared_type "Map"
            if let Some(container) = self.locals_get_by_name(code, &name_str) {
                let info = crate::runtime::ContainerTypeInfo {
                    value_type: String::new(),
                    key_type: None,
                    declared_type: Some("Map".to_string()),
                };
                self.interpreter
                    .register_container_type_metadata(&container, info);
            }
            // Mark the variable read-only to prevent mutation
            self.interpreter.mark_readonly(&name_str);
            self.env_dirty = true;
            return Ok(());
        }

        // Handle `is BagHash`, `is SetHash`, `is MixHash`, `is Bag`, `is Set`, `is Mix`
        // on hash variables: replace the variable with an instance of the appropriate type.
        if name.starts_with('%') {
            let is_hash_like_trait = matches!(
                trait_name.as_str(),
                "BagHash" | "SetHash" | "MixHash" | "Bag" | "Set" | "Mix"
            );
            if is_hash_like_trait {
                if has_arg {
                    self.stack.pop(); // discard unused trait argument
                }
                let name_str = name.to_string();
                // Check if the variable already has initial values from the declaration.
                // If so, construct the QuantHash from those values instead of creating
                // an empty one. This handles `my %h is Bag = <a b b c>`.
                let current_val = self.locals_get_by_name(code, &name_str);
                let has_init_values = match &current_val {
                    Some(Value::Hash(h)) => !h.is_empty(),
                    Some(Value::Array(a, _)) => !a.is_empty(),
                    // Already converted to a QuantHash by type constraint coercion
                    Some(Value::Set(_, _) | Value::Bag(_, _) | Value::Mix(_, _)) => true,
                    _ => false,
                };
                let instance = if has_init_values {
                    let init_val = current_val.unwrap();
                    // If already the target QuantHash type, use it directly
                    let already_target = matches!(
                        (&init_val, trait_name.as_str()),
                        (Value::Mix(_, _), "MixHash" | "Mix")
                            | (Value::Bag(_, _), "BagHash" | "Bag")
                            | (Value::Set(_, _), "SetHash" | "Set")
                    );
                    if already_target {
                        // Ensure mutability flag matches the trait
                        match init_val {
                            Value::Mix(data, _) => {
                                let mutable = trait_name == "MixHash";
                                Value::Mix(data, mutable)
                            }
                            Value::Bag(data, _) => {
                                let mutable = trait_name == "BagHash";
                                Value::Bag(data, mutable)
                            }
                            Value::Set(data, _) => {
                                let mutable = trait_name == "SetHash";
                                Value::Set(data, mutable)
                            }
                            other => other,
                        }
                    } else {
                        // Convert initial values to the target QuantHash type
                        self.try_compiled_method_or_interpret(init_val, &trait_name, vec![])?
                    }
                } else {
                    let type_obj = Value::Package(crate::symbol::Symbol::intern(&trait_name));
                    self.try_compiled_method_or_interpret(type_obj, "new", vec![])?
                };
                // Register container type metadata so assignment operations
                // know to coerce back to BagHash/SetHash/etc.
                let info = crate::runtime::ContainerTypeInfo {
                    value_type: String::new(),
                    key_type: None,
                    declared_type: Some(trait_name.clone()),
                };
                self.interpreter
                    .register_container_type_metadata(&instance, info);
                self.locals_set_by_name(code, &name_str, instance.clone());
                self.set_env_with_main_alias(&name_str, instance.clone());
                // Set type constraint so future assignments are coerced correctly
                self.interpreter
                    .set_var_type_constraint(&name_str, Some(trait_name.clone()));
                self.env_dirty = true;
                return Ok(());
            }
        }

        // For array/hash variables with a class trait that inherits from Array[X] or Hash[V,K],
        // propagate the element type constraint so .of works correctly.
        if name.starts_with('@') || name.starts_with('%') {
            let element_type = self.find_parameterized_container_parent(&trait_name);
            if let Some(et) = element_type {
                if has_arg {
                    self.stack.pop(); // discard unused trait argument
                }
                let name_str = name.to_string();
                self.interpreter
                    .set_var_type_constraint(&name_str, Some(et));
                self.env_dirty = true;
                return Ok(());
            }
        }

        if !(self.interpreter.has_proto("trait_mod:<is>")
            || self.interpreter.has_multi_candidates("trait_mod:<is>"))
        {
            // For uppercase type-like traits (e.g. `is Map`, `is Set`), silently
            // accept them even if trait_mod:<is> is not defined. These are type
            // container traits that may not have runtime trait_mod:<is> handlers.
            if trait_name
                .chars()
                .next()
                .is_some_and(|c| c.is_ascii_uppercase())
            {
                if has_arg {
                    self.stack.pop();
                }
                return Ok(());
            }
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
            base_type,
            language_version,
        } = stmt
        {
            let result = self.interpreter.register_enum_decl(
                &name.resolve(),
                variants,
                *is_export,
                base_type.as_deref(),
            )?;
            // Store language revision metadata from the version captured at parse time
            if !name.resolve().is_empty() {
                self.interpreter
                    .store_language_revision_from_version(&name.resolve(), language_version);
            }
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
            is_lexical,
            hidden_parents,
            does_parents,
            repr,
            body,
            language_version,
        } = stmt
        {
            let resolved_name = if let Some(expr) = name_expr {
                self.interpreter
                    .eval_block_value(&[Stmt::Expr(expr.clone())])?
                    .to_string_value()
            } else {
                name.resolve()
            };
            let current_package = self.interpreter.current_package().to_string();
            let qualified_name = if let Some(stripped) = resolved_name.strip_prefix("GLOBAL::") {
                // `class GLOBAL::Foo` declares Foo in the global namespace
                stripped.to_string()
            } else if resolved_name.contains("::")
                || current_package == "GLOBAL"
                || resolved_name == current_package
            {
                resolved_name.clone()
            } else {
                format!("{current_package}::{resolved_name}")
            };
            // If the name was previously suppressed (e.g. by a `my class` in an
            // earlier block), clear the suppression before running the class body
            // so that references to the class name inside the body can resolve.
            self.interpreter.unsuppress_name(&resolved_name);
            self.interpreter.register_class_decl(
                &qualified_name,
                parents,
                crate::runtime::ClassDeclModifiers {
                    class_is_rw: *class_is_rw,
                    is_hidden: *is_hidden,
                    is_lexical: *is_lexical,
                    hidden_parents,
                    does_parents,
                },
                body,
            )?;
            // Check for assignment to native read-only params before
            // compiling (X::Assignment::RO::Comp).
            if let Some(err) = self
                .interpreter
                .check_class_native_readonly_param_errors(&qualified_name)
            {
                return Err(err);
            }
            // Compile method bodies to bytecode for the fast path
            self.interpreter.compile_class_methods(&qualified_name);
            // Register CUnion repr if present
            if let Some(repr_name) = repr
                && repr_name == "CUnion"
            {
                self.interpreter.register_cunion_class(&qualified_name);
            }
            // Register the class name in the lexical env so that
            // ::("ClassName") indirect lookups can find it in the current scope.
            let env = self.interpreter.env_mut();
            env.insert(
                "_".to_string(),
                Value::Package(Symbol::intern(&qualified_name)),
            );
            // Always insert the class type object so that class names take
            // precedence over same-named `$`-sigiled variables (whose stripped
            // name may already be in the env).
            env.insert(
                qualified_name.clone(),
                Value::Package(Symbol::intern(&qualified_name)),
            );
            // When a nested class is registered inside another class (e.g. class B inside class A
            // becomes A::B), suppress the short name (B) so it cannot be used outside.
            // Only suppress when the parent package is itself a class, not a module.
            // Also register the short name in the lexical env so it is available
            // within the enclosing class body and its methods.
            let parent_is_class = qualified_name
                .rsplit_once("::")
                .map(|(parent, _)| self.interpreter.has_class(parent))
                .unwrap_or(false);
            if qualified_name != resolved_name && !resolved_name.contains("::") && parent_is_class {
                self.interpreter.suppress_name(&resolved_name);
                // Register the short name in the lexical env so it resolves
                // within the enclosing class scope (e.g. `Frog` inside `Forest`).
                let env = self.interpreter.env_mut();
                env.insert(
                    resolved_name.clone(),
                    Value::Package(Symbol::intern(&qualified_name)),
                );
            }
            // When a class is declared with an already-qualified name
            // (e.g. the compiler pre-qualified `class C1` inside
            // `unit module M` to `M::C1`), also register the short name
            // `C1` in the env so that subsequent code inside the same
            // module can refer to it bare. Skip this when the parent
            // package is a class (where suppress_name semantics apply).
            if qualified_name.contains("::") && !parent_is_class {
                let short = qualified_name
                    .rsplit_once("::")
                    .map(|(_, s)| s.to_string())
                    .unwrap_or_else(|| qualified_name.clone());
                if !short.is_empty() && short != qualified_name {
                    let env = self.interpreter.env_mut();
                    env.entry(short)
                        .or_insert_with(|| Value::Package(Symbol::intern(&qualified_name)));
                }
            }
            // When `my class` is used, register the class name as lexically scoped
            // so it gets suppressed when the enclosing block scope exits.
            if *is_lexical {
                self.interpreter
                    .register_lexical_class(resolved_name.clone());
                // Also mark as my-scoped so it's excluded from the parent package stash
                self.interpreter
                    .mark_my_scoped_package_item(qualified_name.clone());
            }
            // Store language revision metadata from the version captured at parse time
            self.interpreter
                .store_language_revision_from_version(&qualified_name, language_version);
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
                return Err(RuntimeError::typed_msg(
                    "X::Syntax::Augment::WithoutMonkeyTyping",
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
            is_export,
            export_tags,
            body,
            is_rw,
            language_version,
        } = stmt
        {
            let name_str = name.resolve();
            let current_package = self.interpreter.current_package().to_string();
            let qualified_name = if let Some(stripped) = name_str.strip_prefix("GLOBAL::") {
                stripped.to_string()
            } else if name_str.contains("::")
                || current_package == "GLOBAL"
                || name_str == current_package
            {
                name_str.clone()
            } else {
                format!("{current_package}::{name_str}")
            };
            // If the short name was suppressed by an earlier lexical type with
            // the same name, re-enable it before registering the new role.
            self.interpreter.unsuppress_name(&name_str);
            self.interpreter.register_role_decl(
                &qualified_name,
                type_params,
                type_param_defs,
                body,
                *is_rw,
            )?;
            if *is_export && !self.interpreter.suppress_exports {
                // The compiler may have pre-qualified the role name
                // (e.g. `R1` → `GH2613::R1`) when compiling under a
                // `unit module`. Exports use the short bare name and
                // the originating package, so split the qualified name.
                let (export_pkg, export_short) =
                    if let Some((pkg, short)) = name_str.rsplit_once("::") {
                        (pkg.to_string(), short.to_string())
                    } else {
                        (current_package.clone(), name_str.clone())
                    };
                self.interpreter.register_exported_var(
                    export_pkg,
                    export_short,
                    export_tags.clone(),
                );
            }
            // Store language revision metadata from the version captured at parse time
            self.interpreter
                .store_language_revision_from_version(&qualified_name, language_version);
            // Compile role method bodies to bytecode
            self.interpreter.compile_role_methods(&qualified_name);
            self.interpreter.env_mut().insert(
                "_".to_string(),
                Value::Package(Symbol::intern(&qualified_name)),
            );
            self.interpreter.env_mut().insert(
                qualified_name.clone(),
                Value::Package(Symbol::intern(&qualified_name)),
            );
            if qualified_name != name_str && !name_str.contains("::") {
                self.interpreter.env_mut().insert(
                    name_str.clone(),
                    Value::Package(Symbol::intern(&qualified_name)),
                );
            }
            // When a role is declared with an already-qualified name
            // (e.g. the compiler pre-qualified `role R1` inside
            // `unit module GH2613` to `GH2613::R1`), also register the
            // short name `R1` in the env so subsequent code in the same
            // module can refer to it bare.
            if qualified_name.contains("::") && qualified_name == name_str {
                let short = qualified_name
                    .rsplit_once("::")
                    .map(|(_, s)| s.to_string())
                    .unwrap_or_else(|| qualified_name.clone());
                if !short.is_empty() && short != qualified_name {
                    self.interpreter
                        .env_mut()
                        .entry(short)
                        .or_insert_with(|| Value::Package(Symbol::intern(&qualified_name)));
                }
            }
            self.env_dirty = true;
            // Execute deferred non-declaration body statements now that the role
            // name is fully available in the environment.  This lets code like
            // `role R { method foo {}; R.foo }` work.
            if type_params.is_empty() {
                let deferred = self
                    .interpreter
                    .get_role_def(&qualified_name)
                    .map(|r| r.deferred_body_stmts.clone())
                    .unwrap_or_default();
                for stmt in &deferred {
                    self.interpreter.run_block_raw(std::slice::from_ref(stmt))?;
                }
            }
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
        // First, pull any pending env updates into locals (e.g., instance attribute
        // mutations from overwrite_instance_bindings_by_identity after bind-stdin).
        // Then flush all locals to env so captured vars are visible/mutable from
        // whenever callbacks.
        self.ensure_locals_synced(code);
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

    /// Walk the MRO of `class_name` to find a parameterized Array or Hash parent.
    /// Returns the element type if found (e.g. "Str" for `Array[Str]`).
    fn find_parameterized_container_parent(&self, class_name: &str) -> Option<String> {
        let parents = self.interpreter.class_parents_readonly(class_name);
        for parent in &parents {
            if let Some(inner) = parent
                .strip_prefix("Array[")
                .or_else(|| parent.strip_prefix("List["))
                .and_then(|s| s.strip_suffix(']'))
            {
                return Some(inner.trim().to_string());
            }
        }
        // Also check the class itself in case it IS a parameterized type
        if let Some(inner) = class_name
            .strip_prefix("Array[")
            .or_else(|| class_name.strip_prefix("List["))
            .and_then(|s| s.strip_suffix(']'))
        {
            return Some(inner.trim().to_string());
        }
        None
    }
}
