use super::*;
use crate::ast::{CallArg, Expr, Stmt};

impl VM {
    fn try_eval_simple_protect_expr(
        &self,
        outer_code: &CompiledCode,
        expr: &Expr,
    ) -> Option<Value> {
        let local_value = |name: &str| {
            outer_code
                .locals
                .iter()
                .position(|candidate| candidate == name)
                .and_then(|slot| self.locals.get(slot).cloned())
        };
        match expr {
            Expr::Literal(value) => Some(value.clone()),
            Expr::Var(name) => local_value(name).or_else(|| self.get_env_with_main_alias(name)),
            Expr::ArrayVar(name) => {
                let sigiled = format!("@{name}");
                local_value(&sigiled).or_else(|| self.get_env_with_main_alias(&sigiled))
            }
            Expr::HashVar(name) => {
                let sigiled = format!("%{name}");
                local_value(&sigiled).or_else(|| self.get_env_with_main_alias(&sigiled))
            }
            _ => None,
        }
    }

    pub(super) fn try_exec_simple_shared_protect_block(
        &mut self,
        outer_code: &CompiledCode,
        code_val: &Value,
    ) -> Result<Option<Value>, RuntimeError> {
        let Value::Sub(data) = code_val else {
            return Ok(None);
        };
        let [Stmt::Call { name, args }] = data.body.as_slice() else {
            return Ok(None);
        };
        let method = name.resolve();
        if method != "push" {
            return Ok(None);
        }
        let Some(CallArg::Positional(Expr::ArrayVar(target_name))) = args.first() else {
            return Ok(None);
        };
        let sigiled_target = format!("@{target_name}");
        if !self.interpreter.shared_vars_active
            || !matches!(
                self.interpreter.get_shared_var(&sigiled_target),
                Some(Value::Array(..))
            )
        {
            return Ok(None);
        }
        let mut values = Vec::with_capacity(args.len().saturating_sub(1));
        for arg in args.iter().skip(1) {
            let CallArg::Positional(expr) = arg else {
                return Ok(None);
            };
            let Some(value) = self.try_eval_simple_protect_expr(outer_code, expr) else {
                return Ok(None);
            };
            values.push(value);
        }
        let Some(target_value) =
            self.try_eval_simple_protect_expr(outer_code, &Expr::ArrayVar(target_name.clone()))
        else {
            return Ok(None);
        };
        let result = self
            .interpreter
            .push_to_shared_var(&sigiled_target, values, &target_value);
        Ok(Some(result))
    }

    /// Try compiled method fast path; fall back to interpreter.
    pub(super) fn try_compiled_method_or_interpret(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Value::Instance { class_name, .. } = &target {
            let class = class_name.resolve();
            if self.interpreter.is_native_method(&class, method) {
                return self
                    .interpreter
                    .call_method_with_values(target, method, args);
            }
        }
        // Pseudo-methods must always go through the interpreter which handles
        // them specially — never intercept via the compiled fast path.
        if matches!(
            method,
            "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
        ) {
            return self
                .interpreter
                .call_method_with_values(target, method, args);
        }
        // Private method fast path: resolve private candidate and run compiled code
        // when caller context clearly allows direct dispatch.
        if method.starts_with('!') {
            let class_name = match &target {
                Value::Instance { class_name, .. } => Some(class_name.resolve()),
                Value::Package(name) => Some(name.resolve()),
                _ => None,
            };
            if let Some(cn) = class_name {
                let resolved = self
                    .interpreter
                    .resolve_private_method_for_vm(&cn, method, &args);
                if let Some((owner_class, method_def)) = resolved {
                    let caller_allowed = self
                        .interpreter
                        .can_fast_dispatch_private_method_vm(&owner_class);
                    if caller_allowed && let Some(ref cc) = method_def.compiled_code {
                        let cc = cc.clone();
                        let target_id = match &target {
                            Value::Instance { id, .. } => Some(*id),
                            _ => None,
                        };
                        let attributes = match &target {
                            Value::Instance { attributes, .. } => (**attributes).clone(),
                            _ => std::collections::HashMap::new(),
                        };
                        let invocant_for_dispatch = if attributes.is_empty() {
                            Value::Package(crate::symbol::Symbol::intern(&cn))
                        } else {
                            target.clone()
                        };
                        let pushed_dispatch = self.interpreter.push_method_dispatch_frame(
                            &cn,
                            method,
                            &args,
                            invocant_for_dispatch,
                        );
                        let invocant = Some(target);
                        let empty_fns = HashMap::new();
                        let method_result = self.call_compiled_method(
                            &cn,
                            &owner_class,
                            method,
                            &method_def,
                            &cc,
                            attributes,
                            args,
                            invocant,
                            &empty_fns,
                        );
                        if pushed_dispatch {
                            self.interpreter.pop_method_dispatch();
                        }
                        self.interpreter.pop_method_samewith_context();
                        let (result, new_attrs) = method_result?;
                        if let Some(id) = target_id {
                            self.interpreter.overwrite_instance_bindings_by_identity(
                                &cn,
                                id,
                                new_attrs.clone(),
                            );
                            if let Value::Proxy { ref fetcher, .. } = result {
                                return self
                                    .interpreter
                                    .proxy_fetch(fetcher, None, &cn, &new_attrs, id);
                            }
                        }
                        return Ok(result);
                    }
                }
            }
        }
        // Only attempt compiled path for Instance or Package targets
        let class_name = match &target {
            Value::Instance { class_name, .. } => Some(class_name.resolve()),
            Value::Package(name) => Some(name.resolve()),
            _ => None,
        };
        if let Some(cn) = class_name
            && let Some((owner_class, method_def)) = self
                .interpreter
                .resolve_method_with_owner(&cn, method, &args)
            && let Some(ref cc) = method_def.compiled_code
        {
            let cc = cc.clone();
            let target_id = match &target {
                Value::Instance { id, .. } => Some(*id),
                _ => None,
            };
            let attributes = match &target {
                Value::Instance { attributes, .. } => (**attributes).clone(),
                _ => std::collections::HashMap::new(),
            };
            // Set up dispatch frame for nextsame/callsame support
            let invocant_for_dispatch = if attributes.is_empty() {
                Value::Package(crate::symbol::Symbol::intern(&cn))
            } else {
                target.clone()
            };
            let pushed_dispatch = self.interpreter.push_method_dispatch_frame(
                &cn,
                method,
                &args,
                invocant_for_dispatch,
            );
            let invocant = Some(target);
            // Method bodies are compiled independently; function calls
            // within them resolve through the interpreter fallback.
            let empty_fns = HashMap::new();
            let method_result = self.call_compiled_method(
                &cn,
                &owner_class,
                method,
                &method_def,
                &cc,
                attributes,
                args,
                invocant,
                &empty_fns,
            );
            if pushed_dispatch {
                self.interpreter.pop_method_dispatch();
            }
            self.interpreter.pop_method_samewith_context();
            let (result, new_attrs) = method_result?;
            // Propagate attribute mutations to all bindings of this instance
            if let Some(id) = target_id {
                self.interpreter.overwrite_instance_bindings_by_identity(
                    &cn,
                    id,
                    new_attrs.clone(),
                );
                // Auto-FETCH if the method returned a Proxy
                if let Value::Proxy { ref fetcher, .. } = result {
                    return self
                        .interpreter
                        .proxy_fetch(fetcher, None, &cn, &new_attrs, id);
                }
            }
            return Ok(result);
        }
        self.interpreter
            .call_method_with_values(target, method, args)
    }

    pub(super) fn try_compiled_method_mut_or_interpret(
        &mut self,
        target_name: &str,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Value::Instance { class_name, .. } = &target {
            let class = class_name.resolve();
            if self.interpreter.is_native_method(&class, method) {
                return self.interpreter.call_method_mut_with_values(
                    target_name,
                    target,
                    method,
                    args,
                );
            }
        }
        if matches!(
            method,
            "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
        ) {
            return self
                .interpreter
                .call_method_mut_with_values(target_name, target, method, args);
        }
        if method.starts_with('!') {
            let class_name = match &target {
                Value::Instance { class_name, .. } => Some(class_name.resolve()),
                Value::Package(name) => Some(name.resolve()),
                _ => None,
            };
            if let Some(cn) = class_name {
                let resolved = self
                    .interpreter
                    .resolve_private_method_for_vm(&cn, method, &args);
                if let Some((owner_class, method_def)) = resolved {
                    let caller_allowed = self
                        .interpreter
                        .can_fast_dispatch_private_method_vm(&owner_class);
                    if caller_allowed && let Some(ref cc) = method_def.compiled_code {
                        let cc = cc.clone();
                        let target_id = match &target {
                            Value::Instance { id, .. } => Some(*id),
                            _ => None,
                        };
                        let attributes = match &target {
                            Value::Instance { attributes, .. } => (**attributes).clone(),
                            _ => std::collections::HashMap::new(),
                        };
                        let invocant_for_dispatch = if attributes.is_empty() {
                            Value::Package(crate::symbol::Symbol::intern(&cn))
                        } else {
                            target.clone()
                        };
                        let pushed_dispatch = self.interpreter.push_method_dispatch_frame(
                            &cn,
                            method,
                            &args,
                            invocant_for_dispatch,
                        );
                        let invocant = Some(target);
                        let empty_fns = HashMap::new();
                        let method_result = self.call_compiled_method(
                            &cn,
                            &owner_class,
                            method,
                            &method_def,
                            &cc,
                            attributes,
                            args,
                            invocant,
                            &empty_fns,
                        );
                        if pushed_dispatch {
                            self.interpreter.pop_method_dispatch();
                        }
                        self.interpreter.pop_method_samewith_context();
                        let (result, new_attrs) = method_result?;
                        if let Some(id) = target_id {
                            self.interpreter.overwrite_instance_bindings_by_identity(
                                &cn,
                                id,
                                new_attrs.clone(),
                            );
                            if let Value::Proxy { ref fetcher, .. } = result {
                                return self
                                    .interpreter
                                    .proxy_fetch(fetcher, None, &cn, &new_attrs, id);
                            }
                        }
                        return Ok(result);
                    }
                }
            }
        }
        let class_name = match &target {
            Value::Instance { class_name, .. } => Some(class_name.resolve()),
            Value::Package(name) => Some(name.resolve()),
            _ => None,
        };
        if let Some(cn) = class_name
            && let Some((owner_class, method_def)) = self
                .interpreter
                .resolve_method_with_owner(&cn, method, &args)
            && let Some(ref cc) = method_def.compiled_code
        {
            let cc = cc.clone();
            let target_id = match &target {
                Value::Instance { id, .. } => Some(*id),
                _ => None,
            };
            let attributes = match &target {
                Value::Instance { attributes, .. } => (**attributes).clone(),
                _ => std::collections::HashMap::new(),
            };
            let invocant_for_dispatch = if attributes.is_empty() {
                Value::Package(crate::symbol::Symbol::intern(&cn))
            } else {
                target.clone()
            };
            let pushed_dispatch = self.interpreter.push_method_dispatch_frame(
                &cn,
                method,
                &args,
                invocant_for_dispatch,
            );
            let invocant = Some(target);
            let empty_fns = HashMap::new();
            let method_result = self.call_compiled_method(
                &cn,
                &owner_class,
                method,
                &method_def,
                &cc,
                attributes,
                args,
                invocant,
                &empty_fns,
            );
            if pushed_dispatch {
                self.interpreter.pop_method_dispatch();
            }
            self.interpreter.pop_method_samewith_context();
            let (result, new_attrs) = method_result?;
            if let Some(id) = target_id {
                self.interpreter.overwrite_instance_bindings_by_identity(
                    &cn,
                    id,
                    new_attrs.clone(),
                );
                if let Value::Proxy { ref fetcher, .. } = result {
                    return self
                        .interpreter
                        .proxy_fetch(fetcher, None, &cn, &new_attrs, id);
                }
            }
            return Ok(result);
        }
        self.interpreter
            .call_method_mut_with_values(target_name, target, method, args)
    }

    /// Execute a protect block inline in the current VM, avoiding the overhead
    /// of creating a new VM.
    pub(super) fn exec_protect_block_inline(
        &mut self,
        outer_code: &CompiledCode,
        code_val: &Value,
    ) -> Result<Value, RuntimeError> {
        let outer_local_slots: std::collections::HashMap<&str, usize> = outer_code
            .locals
            .iter()
            .enumerate()
            .map(|(idx, name)| (name.as_str(), idx))
            .collect();
        let (block_cc, block_fns, captured_env, captured_bindings, writeback_bindings) =
            match code_val {
                Value::Sub(data) => {
                    let (
                        block_cc,
                        block_fns,
                        captured_bindings,
                        writeback_bindings,
                        captured_names,
                    ) = self
                        .interpreter
                        .get_or_compile_protect_block_with_slots(data);
                    self.interpreter.sync_shared_vars_for_names(
                        captured_names.iter().map(|name| name.as_str()),
                    );
                    (
                        block_cc,
                        block_fns,
                        Some(&data.env),
                        captured_bindings,
                        writeback_bindings,
                    )
                }
                _ => {
                    // TODO: Handle non-Sub protect blocks (e.g. WeakSub, Routine)
                    // in the VM. Currently these are rare and delegate to interpreter.
                    return self.interpreter.call_protect_block(code_val);
                }
            };

        // Save/swap stack and locals for the block
        let mut saved_locals = std::mem::take(&mut self.locals);
        let saved_stack = std::mem::take(&mut self.stack);
        let saved_env_dirty = self.env_dirty;
        let saved_locals_dirty = self.locals_dirty;

        // Initialize locals for the block
        self.locals = vec![Value::Nil; block_cc.locals.len()];
        self.env_dirty = false;
        self.locals_dirty = false;
        if captured_env.is_some() {
            for (slot, name) in captured_bindings.iter() {
                if (name.starts_with('@') || name.starts_with('%'))
                    && self.interpreter.get_shared_var(name).is_some()
                {
                    // Leave shared collections unmaterialized in locals.
                    // GetLocal will read the shared value on demand.
                    continue;
                }
                if let Some(outer_slot) = outer_local_slots.get(name.as_str())
                    && let Some(val) = saved_locals.get(*outer_slot)
                {
                    self.locals[*slot] = val.clone();
                    continue;
                }
                if let Some(val) = self.interpreter.env().get(name) {
                    self.locals[*slot] = val.clone();
                }
            }
        }

        // Execute the block's opcodes inline
        let mut sub_ip = 0;
        let mut exec_err = None;
        while sub_ip < block_cc.ops.len() {
            if let Err(e) = self.exec_one(&block_cc, &mut sub_ip, &block_fns) {
                exec_err = Some(e);
                break;
            }
        }

        // Sync locals back to env
        if let Some(captured) = captured_env {
            for (slot, name) in writeback_bindings.iter() {
                if matches!(
                    self.interpreter.get_shared_var(name),
                    Some(Value::Array(..) | Value::Hash(..))
                ) {
                    continue;
                }
                if let Some(outer_slot) = outer_local_slots.get(name.as_str())
                    && let Some(target) = saved_locals.get_mut(*outer_slot)
                {
                    *target = self.locals[*slot].clone();
                }
                if captured.contains_key(name)
                    && !matches!(
                        self.interpreter.get_shared_var(name),
                        Some(Value::Array(..) | Value::Hash(..))
                    )
                {
                    self.interpreter
                        .env_mut()
                        .insert(name.clone(), self.locals[*slot].clone());
                }
            }
        }

        // Get return value before restoring state
        let ret_val = self.stack.pop().unwrap_or(Value::Nil);

        // Restore outer state
        self.locals = saved_locals;
        self.stack = saved_stack;
        self.env_dirty = saved_env_dirty;
        self.locals_dirty = saved_locals_dirty;

        match exec_err {
            Some(e) => Err(e),
            None => Ok(ret_val),
        }
    }
}
