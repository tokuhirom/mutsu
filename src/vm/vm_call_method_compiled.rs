use super::*;
use crate::ast::{CallArg, Expr, Stmt};

impl Interpreter {
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
            // Decont via into_deref: a captured-mutated `$` scalar may be boxed
            // into a ContainerRef (see box_captured_lexicals), and this direct
            // locals/env read bypasses the GetLocal/GetGlobal chokepoint, so it
            // must deref here to avoid leaking a raw cell into the method
            // fast-path receiver/args.
            Expr::Var(name) => local_value(name)
                .or_else(|| self.get_env_with_main_alias(name))
                .map(Value::into_deref),
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
        if !self.shared_vars_active
            || !matches!(self.get_shared_var(&sigiled_target), Some(Value::Array(..)))
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
        if let Some(result) = loan_env!(
            self,
            push_to_existing_shared_array(&sigiled_target, values.clone())
        ) {
            return Ok(Some(result));
        }
        let Some(target_value) =
            self.try_eval_simple_protect_expr(outer_code, &Expr::ArrayVar(target_name.clone()))
        else {
            return Ok(None);
        };
        let result = loan_env!(
            self,
            push_to_shared_var(&sigiled_target, values, &target_value)
        );
        Ok(Some(result))
    }

    /// Execute a protect block inline in the current Interpreter, avoiding the overhead
    /// of creating a new Interpreter.
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
                    ) = self.get_or_compile_protect_block_with_slots(data);
                    self.sync_shared_vars_for_names(
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
                    // in the Interpreter. Currently these are rare and delegate to interpreter.
                    return self.call_protect_block(code_val);
                }
            };

        // Save/swap stack and locals for the block
        let mut saved_locals = std::mem::take(&mut self.locals);
        let saved_stack = std::mem::take(&mut self.stack);

        // Initialize locals for the block
        self.locals = vec![Value::Nil; block_cc.locals.len()];
        if captured_env.is_some() {
            for (slot, name) in captured_bindings.iter() {
                if (name.starts_with('@') || name.starts_with('%'))
                    && self.get_shared_var(name).is_some()
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
                if let Some(val) = self.env().get(name) {
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

        // The block runs inline in the current env, so a free variable it
        // assigns (`$x = 99` where `$x` is a captured outer lexical) is written
        // via SetGlobal straight into env — never as a block-local, so the
        // writeback_bindings loop below (which only handles block-locals) misses
        // it. Reconcile the *outer frame's* local slot for each such free-var
        // write from env, so the caller sees the mutation (e.g. a value assigned
        // inside `$lock.protect: { ... }`).
        for sym in &block_cc.free_var_writes {
            sym.with_str(|name| {
                if let Some(outer_slot) = outer_local_slots.get(name)
                    && let Some(val) = self.env().get(name).cloned()
                    && let Some(target) = saved_locals.get_mut(*outer_slot)
                {
                    *target = val;
                }
            });
        }
        // Sync locals back to env
        if let Some(captured) = captured_env {
            for (slot, name) in writeback_bindings.iter() {
                if matches!(
                    self.get_shared_var(name),
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
                        self.get_shared_var(name),
                        Some(Value::Array(..) | Value::Hash(..))
                    )
                {
                    {
                        let __v = self.locals[*slot].clone();
                        self.env_mut().insert(name.clone(), __v);
                    }
                }
            }
        }

        // Get return value before restoring state
        let ret_val = self.stack.pop().unwrap_or(Value::Nil);

        // Restore outer state
        self.locals = saved_locals;
        self.stack = saved_stack;

        match exec_err {
            Some(e) => Err(e),
            None => Ok(ret_val),
        }
    }

    /// Check if a method candidate has a wrap chain from ^lookup().candidates[N].wrap().
    /// If so, dispatch through the wrapper and return Some(result).
    pub(crate) fn check_method_wrap_chain(
        &mut self,
        cn: &str,
        owner_class: &str,
        method: &str,
        method_def: &crate::runtime::MethodDef,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !self.has_any_wrap_chains() || self.is_inside_wrap_dispatch() {
            return None;
        }
        let cand_idx = self.find_method_candidate_index(owner_class, method, method_def)?;
        let chain = self
            .get_method_wrap_chain(owner_class, method, cand_idx)?
            .clone();
        let invocant_for_dispatch = target.clone();
        let pushed_dispatch = loan_env!(
            self,
            push_method_dispatch_frame(cn, method, args, invocant_for_dispatch)
        );
        let mut orig_env = crate::env::Env::new();
        orig_env.insert(
            "__mutsu_method_wrap_original".to_string(),
            Value::Bool(true),
        );
        let original_sub = Value::make_sub(
            crate::symbol::Symbol::intern(owner_class),
            crate::symbol::Symbol::intern(method),
            method_def.params.clone(),
            method_def.param_defs.clone(),
            (*method_def.body).clone(),
            method_def.is_rw,
            orig_env,
        );
        let outermost = chain.last().unwrap().1.clone();
        let mut remaining: Vec<Value> = Vec::new();
        for i in (0..chain.len() - 1).rev() {
            remaining.push(chain[i].1.clone());
        }
        remaining.push(original_sub);
        let mut call_args = vec![target.clone()];
        call_args.extend(args.to_vec());
        let frame = crate::runtime::WrapDispatchFrame {
            sub_id: 0,
            remaining,
            args: call_args.clone(),
        };
        let wrapper_id = if let Value::Sub(ref wd) = outermost {
            Some(wd.id)
        } else {
            None
        };
        self.push_wrap_dispatch_frame(frame);
        let result = self.vm_call_sub_value(outermost, call_args, false);
        self.pop_wrap_dispatch_frame();
        // Propagate closure variable mutations from the wrapper back to the
        // current env so captured variables are visible to the caller.
        if let Some(wid) = wrapper_id
            && let Some(persisted) = self.get_closure_env_override(wid)
        {
            for (k, v) in persisted.iter() {
                if self.env().contains_key_sym(*k) {
                    self.env_mut().insert_sym(*k, v.clone());
                }
            }
        }
        if pushed_dispatch {
            self.pop_method_dispatch();
        }
        self.pop_method_samewith_context();
        Some(result)
    }
}
