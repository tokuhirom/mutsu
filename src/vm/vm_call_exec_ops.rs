use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn exec_exec_call_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let arity = arity as usize;
        if self.stack.len() < arity {
            return Err(RuntimeError::new("Interpreter stack underflow in ExecCall"));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // Flatten any Slip values in the argument list (from |capture slipping)
        let mut args = Vec::new();
        for arg in raw_args {
            match arg {
                Value::Slip(items) => {
                    for item in items.iter() {
                        match item {
                            Value::Capture { positional, named } => {
                                args.extend(positional.iter().cloned());
                                for (k, v) in named.iter() {
                                    args.push(Value::Pair(k.clone(), Box::new(v.clone())));
                                }
                            }
                            other => args.push(other.clone()),
                        }
                    }
                }
                other => args.push(other),
            }
        }
        let arg_sources = self.decode_arg_sources(code, arg_sources_idx);
        let arg_sources = if arg_sources.as_ref().is_some_and(|s| s.len() != args.len()) {
            None
        } else {
            arg_sources
        };
        let args = self.normalize_call_args_for_target(&name, args);
        let (args, callsite_line) = self.sanitize_call_args(&args);
        // Auto-FETCH Proxy args for statement-level calls (same as CallFunc)
        let args = if self.in_lvalue_assignment {
            args
        } else {
            self.auto_fetch_proxy_args(args)?
        };
        loan_env!(self, set_pending_callsite_line(callsite_line));
        // Check wrap chain for named function calls
        if let Some(sub_id) = self.wrap_sub_id_for_name(&name)
            && !self.is_wrap_dispatching(sub_id)
            && let Some(sub_val) = self.get_wrapped_sub(&name)
        {
            let result = self.vm_call_sub_value(sub_val, args, false)?;
            self.stack.push(result);
            self.env_dirty = true;
            return Ok(());
        }
        if let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args) {
            self.set_pending_call_arg_sources(arg_sources.clone());
            let pkg = self.current_package().to_string();
            let call_result =
                self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name);
            self.set_pending_call_arg_sources(None);
            call_result?;
            // No blanket mark: call_compiled_function_named already signals
            // env_dirty precisely from its return merge (matches the hot
            // vm_call_func_ops path). A blanket `= true` here would defeat that
            // precision. See docs/vm-dual-store.md "CP-2 status & corrected plan".
        } else if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            native_result?;
        } else {
            self.set_pending_call_arg_sources(arg_sources);
            let exec_result = loan_env!(self, exec_call_values(&name, args));
            self.set_pending_call_arg_sources(None);
            exec_result?;
            // Carrier may write the caller env by name (e.g. EVAL'd lexicals). Keep
            // the env_dirty flag rather than an eager sync_locals_from_env here: an
            // eager pull at a function-call site can clobber an in-place cell
            // mutation a later op made to a local (cyclic `:=` bind), which the
            // flag-deferred barrier pull avoids by running only once env is fresh.
            self.env_dirty = true;
        }
        Ok(())
    }

    pub(super) fn exec_exec_call_pairs_op(
        &mut self,
        code: &CompiledCode,
        compiled_fns: &HashMap<String, CompiledFunction>,
        name_idx: u32,
        arity: u32,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let arity = arity as usize;
        if self.stack.len() < arity {
            return Err(RuntimeError::new(
                "Interpreter stack underflow in ExecCallPairs",
            ));
        }
        let start = self.stack.len() - arity;
        let args: Vec<Value> = self.stack.drain(start..).collect();
        // Auto-FETCH Proxy args
        let args = if self.in_lvalue_assignment {
            args
        } else {
            self.auto_fetch_proxy_args(args)?
        };
        // Try compiled function dispatch first
        if let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args) {
            let pkg = self.current_package().to_string();
            self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name)?;
            // call_compiled_function_named signals env_dirty precisely; no blanket.
            return Ok(());
        }
        // Try native function (env-pure: no env_dirty mark).
        if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            native_result?;
            return Ok(());
        }
        // Carrier fallback: keep the env_dirty flag (see exec_exec_call_op).
        loan_env!(self, exec_call_pairs_values(&name, args))?;
        self.env_dirty = true;
        Ok(())
    }

    /// Execute a call with capture slip: regular args + 1 slip arg on stack.
    /// The slip arg is flattened into the argument list.
    pub(super) fn exec_exec_call_slip_op(
        &mut self,
        code: &CompiledCode,
        compiled_fns: &HashMap<String, CompiledFunction>,
        name_idx: u32,
        regular_arity: u32,
        _arg_sources_idx: Option<u32>,
        slip_pos: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let total = regular_arity as usize + 1; // +1 for the slip value
        if self.stack.len() < total {
            return Err(RuntimeError::new(
                "Interpreter stack underflow in ExecCallSlip",
            ));
        }
        // Pop all values from the stack in source order
        let stack_start = self.stack.len() - total;
        let raw_args: Vec<Value> = self.stack.drain(stack_start..).collect();
        let mut args: Vec<Value> = Vec::new();
        if let Some(pos) = slip_pos {
            let pos = pos as usize;
            for (i, val) in raw_args.into_iter().enumerate() {
                if i == pos {
                    Self::append_slip_value(&mut args, val);
                } else {
                    args.push(val);
                }
            }
        } else {
            // Legacy: slip is last on stack
            let slip_val = raw_args.last().cloned().unwrap_or(Value::Nil);
            args.extend(raw_args.into_iter().take(regular_arity as usize));
            Self::append_slip_value(&mut args, slip_val);
        }
        // Try compiled function dispatch first
        if let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args) {
            let pkg = self.current_package().to_string();
            self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name)?;
            // call_compiled_function_named signals env_dirty precisely; no blanket.
            return Ok(());
        }
        // Try native function (env-pure: no env_dirty mark).
        if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            self.stack.push(native_result?);
            return Ok(());
        }
        // Carrier fallback: keep the env_dirty flag (see exec_exec_call_op).
        loan_env!(self, exec_call_pairs_values(&name, args))?;
        self.env_dirty = true;
        Ok(())
    }
}
