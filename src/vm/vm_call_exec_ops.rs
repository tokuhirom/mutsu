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
            // A wrapper closure (`&f.wrap(-> { $seen = True; callsame })`) may mutate
            // a captured-outer lexical; the closure dispatch recorded it precisely
            // (`pending_*_writeback`). Drain it so the caller's slot refreshes without
            // the blanket env→locals pull (env_dirty-removal substrate).
            self.apply_pending_rw_writeback(code);
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
            // Slice F: write any `is rw` param writeback through to the caller's
            // local slot (and clear the pending list so it never leaks to the
            // next call site).
            self.apply_pending_rw_writeback(code);
            // No blanket mark: call_compiled_function_named already signals
            // env_dirty precisely from its return merge (matches the hot
            // vm_call_func_ops path). A blanket `= true` here would defeat that
            // precision. See docs/vm-dual-store.md "CP-2 status & corrected plan".
        } else if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            native_result?;
        } else {
            self.set_pending_call_arg_sources(arg_sources);
            // Carrier may write the caller env by name (e.g. EVAL'd lexicals).
            // Slice B logs those writes (`begin_carrier`) and reconciles them into
            // the caller's slots on return (`writeback_carrier_writes`), so the
            // reverse sync is precise. For a fully-reconciled EVAL we then drop the
            // blanket `env_dirty` net (see below); other carriers keep it. See
            // docs/vm-single-store.md.
            let pre_dirty = self.env_dirty;
            let carrier_saved = self.begin_carrier();
            let exec_result = loan_env!(self, exec_call_values(&name, args));
            self.set_pending_call_arg_sources(None);
            let written = self.end_carrier(carrier_saved);
            exec_result?;
            let fully = self.writeback_carrier_writes(code, &written);
            // This bareword carrier (EVAL and other interpreter-only routines)
            // writes caller lexicals through `set_env_with_main_alias` (EVAL's
            // SetGlobal) or — for an embedded regex `{ }`/`:my`/`:let` block —
            // directly into env, which now also logs into the carrier set
            // (regex_eval.rs, Slice C' / open-question #2). The writeback above
            // reconciled every scalar it wrote into a current-frame slot; a
            // diverged container leaves `fully` false (cell-aware, #3227) and
            // ancestor lexicals have no current-frame slot (read straight from
            // env). When the writeback was fully precise we drop the blanket
            // env_dirty net that the nested run (`with_nested_registers`) sets
            // unconditionally, eliminating the spurious per-carrier barrier pull
            // (docs/vm-single-store.md Slice C': 1001 -> ~1 pulls on an
            // EVAL-in-a-hot-loop). Restoring the *pre-carrier* dirty (not a blind
            // clear) preserves a dirty the caller already owed. Validated by the
            // full roast whitelist (byte-identical to the EVAL-only baseline) plus
            // the regex/`:let`/`s///` t/ pins. (The `pairs`/`slip` carriers below
            // keep their blanket — their interpreter builtins write through
            // unlogged paths; see there.)
            if fully {
                self.env_dirty = pre_dirty;
            } else {
                self.env_dirty = true;
                // Stage 3: the carrier writeback was not fully precise — an
                // interpreter routine wrote a caller lexical by name through a
                // path the carrier set does not log (e.g. an `is rw` for-loop
                // alias mutated inside a `lives-ok { }` block, or a role/mixin
                // composition). Reconcile the caller's slots from env here, at
                // the call site, since the reverse `sync_locals_from_env` pull is
                // no longer there to do it at the next sync point. Byte-identical
                // to that pull under reverse-sync ON (same per-slot skips); only
                // runs on the imprecise carrier path, never for a fully-reconciled
                // EVAL (which restores `pre_dirty` above).
                self.reconcile_locals_from_env_at_site(code);
            }
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
            // Slice F: drain any `is rw` param writeback into the caller's slots.
            self.apply_pending_rw_writeback(code);
            // call_compiled_function_named signals env_dirty precisely; no blanket.
            return Ok(());
        }
        // Try native function (env-pure: no env_dirty mark).
        if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            native_result?;
            return Ok(());
        }
        // Carrier fallback: precise scalar writeback + unconditional env_dirty net.
        // Keeps the blanket: deep `:=` bind-cell mutations through interpreter
        // builtins are not name-trackable and dropping the net corrupts cell
        // coherence (the CP-2 wall; t/element-bind-cell.t). See docs/vm-single-store.md.
        let carrier_saved = self.begin_carrier();
        let exec_result = loan_env!(self, exec_call_pairs_values(&name, args));
        let written = self.end_carrier(carrier_saved);
        exec_result?;
        self.writeback_carrier_writes(code, &written);
        self.env_dirty = true;
        // Stage 3: a block-taking Test function (`lives-ok { ... }`,
        // `throws-like`, ...) routes here (its `__mutsu_test_callsite_line`
        // named arg makes it a pairs call) and runs its block through the
        // interpreter, which can mutate a caller lexical by name through a path
        // the carrier set does not log (an `is rw` for-loop alias inside the
        // block). Reconcile the caller's slots from env at the call site, since
        // the reverse `sync_locals_from_env` pull is gone. Byte-identical to that
        // pull under reverse-sync ON (same per-slot HashSlotRef/`!attr` skips).
        self.reconcile_locals_from_env_at_site(code);
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
            // Slice F: drain any `is rw` param writeback into the caller's slots.
            self.apply_pending_rw_writeback(code);
            // call_compiled_function_named signals env_dirty precisely; no blanket.
            return Ok(());
        }
        // Try native function (env-pure: no env_dirty mark).
        if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            self.stack.push(native_result?);
            return Ok(());
        }
        // Carrier fallback: precise scalar writeback + unconditional env_dirty net.
        // Keeps the blanket: deep `:=` bind-cell mutations through interpreter
        // builtins are not name-trackable and dropping the net corrupts cell
        // coherence (the CP-2 wall; t/element-bind-cell.t). See docs/vm-single-store.md.
        let carrier_saved = self.begin_carrier();
        let exec_result = loan_env!(self, exec_call_pairs_values(&name, args));
        let written = self.end_carrier(carrier_saved);
        exec_result?;
        self.writeback_carrier_writes(code, &written);
        self.env_dirty = true;
        Ok(())
    }
}
