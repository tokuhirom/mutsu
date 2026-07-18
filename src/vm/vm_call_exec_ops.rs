use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn exec_exec_call_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
        compiled_fns: &CompiledFns,
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
            match arg.view() {
                ValueView::Slip(items) => {
                    for item in items.iter() {
                        match item.view() {
                            ValueView::Capture { positional, named } => {
                                args.extend(positional.iter().cloned());
                                for (k, v) in named.iter() {
                                    args.push(Value::pair(k.clone(), v.clone()));
                                }
                            }
                            _ => args.push(item.clone()),
                        }
                    }
                }
                _ => args.push(arg),
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
        if self.wrap_sub_id_for_name(&name).is_some()
            && let Some(sub_val) = self.get_wrapped_sub(&name)
        {
            let result = self.vm_call_sub_value(sub_val, args, false)?;
            self.stack.push(result);
            // A wrapper closure (`&f.wrap(-> { $seen = True; callsame })`) may mutate
            // a captured-outer lexical; the closure dispatch recorded it precisely
            // (`pending_*_writeback`). Drain it so the caller's slot refreshes without
            // the blanket env→locals pull (env_dirty-removal substrate).
            self.apply_pending_rw_writeback(code);
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
            // reverse sync is precise. See docs/vm-single-store.md.
            let carrier_saved = self.begin_carrier();
            let exec_result = loan_env!(self, exec_call_values(&name, args));
            self.set_pending_call_arg_sources(None);
            let written = self.end_carrier(carrier_saved);
            exec_result?;
            // This bareword carrier (EVAL and other interpreter-only routines)
            // writes caller lexicals through `set_env_with_main_alias` (EVAL's
            // SetGlobal) or — for an embedded regex `{ }`/`:my`/`:let` block —
            // directly into env, which logs into the carrier set (regex_eval.rs,
            // Slice C' / open-question #2). This writeback reconciles every scalar
            // it wrote into a current-frame slot; cell-boxing keeps any diverged
            // container / ancestor lexical coherent.
            self.writeback_carrier_writes(code, &written);
        }
        Ok(())
    }

    pub(super) fn exec_exec_call_pairs_op(
        &mut self,
        code: &CompiledCode,
        compiled_fns: &CompiledFns,
        name_idx: u32,
        arity: u32,
        slip_positions_idx: Option<u32>,
        keep_value: bool,
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
        let args = Self::spread_slip_positions(code, args, slip_positions_idx);
        // Auto-FETCH Proxy args
        let args = if self.in_lvalue_assignment {
            args
        } else {
            self.auto_fetch_proxy_args(args)?
        };
        // Try compiled function dispatch first
        if let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args) {
            let pkg = self.current_package().to_string();
            let v = self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name)?;
            // Slice F: drain any `is rw` param writeback into the caller's slots.
            self.apply_pending_rw_writeback(code);
            // call_compiled_function_named signals env_dirty precisely; no blanket.
            if keep_value {
                self.stack.push(v);
            }
            return Ok(());
        }
        // Try native function (env-pure: no env_dirty mark).
        if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            let v = native_result?;
            if keep_value {
                self.stack.push(v);
            }
            return Ok(());
        }
        // Carrier fallback: precise scalar writeback + unconditional env_dirty net.
        // Keeps the blanket: deep `:=` bind-cell mutations through interpreter
        // builtins are not name-trackable and dropping the net corrupts cell
        // coherence (the CP-2 wall; t/element-bind-cell.t). See docs/vm-single-store.md.
        //
        // A block Test function (`lives-ok { $b<a> = 42 }` / `lives-ok { $a does
        // Role }`) mutates a captured-outer caller lexical through env. Snapshot
        // the caller frame's slot-backing env values for the overwritable slots
        // before the carrier, then write through only the slots whose env value
        // changed. Plain Array/Hash and binding-cell slots are excluded (see
        // `slot_carrier_overwritable`).
        let pre_env: Vec<Option<Value>> = self.snapshot_carrier_overwritable_env(code);
        let carrier_saved = self.begin_carrier();
        // Tail position (`keep_value`) routes through the standard expression
        // dispatcher first (call_function — same as exec_call_values) so the
        // call's value is the real return value; the legacy exec_call carrier
        // reconstructs an implicit return from the topic, which is unreliable
        // for a value that must propagate (JSON::Marshal's tail
        // `to-json($ret, :$sorted-keys, :$pretty)`).
        let exec_result = loan_env!(self, exec_call_pairs_values(&name, args));
        let written = self.end_carrier(carrier_saved);
        let v = exec_result?;
        self.writeback_carrier_writes(code, &written);
        self.carrier_writeback_changed_aggregates(code, &pre_env);
        if keep_value {
            self.stack.push(v);
        }
        Ok(())
    }
}
