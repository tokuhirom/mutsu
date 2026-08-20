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
        // ADR-0054 S2: spread only the positions the caller wrote as
        // `|EXPR` -- decided by call-site syntax, not by a value merely
        // evaluating to a Slip (e.g. `show maybe(0);` as a bare statement,
        // where `maybe`'s tail `if` didn't fire and returned an Empty Slip,
        // must pass exactly one argument).
        let decoded_sources = self.decode_arg_sources(code, arg_sources_idx);
        let (args, arg_sources) =
            Self::spread_call_args_by_syntax(code, raw_args, arg_sources_idx, decoded_sources);
        // NativeCall: a statement-level call to an `is native(...)` sub compiles
        // to `ExecCall` (a bare call statement whose value is sunk), not
        // `CallFunc` — but only `CallFunc`'s handler checked `native_call_specs`.
        // A sunk native call (`sqlite3_extended_result_codes($p, 1);`, its return
        // discarded) therefore ran its literal `{ ... }` stub body instead of
        // dispatching over FFI, dying with "Stub code executed". Mirror
        // `exec_call_func_op`'s native dispatch here so the check applies
        // regardless of which opcode a given callsite compiled to.
        if !self.native_call_specs.is_empty() {
            let spec = self.native_call_specs.get(&name).cloned().or_else(|| {
                name.rsplit_once("::")
                    .and_then(|(_, short)| self.native_call_specs.get(short).cloned())
            });
            if let Some(mut spec) = spec {
                self.resolve_native_ret_struct(&mut spec);
                let mut call_args = args;
                call_args.retain(|a| !Self::is_callsite_line_marker(a));
                let (result, out_args) =
                    crate::runtime::nativecall::call_native_with_out_args(&spec, &call_args)?;
                if !out_args.is_empty() {
                    let mut wrote = false;
                    for (idx, val) in out_args {
                        if let ValueView::VarRef { name, .. } = call_args[idx].view() {
                            let n = name.resolve().to_string();
                            self.env_mut().insert(n.clone(), val);
                            self.pending_rw_writeback_sources.push(n);
                            wrote = true;
                        }
                    }
                    if wrote {
                        self.apply_pending_rw_writeback(code);
                    }
                }
                self.stack.push(result);
                return Ok(());
            }
        }
        let args = self.normalize_call_args_for_target(&name, args);
        let (args, callsite_line) = self.sanitize_call_args_owned(args);
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
            let value = call_result?;
            // Slice F: write any `is rw` param writeback through to the caller's
            // local slot (and clear the pending list so it never leaks to the
            // next call site).
            self.apply_pending_rw_writeback(code);
            // No blanket mark: call_compiled_function_named already signals
            // env_dirty precisely from its return merge (matches the hot
            // vm_call_func_ops path). A blanket `= true` here would defeat that
            // precision. See docs/vm-dual-store.md "CP-2 status & corrected plan".
            self.sink_discarded_call_value(&value)?;
        } else if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            let value = native_result?;
            self.sink_discarded_call_value(&value)?;
        } else {
            // A user-defined (or imported) sub shadows a same-named builtin. The
            // `CallFunc` path has always honoured that (`dispatch_func_call_inner`'s
            // `user_function_matches_call` branch); `ExecCall` did not, and
            // `exec_call_values` tries `call_function` FIRST — which answers with
            // the builtin and only falls back to user dispatch when the name is
            // not a builtin at all. So a shadowed name reached the builtin here.
            //
            // `Cro::HTTP::Router` exports `get`, and mutsu has a builtin `get`
            // (read a line from a handle): a `route` block whose `get -> {...}` sat
            // in *non-final* (sink) position compiled to `ExecCall` and died with
            // "Expected IO::Handle", while the same call in final position went
            // through `CallFunc` and worked.
            let shadows_builtin = loan_env!(self, user_function_matches_call(&name, &args));
            self.set_pending_call_arg_sources(arg_sources);
            // Carrier may write the caller env by name (e.g. EVAL'd lexicals).
            // Slice B logs those writes (`begin_carrier`) and reconciles them into
            // the caller's slots on return (`writeback_carrier_writes`), so the
            // reverse sync is precise. See docs/vm-single-store.md.
            let carrier_saved = self.begin_carrier();
            let exec_result = if shadows_builtin {
                loan_env!(self, exec_call(&name, args))
            } else {
                loan_env!(self, exec_call_values(&name, args))
            };
            self.set_pending_call_arg_sources(None);
            let written = self.end_carrier(carrier_saved);
            let value = exec_result?;
            // This bareword carrier (EVAL and other interpreter-only routines)
            // writes caller lexicals through `set_env_with_main_alias` (EVAL's
            // SetGlobal) or — for an embedded regex `{ }`/`:my`/`:let` block —
            // directly into env, which logs into the carrier set (regex_eval.rs,
            // Slice C' / open-question #2). This writeback reconciles every scalar
            // it wrote into a current-frame slot; cell-boxing keeps any diverged
            // container / ancestor lexical coherent.
            self.writeback_carrier_writes(code, &written);
            self.sink_discarded_call_value(&value)?;
        }
        Ok(())
    }

    /// A statement-level call discards its value, so that value is *sunk* —
    /// and sinking an unhandled `Failure` throws, exactly as `OpCode::SinkPop`
    /// does for the call shapes that leave their result on the stack.
    /// `OpCode::ExecCall`/`ExecCallPairs` leave nothing on the stack, so they
    /// never reached `SinkPop` and swallowed the Failure instead: `EVAL 'use
    /// fatal; "foo"[2]';` ran on to the next statement where raku throws.
    ///
    /// A deferred `LazyList`/`LazyIoLines` (e.g. a bare `gather { ... }` as an
    /// `EVAL`'d snippet's tail statement) must also be *forced* here, exactly
    /// as `SinkPop` forces one — otherwise `EVAL 'gather { return 1 }';`
    /// never runs the body at all, so `throws-like`'s own `EVAL $code, context
    /// => $ctx;` call (a statement-level call with named args, routed through
    /// `ExecCallPairs`) never sees the escaping `return`.
    fn sink_discarded_call_value(&mut self, value: &Value) -> Result<(), RuntimeError> {
        match value.view() {
            ValueView::LazyList(list) if list.is_cached_no_sink() => {}
            ValueView::LazyList(list) => {
                self.force_lazy_list_vm(&list)?;
            }
            ValueView::Seq(body) if body.needs_touch() => {
                let body = std::sync::Arc::clone(&body);
                self.sink_seq_body(&body)?;
            }
            _ => {
                if let Some(err) = self.failure_to_runtime_error_if_unhandled(value) {
                    return Err(err);
                }
                // Under `use fatal`, a sunk list/Seq holding an unhandled Failure
                // throws too; without the pragma such a list stays soft. Same
                // rule as SinkPop.
                if self.fatal_mode
                    && let Some(err) = self.unhandled_failure_in_list_for_fatal(value)
                {
                    return Err(err);
                }
            }
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
            } else {
                self.sink_discarded_call_value(&v)?;
            }
            return Ok(());
        }
        // Try native function (env-pure: no env_dirty mark).
        if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            let v = native_result?;
            if keep_value {
                self.stack.push(v);
            } else {
                self.sink_discarded_call_value(&v)?;
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
        } else {
            self.sink_discarded_call_value(&v)?;
        }
        Ok(())
    }
}
