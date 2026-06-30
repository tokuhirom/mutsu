use super::*;

impl Interpreter {
    pub(super) fn call_compiled_function_named_inner(
        &mut self,
        cf: &CompiledFunction,
        args: Vec<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
        fn_package: &str,
        fn_name: &str,
    ) -> Result<Value, RuntimeError> {
        // Slice 6.3 step 2: signal env_dirty *precisely* (like
        // call_compiled_function_fast) instead of relying on a blanket post-call
        // mark at the call site. Save the caller's incoming dirtiness; the body's
        // own nested-call dirtiness is about the callee env (subsumed by the merge
        // below), so it is reset before the body and recomputed from what the
        // merge actually wrote back to the caller env.
        // Slice F: the rw-writeback list is drained by the call-site op right
        // after each dispatch returns, so it must hold *only* this call's
        // sources on return. Clear any leftover from a sibling whose call site
        // did not drain (a non-rw path), so it can never be written into the
        // wrong slot. Nested calls in the body self-drain via their own ExecCall
        // ops, leaving the list empty before this frame records its own sources.
        self.pending_rw_writeback_sources.clear();
        let (args, callsite_line) = self.sanitize_call_args(&args);
        if callsite_line.is_some() {
            loan_env!(self, set_pending_callsite_line(callsite_line));
        }
        // Record deprecation for cached compiled functions
        self.record_cf_deprecation(cf);
        // Inject callsite line BEFORE push_call_frame so the parent env
        // contains the updated ?LINE. This avoids triggering Arc::make_mut
        // deep clone after the env Arc is shared with the call frame.
        loan_env!(self, inject_pending_callsite_line());
        self.push_call_frame();
        let saved_stack_depth = self.call_frames.last().unwrap().saved_stack_depth;
        let return_spec = cf.return_type.clone();

        loan_env!(self, push_caller_env());

        // Push Sub value to block_stack for callframe().code
        let sub_val = Value::make_sub(
            Symbol::intern(fn_package),
            Symbol::intern(fn_name),
            cf.params.clone(),
            cf.param_defs.clone(),
            vec![],
            false,
            // Flatten: this Sub is pushed for callframe().code introspection and
            // must expose the full lexical view, not a scoped overlay.
            self.clone_env(),
        );
        self.push_block(sub_val);

        // Scoped-overlay (docs/vm-dual-store.md Slice 6): install an empty
        // born-owned overlay over the caller now that sub_val / push_caller_env
        // captured the flat caller. Callee setup/body env writes land in the
        // overlay; on return the merge iterates it overlay-only into the restored
        // caller env. frame.saved_env holds the flat caller for restoration.
        {
            let parent = self.env().clone();
            self.set_env(crate::env::Env::scoped_child(parent));
        }

        // Always push a routine frame so that &?ROUTINE works inside anonymous
        // subs too. Use "<anon>" as a sentinel name when fn_name is empty.
        let routine_push_name = if fn_name.is_empty() {
            "<anon>".to_string()
        } else {
            fn_name.to_string()
        };
        self.push_routine_with_location(
            fn_package.to_string(),
            routine_push_name,
            self.current_source_line(),
            self.current_source_file(),
        );
        let mut callable_id: Option<u64> = None;
        if !fn_name.is_empty() {
            let callable_key = format!("__mutsu_callable_id::{fn_package}::{fn_name}");
            let resolved_callable_id = self
                .env()
                .get(&callable_key)
                .and_then(|v| match v {
                    Value::Int(i) => Some(*i),
                    _ => None,
                })
                .unwrap_or(0);
            callable_id = (resolved_callable_id != 0).then_some(resolved_callable_id as u64);
            // Only insert __mutsu_callable_id when non-zero; readers handle
            // the missing/None case correctly. This avoids triggering
            // Arc::make_mut deep clone on the CoW env for simple functions.
            if resolved_callable_id != 0 {
                self.env_mut().insert(
                    "__mutsu_callable_id".to_string(),
                    Value::Int(resolved_callable_id),
                );
            }
        }
        let is_test_assertion = if fn_name.is_empty() {
            false
        } else {
            loan_env!(self, routine_is_test_assertion_by_name(fn_name, &args))
        };
        let pushed_assertion = self.push_test_assertion_context(is_test_assertion);

        if cf.empty_sig && !args.is_empty() {
            self.pop_routine();
            self.pop_test_assertion_context(pushed_assertion);
            self.pop_caller_env();
            self.stack.truncate(saved_stack_depth);
            let frame = self.pop_call_frame();
            // Drop the scoped overlay, restoring the caller env.
            self.set_env(frame.saved_env);
            return Err(Interpreter::reject_args_for_empty_sig(&args));
        }

        // Set current_package to the function's defining package so that default
        // value expressions can resolve package-scoped functions (e.g. &double)
        // AND package-scoped variables (`our $x`, a `package { my $x }` lexical)
        // are resolvable from inside the sub on every call. Callers pass the
        // *caller's* package as `fn_package`, which is wrong for a by-name call
        // into another package (`P::inc()` from `GLOBAL`); the authoritative
        // declaring package lives on the CompiledFunction. Only the first OTF
        // compile previously set it correctly (via `def.package`), so a 2nd+
        // call read/wrote package vars under `GLOBAL` and silently lost them.
        // Skip a mangled state-scope package (`Pkg::&sub/arity`, used for nested
        // subs) and fall back to the passed name in that case.
        let def_package: &str = if !cf.package.is_empty() && !cf.package.contains("::&") {
            cf.package.as_str()
        } else {
            fn_package
        };
        let saved_package = self.current_package().to_string();
        if !def_package.is_empty() && def_package != "GLOBAL" {
            self.set_current_package(def_package.to_string());
        }
        // When the function has where constraints and there is a &name Sub in
        // env (which carries closure env), merge the Sub's captured variables
        // into the current env so where-constraint expressions can access them.
        if !fn_name.is_empty() && cf.param_defs.iter().any(|pd| pd.where_constraint.is_some()) {
            let ampname = format!("&{}", fn_name);
            if let Some(Value::Sub(ref sub_data)) = self.env().get(&ampname).cloned() {
                for (k, v) in &sub_data.env {
                    // Skip internal variables, parameters, and sigiled variables
                    // that belong to the calling scope. Only merge simple lexical
                    // variables that the where constraint might reference.
                    if !k.starts_with("__mutsu_")
                        && !k.starts_with("?")
                        && !k.starts_with("!")
                        && k != "_"
                        && k != "@_"
                        && k != "%_"
                    {
                        self.env_mut().insert_sym(*k, v.clone());
                    }
                }
            }
        }
        // Skip bind_function_args_values for 0-arg functions with no params,
        // avoiding the @_ env insert that triggers Arc::make_mut deep clone.
        let rw_bindings = if args.is_empty() && cf.param_defs.is_empty() && cf.params.is_empty() {
            vec![]
        } else {
            match loan_env!(
                self,
                bind_function_args_values(&cf.param_defs, &cf.params, &args)
            ) {
                Ok(bindings) => bindings,
                Err(e) => {
                    self.set_current_package(saved_package);
                    self.pop_routine();
                    self.pop_test_assertion_context(pushed_assertion);
                    self.pop_caller_env();
                    self.stack.truncate(saved_stack_depth);
                    let frame = self.pop_call_frame();
                    *self.env_mut() = frame.saved_env;
                    return Err(Interpreter::enhance_binding_error(
                        e,
                        fn_name,
                        &cf.param_defs,
                        &args,
                    ));
                }
            }
        };
        self.prepare_definite_return_slot(return_spec.as_deref());

        // Raku: $! is scoped per routine — fresh Nil on entry.
        // Only insert if $! isn't already Nil, to avoid triggering
        // Arc::make_mut deep clone on the CoW env.
        if !fn_name.is_empty() {
            let needs_reset = self
                .env()
                .get("!")
                .is_some_and(|v| !matches!(v, Value::Nil));
            if needs_reset {
                self.env_mut().insert("!".to_string(), Value::Nil);
            }
        }

        // Raku: routines get their own $_ initialized to (Any).
        if cf.code.is_routine && !cf.param_defs.iter().any(|pd| pd.name == "_") {
            self.env_mut().insert(
                "_".to_string(),
                Value::Package(crate::symbol::Symbol::intern("Any")),
            );
        }

        self.locals = vec![Value::Nil; cf.code.locals.len()];
        for (i, local_name) in cf.code.locals.iter().enumerate() {
            if let Some(val) = self.env().get(local_name) {
                self.locals[i] = val.clone();
            }
        }
        // Load persisted state variable values
        for (slot, key) in &cf.code.state_locals {
            if let Some(val) = self.get_state_var(key) {
                self.locals[*slot] = val.clone();
            }
        }

        let let_mark = self.let_saves_len();
        // Body-internal env_dirty (from nested calls) concerns the callee env,
        // which the return merge reconciles; reset so the post-merge value
        // reflects only what was actually written back to the caller.
        let mut ip = 0;
        let mut result = Ok(());
        let mut explicit_return: Option<Value> = None;
        let mut fail_bypass = false;
        while ip < cf.code.ops.len() {
            match self.exec_one(&cf.code, &mut ip, compiled_fns) {
                Ok(()) => {}
                Err(mut e) if e.is_leave => {
                    let routine_key = format!("{fn_package}::{fn_name}");
                    let matches_frame = if let Some(target_id) = e.leave_callable_id {
                        Some(target_id) == callable_id
                    } else if let Some(target_routine) = e.leave_routine.as_ref() {
                        target_routine == &routine_key
                    } else {
                        e.label.is_none()
                    };
                    if matches_frame {
                        e.is_leave = false;
                        e.control = None;
                        let ret_val = e.return_value.unwrap_or(Value::Nil);
                        explicit_return = Some(ret_val.clone());
                        self.stack.truncate(saved_stack_depth);
                        self.stack.push(ret_val);
                        self.resolve_let_saves_on_success(let_mark, true);
                        result = Ok(());
                        break;
                    }
                    loan_env!(self, restore_let_saves(let_mark));
                    result = Err(e);
                    break;
                }
                Err(e) if e.return_value.is_some() => {
                    // Non-local return: if the signal targets a specific callable,
                    // only catch it if this routine is the target.
                    if let Some(target_id) = e.return_target_callable_id
                        && callable_id != Some(target_id)
                    {
                        loan_env!(self, restore_let_saves(let_mark));
                        result = Err(e);
                        break;
                    }
                    let ret_val = e.return_value.unwrap();
                    explicit_return = Some(ret_val.clone());
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val);
                    self.resolve_let_saves_on_success(let_mark, true);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail() => {
                    // fail() — restore let saves and return a Failure value
                    fail_bypass = true;
                    let failure = self.fail_error_to_failure_value(&e);
                    loan_env!(self, restore_let_saves(let_mark));
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(failure);
                    result = Ok(());
                    break;
                }
                Err(e) => {
                    loan_env!(self, restore_let_saves(let_mark));
                    result = Err(e);
                    break;
                }
            }
            if self.is_halted() {
                break;
            }
        }

        // Natural fall-through completion (no explicit return / fail / error
        // break arm): restore any `temp` bindings the body introduced so a
        // `sub f { temp $x = ... }` with no explicit return does not leak the
        // temporized value into the caller's scope.
        if result.is_ok() && explicit_return.is_none() && !fail_bypass {
            self.resolve_let_saves_on_success(let_mark, true);
        }

        let mut ret_val = if result.is_ok() {
            if self.stack.len() > saved_stack_depth {
                self.stack.pop().unwrap_or(Value::Nil)
            } else {
                Value::Nil
            }
        } else {
            Value::Nil
        };
        // Raku semantics: `sub foo(...) { ... }` as the last statement
        // of a block returns the Sub. If the return value is Nil/Any and
        // the last opcode was RegisterSub, create the Sub value.
        if result.is_ok()
            && (matches!(ret_val, Value::Nil)
                || matches!(&ret_val, Value::Package(n) if n.resolve() == "Any"))
            && let Some(crate::opcode::OpCode::RegisterSub(idx)) = cf.code.ops.last()
            && let crate::ast::Stmt::SubDecl {
                name: sub_name,
                params,
                param_defs,
                body,
                is_rw,
                ..
            } = &cf.code.stmt_pool[*idx as usize]
        {
            ret_val = Value::make_sub(
                Symbol::intern(&self.current_package()),
                *sub_name,
                params.clone(),
                param_defs.clone(),
                body.clone(),
                *is_rw,
                // Flatten: a Sub returned as a value is dispatched cross-scope.
                self.clone_env(),
            );
        }

        self.stack.truncate(saved_stack_depth);

        // Sync state variables back to persistent storage.
        // Read from env first (methods like push update env directly),
        // falling back to locals.
        for (slot, key) in &cf.code.state_locals {
            let local_name = &cf.code.locals[*slot];
            let val = self
                .env()
                .get(local_name)
                .cloned()
                .unwrap_or_else(|| self.locals[*slot].clone());
            loan_env!(self, set_state_var(key.clone(), val));
        }

        // A scalar `is rw` / `is raw` parameter (`$a is rw`, stored sigil-less as
        // `a`) is bound to a slot-only local in the body (read/written via
        // GetLocal/SetLocal), so a `$a = …` write never reaches env
        // (`flush_local_to_env` skips it: needs_env_sync is false).
        // `apply_rw_bindings_to_env` (below) reads the param's value from env to
        // write it back to the caller's variable, so flush the scalar rw-param
        // slots into env now — while `self.locals` is still the callee's array,
        // before `pop_call_frame` restores the caller's locals. `@`/`%` container
        // params are intentionally excluded: their in-place mutations (`@x.push`,
        // `@x.splice`, ...) go *through* env by name already, so the local slot
        // holds a stale copy and flushing it would clobber the live container.
        if !rw_bindings.is_empty() {
            for (param_name, _source) in &rw_bindings {
                if param_name.starts_with(['@', '%', '&']) {
                    continue;
                }
                if let Some(slot) = cf.code.locals.iter().position(|n| n == param_name) {
                    let final_val = self.locals[slot].clone();
                    self.env_mut().insert(param_name.clone(), final_val);
                }
            }
        }

        self.set_current_package(saved_package);
        self.pop_routine();
        self.pop_test_assertion_context(pushed_assertion);
        self.pop_block();
        let effective_return_spec = return_spec
            .as_deref()
            .map(|spec| loan_env!(self, resolved_type_capture_name(spec)));

        let frame = self.pop_call_frame();
        let restored_env = frame.saved_env;
        // Slice 6.3 step 2: track whether the merge actually wrote a *caller-slot-
        // aliasing* value back, so env_dirty (a caller locals re-sync) is set only
        // when needed. A plain-lexical writeback (captured-outer mutation) or an
        // `is rw` param writeback can alias a caller local slot; dynamic-var
        // writeback (pop_caller_env_with_writeback) targets `$*x` names that have
        // no compiled slot, so it never obliges a pull.
        // Fast path: if the env wasn't mutated during the call (Arc still shared),
        // we can skip the expensive env merge and just restore directly.
        if restored_env.ptr_eq(self.env()) {
            self.pop_caller_env();
        } else {
            let mut restored_env = restored_env;
            self.pop_caller_env_with_writeback(&mut restored_env);
            loan_env!(
                self,
                apply_rw_bindings_to_env(&rw_bindings, &mut restored_env)
            );
            let rw_sources: std::collections::HashSet<String> = rw_bindings
                .iter()
                .map(|(_, source)| source.clone())
                .collect();
            // Slice F: record the caller-source names just written back so the
            // call-site op (which holds the caller's `code`) can write each value
            // straight through to the caller's local slot, keeping it coherent
            // without relying on the reverse `sync_locals_from_env` pull.
            if !rw_sources.is_empty() {
                self.pending_rw_writeback_sources
                    .extend(rw_sources.iter().cloned());
                // Slice F (carrier completeness, open-question #2): when this sub
                // runs *inside a carrier* (an interpreter routine like
                // `lives-ok { f(@a, $x) }` executing it), the rw writeback above
                // mutated a caller lexical (`$x`) by name through a path the
                // carrier does not otherwise log (`set_env_with_main_alias`). Log
                // it into the active carrier set too, so the carrier's
                // `writeback_carrier_writes` reconciles the carrier-caller's slot
                // — otherwise the write is invisible to the carrier and the slot
                // stays stale once the reverse pull is gone.
                if self.carrier_writes.is_some() {
                    for source in &rw_sources {
                        self.note_caller_env_write(source);
                    }
                }
            }
            // Use declared_locals (vars declared via `my` or as params) instead
            // of all locals. Captured outer variables should propagate their
            // modifications back to the caller.
            let local_names: std::collections::HashSet<&str> =
                if let Some(ref declared) = cf.declared_locals {
                    declared.iter().map(|s| s.as_str()).collect()
                } else {
                    cf.code.locals.iter().map(|s| s.as_str()).collect()
                };
            for (k, v) in self.env().iter() {
                if *k == "_" || *k == "@_" || *k == "%_" {
                    continue;
                }
                // __mutsu_callable_id must not leak from callee back to
                // caller; it identifies the current routine scope for
                // non-local return targeting.
                if *k == "__mutsu_callable_id" {
                    continue;
                }
                if restored_env.contains_key_sym(*k)
                    && !k.with_str(|s| local_names.contains(s))
                    && !k.with_str(|s| rw_sources.contains(s))
                {
                    restored_env.insert_sym(*k, v.clone());
                }
            }
            // A captured-outer write to an enclosing lexical that is not yet
            // present in the caller env is dropped by the `contains_key`-gated
            // merge above. This happens when a hoisted sub is *called before* the
            // `my $x` declaration it closes over has run (the declaration is
            // compile-time, so the lexical genuinely exists, but the runtime env
            // entry is created only when the `my` statement executes). Such a name
            // is recorded in `free_var_writes` (the body writes a free var), so
            // propagate the callee's new value into the restored caller env
            // unconditionally — otherwise the first call's write is lost and the
            // accumulation starts one call late (`0,0,1,2` instead of `0,1,2,3`).
            for sym in &cf.code.free_var_writes {
                if restored_env.contains_key_sym(*sym) {
                    continue;
                }
                let skip = sym.with_str(|s| {
                    s == "_"
                        || s == "@_"
                        || s == "%_"
                        || local_names.contains(s)
                        || rw_sources.contains(s)
                        // Dynamic vars (`$*x`) are reconciled by
                        // pop_caller_env_with_writeback, not the lexical merge.
                        || s.as_bytes().get(1) == Some(&b'*')
                });
                if skip {
                    continue;
                }
                if let Some(v) = self.env().get_sym(*sym) {
                    let v = v.clone();
                    restored_env.insert_sym(*sym, v);
                }
            }
            *self.env_mut() = restored_env;
        }

        // Slice F (env<->locals coherence): record the captured-outer variables
        // this body writes so the call-site op writes their new env values
        // straight through to the caller's local slots (`apply_pending_rw_writeback`),
        // dropping the dependency on the reverse `sync_locals_from_env` pull. This
        // mirrors the 0-arg fast-call path (#3317): a qualified/`our` sub reached by
        // name (`module M { our sub foo() { $called = True } }`) goes through this
        // path rather than the fast path, but writes its enclosing `$called` the
        // same way. `free_var_writes` is the compile-time set of free vars this
        // body writes, so a pure body records nothing and pays no cost. The topic
        // (`_`/`@_`/`%_`) is excluded — it is a per-call alias, never a caller
        // lexical. (Cross-frame propagation still relies on the reverse pull.)
        for sym in &cf.code.free_var_writes {
            sym.with_str(|fname| {
                if fname != "_" && fname != "@_" && fname != "%_" {
                    self.pending_rw_writeback_sources.push(fname.to_string());
                }
            });
        }

        match result {
            Ok(()) if fail_bypass => Ok(ret_val),
            Ok(()) => {
                let base_result = if let Some(v) = explicit_return {
                    let mut e = RuntimeError::new("return");
                    e.return_value = Some(v);
                    Err(e)
                } else {
                    Ok(ret_val)
                };
                loan_env!(
                    self,
                    finalize_return_with_spec(base_result, effective_return_spec.as_deref())
                )
            }
            Err(e) => Err(e),
        }
    }
}
