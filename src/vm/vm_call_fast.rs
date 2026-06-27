use super::*;

impl Interpreter {
    /// Fast path for calling simple compiled functions.
    /// Eligible when: zero args, no params, no return type spec, not a test assertion,
    /// package is GLOBAL. Skips block_stack, routine_stack, caller_env, readonly vars,
    /// and callframe bookkeeping for significant performance gains in tight loops.
    ///
    /// Unlike `call_compiled_function_named`, this does NOT save/restore the env via
    /// Arc clone. Instead, it runs the function in-place and cleans up the function's
    /// local variables from env afterward. This avoids the expensive deep clone
    /// triggered by Arc::make_mut when the function body mutates env.
    pub(super) fn call_compiled_function_fast(
        &mut self,
        cf: &CompiledFunction,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        self.record_cf_deprecation(cf);
        // A routine declared directly in this body is lexical; snapshot the
        // registry so it is removed on return unless it escaped (see
        // `call_compiled_function_named` / `return_value_escapes_routine`).
        let routine_snapshot = if cf.declares_inner_routines {
            Some(self.snapshot_routine_registry())
        } else {
            None
        };
        // Only save env when there are local variables to clean up.
        // When the function has no locals, the env save/restore is a
        // no-op (nothing to remove), but still causes an Arc clone that
        // raises the refcount and triggers O(env_size) deep clones on
        // any env write inside the function body (e.g. `$ = expr`).
        let has_locals = !cf.code.locals.is_empty();
        // Scoped-overlay pilot (docs/vm-dual-store.md Slice 6): instead of cloning
        // the whole caller env and merging non-local writes back with an
        // O(full-env) scan, install an empty scoped overlay over the caller. The
        // callee's writes accumulate in the overlay; on return we merge the
        // *overlay only* (the callee's actual writes) back, and discard
        // callee-local writes for free. Gated to bodies that never capture or
        // iterate the env for a full lexical view: no inner subs (no
        // closure/thread/block creation) and no reflective by-name access
        // (EVAL / CALLER:: / symbolic deref / pseudo-stash).
        let use_scoped =
            has_locals && !cf.has_inner_subs && !crate::opcode::reflective_name_access_possible();
        let caller_env: Option<Env> = if use_scoped {
            // Chain a child over the whole caller env (itself possibly scoped):
            // no flatten, so nested fast calls don't pay the O(env) merge.
            let parent = self.env().clone();
            let scoped = crate::env::Env::scoped_child(parent);
            Some(std::mem::replace(self.env_mut(), scoped))
        } else {
            None
        };
        let saved_env = if !use_scoped && has_locals {
            crate::vm::vm_stats::record_clone_env();
            Some(self.clone_env())
        } else {
            None
        };
        let saved_locals = std::mem::take(&mut self.locals);
        let saved_stack_depth = self.stack.len();

        // Raku: routines get their own $_ initialized to (Any).
        let saved_topic = if cf.code.is_routine {
            let old = self.env().get("_").cloned();
            self.env_mut().insert(
                "_".to_string(),
                Value::Package(crate::symbol::Symbol::intern("Any")),
            );
            old
        } else {
            None
        };

        // Reuse locals vec to avoid per-call allocation
        let num_locals = cf.code.locals.len();
        self.locals.clear();
        self.locals.resize(num_locals, Value::Nil);
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
        let mut ip = 0;
        let mut result = Ok(());
        let mut explicit_return: Option<Value> = None;
        let mut fail_bypass = false;
        while ip < cf.code.ops.len() {
            match self.exec_one(&cf.code, &mut ip, compiled_fns) {
                Ok(()) => {}
                Err(e) if e.return_value.is_some() => {
                    let ret_val = e.return_value.unwrap();
                    explicit_return = Some(ret_val.clone());
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val);
                    self.resolve_let_saves_on_success(let_mark, true);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail() => {
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

        let ret_val = if result.is_ok() {
            if self.stack.len() > saved_stack_depth {
                self.stack.pop().unwrap_or(Value::Nil)
            } else {
                Value::Nil
            }
        } else {
            Value::Nil
        };

        self.stack.truncate(saved_stack_depth);

        // Sync state variables back to persistent storage. Prefer the env
        // value over locals because methods like .push() mutate the Value
        // in the env in-place, and the locals copy may be stale.
        for (slot, key) in &cf.code.state_locals {
            let local_name = &cf.code.locals[*slot];
            let val = self
                .env()
                .get(local_name)
                .cloned()
                .unwrap_or_else(|| self.locals[*slot].clone());
            loan_env!(self, set_state_var(key.clone(), val));
        }

        // Flush any dirty locals to env before restoring, so that captured
        // outer variable modifications (e.g. $a++ where $a is from outer scope)
        // are visible in env for the merge step below.

        // Restore state
        self.locals = saved_locals;

        // Restore env: if env was mutated, merge non-local changes back.
        // When has_locals is false, saved_env is None and no restore is needed
        // (functions without locals cannot leak local variables into the
        // caller's env — any env writes they do are intentional global state).
        if let Some(caller_env) = caller_env {
            // Scoped path: restore the caller env, then merge the callee's
            // overlay (its own writes only) back, dropping callee-local writes.
            let local_names: std::collections::HashSet<&str> =
                if let Some(ref declared) = cf.declared_locals {
                    declared.iter().map(|s| s.as_str()).collect()
                } else {
                    cf.code.locals.iter().map(|s| s.as_str()).collect()
                };
            let scoped = std::mem::replace(self.env_mut(), caller_env);
            for (k, v) in scoped.overlay_iter() {
                // The callee's private topic / arg array / routine-id must not
                // leak to the caller (the caller env already holds its own).
                if *k == "_" || *k == "@_" || *k == "%_" || *k == "__mutsu_callable_id" {
                    continue;
                }
                if !k.with_str(|s| local_names.contains(s)) {
                    self.env_mut().insert_sym(*k, v.clone());
                }
            }
        } else if let Some(saved_env) = saved_env {
            if saved_env.ptr_eq(self.env()) {
                // No env changes, nothing to merge
            } else {
                // Use declared_locals to only filter out function-local vars.
                // Captured outer variables should propagate their modifications.
                let local_names: std::collections::HashSet<&str> =
                    if let Some(ref declared) = cf.declared_locals {
                        declared.iter().map(|s| s.as_str()).collect()
                    } else {
                        cf.code.locals.iter().map(|s| s.as_str()).collect()
                    };
                let mut restored_env = saved_env;
                for (k, v) in self.env().iter() {
                    if !k.with_str(|s| local_names.contains(s)) {
                        restored_env.insert_sym(*k, v.clone());
                    }
                }
                *self.env_mut() = restored_env;
                // Mark env as dirty so caller re-syncs its locals from env.
                // This is needed when captured outer variables were modified
                // by the function (e.g. $a++ where $a is from the caller's scope).
            }
        } else {
            // Slice 6.3 step 2: the no-merge case — a 0-local function (no overlay,
            // no env clone). Its body wrote directly to the live caller env, so we
            // can't detect a captured-outer write by a merge. Gate on the
            // compile-time `has_env_writes` flag instead of the old blanket
            // post-call mark: a body that performs NO env write (`sub f { 42 }`,
            // no assign / increment / nested call / registration) cannot dirty a
            // caller slot — the dispatch's routine `_` is restored above — so it
            // needs no pull. Only a body that can write env forces the re-sync.
            if cf.code.has_env_writes {}
        }

        // Restore caller's $_ after routine call. In the scoped path the caller
        // env already retains its own `_` (the callee's `_` lived in the dropped
        // overlay and is skipped by the merge above), so this only runs for the
        // non-scoped clone path.
        if cf.code.is_routine && !use_scoped {
            match saved_topic {
                Some(v) => {
                    self.env_mut().insert("_".to_string(), v);
                }
                None => {
                    self.env_mut().remove("_");
                }
            }
        }

        // Slice F (env<->locals coherence): record the captured-outer variables
        // this body writes so the fast-call site writes their new env values
        // straight through to the caller's local slots (`apply_pending_rw_writeback`),
        // dropping the dependency on the reverse `sync_locals_from_env` pull. This
        // is the 0-arg-function analog of the closure free-var writeback (#3307):
        // `sub bump { $acc = $acc + 5 }` mutates the enclosing `$acc` by name, and
        // without this the caller's `$acc` slot stays stale when the reverse pull
        // is disabled. `free_var_writes` is the compile-time set of free vars this
        // body (or a nested closure) writes, so a pure body (`sub f { 42 }`) records
        // nothing and pays no cost. The fast-call site drains this immediately on
        // return, placing each source in the directly-enclosing caller's slot.
        // The topic (`_`/`@_`/`%_`) is excluded: it is the per-call loop/topic
        // alias, never a caller lexical to write back. (Cross-frame propagation —
        // a write a nested callee performed against a grand-ancestor's variable, as
        // in `note-twice`->`note-x`->`$log` or 0-arg recursion — still relies on the
        // reverse pull: a single-level call-site drain cannot reach it, and carrying
        // sources up the stack collides with lazy-iteration topics.)
        for sym in &cf.code.free_var_writes {
            sym.with_str(|name| {
                if name != "_" && name != "@_" && name != "%_" {
                    self.pending_rw_writeback_sources.push(name.to_string());
                }
            });
        }

        let final_result = match result {
            Ok(()) if fail_bypass => Ok(ret_val),
            Ok(()) => {
                if let Some(v) = explicit_return {
                    Ok(v)
                } else {
                    Ok(ret_val)
                }
            }
            Err(e) => Err(e),
        };
        // Restore the routine registry (removing this body's lexical routines)
        // unless an inner routine escaped via the return value.
        if let Some(snapshot) = routine_snapshot {
            match &final_result {
                Ok(v) if Self::return_value_escapes_routine(v) => {}
                _ => self.restore_routine_registry(snapshot),
            }
        }
        final_result
    }
}
