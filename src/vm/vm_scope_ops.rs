//! Scope ops: `subtest`, `react`, and `whenever` scope execution.
use super::*;

impl Interpreter {
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
        let ctx = self.begin_subtest();
        let saved_depth = self.stack.len();
        let run_result = self.run_range(code, body_start, end, compiled_fns);
        self.stack.truncate(saved_depth);
        self.finish_subtest(ctx, &label, run_result)?;
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

        // `whenever` callbacks run as compiled bytecode (the drive loop lives on
        // `impl Interpreter`, see `vm_react_loop.rs`) but still capture their lexicals from
        // env. First pull any pending env updates into locals (e.g. instance
        // attribute mutations written into the shared cell after bind-stdin), then
        // flush all locals to env so captured vars are visible/mutable from the
        // whenever callbacks.
        self.sync_env_from_locals(code);

        // Enter react mode: whenever blocks will register subscriptions
        self.enter_react();
        let saved_depth = self.stack.len();
        let run_result = self.run_range(code, body_start, end, compiled_fns);
        self.stack.truncate(saved_depth);

        // If `done;` was called in the react body, skip the event loop —
        // the body already signaled that no further events should be processed.
        let body_done = matches!(&run_result, Err(e) if e.is_react_done());
        // The react/supply drive loop runs Interpreter-side and dispatches every
        // whenever / LAST / QUIT / CLOSE callback as compiled bytecode
        // (Stage 2 #3038/#3039; QUIT handlers Interpreter-native in the Stage 3 follow-up).
        // No drive-loop callback routes back through the tree-walk interpreter.
        // The `whenever` callbacks mutate captured-outer lexicals by name in env,
        // with no per-write record this site can drain. Snapshot the caller frame's
        // slot-backing env values right before the event loop so that, after it,
        // only the slots whose env value actually changed are written through.
        let pre_env: Vec<Option<Value>> = code
            .locals
            .iter()
            .map(|n| {
                self.env().get(n).cloned().or_else(|| {
                    n.strip_prefix('$')
                        .or_else(|| n.strip_prefix('@'))
                        .or_else(|| n.strip_prefix('%'))
                        .or_else(|| n.strip_prefix('&'))
                        .and_then(|b| self.env().get(b).cloned())
                })
            })
            .collect();
        let event_result = if body_done {
            // Drain any queued subscriptions so they don't leak
            self.run_react_event_loop_drain();
            Ok(())
        } else {
            self.run_react_event_loop()
        };
        // Slice F (react/whenever coherence): the `whenever` callbacks ran as
        // compiled bytecode on *this* VM (synchronous `from-list` emit) and
        // mutated captured-outer caller lexicals (`my $i; whenever ... { $i++ }`)
        // straight into `env` by name. Reconcile the caller's local slots from
        // env so the slot stays coherent (same HashEntryRef / `!attr` per-slot
        // skips); this is what keeps `$i` correct.
        for (i, name) in code.locals.iter().enumerate() {
            if name.starts_with('!') || matches!(self.locals[i], Value::HashEntryRef { .. }) {
                continue;
            }
            let cur = self.env().get(name).cloned().or_else(|| {
                name.strip_prefix('$')
                    .or_else(|| name.strip_prefix('@'))
                    .or_else(|| name.strip_prefix('%'))
                    .or_else(|| name.strip_prefix('&'))
                    .and_then(|b| self.env().get(b).cloned())
            });
            if let Some(cur) = cur
                && pre_env.get(i).map(|p| p.as_ref()) != Some(Some(&cur))
            {
                self.locals[i] = cur;
            }
        }

        *ip = end;
        if let Err(err) = run_result
            && !err.is_react_done()
        {
            return Err(err);
        }
        if let Err(err) = event_result
            && !err.is_react_done()
        {
            // Wrap in X::React::Died if not already wrapped
            return Err(crate::runtime::Interpreter::wrap_react_died_if_needed(err));
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
            loan_env!(
                self,
                run_whenever_with_value(supply_val, target_var, &param, body)
            )?;
            // Slice F (env<->locals coherence): a `my $tap = do whenever $sup {…}`
            // binds the tap handle by writing `env[target_var]` directly (see
            // `run_whenever_with_value`), but never updates the caller's local
            // slot. With the reverse env->locals pull disabled, a later read of
            // that variable *within the same react block* (e.g.
            // `isa-ok $tap, Tap`) sees the stale slot (the `do` block's own
            // result) instead of the bound tap. Reconcile the caller's slots from
            // env here so the binding is visible immediately. Byte-identical with
            // the reverse pull enabled.
            //
            // env_dirty substrate (docs/captured-outer-cell-sharing.md §10): the
            // bound name is known exactly (`target_var`), so write just that slot
            // through from env — the precise form of the blanket reconcile below.
            // Armed only under boxing; the default build keeps the blanket pull.
            if let Some(name) = target_var
                && let Some(slot) = self.find_local_slot(code, name)
                && !matches!(self.locals[slot], Value::HashEntryRef { .. })
                && let Some(val) = self.env().get(name).cloned()
            {
                self.locals[slot] = val;
            }
            Ok(())
        } else {
            Err(RuntimeError::new("WheneverScope expects Block body"))
        }
    }

    /// Walk the MRO of `class_name` to find a parameterized Array or Hash parent.
    /// Returns the element type if found (e.g. "Str" for `Array[Str]`).
    pub(super) fn find_parameterized_container_parent(&self, class_name: &str) -> Option<String> {
        let parents = self.class_parents_readonly(class_name);
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
