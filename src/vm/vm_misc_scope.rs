use super::*;

impl Interpreter {
    pub(super) fn exec_state_var_init_op(&mut self, code: &CompiledCode, slot: u32, key_idx: u32) {
        let init_val = self.stack.pop().unwrap_or(Value::Nil);
        let base_key = Self::const_str(code, key_idx);
        let scoped_key = self.scoped_state_key(base_key);
        let slot_idx = slot as usize;
        let name = &code.locals[slot_idx];
        // Track C: while a thread is running, a user `state` variable lives in a
        // shared `ContainerRef` cell (keyed in shared_vars) so concurrent calls
        // to the same routine across threads — `await (^3).map: { start f() }`
        // where `f` has `state $n` — share one live cell. The cell makes the
        // increment atomic (the ContainerRef inc/dec chokepoint) and visible to
        // the parent after `await`. `StateVarInit` is emitted only for genuine
        // `state` declarations, so `ff`/`fff`/smart-match internal state (which
        // uses `set_state_var` directly, never a cell) is unaffected.
        let val = if self.shared_vars_active {
            let coerced_initial = if name.starts_with('@') {
                runtime::coerce_to_array(init_val)
            } else if name.starts_with('%') {
                runtime::coerce_to_hash(init_val)
            } else {
                init_val
            };
            // Seed the shared cell from any value the local snapshot already
            // holds (state mutated before the first thread spawned), else from
            // this declaration's initializer.
            let initial = self
                .get_state_var(&scoped_key)
                .cloned()
                .unwrap_or(coerced_initial);
            // The cross-thread cell key must be stable across mutsu's two
            // compilations of the same routine (the registered body `&f` and the
            // on-the-fly multi-candidate body `&f/0` reach `state $n` under
            // different `current_package` suffixes and opcode positions, so
            // `scoped_key` alone differs between `start f()` and a direct `f()`).
            // Normalize away the `/<n>` candidate suffix and the trailing
            // `@<ip>` position so both paths share one cell.
            let shared_key = format!(
                "__mutsu_shared_state::{}",
                crate::runtime::Interpreter::normalize_state_key(&scoped_key)
            );
            let cell = self.get_or_init_shared_state_cell(&shared_key, initial);
            // Keep the local store pointing at the cell too, so the exit-time
            // writeback and any non-cell reader observe the same Arc.
            self.set_state_var(scoped_key.clone(), cell.clone());
            cell
        } else if let Some(stored) = self.get_state_var(&scoped_key) {
            stored.clone()
        } else {
            // Coerce @ variables to Array and % variables to Hash,
            // matching the behavior of SetLocal for these sigils.
            let coerced = if name.starts_with('@') {
                runtime::coerce_to_array(init_val)
            } else if name.starts_with('%') {
                runtime::coerce_to_hash(init_val)
            } else {
                init_val
            };
            self.set_state_var(scoped_key.clone(), coerced.clone());
            coerced
        };
        self.locals[slot_idx] = val.clone();
        let name = name.to_string();
        // Only insert into env if the value differs from what's already there.
        // This avoids triggering Arc::make_mut deep clone on the CoW env when
        // the state variable is already initialized with the same value (common
        // case in tight loops).
        let needs_env_insert = self.env().get(&name) != Some(&val);
        if needs_env_insert {
            self.env_mut().insert(name.clone(), val);
        }
        // Store metadata mapping variable name to its state storage key.
        // Closures that capture this variable can use this to update state
        // storage when they modify the variable.
        let meta_key = format!("__mutsu_state_key::{}", name);
        let scoped_key_val = Value::str(scoped_key.clone());
        let needs_meta_insert = self.env().get(&meta_key) != Some(&scoped_key_val);
        if needs_meta_insert {
            self.env_mut().insert(meta_key, scoped_key_val);
        }
    }

    pub(super) fn exec_block_scope_op(
        &mut self,
        code: &CompiledCode,
        bounds: [u32; 7],
        is_bare_block: bool,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let [
            pre_end,
            enter_end,
            body_end,
            keep_start,
            undo_start,
            post_start,
            end,
        ] = bounds;
        let pre_start = *ip + 1;
        let enter_start = pre_end as usize;
        let body_start = enter_end as usize;
        let queue_start = body_end as usize;
        let keep_start = keep_start as usize;
        let undo_start = undo_start as usize;
        let post_start = post_start as usize;
        let end = end as usize;
        let routine_snapshot = self.snapshot_routine_registry();
        let saved_env = self.env().clone();
        let saved_locals = self.locals.clone();
        let once_scope = self.next_once_scope_id();
        // Track variables declared within this block scope.
        self.block_declared_vars
            .push(std::collections::HashSet::new());
        // Push saved locals for $OUTER:: variable access.
        self.outer_scope_locals.push(saved_locals.clone());
        // Baseline for the ENTER-result stack: any value captured by this block's
        // ENTER section (PushEnterResult) must be cleared on exit even if the body
        // throws before reaching LoadEnterResult, so no stale value leaks upward.
        let enter_result_base = self.enter_result_stack.len();

        // Run PRE phasers first (before ENTER)
        self.run_range(code, pre_start, enter_start, compiled_fns)?;

        // A genuine bare block `{ ... }` is a Raku callframe: a backtrace captured
        // while executing inside it (e.g. a `fail`/`die` in a sub called here)
        // must include an anonymous frame for this block. Record the routine-stack
        // depth so the cleanup below can drop this frame and any frames leaked by
        // a nested bare block whose body threw past its own cleanup.
        let routine_base = self.routine_stack_len();
        if is_bare_block {
            let call_line = self.current_source_line();
            let call_file = self.current_source_file();
            self.push_block_routine_with_location(
                self.current_package(),
                String::new(),
                call_line,
                call_file,
            );
        }

        let enter_result = self.run_range(code, enter_start, body_start, compiled_fns);
        self.push_once_scope(once_scope);
        self.push_block_scope_depth();
        self.push_lexical_class_scope();
        self.push_enum_scope();
        let stack_base = self.stack.len();
        let topic_before = self.last_topic_value.clone();
        let end_phaser_count_before = self.end_phaser_count();
        // If ENTER died, skip the body but still run LEAVE phasers
        let mut body_result = if let Err(e) = enter_result {
            Err(e)
        } else {
            self.run_range(code, body_start, queue_start, compiled_fns)
        };
        let body_value = if self.stack.len() > stack_base {
            self.stack.last().cloned()
        } else if self.last_topic_value != topic_before {
            self.last_topic_value.clone()
        } else {
            None
        };
        let ran_undo = !Self::should_run_success_queue(&body_result, body_value);

        // Set $! before LEAVE/UNDO phasers run so they can see the exception
        if let Err(ref e) = body_result
            && Self::is_exceptional_block_exit(e)
        {
            let err_val = if let Some(ex) = e.exception.as_ref() {
                *ex.clone()
            } else {
                let mut exc_attrs = std::collections::HashMap::new();
                exc_attrs.insert("message".to_string(), Value::str(e.message.clone()));
                // Untyped runtime error -> X::AdHoc (see vm_control_ops.rs).
                Value::make_instance(crate::symbol::Symbol::intern("X::AdHoc"), exc_attrs)
            };
            self.env_mut().insert("!".to_string(), err_val.clone());
            for (i, name) in code.locals.iter().enumerate() {
                if name == "!" {
                    self.locals[i] = err_val;
                    break;
                }
            }
        }

        let queue_res = if !ran_undo {
            self.run_leave_queue_guarded(code, keep_start, undo_start, compiled_fns)
        } else {
            self.run_leave_queue_guarded(code, undo_start, post_start, compiled_fns)
        };

        // When UNDO phasers ran in response to a fail(), mark the error so the
        // resulting Failure value will be created with handled=True (Raku semantics:
        // UNDO acts as a handler for the failure).
        if ran_undo
            && undo_start < post_start
            && let Err(ref mut e) = body_result
            && e.is_fail()
        {
            e.fail_handled = true;
        }

        // Run POST phasers after LEAVE (regardless of success/failure path)
        // Set $_ to the return/result value so POST can inspect it
        // Set $! to the exception if the body threw one
        if post_start < end {
            let post_topic = match &body_result {
                Ok(()) => self.last_topic_value.clone().unwrap_or(Value::Nil),
                Err(e) => e.return_value.clone().unwrap_or(Value::Nil),
            };
            self.env_mut().insert("_".to_string(), post_topic.clone());
            // Also update the local slot for $_ if present
            for (i, name) in code.locals.iter().enumerate() {
                if name == "_" {
                    self.locals[i] = post_topic;
                    break;
                }
            }
            // If the body threw an exception, set $! so POST can see it
            if let Err(ref e) = body_result
                && Self::is_exceptional_block_exit(e)
            {
                let err_val = if let Some(ex) = e.exception.as_ref() {
                    *ex.clone()
                } else {
                    let mut exc_attrs = std::collections::HashMap::new();
                    exc_attrs.insert("message".to_string(), Value::str(e.message.clone()));
                    // Untyped runtime error -> X::AdHoc (see vm_control_ops.rs).
                    Value::make_instance(crate::symbol::Symbol::intern("X::AdHoc"), exc_attrs)
                };
                self.env_mut().insert("!".to_string(), err_val.clone());
                // Also update the local slot for $! if present
                for (i, name) in code.locals.iter().enumerate() {
                    if name == "!" {
                        self.locals[i] = err_val;
                        break;
                    }
                }
            }
        }
        let post_res = self.run_range(code, post_start, end, compiled_fns);

        self.restore_routine_registry(routine_snapshot);

        // Pop the block-declared variables set.
        let block_declared = self.block_declared_vars.pop().unwrap_or_default();
        // Pop the outer scope locals snapshot.
        self.outer_scope_locals.pop();

        // Update captured envs of END phasers registered during this block
        // so they see the final values of block-scoped variables (which will
        // be removed from env when the block scope is restored below).
        if self.end_phaser_count() > end_phaser_count_before {
            let current = self.env().clone();
            self.update_end_phaser_envs(end_phaser_count_before, &current);
        }
        let current_env = self.env().clone();
        let mut restored_env = saved_env.clone();
        for (k, v) in current_env {
            // Package-qualified names (e.g. Test1::ns, Foo::Bar) are package-global
            // and must propagate out of any block scope where they were declared.
            // Sigils may appear before the qualifier (e.g. &Test1::ns, $Foo::var).
            // Skip internal Interpreter metadata keys (which contain `::` but are not
            // user-visible package names, e.g. `__mutsu_var_meta::x`).
            let is_package_qualified = k.with_str(|s| {
                let stripped = s.trim_start_matches(['$', '@', '%', '&']);
                stripped.contains("::") && !stripped.starts_with("__mutsu_")
            });
            if is_package_qualified {
                restored_env.insert_sym(k, v);
                continue;
            }
            // Package type objects declared inside a block (e.g.
            // `{ package Foo { ... } }`) must remain visible outside the
            // block as type objects, just like classes/roles. The
            // `RegisterPackage` opcode stores them in env under the bare
            // package name. Restrict this to keys that look like a real
            // package identifier (uppercase ASCII start, not internal/
            // special variables like `_` or `__mutsu_*`).
            if matches!(&v, Value::Package(_))
                && !saved_env.contains_key_sym(k)
                && k.with_str(|s| s.chars().next().is_some_and(|c| c.is_ascii_uppercase()))
            {
                restored_env.insert_sym(k, v);
                continue;
            }
            if saved_env.contains_key_sym(k) {
                // Lexical topic is block-scoped; don't write inner `$_` back
                // to the outer scope on block exit. Also preserve the alias
                // metadata for `$_` so that `:=` bindings survive block exit.
                if k == "_" || k == "__mutsu_sigilless_alias::_" {
                    continue;
                }
                // Dynamic variables (e.g. $*VAR) are scoped to the block:
                // restore to the saved value rather than propagating the inner value.
                if k.starts_with("*") {
                    continue;
                }
                // Variables declared with `my` inside this block should not
                // propagate their values to the outer scope. Restore the outer
                // scope's original value instead.
                if k.with_str(|s| block_declared.contains(s)) {
                    continue;
                }
                restored_env.insert_sym(k, v);
            }
        }
        self.locals = saved_locals;
        for (idx, name) in code.locals.iter().enumerate() {
            if let Some(val) = restored_env.get(name).cloned() {
                self.locals[idx] = val;
            }
        }
        // For `our`-scoped locals declared inside this block (or in the outer
        // scope but reassigned inside via `our $x = ...`), the persistent
        // package value in `our_vars` may have been updated. Refresh the
        // lexical alias from the package store so the outer scope sees the
        // new value (Raku semantics: `our` is an alias for a package var).
        for (slot, qualified) in &code.our_locals {
            let Some(local_name) = code.locals.get(*slot) else {
                continue;
            };
            // Only refresh slots whose lexical alias existed in the outer
            // scope (i.e., the outer scope also declared `our $x`). For
            // `our` declarations made only inside the block, the lexical
            // alias is block-scoped and must not leak to the outer scope.
            if !saved_env.contains_key(local_name) {
                continue;
            }
            if let Some(val) = self.get_our_var(qualified).cloned() {
                if *slot < self.locals.len() {
                    self.locals[*slot] = val.clone();
                }
                restored_env.insert(local_name.clone(), val);
            }
        }
        *self.env_mut() = restored_env;
        // After block scope restoration, sync `:=` alias bindings.
        // If a local variable was modified inside the block and has a
        // sigilless alias (e.g. `my $a := $_`), propagate the local's
        // current value to the alias target in env so they stay in sync.
        for (idx, name) in code.locals.iter().enumerate() {
            let alias_key = format!("__mutsu_sigilless_alias::{}", name);
            if let Some(Value::Str(target)) = self.env().get(&alias_key).cloned() {
                let local_val = self.locals[idx].clone();
                let differs = self
                    .env()
                    .get(target.as_str())
                    .is_some_and(|env_val| &local_val != env_val);
                if differs {
                    self.env_mut().insert(target.to_string(), local_val);
                }
            }
        }
        // Drop readonly flags for block-declared variables so a later outer
        // `my` redeclaration of the same name is not marked as readonly by
        // the now-gone binding (`my %h := SetHash.new(...)` inside a block
        // must not leave `%h` readonly in the outer scope).
        for name in &block_declared {
            self.readonly_vars_mut().remove(name);
        }
        // Note: `our`-scoped variables persist in our_vars and are accessible
        // via package-qualified names (e.g., $Pkg::var) after block exit.
        self.pop_enum_scope();
        self.pop_lexical_class_scope();
        self.pop_block_scope_depth();
        self.pop_once_scope();
        self.enter_result_stack.truncate(enter_result_base);
        // Drop this block's callframe (and any leaked nested bare-block frames)
        // on every exit path. Runs regardless of body success/failure because the
        // body result is carried as a value (`body_result`) and only `?`-ed at the
        // very end, so this restore is exception-safe.
        self.truncate_routine_stack(routine_base);

        if let Err(e) = post_res {
            // POST failure overrides successful body and return-value body exits
            if body_result.is_ok() && queue_res.is_ok() {
                return Err(e);
            }
            // POST failure also overrides a "return" (non-exceptional exit)
            if let Err(ref be) = body_result
                && be.return_value.is_some()
            {
                return Err(e);
            }
        }
        if let Err(e) = queue_res
            && body_result.is_ok()
        {
            return Err(e);
        }
        body_result?;
        *ip = end;
        Ok(())
    }
}
