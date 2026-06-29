//! Misc interpreter state: my-global-stash visibility, END phasers,
//! routine-registry snapshot/restore, block-scope depth, and `let`-saves.
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn should_hide_from_my_global_stash(&self, key: &str) -> bool {
        if key.starts_with('$')
            || key.starts_with('@')
            || key.starts_with('%')
            || key.starts_with('&')
        {
            return false;
        }
        self.need_hidden_classes.contains(key)
            || key
                .strip_prefix("GLOBAL::")
                .is_some_and(|name| self.need_hidden_classes.contains(name))
            || key
                .rsplit_once("::")
                .is_some_and(|(_, short)| self.need_hidden_classes.contains(short))
            || self
                .need_hidden_classes
                .iter()
                .any(|name| key.ends_with(&format!("::{name}")))
    }

    /// Check if a name is a known pseudo-package (MY, OUTER, CORE, etc.)
    pub(crate) fn is_pseudo_package_name(name: &str) -> bool {
        matches!(
            name,
            "SETTING"
                | "CALLER"
                | "CALLERS"
                | "OUTER"
                | "OUTERS"
                | "CORE"
                | "GLOBAL"
                | "MY"
                | "OUR"
                | "DYNAMIC"
                | "UNIT"
                | "LEXICAL"
                | "CLIENT"
        )
    }

    pub(crate) fn push_end_phaser(&mut self, body: Vec<Stmt>) {
        let captured_env = self.env.clone();
        self.end_phasers.push((body, captured_env));
    }

    /// Return the number of currently registered END phasers.
    pub(crate) fn end_phaser_count(&self) -> usize {
        self.end_phasers.len()
    }

    /// Update captured envs for END phasers registered since `start_idx`
    /// with the current values from the given env.  This ensures that END
    /// phasers see final variable values rather than stale copies captured
    /// at registration time (e.g., inside closures or block scopes).
    pub(crate) fn update_end_phaser_envs(&mut self, start_idx: usize, current_env: &Env) {
        for phaser in self.end_phasers[start_idx..].iter_mut() {
            let captured = &mut phaser.1;
            for (k, v) in current_env {
                if captured.contains_key_sym(*k) {
                    captured.insert_sym(*k, v.clone());
                }
            }
        }
    }

    /// Update captured envs of ALL END phasers, but only for the specified
    /// variable names.  Used after closure calls to propagate changes to
    /// captured variables without overwriting unrelated variables.
    pub(crate) fn update_end_phaser_envs_for_keys(
        &mut self,
        keys: &std::collections::HashSet<&str>,
        current_env: &Env,
    ) {
        for phaser in self.end_phasers.iter_mut() {
            let captured = &mut phaser.1;
            for k in keys {
                if captured.contains_key(k)
                    && let Some(v) = current_env.get(k)
                {
                    captured.insert(k.to_string(), v.clone());
                }
            }
        }
    }

    /// Register an END phaser site_id. Returns true if this is the first
    /// registration (phaser should be pushed), false if already registered.
    pub(crate) fn register_end_phaser_site(&mut self, site_id: u64) -> bool {
        self.end_phaser_sites.insert(site_id)
    }

    pub(crate) fn snapshot_routine_registry(&self) -> RoutineRegistrySnapshot {
        // Single guard for all six reads (avoids stacking read guards).
        let registry = self.registry();
        (
            registry.functions.clone(),
            registry.proto_functions.clone(),
            registry.token_defs.clone(),
            registry.proto_subs.clone(),
            registry.proto_tokens.clone(),
            registry.our_scoped_functions.keys().copied().collect(),
        )
    }

    pub(crate) fn restore_routine_registry(&mut self, snapshot: RoutineRegistrySnapshot) {
        self.restore_routine_registry_impl(snapshot, false);
    }

    pub(crate) fn restore_routine_registry_eval(&mut self, snapshot: RoutineRegistrySnapshot) {
        self.restore_routine_registry_impl(snapshot, true);
    }

    fn restore_routine_registry_impl(&mut self, snapshot: RoutineRegistrySnapshot, is_eval: bool) {
        let (functions, proto_functions, token_defs, proto_subs, proto_tokens, our_scoped_keys) =
            snapshot;
        // Collect our-scoped functions that were newly added during this block
        // (not present in the snapshot) that need to persist after scope restoration.
        // Preserve functions defined in the current package (original behavior)
        // OR functions newly registered during this block (so nested package
        // declarations like `{ package Foo { our sub bar {} } }` survive). We
        // distinguish "newly registered during this block" by tracking the set
        // of our_scoped_functions keys that existed at snapshot time.
        let current_pkg = self.current_package();
        let mut new_our: Vec<(Symbol, std::sync::Arc<FunctionDef>)> = Vec::new();
        // Collect under a read guard, which drops before the writes below
        // (read->write on the same lock would deadlock).
        {
            let registry = self.registry();
            for (key, def) in &registry.our_scoped_functions {
                if functions.contains_key(key) {
                    continue;
                }
                let def_pkg = def.package.resolve();
                // Always preserve same-package subs (original behavior).
                if def_pkg == current_pkg {
                    new_our.push((*key, def.clone()));
                    continue;
                }
                // Also preserve subs that were newly added to our_scoped_functions
                // during this block (i.e. their key was not in the snapshot). This
                // covers nested `package Foo { our sub bar {} }` blocks. Subs from
                // module loading (`use Foo`) are typically already in the snapshot
                // by the time the block is restored, so they are not preserved here.
                if !our_scoped_keys.contains(key) {
                    new_our.push((*key, def.clone()));
                }
            }
        }
        let mut registry = self.registry_mut();
        registry.functions = functions;
        registry.proto_functions = proto_functions;
        registry.token_defs = token_defs;
        registry.proto_subs = proto_subs;
        registry.proto_tokens = proto_tokens;
        // Re-apply only newly added our-scoped functions so they survive block scope exit.
        // In EVAL context, our-scoped functions should NOT leak into the outer lexical
        // scope — they remain accessible only via OUR:: pseudo-package resolution.
        if !is_eval {
            for (key, def) in new_our {
                registry.functions.insert(key, def);
            }
        }
        drop(registry);
        // The routine registry just changed: a lexical (`my sub`) registered
        // inside the block was removed. Invalidate the name-keyed resolution
        // caches so a subsequent call to that name re-resolves against the
        // restored registry rather than returning the now-out-of-scope
        // CompiledFunction the first in-block call cached (the compiled-function
        // map is program-global and still contains the lexical sub's body).
        self.fn_resolve_gen += 1;
        self.method_resolve_cache.clear();
        self.last_method_resolve = None;
        self.fast_method_cache.clear();
        self.multi_resolve_cache.clear();
        self.multi_type_cacheable.clear();
        self.func_multi_resolve_cache.clear();
        self.func_multi_type_cacheable.clear();
        self.dispatch_multi_candidate.clear();
    }

    pub(crate) fn push_block_scope_depth(&mut self) {
        self.block_scope_depth += 1;
    }

    pub(crate) fn pop_block_scope_depth(&mut self) {
        self.block_scope_depth = self.block_scope_depth.saturating_sub(1);
    }

    /// Push a saved variable value for `let`/`temp` scope management.
    /// `is_temp`: true for `temp` (always restore), false for `let` (restore on failure only).
    pub(crate) fn let_saves_push(&mut self, name: String, value: Value, is_temp: bool) {
        // Take an independent snapshot: an instance value must be deep-copied so
        // a later in-place mutation through its shared cell does not corrupt the
        // saved-for-restore value (Phase 3, Stage 1).
        self.let_saves
            .push((name, value.into_temp_snapshot(), is_temp));
    }

    /// Restore one `let`/`temp` save. For an instance, write the saved
    /// attributes back into the *live* shared cell of the variable's current
    /// binding so every alias sees the restoration and object identity (id +
    /// cell) is preserved, rather than rebinding the name to a detached copy.
    fn restore_let_value(&mut self, name: String, restored: Value) {
        // A boxed (shared-cell) binding: write the restored value THROUGH the live
        // cell so the owner's local slot (which holds the same Arc) sees the
        // restoration, rather than replacing the env entry with a detached plain
        // value (which would strand the slot's stale cell). The `let`-save already
        // captured the inner value (see `exec_let_save_op`). Covers named-sub
        // captured-outer boxing (docs/captured-outer-cell-sharing.md). Gated on the
        // same toggle as the boxing it supports, so the default build is
        // byte-identical to before.
        if let Some(Value::ContainerRef(arc)) = self.env.get(&name).cloned() {
            arc.lock().unwrap().clone_from(&restored);
            return;
        }
        // Non-cell restore writes the saved value into `env` by name only; record
        // it so the block-exit op refreshes the owning local slot precisely
        // (`apply_pending_rw_writeback`) instead of relying on the blanket
        // env→locals pull (env_dirty-removal substrate). The owner is usually this
        // same frame (a bare `{ temp $x = … }` block), but a `temp`/`let` inside a
        // nested callee owns the slot a frame up — record on both the drop-on-miss
        // and retain-on-miss lists so either reaches it.
        self.pending_rw_writeback_sources.push(name.clone());
        self.record_caller_var_writeback(&name);
        if let Value::Instance {
            attributes: saved_attrs,
            ..
        } = &restored
        {
            // The current binding for `name` holds the live instance (the same
            // shared cell that was mutated during the dynamic scope). Write the
            // saved snapshot straight back into that cell; the env binding already
            // aliases it, so no re-insert is needed.
            if let Some(Value::Instance {
                attributes: live_attrs,
                ..
            }) = self.env.get(&name)
            {
                live_attrs.commit_attrs(saved_attrs.to_map());
                return;
            }
            self.env.insert(name, restored);
        } else {
            self.env.insert(name, restored);
        }
    }

    /// Current length of let_saves stack (used as a mark).
    pub(crate) fn let_saves_len(&self) -> usize {
        self.let_saves.len()
    }

    /// Resolve the value to restore, applying `is default(...)` when restoring Nil.
    fn resolve_restore_value(&self, name: &str, val: &Value) -> Value {
        if matches!(val, Value::Nil)
            && let Some(default) = self.var_defaults.get(name)
        {
            return default.clone();
        }
        val.clone()
    }

    /// Restore all variables from let_saves starting at `mark`, then truncate.
    pub(crate) fn restore_let_saves(&mut self, mark: usize) {
        for i in (mark..self.let_saves.len()).rev() {
            let (name, old_val, _is_temp) = self.let_saves[i].clone();
            let restored = self.resolve_restore_value(&name, &old_val);
            self.restore_let_value(name, restored);
        }
        self.let_saves.truncate(mark);
    }

    /// On successful block exit: restore `temp` saves, discard `let` saves.
    /// For `let`, only restore if the block returned an unsuccessful value.
    pub(crate) fn resolve_let_saves_on_success(&mut self, mark: usize, success: bool) {
        // Collect restore actions first to avoid borrow conflicts.
        let restores: Vec<(String, Value)> = (mark..self.let_saves.len())
            .rev()
            .filter_map(|i| {
                let (ref name, ref old_val, is_temp) = self.let_saves[i];
                if is_temp || !success {
                    Some((name.clone(), old_val.clone()))
                } else {
                    None
                }
            })
            .collect();
        for (name, old_val) in restores {
            let restored = self.resolve_restore_value(&name, &old_val);
            self.restore_let_value(name, restored);
        }
        self.let_saves.truncate(mark);
    }

    /// Discard let_saves from `mark` without restoring (block succeeded).
    pub(crate) fn discard_let_saves(&mut self, mark: usize) {
        self.let_saves.truncate(mark);
    }
}
