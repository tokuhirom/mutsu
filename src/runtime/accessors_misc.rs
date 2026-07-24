//! Misc interpreter state: my-global-stash visibility, END phasers,
//! routine-registry snapshot/restore, block-scope depth, and `let`-saves.
use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

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
        let package = self.current_package();
        self.end_phasers.push((body, captured_env, package));
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
        // In EVAL context, a `grammar`/`token`/`rule` declared inside the EVAL'd
        // code registers its class (which IS merged back into `registry.classes`
        // by the caller) plus its tokens here. Restoring `token_defs`/`proto_tokens`
        // to the snapshot would drop those tokens, so a later `.parse` on the
        // returned grammar type object fails with "Unknown method parse". Preserve
        // newly-registered tokens (keys absent from the snapshot) so the EVAL'd
        // grammar stays usable, mirroring how EVAL'd classes/roles persist.
        //
        // The same applies to a `grammar`/`class` declared inside a BARE BLOCK:
        // a `token`/`rule` in its body is a METHOD of that package, and Raku
        // scopes a package declaration to the package (`our`), not to the
        // enclosing block — so `{ grammar B { token id {...} } }` must leave
        // `B`'s tokens usable afterwards (`grammar D is B { rule TOP { <id> } }`
        // declared later still resolves `<id>`). The class itself already
        // survives; dropping only its tokens left `<id>` unresolvable, and the
        // engine then fell back to method dispatch — "No such method 'id' for
        // invocant of type 'Match'" (99problems-41-to-50.t P47, whose P46 block
        // declares the base grammar that P47's block subclasses).
        //
        // A block-local `my token` is owned by no declared package, so it is not
        // preserved here and still drops with the block, as before.
        let mut new_tokens: Vec<(Symbol, Vec<std::sync::Arc<FunctionDef>>)> = Vec::new();
        let mut new_proto_tokens: Vec<String> = Vec::new();
        {
            let registry = self.registry();
            let token_owned_by_package = |defs: &[std::sync::Arc<FunctionDef>]| {
                defs.iter()
                    .any(|d| registry.classes.contains_key(&d.package.resolve()))
            };
            for (key, defs) in &registry.token_defs {
                if token_defs.contains_key(key) {
                    continue;
                }
                if is_eval || token_owned_by_package(defs) {
                    new_tokens.push((*key, defs.clone()));
                }
            }
            for pt in &registry.proto_tokens {
                if proto_tokens.contains(pt) {
                    continue;
                }
                // Keep a proto declaration exactly when its candidates survive.
                let kept = is_eval
                    || registry
                        .token_defs
                        .get(&Symbol::intern(pt))
                        .is_some_and(|defs| token_owned_by_package(defs))
                    || new_tokens.iter().any(|(k, _)| {
                        let n = k.resolve();
                        n == *pt || n.starts_with(&format!("{pt}:sym"))
                    });
                if kept {
                    new_proto_tokens.push(pt.clone());
                }
            }
        }
        let mut registry = self.registry_mut();
        registry.functions = functions;
        registry.proto_functions = proto_functions;
        registry.token_defs = token_defs;
        registry.proto_subs = proto_subs;
        registry.proto_tokens = proto_tokens;
        for (key, defs) in new_tokens {
            registry.token_defs.insert(key, defs);
        }
        for pt in new_proto_tokens {
            registry.proto_tokens.insert(pt);
        }
        // token_defs was rewritten wholesale: invalidate regex parses that may
        // have folded token content in.
        crate::runtime::regex_parse::TOKEN_DEFS_GEN
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
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
        self.native_ctor_plan_cache.clear();
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
    pub(crate) fn let_saves_push(
        &mut self,
        name: String,
        value: Value,
        is_temp: bool,
        slot: Option<u32>,
    ) {
        // Take an independent snapshot: an instance value must be deep-copied so
        // a later in-place mutation through its shared cell does not corrupt the
        // saved-for-restore value (Phase 3, Stage 1).
        self.let_saves
            .push((name, value.into_temp_snapshot(), is_temp, slot));
    }

    /// Restore one `let`/`temp` save. For an instance, write the saved
    /// attributes back into the *live* shared cell of the variable's current
    /// binding so every alias sees the restoration and object identity (id +
    /// cell) is preserved, rather than rebinding the name to a detached copy.
    fn restore_let_value(&mut self, name: String, restored: Value, slot: Option<u32>) {
        // A boxed (shared-cell) binding: write the restored value THROUGH the live
        // cell so the owner's local slot (which holds the same Arc) sees the
        // restoration, rather than replacing the env entry with a detached plain
        // value (which would strand the slot's stale cell). The `let`-save already
        // captured the inner value (see `exec_let_save_op`). Covers named-sub
        // captured-outer boxing (docs/captured-outer-cell-sharing.md). Gated on the
        // same toggle as the boxing it supports, so the default build is
        // byte-identical to before.
        if let Some(v) = self.env.get(&name).cloned()
            && let ValueView::ContainerRef(arc) = v.view()
        {
            arc.lock().unwrap().clone_from(&restored);
            return;
        }
        // §1.4/§1.5: when the compiler baked THIS frame's slot for `name`, write it
        // directly so a live INNER shadow is restored — not the OUTER slot that a
        // by-name `find_local_slot` (position) would pick. A baked slot means the
        // name is a local of the current routine frame (its owner IS this frame),
        // so the caller-frame writeback machinery is unnecessary. With the shadow
        // gate OFF the baked slot equals the single (position) slot, so this is
        // byte-identical to the old by-name drain, just applied at restore time.
        //
        // Non-cell restore also writes the saved value into `env` by name (below)
        // for by-name readers. With NO baked slot (a non-local target, or a
        // `temp`/`let` whose owner is a caller frame) fall back to the name-based
        // `pending_rw_writeback` drain, recording both the drop-on-miss and
        // retain-on-miss lists so either reaches the owning frame's slot.
        match slot {
            Some(s) if (s as usize) < self.locals.len() => {
                self.locals[s as usize] = restored.clone();
            }
            _ => {
                self.pending_rw_writeback_sources.push(name.clone());
                self.record_caller_var_writeback(&name);
            }
        }
        if let ValueView::Instance {
            attributes: saved_attrs,
            ..
        } = restored.view()
        {
            // The current binding for `name` holds the live instance (the same
            // shared cell that was mutated during the dynamic scope). Write the
            // saved snapshot straight back into that cell; the env binding already
            // aliases it, so no re-insert is needed.
            if let Some(v) = self.env.get(&name)
                && let ValueView::Instance {
                    attributes: live_attrs,
                    ..
                } = v.view()
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
        if val.is_nil()
            && let Some(default) = self.var_defaults.get(name)
        {
            return default.clone();
        }
        val.clone()
    }

    /// Restore all variables from let_saves starting at `mark`, then truncate.
    pub(crate) fn restore_let_saves(&mut self, mark: usize) {
        for i in (mark..self.let_saves.len()).rev() {
            let (name, old_val, _is_temp, slot) = self.let_saves[i].clone();
            let restored = self.resolve_restore_value(&name, &old_val);
            self.restore_let_value(name, restored, slot);
        }
        self.let_saves.truncate(mark);
    }

    /// On successful block exit: restore `temp` saves, discard `let` saves.
    /// For `let`, only restore if the block returned an unsuccessful value.
    pub(crate) fn resolve_let_saves_on_success(&mut self, mark: usize, success: bool) {
        // Collect restore actions first to avoid borrow conflicts.
        let restores: Vec<(String, Value, Option<u32>)> = (mark..self.let_saves.len())
            .rev()
            .filter_map(|i| {
                let (ref name, ref old_val, is_temp, slot) = self.let_saves[i];
                if is_temp || !success {
                    Some((name.clone(), old_val.clone(), slot))
                } else {
                    None
                }
            })
            .collect();
        for (name, old_val, slot) in restores {
            let restored = self.resolve_restore_value(&name, &old_val);
            self.restore_let_value(name, restored, slot);
        }
        self.let_saves.truncate(mark);
    }

    /// Discard let_saves from `mark` without restoring (block succeeded).
    pub(crate) fn discard_let_saves(&mut self, mark: usize) {
        self.let_saves.truncate(mark);
    }
}
