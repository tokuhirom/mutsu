//! Misc interpreter state: my-global-stash visibility, routine-registry
//! snapshot/restore, block-scope depth, and `let`-saves. (The END-phaser
//! accessors this file used to hold moved to `end_phasers`, next to the pass
//! that installs them.)
use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    pub(crate) fn should_hide_from_my_global_stash(&self, key: &str) -> bool {
        // The `self` key is the term's, never a lexical a stash lists: `$self`
        // is the reserved `LEX_SELF` key (ADR-0061).
        if key == "self" {
            return true;
        }
        if key.starts_with('$')
            || key.starts_with('@')
            || key.starts_with('%')
            || key.starts_with('&')
        {
            return false;
        }
        self.module.need_hidden_classes.contains(key)
            || key
                .strip_prefix("GLOBAL::")
                .is_some_and(|name| self.module.need_hidden_classes.contains(name))
            || crate::qualified::split_qualified(Symbol::intern(key))
                .is_some_and(|(_, short)| self.module.need_hidden_classes.contains(short.as_str()))
            || self.key_ends_with_need_hidden_class(key)
    }

    /// [`key_ends_with_any_as_qualified_tail`] over the hidden `need` classes.
    fn key_ends_with_need_hidden_class(&self, key: &str) -> bool {
        key_ends_with_any_as_qualified_tail(key, &self.module.need_hidden_classes)
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

    pub(crate) fn snapshot_routine_registry(&self) -> RoutineRegistrySnapshot {
        // Single guard for all six reads (avoids stacking read guards).
        let registry = self.registry();
        (
            std::sync::Arc::clone(&registry.functions),
            std::sync::Arc::clone(&registry.proto_functions),
            registry.proto_subs_snapshot(),
            std::sync::Arc::clone(&registry.token_defs),
            std::sync::Arc::clone(&registry.proto_tokens),
            std::sync::Arc::clone(&registry.our_scoped_functions),
            std::sync::Arc::clone(&self.dispatch.user_declared_infix_ops),
            std::sync::Arc::clone(&self.module.imported_routine_aliases),
            std::sync::Arc::clone(&self.module.imported_exported_proto_tags),
        )
    }

    pub(crate) fn restore_routine_registry(&mut self, snapshot: RoutineRegistrySnapshot) {
        self.restore_routine_registry_impl(snapshot, false);
    }

    pub(crate) fn restore_routine_registry_eval(&mut self, snapshot: RoutineRegistrySnapshot) {
        self.restore_routine_registry_impl(snapshot, true);
    }

    /// Whether every table `snapshot` holds is still the very `Arc` installed
    /// now. Each table is copy-on-write and the snapshot keeps a second handle
    /// to it, so any write since the snapshot replaced that table's `Arc`
    /// (`Arc::make_mut` clones a shared one): pointer identity of all nine
    /// means nothing the restore would put back has changed.
    // Cost: O(1).
    fn routine_registry_unchanged_since(&self, snapshot: &RoutineRegistrySnapshot) -> bool {
        use std::sync::Arc;
        let registry = self.registry();
        Arc::ptr_eq(&snapshot.0, &registry.functions)
            && Arc::ptr_eq(&snapshot.1, &registry.proto_functions)
            && Arc::ptr_eq(&snapshot.2, &registry.proto_subs_snapshot())
            && Arc::ptr_eq(&snapshot.3, &registry.token_defs)
            && Arc::ptr_eq(&snapshot.4, &registry.proto_tokens)
            && Arc::ptr_eq(&snapshot.5, &registry.our_scoped_functions)
            && Arc::ptr_eq(&snapshot.6, &self.dispatch.user_declared_infix_ops)
            && Arc::ptr_eq(&snapshot.7, &self.module.imported_routine_aliases)
            && Arc::ptr_eq(&snapshot.8, &self.module.imported_exported_proto_tags)
    }

    // Cost: O(1) when the scope changed none of the snapshotted tables (the
    // common bare block, which declares no routine). A scope that declared a
    // routine is O(1) too in the steady state of a loop or a repeated call: the
    // transition memo names the keys it installed, and the our-scoped and
    // module-routine checks are memoized while their tables are untouched. A
    // first run, an install the memo cannot name, or a written our-scoped or
    // token table is O(R), R = routine-registry entries. Rakudo: O(1) -- see
    // #9170.
    fn restore_routine_registry_impl(&mut self, snapshot: RoutineRegistrySnapshot, is_eval: bool) {
        // Nothing to put back: skip the diff and, above all, the token/method
        // generation bumps below, which would otherwise retire every
        // generation-keyed method cache on each exit of a block that declared
        // nothing (#9170).
        if self.routine_registry_unchanged_since(&snapshot) {
            return;
        }
        let tokens_unchanged = {
            let registry = self.registry();
            std::sync::Arc::ptr_eq(&snapshot.3, &registry.token_defs)
                && std::sync::Arc::ptr_eq(&snapshot.4, &registry.proto_tokens)
        };
        let (
            mut functions,
            proto_functions,
            proto_subs,
            token_defs,
            proto_tokens,
            our_scoped_keys,
            user_infix_ops,
            imported_routine_aliases,
            imported_exported_proto_tags,
        ) = snapshot;
        // Restore the user-declared infix operator set.  In Raku, operators are
        // lexically scoped per compilation unit; not restoring this caused a
        // block-local `sub infix:<+>` to keep intercepting imported-module
        // arithmetic (e.g. Test.rakumod's `$num_of_tests_run + 1`) after the
        // block exited, resetting the test counter to Nil.
        self.dispatch.user_declared_infix_ops = user_infix_ops;
        self.module.imported_routine_aliases = imported_routine_aliases;
        self.module.imported_exported_proto_tags = imported_exported_proto_tags;
        self.reinstate_module_functions(&mut functions, is_eval);
        // The our-scoped subs that survive the restore: the current package's,
        // and those this block registered itself (so nested package
        // declarations like `{ package Foo { our sub bar {} } }` survive).
        // Subs from module loading (`use Foo`) are typically already in the
        // snapshot by the time the block is restored, so they are not kept.
        let new_our = self.our_scoped_to_reinstate(&functions, &our_scoped_keys);
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
        // Both loops look for keys absent from the snapshot, so an untouched
        // token table (the snapshot's own `Arc`) has none to find.
        if !tokens_unchanged {
            let registry = self.registry();
            let token_owned_by_package = |defs: &[std::sync::Arc<FunctionDef>]| {
                defs.iter()
                    .any(|d| registry.classes.contains_key(&d.package.resolve()))
            };
            for (key, defs) in registry.token_defs.iter() {
                if token_defs.contains_key(key) {
                    continue;
                }
                if is_eval || token_owned_by_package(defs) {
                    new_tokens.push((*key, defs.clone()));
                }
            }
            for pt in registry.proto_tokens.iter() {
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
        // The base names whose registry keys this restore adds or drops. A
        // routine scope typically installs one or two inner `my sub`s on entry
        // and gives them back here, so the set is tiny next to the map — and
        // naming it keeps `fn_keys_by_base` alive for every OTHER base name
        // (#8314). The diff runs once per restore, where the wholesale drop it
        // replaces cost one full registry re-scan per base name per resolution
        // until the index refilled.
        //
        // When the map being given back was reached from the snapshot by the
        // memoized per-call installs alone -- the steady state of a routine
        // that declares an inner `my sub` -- the transition memo already knows
        // which keys those were, and the O(registry) diff below is skipped
        // (#9073).
        let mut touched_keys: Vec<Symbol> = Vec::new();
        let installed = {
            let registry = self.registry();
            registry
                .fn_transitions
                .keys_installed_since(functions.version(), registry.functions.version())
        };
        if let Some(keys) = installed {
            touched_keys = keys;
        } else {
            let registry = self.registry();
            for key in registry.functions.keys() {
                if !functions.contains_key(key) {
                    touched_keys.push(*key);
                }
            }
            for key in functions.keys() {
                if !registry.functions.contains_key(key) {
                    touched_keys.push(*key);
                }
            }
        }
        // A routine declared `is export` inside a routine of the module being
        // loaded escapes that routine through the module's EXPORT stash:
        // Rakudo exports it at compile time, whether or not (and however
        // often) the enclosing routine runs. So one this scope newly
        // registered -- e.g. a `multi trait_mod:<is>` declared inside
        // `sub EXPORT` (the `Exportable` idiom) -- and its `EXPORT::<tag>`
        // mirror keys survive the restore. Only the loading module's own
        // exports: a module `require`d inside a block is still lexical to it.
        let owned_exports = if is_eval {
            None
        } else {
            self.module
                .module_load_stack
                .last()
                .and_then(|m| self.module.module_owned_exports.get(m))
        };
        let new_exported: Vec<(Symbol, std::sync::Arc<FunctionDef>)> = match owned_exports {
            None => Vec::new(),
            Some(owned) => {
                let registry = self.registry();
                touched_keys
                    .iter()
                    .filter(|key| !functions.contains_key(key))
                    .filter_map(|key| registry.functions.get(key).map(|def| (*key, def.clone())))
                    .filter(|(_, def)| owned.contains_key(&def.name.resolve()))
                    .collect()
            }
        };
        let module_protos = self.module_protos_missing_from(&proto_functions);
        let mut guard = self.registry_mut();
        // Every write below is to the routine/token tables, never to the four
        // type maps, so the lexical type-name index survives the restore.
        let registry = guard.routine_tables_mut();
        registry.functions = functions;
        registry.proto_functions = proto_functions;
        registry.token_defs = token_defs;
        registry.proto_subs_restore(proto_subs);
        for (key, def) in module_protos {
            registry.proto_subs_insert(key.resolve());
            std::sync::Arc::make_mut(&mut registry.proto_functions).insert(key, def);
        }
        registry.proto_tokens = proto_tokens;
        for (key, defs) in new_tokens {
            std::sync::Arc::make_mut(&mut registry.token_defs).insert(key, defs);
        }
        for pt in new_proto_tokens {
            std::sync::Arc::make_mut(&mut registry.proto_tokens).insert(pt);
        }
        // token_defs was rewritten wholesale: invalidate regex parses that may
        // have folded token content in. Putting back the table that is still
        // installed rewrites nothing.
        if !tokens_unchanged {
            crate::runtime::regex_parse::TOKEN_DEFS_GEN
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        }
        // token_defs holds grammar token/rule bodies, which are methods (ADR-0019
        // Phase E). Bump the canonical method generation unconditionally, exactly
        // as unconditionally as the TOKEN_DEFS_GEN bump above, so every
        // generation-keyed method cache (fast_method_cache, method_resolve_cache,
        // native_ctor_plan_cache, multi_resolve_cache, resolved_seq_cache,
        // private_zeroarg_method_cache, ...) self-invalidates through its own
        // read-site refresh instead of depending on the separate eager clear below.
        registry.bump_method_generation();
        // Re-apply only newly added our-scoped functions so they survive block scope exit.
        // In EVAL context, our-scoped functions should NOT leak into the outer lexical
        // scope — they remain accessible only via OUR:: pseudo-package resolution.
        if !is_eval {
            for (key, def) in new_our.iter().map(|(k, d)| (*k, d.clone())) {
                // Re-inserted AFTER the diff above, and sourced from
                // `our_scoped_functions` rather than from either diffed map, so
                // this key may be in neither — name it explicitly or its base
                // name keeps a stale index entry.
                touched_keys.push(key);
                registry.functions_mut().insert(key, def);
            }
        }
        for (key, def) in new_exported {
            registry.functions_mut().insert(key, def);
        }
        drop(guard);
        // The routine registry just changed: a lexical (`my sub`) registered
        // inside the block was removed. Bump the function-namespace generation
        // so a subsequent call to that name re-resolves against the restored
        // registry rather than returning the now-out-of-scope CompiledFunction
        // the first in-block call cached (the compiled-function map is
        // program-global and still contains the lexical sub's body). The
        // method-namespace caches are already covered by the
        // `bump_method_generation()` call above (ADR-0019 F5) -- this used to
        // be a single unconditional `invalidate_method_dispatch_caches()` call
        // that cleared both namespaces' caches directly; splitting it into two
        // generation bumps is behavior-preserving by construction (every cache
        // `invalidate_method_dispatch_caches()` used to clear directly now
        // self-refreshes off one of these two counters at its own read site,
        // per the same by-construction reasoning as F5's `exec_register_sub_op`
        // cutover), not a corpus-sampled cutover.
        //
        // `reinstate_module_functions` mutates the snapshot map before the diff
        // runs, so its keys are already accounted for; the our-scoped
        // re-inserts happen after it and add themselves to `touched_keys` at
        // their own site.
        self.invalidate_fn_resolution_for_keys(touched_keys);
    }

    pub(crate) fn block_scope_depth(&self) -> usize {
        self.block_scope_depth
    }

    // Cost: O(1).
    pub(crate) fn inside_routine(&self) -> bool {
        !self.routine_stack.is_empty()
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
        //
        // Not for a dynamic scalar (`temp $*OUT = $fh`): what it temporizes is
        // the binding, and the restore must put back the very handle object
        // that was bound — a copy would detach it from every other holder (the
        // `$orig` a capture helper restores later, the process stash).
        let value = if name.starts_with('*') {
            value
        } else {
            value.into_temp_snapshot()
        };
        self.control.let_saves.push(super::LetSaveEntry {
            name,
            value,
            is_temp,
            slot,
            elem: None,
        });
    }

    /// Push an element save: `value` is what `container[key]` held, restored
    /// into that element in place at scope exit (see [`super::LetSaveEntry::elem`]).
    pub(crate) fn let_saves_push_elem(
        &mut self,
        container: Value,
        key: Value,
        value: Value,
        is_temp: bool,
    ) {
        self.control.let_saves.push(super::LetSaveEntry {
            name: String::new(),
            value: value.into_temp_snapshot(),
            is_temp,
            slot: None,
            elem: Some((container, key)),
        });
    }

    /// Restore one save: an element save writes its element, a variable save
    /// its variable.
    fn restore_let_entry(&mut self, save: super::LetSaveEntry) {
        match save.elem {
            Some((container, key)) => {
                // The element existed when it was saved (or was vivified by the
                // temporizing assignment), so this write cannot need to grow
                // anything the assignment did not already grow.
                let _ = self.assign_into_computed_target(&container, &key, save.value);
            }
            None => {
                let restored = self.resolve_restore_value(&save.name, &save.value);
                self.restore_let_value(save.name, restored, save.slot);
            }
        }
    }

    /// Restore one `let`/`temp` save. For an instance, write the saved
    /// attributes back into the *live* shared cell of the variable's current
    /// binding so every alias sees the restoration and object identity (id +
    /// cell) is preserved, rather than rebinding the name to a detached copy.
    fn restore_let_value(&mut self, name: String, restored: Value, slot: Option<u32>) {
        // `temp $*OUT = ...` over the process binding temporized the process
        // stash (ADR-11318): restore it there too. Ahead of the `ContainerRef`
        // branch below, which returns without reaching the by-name writer.
        if name.starts_with('*') {
            self.publish_process_dynamic_write(&name, &restored);
        }
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
        // An ATTRIBUTE's source of truth is `self`'s shared attribute cell, not
        // the method frame's mirror slot: `exec_get_local_op` re-reads the cell on
        // every `$!x`/`%!r`, so restoring only the slot and `env` left the
        // temporized value standing (`{ temp $!x = 2 }; say $!x` answered 2 where
        // rakudo answers 1, and `self.x` agreed with the wrong one). Write the
        // cell too; the helper is a no-op when `self` is not an instance or the
        // name is not an attribute, so no other `temp`/`let` target pays for it.
        if crate::value::attr_twigil_base(&name).is_some() {
            self.write_self_attr_cell(&name, restored.clone());
        }
        // Built-in dynamic scalars have two spellings in the environment:
        // `$*OUT` is the output-routing key while `*OUT` is the sigil-less key
        // generated by an lvalue such as `temp $*OUT = $fh`.  A normal store
        // keeps those aliases together through `set_env_with_main_alias`; doing
        // a raw restore here used to repair only `*OUT`.  Closing `$fh` after
        // its dynamic scope then left `say` writing through the stale `$*OUT`
        // alias.  Restore dynamic bindings through the ordinary writer too.
        if name.starts_with('*') {
            self.set_env_with_main_alias(&name, restored);
            return;
        }
        if let ValueView::Instance {
            attributes: saved_attrs,
            id: saved_id,
            ..
        } = restored.view()
        {
            // The current binding for `name` holds the live instance (the same
            // shared cell that was mutated during the dynamic scope). Write the
            // saved snapshot straight back into that cell; the env binding already
            // aliases it, so no re-insert is needed. Only when it IS the saved
            // object: `temp $x = Other.new` rebinds `$x`, and writing the saved
            // attributes into the other object would rewrite every alias of it
            // (`$kept = $x` taken inside the scope).
            // `self` is the frame record's invocant, not an env key
            // (ADR-14843).
            let live = if name == "self" {
                self.frame_invocant_ref().cloned()
            } else {
                self.env.get(&name).cloned()
            };
            if let Some(v) = live
                && let ValueView::Instance {
                    attributes: live_attrs,
                    id: live_id,
                    ..
                } = v.view()
                && live_id == saved_id
            {
                live_attrs.commit_attrs(saved_attrs.to_map());
                return;
            }
            if !self.store_invocant_by_name(&name, &restored) {
                self.env.insert(name, restored);
            }
        } else if !self.store_invocant_by_name(&name, &restored) {
            self.env.insert(name, restored);
        }
    }

    /// Current length of let_saves stack (used as a mark).
    pub(crate) fn let_saves_len(&self) -> usize {
        self.control.let_saves.len()
    }

    /// Discard `let_saves` entries pushed since `mark`, for panic-unwind
    /// recovery (`Interpreter::recover_call_frames_after_panic`).
    ///
    /// Deliberately a bare truncate, NOT [`Self::restore_let_saves`]: that
    /// method writes each saved old value back into `self.env()`, but by the
    /// time panic recovery runs, `self.env()` has already been reset to the
    /// boundary's caller env (via the `call_frames` pop-loop) -- writing a
    /// panicking callee's stale `temp`/`let` values into it would corrupt the
    /// now-restored caller's env instead of cleaning it up. Just dropping the
    /// bookkeeping entries is correct: the frame whose env they were meant to
    /// restore no longer exists. See
    /// `todo/deep/panic-unwind-leaks-side-channel-call-state.md`.
    pub(crate) fn truncate_let_saves(&mut self, mark: usize) {
        self.control
            .let_saves
            .truncate(mark.min(self.control.let_saves.len()));
    }

    /// Resolve the value to restore, applying `is default(...)` when restoring Nil.
    fn resolve_restore_value(&self, name: &str, val: &Value) -> Value {
        if val.is_nil()
            && let Some(default) = self.var_default(name)
        {
            return default.clone();
        }
        val.clone()
    }

    /// Restore all variables from let_saves starting at `mark`, then truncate.
    pub(crate) fn restore_let_saves(&mut self, mark: usize) {
        for i in (mark..self.control.let_saves.len()).rev() {
            let save = self.control.let_saves[i].clone();
            self.restore_let_entry(save);
        }
        self.control.let_saves.truncate(mark);
    }

    /// On successful block exit: restore `temp` saves, discard `let` saves.
    /// For `let`, only restore if the block returned an unsuccessful value.
    /// The empty-range guard is inlined and the body is not: a routine with no
    /// `let`/`temp` in scope -- the overwhelmingly common case on the hot call
    /// path -- must not pay a call plus a `Vec` construction to discover it has
    /// nothing to restore (1.3% of `bench-fib`'s self time).
    #[inline]
    pub(crate) fn resolve_let_saves_on_success(&mut self, mark: usize, success: bool) {
        if mark >= self.control.let_saves.len() {
            self.control.let_saves.truncate(mark);
            return;
        }
        self.resolve_let_saves_on_success_slow(mark, success);
    }

    #[inline(never)]
    fn resolve_let_saves_on_success_slow(&mut self, mark: usize, success: bool) {
        // Collect restore actions first to avoid borrow conflicts.
        let restores: Vec<super::LetSaveEntry> = (mark..self.control.let_saves.len())
            .rev()
            .filter(|&i| self.control.let_saves[i].is_temp || !success)
            .map(|i| self.control.let_saves[i].clone())
            .collect();
        for save in restores {
            self.restore_let_entry(save);
        }
        self.control.let_saves.truncate(mark);
    }

    /// Discard let_saves from `mark` without restoring (block succeeded).
    pub(crate) fn discard_let_saves(&mut self, mark: usize) {
        self.control.let_saves.truncate(mark);
    }
}

/// Whether `key` ends in `::<name>` for some `name` in `names`: equal to asking
/// whether a suffix that starts right after one of the key's `::` separators is
/// one of the names, which is one set probe per separator instead of one
/// `format!` + `ends_with` per name (a program that `need`s a hundred classes
/// built a hundred strings for every env key a stash read looked at).
// Cost: O(m + s * m), m = bytes of the key, s = `::` positions in it (one probe each).
fn key_ends_with_any_as_qualified_tail(key: &str, names: &std::collections::HashSet<String>) -> bool {
    let bytes = key.as_bytes();
    // Every position, so `A:::B` offers `B` as the `ends_with` form does.
    // `i + 2` follows two ASCII colons, so it is a char boundary.
    (0..bytes.len().saturating_sub(1))
        .any(|i| bytes[i] == b':' && bytes[i + 1] == b':' && names.contains(&key[i + 2..]))
}

#[cfg(test)]
mod tests {
    use super::key_ends_with_any_as_qualified_tail;
    use std::collections::HashSet;

    /// The per-name `format!` + `ends_with` form this replaced.
    fn reference(key: &str, names: &HashSet<String>) -> bool {
        names.iter().any(|name| key.ends_with(&format!("::{name}")))
    }

    #[test]
    fn matches_the_format_and_ends_with_form() {
        let names: HashSet<String> = ["Foo", "Bar::Baz", "B", "x"]
            .iter()
            .map(|s| s.to_string())
            .collect();
        for key in [
            "", ":", "::", "Foo", "A::Foo", "A::B::Foo", "Foo::A", "A::Bar::Baz", "Bar::Baz",
            "Q::Bar::Baz", "A:::B", "A::::B", "A::b", "::Foo", "X::Y::x", "naïve::Foo",
            "A::Foo::", "A::\u{1F600}", "$A::Foo", "GLOBAL::Foo",
        ] {
            assert_eq!(
                key_ends_with_any_as_qualified_tail(key, &names),
                reference(key, &names),
                "key {key:?}"
            );
        }
    }
}
