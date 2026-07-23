use super::*;

impl Interpreter {
    /// Create a lightweight clone of this interpreter for use in a spawned thread.
    /// Shares function/class/role/enum definitions but starts with fresh output and test state.
    /// Array (`@`) and scalar (`$`) variables are shared between parent and child via `shared_vars`
    /// so that mutations are visible across threads.
    pub(crate) fn clone_for_thread(&mut self) -> Self {
        // Collapse a scoped (multi-tier overlay) env to a flat one first: the
        // shared-var seeding and the child's env clone below iterate the env
        // overlay-only, which would miss parent-chain lexicals on a scoped env.
        if self.env.is_scoped() {
            self.env = self.env.flattened();
        }
        // Copy user variables into shared_vars so both parent and child see mutations.
        // The compiler stores locals with bare names (no sigil), so we share everything
        // except internal/special variables that should remain thread-local.
        // ADR-0010: seed into THIS lineage's store. The parent's lexicals belong
        // to the parent's lineage; the child (created below) chains to it, so it
        // sees them and its writes resolve back here. A sibling thread seeds into
        // its OWN store, which is why two hyper workers that each declare
        // `my $uri` no longer collide on one bare-name entry.
        let shared = Arc::clone(&self.shared_vars);
        {
            for (key, val) in &self.env {
                // Skip internal variables and topic variables.
                // Also skip $*CWD/*CWD — in Raku, dynamic variables like $*CWD
                // are thread-local; mutations inside `start` blocks must not
                // propagate back to the parent thread.
                if key == "_"
                    || key == "@_"
                    || key == "%_"
                    || key == "/"
                    || key == "!"
                    || key == "$/"
                    || key == "$!"
                    || key == "$*CWD"
                    || key == "*CWD"
                    || key.starts_with("__mutsu_")
                    || key.starts_with("&")
                    || key == "?LINE"
                {
                    continue;
                }
                // Only seed if not already visible — an entry an earlier thread
                // already updated must not be reset to this env's copy.
                // EXCEPT names this thread re-declared: their entry (if any)
                // belongs to the shadowed outer binding, so bind the current one
                // into this lineage, shadowing the ancestor's.
                let key = key.resolve();
                if self.thread_redeclared_vars.contains(&key) {
                    shared.declare(&key, val.clone());
                } else {
                    shared.seed_if_absent(&key, || val.clone());
                }
            }
            // Track C: migrate the parent's existing `state` variables into shared
            // cells (keyed by their normalized cross-compilation key) so a routine
            // whose `state` was already mutated before the first thread spawned
            // (`f(); f(); start { f() }`) carries that value into the threads
            // instead of re-initializing from the declaration. Only seeds cells
            // that don't exist yet; the value becomes the cell's initial content.
            for (skey, sval) in &self.state_vars {
                if matches!(sval.view(), ValueView::ContainerRef(_)) {
                    continue;
                }
                let shared_key =
                    format!("__mutsu_shared_state::{}", Self::normalize_state_key(skey));
                shared.seed_if_absent(&shared_key, || sval.clone().into_container_ref());
            }
        }
        self.shared_vars_active = true;
        // The child captures the parent's CURRENT bindings — including any
        // name the parent re-declared since an earlier spawn (its current
        // value was force-seeded above). From this spawn on, writes to those
        // names must flow both ways again, so drop the parent-side masks.
        // (The child's own mask starts empty via the struct literal below.)
        self.thread_redeclared_vars.clear();
        let mut referenced_handle_ids = std::collections::HashSet::new();
        for value in self.env.values() {
            if let Some(id) = Self::handle_id_from_value(value) {
                referenced_handle_ids.insert(id);
            }
        }
        let mut cloned_handles = HashMap::new();
        let handles_guard = self.io_handles();
        for (id, handle) in &handles_guard.map {
            if handle.closed || !referenced_handle_ids.contains(id) {
                continue;
            }
            let cloned = IoHandleState {
                target: handle.target,
                mode: handle.mode,
                path: handle.path.clone(),
                line_separators: handle.line_separators.clone(),
                line_chomp: handle.line_chomp,
                encoding: handle.encoding.clone(),
                file: handle.file.as_ref().and_then(|f| f.try_clone().ok()),
                socket: handle.socket.as_ref().and_then(|s| s.try_clone().ok()),
                listener: handle.listener.as_ref().and_then(|l| l.try_clone().ok()),
                closed: handle.closed,
                out_buffer_capacity: handle.out_buffer_capacity,
                out_buffer_pending: handle.out_buffer_pending.clone(),
                bin: handle.bin,
                nl_out: handle.nl_out.clone(),
                bytes_written: handle.bytes_written,
                read_attempted: handle.read_attempted,
                utf16_bom_written: handle.utf16_bom_written,
                utf16_detected_be: handle.utf16_detected_be,
                argfiles_index: handle.argfiles_index,
                argfiles_reader: None, // Cannot clone BufReader; will reopen if needed
                argfiles_paths: handle.argfiles_paths.clone(),
                pending_words: handle.pending_words.clone(),
                close_on_word_exhaust: handle.close_on_word_exhaust,
            };
            cloned_handles.insert(*id, cloned);
        }
        let cloned_next_handle_id = handles_guard.next_id;
        drop(handles_guard);
        // Thread clones write through the parent's shared stdout/stderr buffers
        // so concurrent output interleaves in real chronological order.
        // A thread spawned *inside* a subtest must keep buffering its output into
        // `shared_thread_output`: its TAP lines are subtest-internal and are drained
        // (indented) into the subtest, not flushed raw to top-level stdout. The
        // thread clone's own `tap` resets `subtest_depth` to 0, so an immediate
        // flush from such a clone would leak the subtest-internal line to the real
        // top-level stream ("tests out of sequence"). Only threads spawned at top
        // level (no active subtest) may flush immediately.
        let parent_in_subtest = self.tap.subtest_depth() != 0;
        let thread_output_sink = {
            let mut parent_sink = self.output_sink_mut();
            // When the parent flushes stdout immediately (CLI / REPL mode) and the
            // thread is spawned at top level, the clone must do the same so its
            // `say`/`pass` output lands in real chronological order relative to the
            // main thread's direct writes. Otherwise the clone buffers into
            // `shared_thread_output` and is only drained at the next sync point
            // (`await` / `.result`), which lands a worker-thread test line *after*
            // an intervening main-thread one — producing TAP "tests out of
            // sequence". In buffered/capture mode (parent `immediate_stdout ==
            // false`) the shared buffer is still used, so `run()` capture is
            // unaffected.
            let parent_immediate = parent_sink.immediate_stdout && !parent_in_subtest;
            let shared_out = Arc::clone(
                parent_sink
                    .shared_thread_output
                    .get_or_insert_with(|| Arc::new(Mutex::new(String::new()))),
            );
            let shared_err = Arc::clone(
                parent_sink
                    .shared_thread_stderr
                    .get_or_insert_with(|| Arc::new(Mutex::new(String::new()))),
            );
            Arc::new(RwLock::new(OutputSink {
                output: String::new(),
                stderr_output: String::new(),
                output_emitted: false,
                immediate_stdout: parent_immediate,
                is_thread_clone: true,
                shared_thread_output: Some(shared_out),
                shared_thread_stderr: Some(shared_err),
            }))
        };
        let mut cloned = Self {
            env: self.env.clone(),
            output_sink: thread_output_sink,
            warn_output: String::new(),
            warn_suppression_depth: 0,
            tap: self.tap.clone_for_thread(),
            halted: false,
            exit_code: 0,
            main_hidden_from_usage: self.main_hidden_from_usage.clone(),
            explicit_run_main: self.explicit_run_main,
            nested_mode: self.nested_mode,
            native_call_specs: self.native_call_specs.clone(),
            operator_assoc: self.operator_assoc.clone(),
            imported_operator_names: self.imported_operator_names.clone(),
            user_declared_infix_ops: self.user_declared_infix_ops.clone(),
            lib_paths: self.lib_paths.clone(),
            bundled_lib_paths: self.bundled_lib_paths.clone(),
            io_handles: Arc::new(RwLock::new(io_handles::IoHandleTable {
                map: cloned_handles,
                next_id: cloned_next_handle_id,
            })),
            program_path: self.program_path.clone(),
            // Snapshot (fresh lock), not a shared handle: thread-local registry
            // semantics — child sees a copy, writes don't leak to the parent.
            current_package: Arc::new(RwLock::new(self.current_package())),
            routine_stack: Vec::new(),
            callframe_stack: Vec::new(),
            method_class_stack: Vec::new(),
            constructing_class: None,
            defining_class: None,
            pending_call_arg_sources: None,
            pending_call_arg_source_slots: std::collections::HashMap::new(),
            pending_rw_writeback_slots: std::collections::HashMap::new(),
            test_pending_callsite_line: None,
            cur_source_line: 1,
            locals_pool: Vec::new(),
            control_handler_depth: 0,
            test_assertion_line_stack: Vec::new(),
            block_stack: Vec::new(),
            doc_comments: HashMap::new(),
            doc_comment_list: Vec::new(),
            why_cache: HashMap::new(),
            type_metadata: self.type_metadata.clone(),
            when_matched: false,
            gather_items: Vec::new(),
            gather_take_limits: Vec::new(),
            block_scope_depth: self.block_scope_depth,
            // Deep-clone the registry into a fresh Arc so the child thread gets an
            // independent snapshot (matches prior per-field clone semantics: the
            // child sees parent declarations but its own new ones don't leak back).
            registry: Arc::new(RwLock::new(self.registry.read().unwrap().clone())),
            // Fresh per-thread: the snapshot cache must not be shared across
            // threads (each thread's `Interpreter` owns its own registry).
            registry_write_gen: std::sync::atomic::AtomicU64::new(0),
            regex_registry_snapshot: Mutex::new(None),
            proto_dispatch_stack: Vec::new(),
            proto_method_skip: None,
            pending_dispatch_error: None,
            pending_dist_selectors: Vec::new(),
            pending_use_export_args: None,
            end_phasers: Vec::new(),
            end_phaser_sites: HashSet::new(),
            chroot_root: self.chroot_root.clone(),
            loaded_modules: self.loaded_modules.clone(),
            need_hidden_classes: self.need_hidden_classes.clone(),
            cur_repo: self.cur_repo.clone(),
            package_stash_hidden: self.package_stash_hidden.clone(),
            chain_declared_packages: self.chain_declared_packages.clone(),
            module_packages: self.module_packages.clone(),
            closure_env_overrides: self.closure_env_overrides.clone(),
            pending_eval_sigilless: Vec::new(),
            predictive_seq_iters: self.predictive_seq_iters.clone(),
            protect_block_cache: HashMap::new(),
            subset_predicate_cache: HashMap::new(),
            subset_where_fail: None,
            private_zeroarg_method_cache: HashMap::new(),
            module_load_stack: Vec::new(),
            current_distribution: self.current_distribution.clone(),
            package_distributions: self.package_distributions.clone(),
            exported_subs: self.exported_subs.clone(),
            exported_vars: self.exported_vars.clone(),
            exported_sub_values: self.exported_sub_values.clone(),
            unit_module_exported_subs: self.unit_module_exported_subs.clone(),
            unit_module_loading_stack: Vec::new(),
            module_owned_exports: self.module_owned_exports.clone(),
            suppress_exports: false,
            in_lvalue_assignment: false,
            in_does_rhs: false,
            trait_mod_writeback_key: None,
            trait_mod_writeback_value: None,
            hash_autovivify: false,
            newline_mode: self.newline_mode,
            import_scope_stack: Vec::new(),
            strict_mode: self.strict_mode,
            fatal_mode: self.fatal_mode,
            our_vars: HashMap::new(),
            package_lexicals: self.package_lexicals.clone(),
            class_body_static_names: self.class_body_static_names.clone(),
            escaped_our_lexical_cells: self.escaped_our_lexical_cells.clone(),
            escaping_our_lexical_names: self.escaping_our_lexical_names.clone(),
            escaped_our_sub_names: self.escaped_our_sub_names.clone(),
            state_vars: HashMap::new(),
            thread_redeclared_vars: std::collections::HashSet::new(),
            // Mirror state_vars: a thread clone starts with no persisted
            // closure captured state (falls back to the captured-env initial
            // values), exactly as before this store existed.
            closure_captured_state: HashMap::new(),
            // Share the store by handle (not a per-thread copy) so a `once` in a
            // sub run from several `start` blocks fires once across all threads.
            once_values: Arc::clone(&self.once_values),
            once_scope_stack: Vec::new(),
            next_once_scope_id: self.next_once_scope_id,
            var_dynamic_flags: self.var_dynamic_flags.clone(),
            caller_env_stack: Vec::new(),
            var_bindings: HashMap::new(),
            variables_pragma: self.variables_pragma.clone(),
            attributes_pragma: self.attributes_pragma.clone(),
            var_type_constraints: self.var_type_constraints.clone(),
            // Inherit monotonically: if the parent ever registered an atomic var,
            // the child (which shares the atomic storage via shared_vars) must keep
            // running the atomic-variable read check.
            // Inherit monotonically: the parent's env-scoped constraints are copied
            // into the child's env, so the child must keep consulting env-first.
            env_type_constraint_seen: self.env_type_constraint_seen,
            // Inherit monotonically: the parent's sigilless-alias env keys are
            // copied into the child env, so the child must keep walking the chain.
            atomic_var_seen: self.atomic_var_seen,
            sigilless_alias_seen: self.sigilless_alias_seen,
            var_defaults: self.var_defaults.clone(),
            var_hash_key_constraints: self.var_hash_key_constraints.clone(),
            // Per-thread snapshot (not a shared-handle clone): deep-copy the map
            // into a fresh `Arc` so the child thread's instance type metadata is
            // independent of the parent's, mirroring `io_handles`/`current_package`.
            instance_type_metadata: Arc::new(RwLock::new(
                self.instance_type_metadata.read().unwrap().clone(),
            )),
            let_saves: Vec::new(),
            supply_emit_buffer: Vec::new(),
            supply_emit_timed_buffer: Vec::new(),
            supply_stream_consumers: Vec::new(),
            react_active: 0,
            pending_tap_closes: Vec::new(),
            current_react_waker: None,
            // ADR-0010: a child lineage, not a share of one process-wide map.
            shared_vars: crate::runtime::shared_store::SharedStore::child_of(&self.shared_vars),
            shared_vars_active: true,
            sigilless_attrs_active: self.sigilless_attrs_active,
            shared_vars_dirty: Arc::clone(&self.shared_vars_dirty),
            shared_critical_dirty: Arc::clone(&self.shared_critical_dirty),
            critical_section_depth: 0,
            encoding_registry: self.encoding_registry.clone(),
            skip_pseudo_method_native: None,
            dispatch_ambiguous: false,
            rakuseen_active: Vec::new(),
            rakuseen_cycle_hit: std::collections::HashSet::new(),
            raku_leaf_active: Vec::new(),
            raku_leaf_cycle_hit: std::collections::HashSet::new(),
            pending_proxy_subclass_attr: None,
            multi_dispatch_stack: Vec::new(),
            method_dispatch_stack: Vec::new(),
            samewith_context_stack: Vec::new(),
            metamodel_dispatch_stack: Vec::new(),
            wrap_chains: self.wrap_chains.clone(),
            wrap_sub_names: self.wrap_sub_names.clone(),
            wrap_name_to_sub: self.wrap_name_to_sub.clone(),
            wrap_callable_ids: self.wrap_callable_ids.clone(),
            wrap_handle_counter: self.wrap_handle_counter,
            wrap_dispatch_stack: Vec::new(),
            wrap_skip_once: None,
            method_wrap_chains: self.method_wrap_chains.clone(),
            method_fallbacks: self.method_fallbacks.clone(),
            suppressed_names: self.suppressed_names.clone(),
            poisoned_enum_aliases: self.poisoned_enum_aliases.clone(),
            enum_scope_names: self.enum_scope_names.clone(),
            my_scoped_package_items: self.my_scoped_package_items.clone(),
            lexical_class_scopes: self.lexical_class_scopes.clone(),
            lexical_class_sites: self.lexical_class_sites.clone(),
            lexical_class_owner_scopes: self.lexical_class_owner_scopes.clone(),
            last_value: None,
            pending_local_updates: Vec::new(),
            readonly_vars: crate::runtime::ReadonlySet::default(),
            readonly_undo: Vec::new(),
            readonly_frames: 0,
            squish_iterator_meta: HashMap::new(),
            custom_type_data: self.custom_type_data.clone(),
            rebless_map: self.rebless_map.clone(),
            action_made: None,
            current_grammar_actions: None,
            pending_regex_error: None,
            precomp_enabled: self.precomp_enabled,
            monkey_typing: self.monkey_typing,
            json_import_defaults: self.json_import_defaults,

            // Merged VM execution registers (CP-3 collapse): a thread clone starts
            // with fresh per-execution registers, exactly as the former
            // `VM::new(thread_interp)` did for a spawned thread.
            stack: Vec::new(),
            locals: Vec::new(),
            upvalues: Vec::new(),
            frame_authoritative: Vec::new(),
            in_smartmatch_rhs: false,
            transliterate_in_smartmatch: false,
            substitution_in_smartmatch: false,
            last_topic_value: None,
            topic_save_stack: Vec::new(),
            topic_source_save_stack: Vec::new(),
            container_ref_var: None,
            container_ref_reversed: false,
            topic_source_var: None,
            topic_container_source: None,
            element_source: None,
            quanthash_bind_params: Vec::new(),
            for_param_restore_stack: Vec::new(),
            call_frames: Vec::new(),
            control_handlers: Vec::new(),
            current_code: 0,
            carrier_writes: None,
            method_dispatch_pure: false,
            in_regex_code_block: false,
            resume_ip: None,
            jit_error: None,
            bind_context: false,
            scalar_bind_context: false,
            bound_decont_active: false,
            rebind_context: false,
            accessor_ref_pending: false,
            constant_context: false,
            array_share_context: false,
            array_share_source: None,
            array_share_active: false,
            element_share_pending: false,
            explicit_initializer_context: false,
            vardecl_context: false,
            shaped_decl_context: false,
            pending_rw_writeback_sources: Vec::new(),
            pending_caller_var_writeback: Vec::new(),
            local_bind_pairs: Vec::new(),
            otf_compile_cache: HashMap::new(),
            // Share the parent's captured module-sub bodies by value so a `start`
            // block that calls a module sub with `state` reaches the same compiled
            // body (and thus the same cross-thread `state` cell) the parent used.
            imported_compiled_fns: self.imported_compiled_fns.clone(),
            state_scope_id: None,
            fn_resolve_cache: Default::default(),
            fn_resolve_gen: 0,
            fn_resolve_cache_gen: 0,
            multi_candidates_cache: Default::default(),
            multi_candidates_cache_gen: 0,
            light_call_cache: Default::default(),
            light_call_cache_gen: 0,
            pos_light_call_cache: Default::default(),
            pos_light_call_cache_gen: 0,
            amp_param_shadowed_names: std::collections::HashSet::new(),
            empty_sig_proto_names: std::collections::HashSet::new(),
            registered_fn_fingerprints: Default::default(),
            prepared_fn_defs: HashMap::new(),
            method_resolve_cache: rustc_hash::FxHashMap::default(),
            last_method_resolve: None,
            fast_method_cache: rustc_hash::FxHashMap::default(),
            native_ctor_plan_cache: rustc_hash::FxHashMap::default(),
            multi_resolve_cache: rustc_hash::FxHashMap::default(),
            multi_type_cacheable: rustc_hash::FxHashMap::default(),
            dispatch_multi_candidate: rustc_hash::FxHashMap::default(),
            multi_alternate_signature_names: self.multi_alternate_signature_names.clone(),
            method_body_fp_cache: rustc_hash::FxHashMap::default(),
            func_def_fp_cache: rustc_hash::FxHashMap::default(),
            func_multi_resolve_cache: rustc_hash::FxHashMap::default(),
            func_multi_type_cacheable: rustc_hash::FxHashMap::default(),
            user_declared_classes: self.user_declared_classes.clone(),
            block_declared_vars: Vec::new(),
            loop_local_vars: Vec::new(),
            loop_local_saved_env: Vec::new(),
            loop_cond_active: false,
            outer_scope_locals: Vec::new(),
            enter_result_stack: Vec::new(),
            pending_alias_bind_names: Vec::new(),
            otf_call_cache: Default::default(),
            otf_call_cache_gen: 0,
            check_phaser_depth: 0,
            gather_for_loop_resume: None,
            gather_resume_body_ip: None,
            gather_suspend_pending: false,
            lazy_take_boundary_defer: false,
            rw_map_topic_capture: None,
        };
        // Raku gives each start block fresh $/ and $! (they are lexically scoped).
        cloned.env.insert("/".to_string(), Value::NIL);
        cloned.env.insert("!".to_string(), Value::NIL);
        cloned.env.insert("$/".to_string(), Value::NIL);
        cloned.env.insert("$!".to_string(), Value::NIL);
        cloned.init_io_environment();
        cloned
    }

    /// Push values into a shared array variable in-place, avoiding full-array
    /// clones on every push.  When the variable lives in `shared_vars` the
    /// lock is held for the entire read-modify-write so concurrent pushes are
    /// safe and O(1) amortised instead of O(n).
    pub(crate) fn push_to_shared_var(
        &mut self,
        key: &str,
        mut values: Vec<Value>,
        target_fallback: &Value,
    ) -> Value {
        // A plain lexical `@name` already present in the shared store routes
        // through the atomic store (see `push_to_existing_shared_array`); when
        // absent it is thread-local and falls through to the env path below.
        if key.starts_with('@') && self.shared_vars_active && Self::is_plain_lexical_array_name(key)
        {
            let in_shared = {
                let atomic_key = format!("__mutsu_atomic_arr::{key}");
                self.shared_vars
                    .get(&atomic_key)
                    .is_some_and(|v| matches!(v.view(), ValueView::Array(..)))
                    || self
                        .shared_vars
                        .get(key)
                        .is_some_and(|v| matches!(v.view(), ValueView::Array(..)))
            };
            if in_shared {
                return self.shared_array_extend(key, values, false);
            }
        } else if key.starts_with('@') && self.shared_vars_active {
            // Attribute / twigil'd arrays keep the base-key in-place path
            // (per-instance identity — see `push_to_existing_shared_array`).
            // Drop env's copy of the Arc first so that shared_vars holds
            // the only strong reference (refcount=1). This keeps repeated
            // shared pushes in-place instead of degenerating into O(n²) COW.
            self.env.remove(key);
            let is_thread_clone = self.is_thread_clone();
            // In-place read-modify-write under the owning lineage's lock, so
            // concurrent pushes stay safe and O(1) amortised instead of O(n).
            // Lend the values to the in-place attempt and take them back if the
            // closure never ran (the branch below can still fall through).
            let mut pending = Some(std::mem::take(&mut values));
            let in_place = self
                .shared_vars
                .with_entry_mut(key, |v| {
                    v.with_array_mut(|arc_items, kind| {
                        let items = crate::gc::Gc::make_mut(arc_items);
                        items.extend(pending.take().unwrap_or_default());
                        if *kind == ArrayKind::List {
                            *kind = ArrayKind::Array;
                        }
                        Value::array_with_kind(crate::gc::Gc::clone(arc_items), *kind)
                    })
                })
                .flatten();
            if let Some(result) = in_place {
                self.mark_shared_var_dirty(key);
                if !is_thread_clone {
                    self.env.insert(key.to_string(), result.clone());
                }
                return result;
            }
            // `with_array_mut` did not run (absent, or not an Array yet), so the
            // values were never consumed — take them back.
            values = pending.take().unwrap_or_default();
            // Fallback: the value might exist but not be an Array yet
            if let Some(shared_value) = self.shared_vars.get(key)
                && matches!(shared_value.view(), ValueView::Array(..))
            {
                {
                    let (mut arc_items, kind) = shared_value.into_array().unwrap();
                    let items = crate::gc::Gc::make_mut(&mut arc_items);
                    items.extend(values);
                    let normalized_kind = if kind == ArrayKind::List {
                        ArrayKind::Array
                    } else {
                        kind
                    };
                    let result =
                        Value::array_with_kind(crate::gc::Gc::clone(&arc_items), normalized_kind);
                    self.shared_vars
                        .set(key, Value::array_with_kind(arc_items, normalized_kind));
                    self.mark_shared_var_dirty(key);
                    if !is_thread_clone {
                        self.env.insert(key.to_string(), result.clone());
                    }
                    return result;
                }
            }
        }
        // Fallback for non-shared arrays: write through the shared node so
        // same-thread by-value holders observe the push (container identity §3).
        if matches!(
            self.env.get(key).map(Value::view),
            Some(ValueView::Array(..))
        ) {
            return self
                .env
                .get_mut(key)
                .unwrap()
                .with_array_mut(|arc_items, kind| {
                    let items = crate::value::gc_data_mut(arc_items);
                    items.extend(values);
                    // Normalize @-variables only from List to Array while preserving Shaped.
                    if key.starts_with('@') && *kind == ArrayKind::List {
                        *kind = ArrayKind::Array;
                    }
                    Value::array_with_kind(crate::gc::Gc::clone(arc_items), *kind)
                })
                .unwrap();
        }
        let mut items = match target_fallback.view() {
            ValueView::Array(v, ..) => v.to_vec(),
            _ => Vec::new(),
        };
        items.extend(values);
        let result = Value::real_array(items);
        self.env.insert(key.to_string(), result.clone());
        result
    }

    pub(crate) fn push_to_existing_shared_array(
        &mut self,
        key: &str,
        values: Vec<Value>,
    ) -> Option<Value> {
        if !key.starts_with('@') || !self.shared_vars_active {
            return None;
        }
        // A plain lexical `@name` routes through the `__mutsu_atomic_arr::`
        // store (single authoritative container; `set_shared_var` refuses to
        // clobber it with a stale parent snapshot — the t/lock.t lost-push
        // race lived here: this helper used to mutate the *base* key, which a
        // parent-thread env sync could wipe wholesale). "Existing" contract:
        // only handle a var already present in the shared store.
        if Self::is_plain_lexical_array_name(key) {
            let in_shared = {
                let atomic_key = format!("__mutsu_atomic_arr::{key}");
                self.shared_vars
                    .get(&atomic_key)
                    .is_some_and(|v| matches!(v.view(), ValueView::Array(..)))
                    || self
                        .shared_vars
                        .get(key)
                        .is_some_and(|v| matches!(v.view(), ValueView::Array(..)))
            };
            if !in_shared {
                return None;
            }
            return Some(self.shared_array_extend(key, values, false));
        }
        // Attribute / twigil'd arrays have per-instance identity and must NOT
        // funnel into the name-keyed atomic store (roles-6e.t: every C1
        // instance's `@!order` would accumulate cross-object). They keep the
        // base-key in-place path, serialized by the shared_vars write lock.
        let is_thread_clone = self.is_thread_clone();
        if is_thread_clone {
            self.env.remove(key);
        }
        let result = self
            .shared_vars
            .with_entry_mut(key, |v| {
                v.with_array_mut(|arc_items, kind| {
                    let items = crate::gc::Gc::make_mut(arc_items);
                    items.extend(values);
                    if *kind == ArrayKind::List {
                        *kind = ArrayKind::Array;
                    }
                    Value::array_with_kind(crate::gc::Gc::clone(arc_items), *kind)
                })
            })
            .flatten()?;
        if is_thread_clone {
            let dirty_marker = format!("__mutsu_shared_dirty::{key}");
            if !self.env.contains_key(&dirty_marker) {
                self.mark_shared_var_dirty(key);
                self.env.insert(dirty_marker, Value::TRUE);
            }
        } else {
            self.mark_shared_var_dirty(key);
            self.env.insert(key.to_string(), result.clone());
        }
        Some(result)
    }
}
