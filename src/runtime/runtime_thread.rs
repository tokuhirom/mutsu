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
        let shared = Arc::clone(&self.shared_vars);
        {
            let mut sv = shared.write().unwrap();
            for (key, val) in &self.env {
                // Skip internal variables and topic variables.
                // Also skip $*CWD/*CWD — in Raku, dynamic variables like $*CWD
                // are thread-local; mutations inside `start` blocks must not
                // propagate back to the parent thread.
                if key == "_"
                    || key == "@_"
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
                // Only insert if not already present — existing values may have
                // been updated by earlier threads that are already running.
                sv.entry(key.resolve()).or_insert_with(|| val.clone());
            }
            // Track C: migrate the parent's existing `state` variables into shared
            // cells (keyed by their normalized cross-compilation key) so a routine
            // whose `state` was already mutated before the first thread spawned
            // (`f(); f(); start { f() }`) carries that value into the threads
            // instead of re-initializing from the declaration. Only seeds cells
            // that don't exist yet; the value becomes the cell's initial content.
            for (skey, sval) in &self.state_vars {
                if matches!(sval, Value::ContainerRef(_)) {
                    continue;
                }
                let shared_key =
                    format!("__mutsu_shared_state::{}", Self::normalize_state_key(skey));
                sv.entry(shared_key)
                    .or_insert_with(|| sval.clone().into_container_ref());
            }
        }
        self.shared_vars_active = true;
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
        let thread_output_sink = {
            let mut parent_sink = self.output_sink_mut();
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
                immediate_stdout: false,
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
            nested_mode: self.nested_mode,
            native_call_specs: self.native_call_specs.clone(),
            operator_assoc: self.operator_assoc.clone(),
            imported_operator_names: self.imported_operator_names.clone(),
            lib_paths: self.lib_paths.clone(),
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
            pending_call_arg_sources: None,
            test_pending_callsite_line: None,
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
            proto_dispatch_stack: Vec::new(),
            proto_method_skip: None,
            pending_dispatch_error: None,
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
            predictive_seq_iters: self.predictive_seq_iters.clone(),
            protect_block_cache: HashMap::new(),
            subset_predicate_cache: HashMap::new(),
            private_zeroarg_method_cache: HashMap::new(),
            module_load_stack: Vec::new(),
            current_distribution: self.current_distribution.clone(),
            package_distributions: self.package_distributions.clone(),
            exported_subs: self.exported_subs.clone(),
            exported_vars: self.exported_vars.clone(),
            exported_sub_values: self.exported_sub_values.clone(),
            unit_module_exported_subs: self.unit_module_exported_subs.clone(),
            unit_module_loading_stack: Vec::new(),
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
            state_vars: HashMap::new(),
            // Mirror state_vars: a thread clone starts with no persisted
            // closure captured state (falls back to the captured-env initial
            // values), exactly as before this store existed.
            closure_captured_state: HashMap::new(),
            once_values: self.once_values.clone(),
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
            atomic_var_seen: self.atomic_var_seen,
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
            shared_vars: Arc::clone(&self.shared_vars),
            shared_vars_active: true,
            sigilless_attrs_active: self.sigilless_attrs_active,
            shared_vars_dirty: Arc::clone(&self.shared_vars_dirty),
            encoding_registry: self.encoding_registry.clone(),
            skip_pseudo_method_native: None,
            dispatch_ambiguous: false,
            pending_proxy_subclass_attr: None,
            multi_dispatch_stack: Vec::new(),
            method_dispatch_stack: Vec::new(),
            samewith_context_stack: Vec::new(),
            wrap_chains: self.wrap_chains.clone(),
            wrap_sub_names: self.wrap_sub_names.clone(),
            wrap_name_to_sub: self.wrap_name_to_sub.clone(),
            wrap_callable_ids: self.wrap_callable_ids.clone(),
            wrap_handle_counter: self.wrap_handle_counter,
            wrap_dispatch_stack: Vec::new(),
            method_wrap_chains: self.method_wrap_chains.clone(),
            suppressed_names: self.suppressed_names.clone(),
            poisoned_enum_aliases: self.poisoned_enum_aliases.clone(),
            enum_scope_names: self.enum_scope_names.clone(),
            my_scoped_package_items: self.my_scoped_package_items.clone(),
            lexical_class_scopes: self.lexical_class_scopes.clone(),
            last_value: None,
            pending_local_updates: Vec::new(),
            readonly_vars: HashSet::new(),
            squish_iterator_meta: HashMap::new(),
            custom_type_data: self.custom_type_data.clone(),
            rebless_map: self.rebless_map.clone(),
            action_made: None,
            current_grammar_actions: None,
            pending_regex_error: None,
            precomp_enabled: self.precomp_enabled,
            monkey_typing: self.monkey_typing,

            // Merged VM execution registers (CP-3 collapse): a thread clone starts
            // with fresh per-execution registers, exactly as the former
            // `VM::new(thread_interp)` did for a spawned thread.
            stack: Vec::new(),
            locals: Vec::new(),
            upvalues: Vec::new(),
            in_smartmatch_rhs: false,
            transliterate_in_smartmatch: false,
            substitution_in_smartmatch: false,
            last_topic_value: None,
            topic_save_stack: Vec::new(),
            container_ref_var: None,
            container_ref_reversed: false,
            topic_source_var: None,
            element_source: None,
            quanthash_bind_params: Vec::new(),
            for_param_restore_stack: Vec::new(),
            call_frames: Vec::new(),
            control_handlers: Vec::new(),
            current_code: 0,
            carrier_writes: None,
            method_dispatch_pure: false,
            resume_ip: None,
            bind_context: false,
            scalar_bind_context: false,
            bound_decont_active: false,
            rebind_context: false,
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
            state_scope_id: None,
            fn_resolve_cache: HashMap::new(),
            fn_resolve_gen: 0,
            fn_resolve_cache_gen: 0,
            multi_candidates_cache: HashMap::new(),
            multi_candidates_cache_gen: 0,
            light_call_cache: HashMap::new(),
            light_call_cache_gen: 0,
            pos_light_call_cache: HashMap::new(),
            pos_light_call_cache_gen: 0,
            amp_param_shadowed_names: std::collections::HashSet::new(),
            registered_fn_fingerprints: HashMap::new(),
            prepared_fn_defs: HashMap::new(),
            method_resolve_cache: rustc_hash::FxHashMap::default(),
            last_method_resolve: None,
            fast_method_cache: rustc_hash::FxHashMap::default(),
            multi_resolve_cache: rustc_hash::FxHashMap::default(),
            multi_type_cacheable: rustc_hash::FxHashMap::default(),
            dispatch_multi_candidate: rustc_hash::FxHashMap::default(),
            block_declared_vars: Vec::new(),
            loop_local_vars: Vec::new(),
            loop_local_saved_env: Vec::new(),
            loop_cond_active: false,
            outer_scope_locals: Vec::new(),
            enter_result_stack: Vec::new(),
            pending_alias_bind_names: Vec::new(),
            otf_call_cache: HashMap::new(),
            otf_call_cache_gen: 0,
            check_phaser_depth: 0,
            gather_for_loop_resume: None,
            rw_map_topic_capture: None,
        };
        // Raku gives each start block fresh $/ and $! (they are lexically scoped).
        cloned.env.insert("/".to_string(), Value::Nil);
        cloned.env.insert("!".to_string(), Value::Nil);
        cloned.env.insert("$/".to_string(), Value::Nil);
        cloned.env.insert("$!".to_string(), Value::Nil);
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
        values: Vec<Value>,
        target_fallback: &Value,
    ) -> Value {
        if key.starts_with('@') && self.shared_vars_active {
            // Drop env's copy of the Arc first so that shared_vars holds
            // the only strong reference (refcount=1). This keeps repeated
            // shared pushes in-place instead of degenerating into O(n²) COW.
            self.env.remove(key);
            let is_thread_clone = self.is_thread_clone();
            let mut sv = self.shared_vars.write().unwrap();
            // Try in-place mutation via get_mut first (avoids remove+insert overhead)
            if let Some(Value::Array(arc_items, kind)) = sv.get_mut(key) {
                let items = Arc::make_mut(arc_items);
                items.extend(values);
                if *kind == ArrayKind::List {
                    *kind = ArrayKind::Array;
                }
                let result = Value::Array(Arc::clone(arc_items), *kind);
                drop(sv);
                self.mark_shared_var_dirty(key);
                if !is_thread_clone {
                    self.env.insert(key.to_string(), result.clone());
                }
                return result;
            }
            // Fallback: the value might exist but not be an Array yet
            if let Some(shared_value) = sv.remove(key) {
                if let Value::Array(mut arc_items, kind) = shared_value {
                    let items = Arc::make_mut(&mut arc_items);
                    items.extend(values);
                    let normalized_kind = if kind == ArrayKind::List {
                        ArrayKind::Array
                    } else {
                        kind
                    };
                    let result = Value::Array(Arc::clone(&arc_items), normalized_kind);
                    sv.insert(key.to_string(), Value::Array(arc_items, normalized_kind));
                    drop(sv);
                    self.mark_shared_var_dirty(key);
                    if !is_thread_clone {
                        self.env.insert(key.to_string(), result.clone());
                    }
                    return result;
                }
                sv.insert(key.to_string(), shared_value);
            }
        }
        // Fallback for non-shared arrays: use Arc::make_mut for COW
        if let Some(Value::Array(arc_items, kind)) = self.env.get_mut(key) {
            let items = Arc::make_mut(arc_items);
            items.extend(values);
            // Normalize @-variables only from List to Array while preserving Shaped.
            if key.starts_with('@') && *kind == ArrayKind::List {
                *kind = ArrayKind::Array;
            }
            return Value::Array(Arc::clone(arc_items), *kind);
        }
        let mut items = match target_fallback {
            Value::Array(v, ..) => v.to_vec(),
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
        let is_thread_clone = self.is_thread_clone();
        if is_thread_clone {
            self.env.remove(key);
        }
        let mut sv = self.shared_vars.write().unwrap();
        let Some(Value::Array(arc_items, kind)) = sv.get_mut(key) else {
            return None;
        };
        let items = Arc::make_mut(arc_items);
        items.extend(values);
        if *kind == ArrayKind::List {
            *kind = ArrayKind::Array;
        }
        let result = Value::Array(Arc::clone(arc_items), *kind);
        drop(sv);
        if is_thread_clone {
            let dirty_marker = format!("__mutsu_shared_dirty::{key}");
            if !self.env.contains_key(&dirty_marker) {
                self.mark_shared_var_dirty(key);
                self.env.insert(dirty_marker, Value::Bool(true));
            }
        } else {
            self.mark_shared_var_dirty(key);
            self.env.insert(key.to_string(), result.clone());
        }
        Some(result)
    }
}
