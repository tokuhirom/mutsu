use super::*;
use crate::meta_ns::MetaNs;
use crate::runtime::shared_store::atomic_lane_str_key;

impl Interpreter {
    /// ADR-12204 S2: whether the `@`/`%` lexical `name` (with sigil) is held in
    /// a shared `ContainerRef` cell by the spawning frame and may therefore be
    /// captured by that cell instead of the name-keyed lane. Opaque names (a
    /// registered class/role method writes them, attributes, dynamics,
    /// qualified or compiler-internal names) stay on the lane.
    // Cost: O(1) expected, two hash lookups.
    fn aggregate_cell_captured(&self, name: &str) -> bool {
        if !Self::transient_lane_candidate(name)
            || name.contains("__")
            || self
                .lexicals
                .type_body_written_lexicals
                .contains(name.trim_start_matches(['@', '%']))
            || self.lexicals.type_body_written_lexicals.contains(name)
        {
            return false;
        }
        self.env.get(name).is_some_and(|v| {
            matches!(
                v.view(),
                ValueView::ContainerRef(_)
            ) && matches!(v.deref_container().view(), ValueView::Array(..) | ValueView::Hash(..))
        })
    }

    /// Whether `key` is a plain-lexical `@`/`%` name the ADR-0039 §8.6
    /// transient-lane rule applies to. Twigil'd, dynamic and attribute
    /// containers (`@!x`, `@*y`, `%?RESOURCES`) are not plain lexicals and keep
    /// their existing lifetimes — `@*x`/`%*x` in particular depend on a durable
    /// name-lane entry for their cross-thread propagation (see the
    /// aggregate-dynamic carve-out in the seeding loop). The anonymous
    /// container slot names and `::`-qualified names are excluded for the same
    /// reason `collect_unit_lexical_names` excludes them.
    pub(crate) fn transient_lane_candidate(key: &str) -> bool {
        Self::is_plain_lexical_name(key)
            && !key.contains("__ANON")
            && !crate::qualified::is_qualified_str(key)
    }

    /// Union a frame's `type_body_written_lexicals` into the interpreter-wide
    /// set. Called at `RegisterClass` / `RegisterRole`, which always run before
    /// the type can be instantiated (and so before any of its methods can run).
    pub(crate) fn note_type_body_written_lexicals(&mut self, code: &CompiledCode) {
        for sym in &code.type_body_written_lexicals {
            let name = sym.resolve();
            crate::runtime::cow_table_mut(&mut self.lexicals.type_body_written_lexicals)
                .insert(name.trim_start_matches('$').to_string());
        }
    }

    /// [`Self::clone_for_thread`] for a spawn that runs a known block (`start { ... }`,
    /// `Promise.start`, `Thread.start`). The block only feeds the ADR-12204
    /// capture oracle: no plain user lexical is published by the spawn.
    pub(crate) fn clone_for_thread_for_block(&mut self, block: &Value) -> Self {
        if crate::runtime::shared_store::capture_check::enabled() {
            // ADR-12204 S1: the aggregates this spawn's block names.
            if let ValueView::Sub(data) = block.view()
                && let Some(cc) = data.compiled_code.as_ref()
            {
                let names: Vec<String> = cc
                    .free_var_syms
                    .iter()
                    .chain(cc.free_var_writes.iter())
                    .chain(cc.free_var_container_writes.iter())
                    .map(|sym| sym.resolve())
                    .filter(|name| name.starts_with(['@', '%']))
                    .collect();
                let cells: Vec<String> = names
                    .iter()
                    .filter(|name| self.aggregate_cell_captured(name))
                    .cloned()
                    .collect();
                crate::runtime::shared_store::capture_check::record_spawn_captures(&names);
                crate::runtime::shared_store::capture_check::record_cell_captures(&cells);
            }
        }
        self.spawn_clone()
    }

    /// Create a lightweight clone of this interpreter for use in a spawned thread.
    /// Shares function/class/role/enum definitions but starts with fresh output and test state.
    /// The child reaches the parent's lexicals through the cells and objects its
    /// env clone carries (ADR-16595); only opaque names and `@*`/`%*` dynamics are
    /// published into `shared_vars`.
    ///
    /// Most of what this builds is cheap, and deliberately so: the program-global
    /// symbol tables it carries over are `Arc<...>` copy-on-write shares (see the
    /// note on [`Interpreter`]), so cloning them is a refcount bump rather than a
    /// deep copy of every `String` key in the program. Before that, a spawn cost
    /// grew with the size of the loaded program instead of with the work: with
    /// Cro's stack loaded, the identical `Promise(supply { whenever ... })` that
    /// `Cro::MessageWithBody.body-blob` builds cost 5x what it costs in a bare
    /// program, purely because there were more modules to copy tables for.
    pub(crate) fn clone_for_thread(&mut self) -> Self {
        self.spawn_clone()
    }

    /// The workhorse behind every thread clone.
    fn spawn_clone(&mut self) -> Self {
        // ADR-12204 S1 oracle: the lane writes below seed the spawner's
        // lexicals; they are not uses of the names.
        let _seeding = crate::runtime::shared_store::capture_check::SeedingGuard::new();
        // A worker has no mainline callframe of its own. Preserve the spawning
        // source location separately so an anonymous callback can still expose
        // a useful bottom backtrace frame without duplicating a located entry
        // block (see `thread_origin_frame`).
        let thread_spawn_origin = self
            .current_source_file_sym()
            .zip(self.current_source_line());
        // A thread spawned BEFORE the first test call must still share the TAP
        // counter: the first `ok` of the whole program can run on the spawned
        // thread (e.g. supply-tap check closures in Cro's HTTP/2 serializer
        // tests, where every assertion of the first `test()` call fires inside
        // the tap). Without a pre-created `TestState`,
        // `TapState::clone_for_thread` has nothing to share; the child then
        // lazily creates a private state whose increments never reach the
        // parent, and the parent restarts numbering at 1 — "Tests out of
        // sequence" under prove. Gate on the Test module being loaded: an
        // empty `TestState` flips `test_mode_active`, which would otherwise
        // change bare-word resolution for non-Test programs that spawn threads.
        if self.test_module_loaded() && !self.io.tap.active() {
            // `clone_for_thread` below shares the counter of an EXISTING state.
            self.io.tap.ensure_state();
        }
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
        let shared = Arc::clone(&self.threads.shared_vars);
        // Merged with the lineage-seeding walk below (was a separate `for value
        // in self.env.values()` pass): both need one full traversal of the
        // parent env per spawn, so collecting handle ids here — BEFORE any of
        // the seeding loop's `continue`s — does both jobs in one pass. Every
        // value must still be checked for a handle id even when the seeding
        // side skips it (e.g. `$*CWD`/`self`/`__mutsu_*` are never handles, but
        // an excluded captured scalar or an already-visible name legitimately
        // could be).
        let mut referenced_handle_ids = std::collections::HashSet::new();
        // The built-in dynamics live in the per-interpreter base tier, which
        // `&self.env` (map-only) does not yield (ADR-0086) — but `$*OUT` /
        // `$*ERR` / `$*IN` / `$*ARGFILES` are exactly the handles the child
        // must be able to keep using, so collect their ids explicitly. The
        // seeding walk below deliberately skips these names anyway (a dynamic
        // is thread-local), so only the handle-id half applies to them.
        if let Some(base) = self.env.dyn_base() {
            for val in base.values() {
                if let Some(id) = Self::handle_id_from_value(val) {
                    referenced_handle_ids.insert(id);
                }
            }
        }
        {
            // Slice 5 step A instrumentation: how many env entries this walk
            // visits vs. how many actually land in the store. Accumulated
            // locally and recorded once per spawn (one atomic add per counter).
            let mut seed_keys_walked: u64 = 0;
            let mut seed_inserts: u64 = 0;
            for (key, val) in &self.env {
                seed_keys_walked += 1;
                Self::collect_handle_ids(val, 3, &mut referenced_handle_ids);
                // Skip internal variables and topic variables.
                // Also skip $*CWD/*CWD — in Raku, dynamic variables like $*CWD
                // are thread-local; mutations inside `start` blocks must not
                // propagate back to the parent thread.
                // `self` is a per-invocation binding, never a shared mutable
                // variable: seeding it lets one start-block's invocant leak into
                // a sibling thread's env via the await-time shared-var sync
                // (Cro::CompositeConnector read another connector's attributes).
                // `?`-prefixed compile-time pseudo-lexicals (?CLASS, ?ROLE,
                // ?LINE, ...) are per-scope constants with the same problem.
                if key == "_"
                    || key == "@_"
                    || key == "%_"
                    || key == "/"
                    || key == "!"
                    || key == "$/"
                    || key == "$!"
                    || key == "$*CWD"
                    || key == "*CWD"
                    || key == "self"
                    || key.starts_with("__mutsu_")
                    || key.starts_with("&")
                    || key.starts_with("?")
                    // Every SCALAR dynamic variable (`*x` / `$*x`), not just
                    // `$*CWD`: dynamics are thread-local in Raku, so a `start`
                    // block must not seed the parent frame's binding into the
                    // lineage-shared store, or it leaks process-wide after the
                    // frame returns (the child env is a clone of the parent's,
                    // so a spawned worker still reads the current dynamic fine
                    // without this name-lane sharing; and a block that closes
                    // over the dynamic gets it via the normal captured-scalar
                    // cell below regardless).
                    //
                    // `@*x`/`%*x` aggregate dynamics are deliberately NOT
                    // excluded here: unlike scalars, aggregates have no
                    // cell-based closure capture in this codebase — their
                    // cross-thread mutation visibility (e.g. `.then`
                    // callbacks chained onto the SAME promise, roast
                    // S17-promise/then.t's `@*FOO`) depends entirely on this
                    // name lane's `__mutsu_atomic_*` CAS mechanism, same as
                    // any other captured aggregate (see the comment on that
                    // exclusion below). Excluding them here would silently
                    // break that live-chain propagation, not just the leak.
                    || (key.with_str(crate::env::is_dynamic_var_name)
                        && !key.starts_with("@")
                        && !key.starts_with("%"))
                    // An attribute alias (`!x`, `@!x`, `%!x`) is a view of
                    // the invocant's storage, not a lexical of this frame: the
                    // instance is already shared, so seeding a snapshot of the
                    // alias into the lineage store only shadows it — an inline
                    // `$!lock.protect: { @!x ... }` in the spawning method then
                    // read the spawn-time copy and lost every write another
                    // thread made to the attribute since (#8380's
                    // `Test::Scheduler.run-due` dropping a `FutureEvent`).
                    || key.with_str(is_attribute_alias_name)
                {
                    continue;
                }
                let key = key.resolve();
                // ADR-16595 slice 4: the spawn publishes no plain user lexical.
                // A scalar or `@`/`%` the spawned code reaches lives in its
                // frame's slot or in the declaration-site `ContainerRef` cell
                // every escaping capture holds, and the child's env clone below
                // carries that cell (or the shared object itself). What still
                // rides the name lane is a name code on another thread reaches
                // WITHOUT a capture ([`Self::lexical_name_is_opaque`]: a name a
                // registered class/role method writes) and the aggregate
                // dynamics `@*x` / `%*x`, whose live `.then`-chain propagation
                // depends on it (the exclusion list above keeps their scalar
                // siblings off).
                if !self.lexical_name_is_opaque(&key)
                    && !(key.starts_with(['@', '%'])
                        && crate::env::is_dynamic_var_name(&key))
                {
                    continue;
                }
                // Only seed if not already visible — an entry an earlier thread
                // already updated must not be reset to this env's copy.
                if shared.seed_if_absent(&key, || val.clone()) {
                    seed_inserts += 1;
                }
            }
            crate::vm::vm_stats::record_spawn_seeding(seed_keys_walked, seed_inserts);
            // Track C: migrate the parent's `state` variables into shared cells.
            // Incremental: only the keys created since the previous spawn.
            self.seed_unmigrated_state_vars(&shared);
        }
        self.threads.shared_vars_active = true;
        let mut cloned = Self {
            types: self.types.fork_for_thread(),
            literal_native_args: 0,
            static_call_args: false,
            env: self.env.clone(),
            io: self.io.fork_for_thread(&referenced_handle_ids),
            control: self.control.fork_for_thread(),
            main_hidden_from_usage: self.main_hidden_from_usage.clone(),
            explicit_run_main: self.explicit_run_main,
            nested_mode: self.nested_mode,
            module: self.module.fork_for_thread(),
            dispatch: self.dispatch.fork_for_thread(),
            current_unit: self.current_unit,
            closures_created: 0,
            // Snapshot (fresh lock), not a shared handle: thread-local registry
            // semantics — child sees a copy, writes don't leak to the parent.
            current_package_sym: Arc::new(std::sync::atomic::AtomicU32::new(
                self.current_package_sym().id(),
            )),
            routine_stack: crate::runtime::routine_stack::RoutineStack::default(),
            callframe_stack: Vec::new(),
            pending_call_arg_sources: None,
            pending_where_exception: None,
            pending_skip_constraint_recheck: false,
            pending_raw_invocant: None,
            pending_call_arg_source_slots: rustc_hash::FxHashMap::default(),
            pending_rw_writeback_slots: super::rw_writeback_slots::RwWritebackSlots::new(),
            test_pending_callsite_line: None,
            nqp_arg_scratch: Vec::new(),
            cur_source_line: 1,
            thread_spawn_origin,
            args_scratch_pool: Vec::new(),
            block_stack: Vec::new(),
            declarator_docs: declarator_docs::DeclaratorDocs::default(),
            topic_state: self.topic_state.fork_for_thread(),
            async_state: self.async_state.fork_for_thread(),
            block_scope_depth: self.block_scope_depth,
            pending_sigilless_store: None,
            regex_state: self.regex_state.fork_for_thread(),
            caches: self.caches.fork_for_thread(),
            pending_eval_sigilless: Vec::new(),
            pending_eval_placeholder_params: Vec::new(),
            pending_eval_rw_tail: false,
            pending_eval_context_routine: None,
            repl_compiler: Default::default(),
            pending_supply_block_body: false,
            pending_supply_emitter_sym: None,
            pending_supply_authoritative_free_vars: Vec::new(),
            last_block_my_declared: Vec::new(),
            pending_runtime_name_writes: Vec::new(),
            threads: self.threads.fork_for_thread(),
            lexicals: self.lexicals.fork_for_thread(),
            in_lvalue_assignment: false,
            rw_return_context: false,
            trait_mod_writeback_key: None,
            trait_mod_writeback_value: None,
            trait_mod_attr_writeback_value: None,
            trait_mod_default_writeback: None,
            hash_autovivify: false,
            caller_env_stack: Vec::new(),
            raku_cycle_guards: self.raku_cycle_guards.fork_for_thread(),
            last_value: None,
            pending_local_updates: Vec::new(),

            // Merged VM execution registers (CP-3 collapse): a thread clone starts
            // with fresh per-execution registers, exactly as the former
            // `VM::new(thread_interp)` did for a spawned thread.
            stack: Vec::new(),
            locals: crate::runtime::locals::Locals::new(),
            trir: crate::trir::frame::TrStacks::default(),
            trir_outer_cache: rustc_hash::FxHashMap::default(),
            upvalues: Vec::new(),
            frame_authoritative: Vec::new(),
            frame_owned: Vec::new(),
            call_frames: Vec::new(),
            stack_check_countdown: 0,
            handler_fns_snapshot: None,
            current_code: 0,
            numeric_op_site: (0, 0),
            carrier_writes: None,
            resume_ip: None,
            #[cfg(feature = "jit")]
            jit_error: None,
            accessor_ref_pending: false,
            sigilless_bind_source: None,
            mark_ctx: Box::default(),
            array_share_active: false,
            vardecl_init_raw: None,
            pending_rw_writeback_sources: Vec::new(),
            pending_caller_var_writeback: rustc_hash::FxHashSet::default(),
            inline_control_env_writes: Vec::new(),
            local_bind_pairs: Vec::new(),
            rw_param_rebinds: Vec::new(),
            call_ic: [crate::opcode::CallIcSlot::EMPTY; crate::opcode::CALL_IC_WAYS],
            pos_light_ic_epoch: 1,
            outer_scope_locals: Vec::new(),
            enter_result_stack: Vec::new(),
            pending_alias_bind_names: Vec::new(),
            nested_run_depth: 0,
        };
        // Raku gives each start block fresh $/ and $! (they are lexically scoped).
        cloned.env.insert("/".to_string(), Value::NIL);
        cloned.env.insert("!".to_string(), Value::NIL);
        cloned.env.insert("$/".to_string(), Value::NIL);
        cloned.env.insert("$!".to_string(), Value::NIL);
        cloned.init_io_environment_for_thread_clone();
        // The rebuild above writes the child's own `$*CWD`/`$*TMPDIR`/… into
        // its env map; hoist them straight back down into a base tier of the
        // child's own, so a closure created inside the spawned block captures
        // no more than one created on the parent would (ADR-0086). The child's
        // base starts as a copy of the parent's, so an inherited `$*OUT`
        // redirection is preserved and a later write on either side promotes
        // into that side's overlay only.
        cloned.hoist_builtin_dynamics();
        cloned.bootstrap_hll_syms();
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
        // ADR-0039 slice 1: a compunit's own file-scope `@`/`%` (or a
        // mainline named sub's captured free variable) resolves through
        // `unit_lexicals` BEFORE `env` (`get_env_with_main_alias`'s doc
        // comment) — `env[key]` can hold a completely unrelated same-named
        // binding (e.g. the loading scope's own `my @items`, restored there
        // once the module's mainline finished running). Falling through to
        // the plain-env / `real_array` paths below for such a name would
        // either push onto that WRONG array, or replace the cell's contents
        // with a detached copy that the cell's own readers never see again
        // — the "miss -> construct -> insert" hazard this ADR is about.
        // Mutate the shared node in place instead (container mutation is
        // write-through-the-node, ADR-0013 §7) and return; no write-back is
        // needed since the cell and this dereferenced value share the same
        // `Gc<ArrayData>`. Excludes the anonymous-container slot names for
        // the same reason `collect_unit_lexical_names` does (they are never
        // actually placed in `unit_lexicals`, so this is defense in depth).
        if !key.contains("__ANON")
            && let Some(mut existing) = self.unit_lexical_container(key)
            && matches!(existing.view(), ValueView::Array(..))
        {
            return existing
                .with_array_mut(|arc_items, kind| {
                    let items = crate::value::gc_data_mut(arc_items);
                    items.extend(values);
                    if key.starts_with('@') && *kind == ArrayKind::List {
                        *kind = ArrayKind::Array;
                    }
                    Value::array_with_kind(crate::gc::Gc::clone(arc_items), *kind)
                })
                .unwrap();
        }
        let package_array = crate::qualified::is_package_array(Symbol::intern(key));
        if package_array
            && let Some(stored) = self
                .get_our_var(key)
                .cloned()
                .or_else(|| self.our_var_pseudo_unqualified(key))
        {
            // A prior call's env overlay has gone away. Restore the package
            // container before mutating so the same array kind and data survive.
            self.env.insert(key.to_string(), stored);
        }
        // A plain lexical `@name` already present in the shared store routes
        // through the atomic store (see `push_to_existing_shared_array`); when
        // absent it is thread-local and falls through to the env path below.
        // A name this lineage re-declared skips BOTH shared branches: the entry
        // under it belongs to the shadowed outer binding.
        if !self.container_lane_applies(key) {
            // fall through to the env path below
        } else if key.starts_with('@')
            && self.threads.shared_vars_active
            && Self::is_plain_lexical_array_name(key)
        {
            let in_shared = {
                let atomic_key = atomic_lane_str_key(key, false);
                self.threads
                    .shared_vars
                    .get(atomic_key)
                    .is_some_and(|v| matches!(v.view(), ValueView::Array(..)))
                    || self
                        .threads
                        .shared_vars
                        .get(key)
                        .is_some_and(|v| matches!(v.view(), ValueView::Array(..)))
            };
            if in_shared {
                return self.shared_array_extend(key, values, false);
            }
        } else if key.starts_with('@')
            && self.threads.shared_vars_active
            && !package_array
            // Only a name the shared store holds: dropping the env binding of
            // one it does not (a `my @*LOG` declared after a thread ran) left
            // the push on a detached copy, written by name into whatever frame
            // runs the push instead of the declaring frame's array.
            && self.threads.shared_vars.contains_key(key)
            && !is_dynamic_array_name(key)
        {
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
                .threads
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
            if let Some(shared_value) = self.threads.shared_vars.get(key)
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
                    self.threads
                        .shared_vars
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
        //
        // `env_root_descended_mut` (not a raw `self.env.get`/`get_mut`) is
        // required here for the same reason the `append`/`unshift`/`prepend`
        // arms in `call_method_mut_with_values` already use it: a captured,
        // escape-boxed `@`/`%` local (ADR-0039's `needs_cell_captured_containers`
        // — e.g. `my Str:D @a` captured by a block passed as a `.map`/`.grep`
        // argument) lives in a `ContainerRef` cell, not a plain `Array`, so a
        // raw `self.env.get(key)` never matched `ValueView::Array` and fell
        // through to the detached-array fallback below, which rebuilds a
        // fresh array and overwrites `env[key]` with it — severing the cell.
        // Every OTHER holder of that cell (in particular the closure's own
        // captured-env snapshot that an eager `.map` loop restores its
        // temporary bindings from) still saw the stale, pre-push cell
        // contents, so the pushed elements vanished the moment the loop's
        // env restore ran (#8503).
        if let Some(slot) = self.env_root_descended_mut(key)
            && matches!(slot.view(), ValueView::Array(..))
        {
            // The node is extended in place and other threads may hold the same
            // node (an attribute `@!a` every thread reaches through the shared
            // instance, #11701): a concurrent `Vec` growth under a reader or a
            // second writer is a use after free. Exclude them on the node, the
            // key the mutating-method funnel (`methods_call_dispatch.rs`) and the
            // leaf structure reads take for the same uncelled container (ADR-0068).
            let _structure = crate::value::container_lock::ContainerStructGuard::acquire_for(
                None, slot,
            );
            let result = slot
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
            if package_array {
                self.set_our_var(key.to_string(), result.clone());
            }
            return result;
        }
        let mut items = match target_fallback.view() {
            ValueView::Array(v, ..) => v.to_vec(),
            _ => Vec::new(),
        };
        items.extend(values);
        let result = Value::real_array(items);
        let stored = if package_array {
            Self::itemize_scalar_store_value(result.clone())
        } else {
            result.clone()
        };
        self.env.insert(key.to_string(), stored.clone());
        if package_array {
            self.set_our_var(key.to_string(), stored);
        }
        result
    }

    pub(crate) fn push_to_existing_shared_array(
        &mut self,
        key: &str,
        values: Vec<Value>,
    ) -> Option<Value> {
        // A dynamic (`@*x`) is per frame: an entry the name-keyed store holds
        // for it is a seed from some thread's spawn, not the binding this
        // frame sees, and pushing there rebound the name to that array
        // (#13732).
        if !key.starts_with('@') || !self.threads.shared_vars_active || is_dynamic_array_name(key) {
            return None;
        }
        // A plain user lexical lives in its own place, never on the lane
        // (ADR-16595 §2 rule 3).
        if self.lexical_has_own_place(key) {
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
                let atomic_key = atomic_lane_str_key(key, false);
                self.threads
                    .shared_vars
                    .get(atomic_key)
                    .is_some_and(|v| matches!(v.view(), ValueView::Array(..)))
                    || self
                        .threads
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
            .threads
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
            let dirty_marker = MetaNs::SharedDirty.owned_key_for_str(key);
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

/// Whether an env key is an attribute alias (`!x`, `@!x`, `%!x`, `&!x`) rather
/// than a lexical.
fn is_attribute_alias_name(key: &str) -> bool {
    let bare = key.trim_start_matches(['$', '@', '%', '&']);
    bare.len() > 1 && bare.starts_with('!')
}

/// Whether `key` names a dynamic array (`@*x`), which the name-keyed shared
/// store must not stand in for.
// Cost: O(1).
fn is_dynamic_array_name(key: &str) -> bool {
    key.as_bytes().get(1) == Some(&b'*')
}
