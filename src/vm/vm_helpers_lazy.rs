use super::*;

impl Interpreter {
    /// If the value is a `LazyIoLines`, force it into an eager array by reading
    /// all remaining lines from the file handle. Otherwise return the value as-is.
    pub(super) fn force_if_lazy_io_lines(&mut self, val: Value) -> Result<Value, RuntimeError> {
        if let Value::LazyIoLines {
            ref handle,
            kv,
            words,
        } = val
        {
            let forced = loan_env!(self, force_lazy_io_lines(handle, words))?;
            if kv {
                // Apply .kv transformation on the forced array
                let items = crate::runtime::utils::value_to_list(&forced);
                let mut kv_items = Vec::with_capacity(items.len() * 2);
                for (i, v) in items.iter().enumerate() {
                    kv_items.push(Value::Int(i as i64));
                    kv_items.push(v.clone());
                }
                Ok(Value::array(kv_items))
            } else {
                Ok(forced)
            }
        } else {
            Ok(val)
        }
    }

    pub(super) fn thread_right_first(
        left: &crate::value::JunctionKind,
        right: &crate::value::JunctionKind,
    ) -> bool {
        use crate::value::JunctionKind::{All, Any, None, One};
        matches!(left, Any | One) && matches!(right, All | None)
    }

    pub(super) fn label_matches(error_label: &Option<String>, loop_label: &Option<String>) -> bool {
        error_label.as_deref() == loop_label.as_deref() || error_label.is_none()
    }

    /// Check if a method on LazyList requires forcing the list first.
    /// Methods that coerce a lazy `Seq` to another lazy view and so must
    /// preserve the laziness of a map/grep pipeline (return it unchanged)
    /// instead of forcing it. `.eager`/`.elems`/`.sort`/… are NOT here — those
    /// genuinely need the whole list and correctly raise X::Cannot::Lazy.
    pub(crate) fn lazy_pipe_preserving_coercion(method: &str) -> bool {
        matches!(
            method,
            "Seq" | "List" | "list" | "Array" | "cache" | "values" | "lazy"
        )
    }

    /// If the variable `name` holds a lazy `@`-array backed by a cache-bearing
    /// spec (infinite sequence/closure/scan), reify its cached prefix into a
    /// real Array and write it back, so a subsequent element mutation
    /// (`@a[i] = v`, `:delete`) operates on a materialized backing. Front
    /// mutations collapse the list to its prefix (no worse than the pre-L2
    /// capped Array); reads keep it lazy. Reifies only cache-backed specs, so it
    /// never runs user code or hangs. (L2)
    pub(super) fn reify_lazy_array_slot(&mut self, name: &str) -> Result<(), RuntimeError> {
        let lazy = match self.env().get(name) {
            Some(Value::LazyList(ll))
                if ll.in_array_context()
                    && (ll.sequence_spec.is_some()
                        || ll.closure_seq.is_some()
                        || ll.scan_spec.is_some()) =>
            {
                Some(ll.clone())
            }
            _ => None,
        };
        if let Some(ll) = lazy {
            // Reify a bounded prefix (matching the historical reify-to-cap of a
            // capped `ArrayKind::Lazy` Array). With the L2b `[start]` seed the
            // cache holds only one element, so `force_lazy_list_vm` (cache read)
            // would lose the prefix on a front-mutation — extend to the cap.
            const MAX_ARRAY_EXPAND: usize = 100_000;
            let items = self.force_lazy_list_vm_n(&ll, MAX_ARRAY_EXPAND)?;
            self.env_mut()
                .insert(name.to_string(), Value::real_array(items));
        }
        Ok(())
    }

    pub(super) fn lazy_list_needs_forcing(method: &str) -> bool {
        matches!(
            method,
            "list"
                | "Array"
                | "Numeric"
                | "Int"
                | "elems"
                | "hyper"
                | "race"
                | "first"
                | "grep"
                | "map"
                | "sort"
                | "reverse"
                | "join"
                | "head"
                | "tail"
                | "min"
                | "max"
                | "minmax"
                | "sum"
                | "flat"
                | "unique"
                | "repeated"
                | "squish"
                | "classify"
                | "categorize"
                | "produce"
                | "rotor"
                | "batch"
                | "reduce"
                | "Supply"
                | "combinations"
                | "permutations"
                | "values"
                | "List"
                | "Str"
                | "Stringy"
                | "gist"
                | "raku"
                | "perl"
                | "Seq"
                | "item"
                | "cache"
                | "pick"
                | "roll"
                | "keys"
                | "kv"
                | "pairs"
                | "antipairs"
        )
    }

    /// For `.head` on a gather-sourced `LazyList`, return how many leading
    /// elements need to be produced (so an infinite gather is pulled lazily via
    /// [`Self::force_lazy_list_vm_n`] instead of forced to completion).
    /// Returns `None` for forms that need the whole list (e.g. `.head(*-3)`).
    pub(super) fn gather_head_bound(method: &str, args: &[Value]) -> Option<usize> {
        if method != "head" {
            return None;
        }
        match args {
            [] => Some(1),
            [Value::Int(n)] => Some((*n).max(0) as usize),
            [Value::Num(f)] => Some((*f as i64).max(0) as usize),
            _ => None,
        }
    }

    /// Force a LazyList by running its compiled bytecode in the Interpreter.
    /// Falls back to interpreter if no compiled code is available.
    pub(crate) fn force_lazy_list_vm(
        &mut self,
        list: &LazyList,
    ) -> Result<Vec<Value>, RuntimeError> {
        let caller_code = self.current_code;
        let r = self.force_lazy_list_vm_inner(list);
        self.reconcile_caller_after_lazy_force(caller_code);
        r
    }

    fn force_lazy_list_vm_inner(&mut self, list: &LazyList) -> Result<Vec<Value>, RuntimeError> {
        // A lazy `WALK(method)()` list is finite (one element per MRO-level
        // candidate): force them all by invoking every remaining candidate.
        if let Some(ref wp) = list.walk_pending {
            let total = wp.lock().unwrap().targets.len();
            return self.force_walk_pending(list, total);
        }
        // Handle scan-based lazy lists: compute elements on demand
        if list.scan_spec.is_some() {
            return self.force_scan_lazy_list(list, 200_000);
        }

        // A lazy map/grep pipeline is rooted at an infinite source. It can still
        // be forced when the callback runs `last` (which ends the sequence) —
        // e.g. `(^Inf).grep({ last if $_ > 5; True }).eager`. So attempt a
        // bounded force: if the pipe terminates within the cap (via `last` or a
        // finite source), return it; otherwise it is genuinely infinite and we
        // throw X::Cannot::Lazy, matching raku. Methods that know their own name
        // (e.g. `.elems`/`.sort`) raise a more specific message at the dispatch
        // site before reaching here.
        if list.lazy_pipe.is_some() {
            const EAGER_FORCE_CAP: usize = 1_000_000;
            let forced = self.force_lazy_pipe(list, EAGER_FORCE_CAP)?;
            let done = list
                .lazy_pipe
                .as_ref()
                .map(|p| p.lock().unwrap().done)
                .unwrap_or(true);
            if done {
                return Ok(forced);
            }
            return Err(RuntimeError::typed_msg(
                "X::Cannot::Lazy",
                "Cannot coerce an infinite lazy list to a strict list",
            ));
        }

        // For sequence-spec lazy lists, a strict force cannot materialize the
        // infinite tail, so return a bounded prefix (the historical 100k cap).
        // With the L2b `[start]` seed the cache is O(1), so extend it to the cap
        // here — every strict-force caller (front mutation reify, eager coerce,
        // ...) expects the prefix, not the seed. Lazy *read* paths (index, head,
        // first, map/grep pipes) use `force_lazy_list_vm_n` / pull and stay O(1).
        if let Some(ref spec) = list.sequence_spec {
            const MAX_ARRAY_EXPAND: usize = 100_000;
            return Self::extend_sequence_cache(list, spec, MAX_ARRAY_EXPAND);
        }

        // A lazy `IO::CatHandle.lines` / `.handles` list is finite (it reads to
        // the end of the cat's handles): force it fully by pulling until the cat
        // is exhausted.
        if list.cat_pull.is_some() {
            return self.force_cat_pull(list, usize::MAX);
        }

        // Check cache first
        if let Some(cached) = list.cache.lock().unwrap().clone() {
            return Ok(cached);
        }

        // If no compiled code, fall back to interpreter
        let (cc, fns) = match (&list.compiled_code, &list.compiled_fns) {
            (Some(cc), Some(fns)) => (cc.clone(), fns.clone()),
            _ => return self.force_lazy_list_bridge(list),
        };

        // Save current Interpreter state. Locals are kept coherent with env by
        // write-through (`flush_local_to_env`), so no explicit flush is needed
        // here; we restore locals directly on return.
        crate::vm::vm_stats::record_clone_env();
        let saved_env = self.clone_env();
        let saved_locals = std::mem::take(&mut self.locals);
        let saved_stack = std::mem::take(&mut self.stack);

        // Set up the lazy list's environment as a scoped overlay's parent: the
        // gather body reads its captured lexicals through to `list.env` and its
        // own writes land in a fresh born-owned overlay (no fork of `list.env`).
        // The merge below iterates the overlay (overlay-only) = the body's writes.
        // See docs/vm-dual-store.md (Slice 6).
        *self.env_mut() = crate::env::Env::scoped_child(list.env.flattened());

        // Push gather items collector
        let saved_gather_len = self.gather_items_len();
        self.push_gather_items(Vec::new());
        self.push_gather_take_limit(None);

        // Initialize locals for the compiled code
        self.locals = vec![Value::Nil; cc.locals.len()];
        for (i, name) in cc.locals.iter().enumerate() {
            if let Some(val) = self.env().get(name) {
                self.locals[i] = val.clone();
            }
        }
        self.stack = Vec::new();

        // Run the compiled code using the lazy list's own compiled_fns.
        // Outer scope subs are available via the env as Value::Sub.
        let run_fns = fns.as_ref();

        let mut ip = 0;
        let mut run_result = Ok(());
        while ip < cc.ops.len() {
            match self.exec_one(&cc, &mut ip, run_fns) {
                Ok(()) => {}
                Err(e) if e.is_warn() => {
                    if !self.warning_suppressed() {
                        self.write_warn_to_stderr(&e.message);
                    }
                    if let Some(v) = e.return_value {
                        self.stack.push(v);
                    }
                    ip += 1;
                    continue;
                }
                Err(e) => {
                    run_result = Err(e);
                    break;
                }
            }
            if self.is_halted() {
                break;
            }
        }

        // Collect gather items
        let items = self.pop_gather_items().unwrap_or_default();
        self.pop_gather_take_limit();

        // Clean up extra gather items if needed
        while self.gather_items_len() > saved_gather_len {
            self.pop_gather_items();
            self.pop_gather_take_limit();
        }

        // Sync locals back to env before reading the result environment.
        // During Interpreter execution, variable assignments go to self.locals, not
        // to the interpreter env. We must flush them so the merge logic below
        // can see the changes made by the gather body.
        for (i, name) in cc.locals.iter().enumerate() {
            {
                let __v = self.locals[i].clone();
                self.env_mut().insert(name.clone(), __v);
            }
        }

        // Restore the outer environment, selectively merging changes from
        // the gather body. Only propagate variables that:
        // 1. Existed in the outer scope, AND
        // 2. Were actually modified during gather body execution
        //    (i.e., their value changed from the gather body's initial env).
        // This prevents nested gather closures from corrupting each other's
        // captured variables (e.g., `$n` in nested grep-div calls), while
        // still propagating genuine side effects (e.g., `$x += 1`).
        let gather_result_env = self.env().clone();
        let mut merged_env = saved_env.clone();
        for (k, v) in gather_result_env.iter() {
            if !saved_env.contains_key_sym(*k) {
                continue;
            }
            if let Some(initial) = list.env.get_sym(*k) {
                // Variable existed in both outer and gather env.
                // Only propagate if the value actually changed during execution.
                // Compare string representations as a proxy for value equality.
                if v.to_string_value() != initial.to_string_value() {
                    merged_env.insert_sym(*k, v.clone());
                }
            } else {
                // Variable existed in outer scope but not in gather's initial env;
                // always propagate changes.
                merged_env.insert_sym(*k, v.clone());
            }
        }
        *self.env_mut() = merged_env;
        // Precise (env_dirty-independent) writeback: record the gather body's
        // captured-outer writes so `apply_pending_rw_writeback` (called from
        // `reconcile_caller_after_lazy_force`) drains them into the caller's local
        // slots, exactly like a `map`/`grep` callback. Without this the gather
        // case relied solely on the blanket `env_dirty` reconcile (the surface
        // docs/captured-outer-cell-sharing.md is retiring).
        self.record_eager_block_free_var_writeback(cc.as_ref(), &[]);

        // Restore Interpreter state
        self.locals = saved_locals;
        self.stack = saved_stack;

        // Check for errors
        run_result?;

        // Cache the result
        *list.cache.lock().unwrap() = Some(items.clone());
        Ok(items)
    }

    /// Force a gather-based LazyList to produce at least `needed` elements.
    /// Uses coroutine-style suspend/resume: the gather body pauses at each
    /// `take` once enough elements are available, and can be resumed later.
    /// Side effects (e.g. `$count++`) are correctly scoped because we pause
    /// mid-execution rather than re-running from scratch.
    pub(super) fn force_lazy_list_vm_n(
        &mut self,
        list: &LazyList,
        needed: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        let caller_code = self.current_code;
        let r = self.force_lazy_list_vm_n_inner(list, needed);
        self.reconcile_caller_after_lazy_force(caller_code);
        r
    }

    /// Reconcile the caller frame's local slots after a lazy force, so a
    /// captured-outer lexical mutated at reify time (e.g. `map({$c++})`,
    /// `gather`) is visible in the caller's slots even when the blanket reverse
    /// pull (`sync_locals_from_env`) is disabled (Slice F campaign). Two reify
    /// shapes record their writes differently and both must be drained here —
    /// the force machinery is the effective "call site" for the callbacks it
    /// runs, but unlike a real call op it never drains them:
    ///
    /// - a `map`/`grep` callback runs via `call_compiled_closure`, which logs
    ///   the changed caller free-vars into `pending_rw_writeback_sources`
    ///   (drained by `apply_pending_rw_writeback`, as every call op does);
    /// - a `gather` body merges its env changes and sets `env_dirty`, mirrored
    ///   by `reconcile_locals_from_env_at_site` (the #3331 helper).
    ///
    /// Both are byte-identical to the work reverse-sync ON would do (the call-op
    /// drain + the barrier pull), so ON behavior is unchanged.
    ///
    /// Also used by op handlers that run user code without a `code` parameter in
    /// hand (`say`/`note`, which dispatch a `.gist`/`.Str` closure that can
    /// mutate a captured-outer lexical): they capture `self.current_code` before
    /// the dispatch and pass it here afterwards. `caller_code` is the address of
    /// the caller frame's `CompiledCode`, captured before the inner `exec_one`
    /// runs clobbered `current_code`.
    pub(super) fn reconcile_caller_after_lazy_force(&mut self, caller_code: usize) {
        // The force body's own `exec_one` runs reset `current_code` to the lazy
        // body; restore it to the caller so a subsequent force in the same op
        // handler reconciles the right frame.
        self.current_code = caller_code;
        if caller_code == 0 {
            return;
        }
        if !self.pending_rw_writeback_sources.is_empty() {
            // SAFETY: `caller_code` is the address of the `CompiledCode` of the
            // bytecode frame that invoked this force. That frame is an ancestor
            // on the call stack (the op handler driving the force) and is alive
            // for the whole synchronous duration of the force, so the pointer is
            // valid here.
            let code = unsafe { &*(caller_code as *const CompiledCode) };
            self.apply_pending_rw_writeback(code);
        }
    }

    /// Reconcile after an *internal redispatch* (a user coercion/render method run
    /// mid-opcode without a surrounding CallMethod op: `+$obj`/`~$obj`/`if $obj`,
    /// string interpolation, `put`/`print`, …). Like
    /// [`Self::reconcile_caller_after_lazy_force`] it drains the captured-outer
    /// writeback into the caller frame's local slots, BUT it **retains** any
    /// `pending_rw_writeback_sources` entry whose name is not a slot of
    /// `caller_code` instead of dropping it.
    ///
    /// The retain matters because an internal redispatch can fire *inside another
    /// method body that has not yet returned* — e.g. a `submethod BUILD`'s
    /// `$gather ~= "($a)"` interpolation runs while a *sibling* `BUILD`'s
    /// captured-outer write (`$parent-counter++`) is still sitting in
    /// `pending_rw_writeback_sources`, queued for the outer `.new` call site to
    /// drain. The drop-on-miss `apply_pending_rw_writeback` would consume and
    /// discard that sibling write here (its slot lives in the outer frame, not the
    /// BUILD frame), so the outer `.new` drain finds nothing and the caller's slot
    /// stays stale. Retaining the miss leaves it for the frame that actually owns
    /// the slot.
    pub(super) fn reconcile_caller_after_internal_dispatch(&mut self, caller_code: usize) {
        self.current_code = caller_code;
        if caller_code == 0 {
            return;
        }
        // SAFETY: see `reconcile_caller_after_lazy_force` — `caller_code` is the
        // live ancestor frame's `CompiledCode` address.
        let code = unsafe { &*(caller_code as *const CompiledCode) };
        if !self.pending_rw_writeback_sources.is_empty() {
            let sources = std::mem::take(&mut self.pending_rw_writeback_sources);
            let mut retained = Vec::new();
            for source in sources {
                if let Some(slot) = self.find_local_slot(code, &source) {
                    if !matches!(self.locals[slot], Value::HashEntryRef { .. })
                        && let Some(val) = self.env().get(&source).cloned()
                    {
                        self.locals[slot] = val;
                    }
                    // matched (slot in this frame) → applied, do not retain
                } else {
                    retained.push(source);
                }
            }
            self.pending_rw_writeback_sources = retained;
        }
        self.apply_pending_caller_var_writeback(code);
    }
}
