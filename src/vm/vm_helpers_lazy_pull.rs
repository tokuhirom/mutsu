use super::*;

impl Interpreter {
    pub(super) fn force_lazy_list_vm_n_inner(
        &mut self,
        list: &LazyList,
        needed: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        // Check cache first
        {
            let cache = list.cache.lock().unwrap();
            if let Some(cached) = cache.as_ref()
                && cached.len() >= needed
            {
                return Ok(cached[..needed].to_vec());
            }
        }

        // Lazy `WALK(method)()`: invoke the next MRO-level candidate(s) on demand,
        // one method call per pulled element (Rakudo's lazy WALK semantics).
        if list.walk_pending.is_some() {
            return self.force_walk_pending(list, needed);
        }

        // For sequence-spec lazy lists, generate more elements on demand
        if let Some(ref spec) = list.sequence_spec {
            return Self::extend_sequence_cache(list, spec, needed);
        }

        // For infinite closure-based sequences (`1, 1, * + * ... *`), re-invoke
        // the generator closure over the growing history to produce elements on
        // demand, so an unbounded sequence stays lazy instead of truncating to
        // its eager prefix.
        if list.closure_seq.is_some() {
            return self.extend_closure_sequence(list, needed);
        }

        // Lazy map/grep pipeline: pull from the source and apply the stage on
        // demand (one source element at a time), so an infinite source stays
        // lazy instead of materializing.
        if list.lazy_pipe.is_some() {
            return self.force_lazy_pipe(list, needed);
        }

        // Check if coroutine is finished (body completed, all elements produced)
        if let Some(ref coro_mutex) = list.coroutine {
            let coro = coro_mutex.lock().unwrap();
            if coro.finished {
                // Body is done; return whatever we have cached
                let cache = list.cache.lock().unwrap();
                return Ok(cache.as_ref().cloned().unwrap_or_default());
            }
        }

        // Need compiled code
        let (cc, fns) = match (&list.compiled_code, &list.compiled_fns) {
            (Some(cc), Some(fns)) => (cc.clone(), fns.clone()),
            _ => {
                // Fall back to interpreter prefix bridge
                return self.force_lazy_list_prefix_bridge(list, needed);
            }
        };

        // Save current Interpreter state
        crate::vm::vm_stats::record_clone_env();
        let saved_env = self.clone_env();
        let saved_locals = std::mem::take(&mut self.locals);
        let saved_stack = std::mem::take(&mut self.stack);

        // Determine starting IP and locals from coroutine state or fresh start
        let mut ip;
        let has_prior_state;

        if let Some(ref coro_mutex) = list.coroutine {
            let coro = coro_mutex.lock().unwrap();
            if !coro.finished && coro.ip > 0 {
                // Resume from saved state
                ip = coro.ip;
                self.locals = coro.locals.clone();
                self.stack = coro.stack.clone();
                *self.env_mut() = coro.env.clone();
                self.gather_for_loop_resume = coro.for_loop_resume.clone();
                has_prior_state = true;
            } else {
                // Fresh start (scoped overlay over the gather's captured env; see
                // docs/vm-dual-store.md Slice 6). On suspend the scoped env is
                // saved into the coroutine state and restored on resume (clone
                // preserves overlay+parent+tombstones), so the body's writes
                // accumulate across takes without forking `list.env`.
                ip = 0;
                *self.env_mut() = crate::env::Env::scoped_child(list.env.flattened());
                self.locals = vec![Value::Nil; cc.locals.len()];
                for (i, name) in cc.locals.iter().enumerate() {
                    if let Some(val) = self.env().get(name) {
                        self.locals[i] = val.clone();
                    }
                }
                self.stack = Vec::new();
                has_prior_state = false;
            }
        } else {
            // Fresh start (no coroutine slot yet)
            ip = 0;
            *self.env_mut() = crate::env::Env::scoped_child(list.env.flattened());
            self.locals = vec![Value::Nil; cc.locals.len()];
            for (i, name) in cc.locals.iter().enumerate() {
                if let Some(val) = self.env().get(name) {
                    self.locals[i] = val.clone();
                }
            }
            self.stack = Vec::new();
            has_prior_state = false;
        }

        // Push gather items collector with the take limit
        let saved_gather_len = self.gather_items_len();

        // If resuming, restore already-cached items into the gather collector
        // so that the take_value limit check accounts for them.
        let _already_cached = if has_prior_state {
            let cache = list.cache.lock().unwrap();
            let items = cache.as_ref().cloned().unwrap_or_default();
            let len = items.len();
            self.push_gather_items(items);
            len
        } else {
            self.push_gather_items(Vec::new());
            0
        };
        self.push_gather_take_limit(Some(needed));

        // Run the compiled code
        let run_fns = fns.as_ref();
        let mut run_result = Ok(());
        let mut body_finished = false;

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
                Err(e)
                    if e.message == crate::runtime::Interpreter::LAZY_GATHER_TAKE_LIMIT_SIGNAL =>
                {
                    // Take limit reached — the gather body yielded.
                    if self.gather_for_loop_resume.is_some() {
                        // From inside a compound ForLoop op — keep ip here.
                    } else {
                        // ip points to the Take instruction; advance past it.
                        ip += 1;
                    }
                    break;
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

        // If we exited the loop normally (ip >= cc.ops.len()), body is finished
        if run_result.is_ok() && ip >= cc.ops.len() {
            body_finished = true;
        }

        // Collect gather items
        let items = self.pop_gather_items().unwrap_or_default();
        self.pop_gather_take_limit();

        while self.gather_items_len() > saved_gather_len {
            self.pop_gather_items();
            self.pop_gather_take_limit();
        }

        // Sync locals back to env
        for (i, name) in cc.locals.iter().enumerate() {
            {
                let __v = self.locals[i].clone();
                self.env_mut().insert(name.clone(), __v);
            }
        }

        // Save coroutine state before restoring outer env
        if !body_finished && run_result.is_ok() {
            let for_loop_resume = self.gather_for_loop_resume.take();
            let coro_state = GatherCoroutineState {
                ip,
                locals: self.locals.clone(),
                stack: self.stack.clone(),
                env: self.env().clone(),
                finished: false,
                for_loop_resume,
            };
            if let Some(ref coro_mutex) = list.coroutine {
                *coro_mutex.lock().unwrap() = coro_state;
            }
            // Note: if list.coroutine is None, we can't save state (shouldn't happen
            // for gather-based lists since we set it up in MakeGather)
        } else if let Some(ref coro_mutex) = list.coroutine {
            let mut coro = coro_mutex.lock().unwrap();
            coro.finished = true;
        }

        // Merge env changes back to outer scope
        let gather_result_env = self.env().clone();
        let initial_env = &list.env;
        let mut merged_env = saved_env.clone();
        for (k, v) in gather_result_env.iter() {
            if !saved_env.contains_key_sym(*k) {
                continue;
            }
            if let Some(initial) = initial_env.get_sym(*k) {
                if v.to_string_value() != initial.to_string_value() {
                    merged_env.insert_sym(*k, v.clone());
                }
            } else {
                merged_env.insert_sym(*k, v.clone());
            }
        }
        *self.env_mut() = merged_env;
        // Precise (env_dirty-independent) writeback of the gather body's
        // captured-outer writes — see the matching comment in
        // `force_lazy_list_vm_inner`.
        self.record_eager_block_free_var_writeback(cc.as_ref(), &[]);

        // Restore Interpreter state
        self.locals = saved_locals;
        self.stack = saved_stack;

        run_result?;

        // Update cache
        *list.cache.lock().unwrap() = Some(items.clone());

        let result = if items.len() > needed {
            items[..needed].to_vec()
        } else {
            items
        };
        Ok(result)
    }

    /// Produce at least `needed` output elements of a lazy `map`/`grep` pipeline
    /// (a `LazyList` carrying a [`crate::value::MapGrepSpec`]).
    ///
    /// Elements are produced by pulling from the stage's source one at a time
    /// and applying the `map`/`grep` callback, accumulating outputs in the
    /// list's cache. A `grep` stage filters (0 or 1 output per source element);
    /// a `map` stage transforms (a `Slip` result contributes multiple). The
    /// source itself may be another lazy pipeline, so chains nest.
    pub(crate) fn force_lazy_pipe(
        &mut self,
        list: &LazyList,
        needed: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        loop {
            // Fast path: enough already cached, or the source is exhausted.
            {
                let cache = list.cache.lock().unwrap();
                let done = list
                    .lazy_pipe
                    .as_ref()
                    .map(|p| p.lock().unwrap().done)
                    .unwrap_or(true);
                if let Some(c) = cache.as_ref()
                    && (c.len() >= needed || done)
                {
                    let n = needed.min(c.len());
                    return Ok(c[..n].to_vec());
                }
            }

            // Snapshot the stage so no pipe lock is held across the pull/apply
            // (which may recursively pull from a nested pipe source).
            let (source, func, is_grep, source_idx, index_transform) = {
                let spec = list.lazy_pipe.as_ref().unwrap().lock().unwrap();
                (
                    spec.source.clone(),
                    spec.func.clone(),
                    spec.is_grep,
                    spec.source_idx,
                    spec.index_transform,
                )
            };

            match self.pull_source_element(&source, source_idx)? {
                None => {
                    // Source exhausted: mark done and return what we have.
                    let mut spec = list.lazy_pipe.as_ref().unwrap().lock().unwrap();
                    spec.done = true;
                    drop(spec);
                    let cache = list.cache.lock().unwrap();
                    let c = cache.as_ref().cloned().unwrap_or_default();
                    let n = needed.min(c.len());
                    return Ok(c[..n].to_vec());
                }
                Some(elem) if index_transform.is_some() => {
                    // `.pairs`/`.antipairs`/`.kv` over a lazy source: emit the
                    // index-based transform using the source position as the key,
                    // bypassing the map/grep callback entirely.
                    let idx = Value::Int(source_idx as i64);
                    let produced: Vec<Value> = match index_transform.unwrap() {
                        crate::value::IndexTransform::Pairs => {
                            vec![Value::ValuePair(Box::new(idx), Box::new(elem))]
                        }
                        crate::value::IndexTransform::AntiPairs => {
                            vec![Value::ValuePair(Box::new(elem), Box::new(idx))]
                        }
                        crate::value::IndexTransform::Kv => vec![idx, elem],
                    };
                    {
                        let mut spec = list.lazy_pipe.as_ref().unwrap().lock().unwrap();
                        spec.source_idx = source_idx + 1;
                    }
                    let mut cache = list.cache.lock().unwrap();
                    cache.get_or_insert_with(Vec::new).extend(produced);
                }
                Some(elem) => {
                    // Apply the stage with Interpreter-native dispatch so the callback
                    // runs in *this* Interpreter (keeping locals/env coherent and letting
                    // side effects reach the enclosing scope). A `grep` keeps the
                    // element when the matcher is truthy; a `map` transforms it (a
                    // `Slip` result contributes multiple elements).
                    //
                    // Loop control inside the callback is honored: `last` ends the
                    // sequence (excluding the current element), `next` skips the
                    // current element. This makes `(^Inf).grep({last if …})`
                    // terminate instead of running forever.
                    let applied: Result<Vec<Value>, RuntimeError> = if is_grep {
                        match self.vm_grep_item_matches(&func, &elem) {
                            Ok(true) => Ok(vec![elem]),
                            Ok(false) => Ok(Vec::new()),
                            Err(e) => Err(e),
                        }
                    } else {
                        self.vm_call_on_value(func, vec![elem], None)
                            .map(|v| match v {
                                Value::Slip(items) => items.as_ref().clone(),
                                v => vec![v],
                            })
                    };
                    let produced: Vec<Value> = match applied {
                        Ok(p) => p,
                        Err(e) if e.is_last() => {
                            // `last`: terminate the sequence, current element excluded.
                            let mut spec = list.lazy_pipe.as_ref().unwrap().lock().unwrap();
                            spec.done = true;
                            drop(spec);
                            let cache = list.cache.lock().unwrap();
                            let c = cache.as_ref().cloned().unwrap_or_default();
                            let n = needed.min(c.len());
                            return Ok(c[..n].to_vec());
                        }
                        // `next`: skip the current element and continue.
                        Err(e) if e.is_next() => Vec::new(),
                        Err(e) => return Err(e),
                    };
                    {
                        let mut spec = list.lazy_pipe.as_ref().unwrap().lock().unwrap();
                        spec.source_idx = source_idx + 1;
                    }
                    let mut cache = list.cache.lock().unwrap();
                    cache.get_or_insert_with(Vec::new).extend(produced);
                }
            }
        }
    }
}
