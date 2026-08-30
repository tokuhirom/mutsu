use super::vm_control_ops::ForLoopSpec;
use super::*;
use crate::opcode::{CompiledCode, CompiledFns};

impl Interpreter {
    /// Worker count for a hyper/race op over `n` iterations: one worker per
    /// item for tiny lists (so inter-item `await` still sees a peer thread),
    /// otherwise `available_parallelism` (at least 1).
    fn hyper_worker_degree(n: usize) -> usize {
        if n == 0 {
            return 1;
        }
        let cores = std::thread::available_parallelism()
            .map(|c| c.get())
            .unwrap_or(4)
            .max(1);
        n.min(cores)
    }

    /// Parallel `hyper for` / `race for`: split the item list into batches,
    /// run the compiled body on a cloned interpreter per batch, concatenate
    /// collected results in input order.
    pub(super) fn exec_threaded_for_loop(
        &mut self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        items: &[Value],
        body_start: usize,
        loop_end: usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        // A `last` or `return` in the body controls the enclosing loop/routine,
        // not merely the worker batch that happens to encounter it.  Submitted
        // batches cannot retract work that has already started, so preserve the
        // sequential control-flow semantics for such bodies.  This deliberately
        // also catches a nested-loop control op: losing parallelism there is safe,
        // while guessing which loop a labelled control targets is not.
        let has_nonlocal_control = code.ops[body_start..loop_end].iter().any(|op| {
            matches!(
                op,
                OpCode::Last(_) | OpCode::Return | OpCode::ReturnFromNonRoutine(..)
            )
        });
        if has_nonlocal_control {
            return self
                .exec_for_loop_body(code, spec, items, body_start, loop_end, compiled_fns, 0)
                .map(|_| ());
        }
        if items.is_empty() {
            return self
                .exec_for_loop_body(code, spec, items, body_start, loop_end, compiled_fns, 0)
                .map(|_| ());
        }
        let arity = spec.arity.max(1) as usize;
        let n_iters = items.len().div_ceil(arity);
        let degree = Self::hyper_worker_degree(n_iters);
        let iters_per_batch = n_iters.div_ceil(degree);
        let batch_size = iters_per_batch.saturating_mul(arity).max(arity);
        let batches: Vec<Vec<Value>> = items
            .chunks(batch_size)
            .map(|chunk| chunk.to_vec())
            .collect();
        let pre_shared_keys = self.shared_var_keys_snapshot();
        let locals_snapshot = self.locals.clone();
        type ThreadResult = (Result<Vec<Value>, RuntimeError>, Vec<Value>, String, String);
        let mut batch_results: Vec<ThreadResult> = Vec::with_capacity(batches.len());
        let collect = spec.collect;
        let mut handles = Vec::with_capacity(batches.len());
        for batch in batches {
            let mut vm = self.clone_for_thread();
            let task_code = code.clone();
            let task_spec = spec.clone();
            let task_fns = compiled_fns.clone();
            // The for-loop body is the enclosing frame's bytecode, so
            // GetLocal slots must exist. `clone_for_thread` starts a
            // fresh frame for `start {}` / Promise workers; copy the
            // current locals (and upvalues) so the body can run here.
            vm.locals.clone_from(&locals_snapshot);
            vm.upvalues.clone_from(&self.upvalues);
            // Joined hyper/race fan-out belongs on the elastic worker pool
            // (ADR-0020 §3.6), not on one fresh OS thread per batch.
            handles.push(crate::runtime::worker_pool::submit_joinable(move || {
                let run = crate::vm::guard_worker_panic(|| {
                    vm.exec_for_loop_body(
                        &task_code, &task_spec, &batch, body_start, loop_end, &task_fns, 0,
                    )?;
                    let collected = if collect {
                        match vm.stack.pop() {
                            Some(v) => match v.view() {
                                ValueView::Array(items, _) => items.to_vec(),
                                _ => vec![v],
                            },
                            None => Vec::new(),
                        }
                    } else {
                        Vec::new()
                    };
                    Ok(collected)
                });
                let wlocals = vm.locals.clone();
                let output = vm.take_output();
                let stderr = vm.take_stderr_output();
                (run, wlocals, output, stderr)
            }));
        }
        for handle in handles {
            let joined = crate::gc::block_quiescent(|| handle.join()).unwrap_or_else(|_| {
                (
                    Err(RuntimeError::new("thread panicked in race/hyper for")),
                    Vec::new(),
                    String::new(),
                    String::new(),
                )
            });
            batch_results.push(joined);
        }
        crate::gc::gc_safepoint(crate::gc::SafepointKind::ThreadJoin);
        let mut all_collected = Vec::with_capacity(items.len());
        let mut first_error: Option<RuntimeError> = None;
        for (batch_result, wlocals, output, stderr) in batch_results {
            self.emit_output(&output);
            self.emit_stderr(&stderr);
            match batch_result {
                Ok(vals) => {
                    if first_error.is_none() {
                        all_collected.extend(vals);
                    }
                }
                Err(e) => {
                    if first_error.is_none() {
                        first_error = Some(e);
                    }
                }
            }
            // Last-writer-wins merge of outer lexicals the body assigned
            // (`$saw = True if $*THREAD.id != $main`). Raku tells you not
            // to share mutable state in a hyper/race loop; this is enough
            // for the $*THREAD.id probe and similar flags.
            for (i, val) in wlocals.iter().enumerate() {
                if i < self.locals.len() && *val != locals_snapshot[i] {
                    self.locals[i] = val.clone();
                }
            }
        }
        self.sync_shared_vars_to_env();
        self.retain_shared_var_keys(&pre_shared_keys);
        if let Some(e) = first_error {
            return Err(e);
        }
        if collect {
            self.stack.push(Value::array(all_collected));
        }
        Ok(())
    }

    /// Parallel map/grep for HyperSeq/RaceSeq.
    /// Each item is processed in its own thread to support concurrent
    /// operations like `await` inside the map/grep block.
    pub(super) fn exec_hyper_race_map_grep(
        &mut self,
        items: &[Value],
        block: Value,
        is_map: bool,
        _is_hyper: bool,
    ) -> Result<Vec<Value>, RuntimeError> {
        if items.is_empty() {
            return Ok(Vec::new());
        }
        // For grep with non-callable arguments (Regex, Type, etc.),
        // fall back to sequential smartmatch-based grep.
        let is_callable = matches!(
            block.view(),
            ValueView::Sub(..)
                | ValueView::WeakSub(..)
                | ValueView::Routine { .. }
                | ValueView::Mixin(..)
        );
        if !is_map && !is_callable {
            return self.exec_hyper_grep_smartmatch(items, &block);
        }
        // Cap concurrency. For small lists (<=64 items), give each item its
        // own thread so that inter-item synchronization (e.g. Promises) works.
        // Larger lists use one worker per CPU (not a hard cap of 4, and not
        // a sequential fallback past 1000 items).
        // TODO: store batch/degree on HyperSeq/RaceSeq so user params are used.
        let degree = if items.len() <= 64 {
            items.len()
        } else {
            Self::hyper_worker_degree(items.len())
        };
        let batch_size = std::cmp::max(1, items.len().div_ceil(degree));
        let batches: Vec<Vec<Value>> = items
            .chunks(batch_size)
            .map(|chunk| chunk.to_vec())
            .collect();
        let num_batches = batches.len();
        // Snapshot the shared-var keys that exist *before* this op migrates the
        // parent's lexicals into `shared_vars` (each `clone_for_thread` below
        // copies the current env in). Because every batch thread is JOINED before
        // this method returns (unlike a detached `start`/Promise), the read-only
        // lexicals this op migrates — e.g. the map block's captured `@search-for`
        // — are dead once the op completes. `clone_for_thread` inserts with
        // `.or_insert_with`, so if left behind they shadow a *later* hyper op's
        // freshly-bound same-named lexical (thread-clone `@`/`%` reads prefer
        // `shared_vars`), freezing it at this op's value. We roll back the keys
        // this op added after joining + syncing dirty mutations back to env.
        let pre_shared_keys = self.shared_var_keys_snapshot();
        type ThreadResult = (
            crate::runtime::Interpreter,
            Result<Vec<Value>, RuntimeError>,
            String,
            String,
        );
        let mut handles: Vec<crate::runtime::worker_pool::TaskHandle<ThreadResult>> =
            Vec::with_capacity(num_batches);
        for batch in batches {
            let thread_interp = self.clone_for_thread();
            let block_clone = block.clone();
            let is_map_flag = is_map;
            // Pooled (ADR-0020 slice 3). Inter-batch synchronization (e.g.
            // Promises between items) still works: the submit-side starvation
            // check gives every batch its own worker, same concurrency as
            // thread-per-batch. Workers keep the large user-code stack.
            handles.push(crate::runtime::worker_pool::submit_joinable(move || {
                // CP-3 collapse: the cloned per-thread Interpreter *is* the Interpreter.
                let mut vm = thread_interp;
                let mut results = Vec::with_capacity(batch.len());
                let mut error: Option<RuntimeError> = None;
                for item in &batch {
                    if error.is_some() {
                        break;
                    }
                    // Route the per-item user-code call through the same
                    // panic->X::AdHoc boundary that `start{}`/Promise workers use
                    // (`guard_worker_panic`). Without it, a Rust panic raised by
                    // user code in this batch thread only surfaces as a generic
                    // "Thread panicked in hyper/race" at `join()` and leaks the raw
                    // Rust panic message to stderr; the guard converts it into a
                    // catchable X::AdHoc and suppresses the default backtrace dump,
                    // making worker panic handling uniform across all spawn sites.
                    let call_result = crate::vm::guard_worker_panic(|| {
                        vm.vm_call_on_value(block_clone.clone(), vec![item.clone()], None)
                    });
                    match call_result {
                        Ok(val) => {
                            if is_map_flag {
                                // A callback returning a finite lazy `.map`/
                                // `.grep` pipe must reify here — the wrapped
                                // HyperSeq's downstream `.flat`/`for` use static
                                // readers that cannot force a nested pipe.
                                match vm.reify_finite_pipe_value(val) {
                                    Ok(val) => {
                                        if let ValueView::Slip(s) = val.view() {
                                            results.extend(s.iter().cloned());
                                        } else {
                                            results.push(val);
                                        }
                                    }
                                    Err(e) => error = Some(e),
                                }
                            } else if val.truthy() {
                                results.push(item.clone());
                            }
                        }
                        Err(e) => {
                            error = Some(e);
                        }
                    }
                }
                let output = vm.take_output();
                let stderr = vm.take_stderr_output();
                let final_result = match error {
                    Some(e) => Err(e),
                    None => Ok(results),
                };
                (vm, final_result, output, stderr)
            }));
        }
        let mut all_results = Vec::with_capacity(items.len());
        let mut first_error: Option<RuntimeError> = None;
        for handle in handles {
            // STW-aware: blocked on a worker join = quiescent for the GC.
            let (thread_interp, batch_result, output, stderr) =
                crate::gc::block_quiescent(|| handle.join()).unwrap_or_else(|_| {
                    let interp = self.clone_for_thread();
                    (
                        interp,
                        Err(RuntimeError::new("Thread panicked in hyper/race")),
                        String::new(),
                        String::new(),
                    )
                });
            self.emit_output(&output);
            self.emit_stderr(&stderr);
            // Shared vars are synced through the shared Arc<RwLock<>>
            drop(thread_interp);
            match batch_result {
                Ok(results) => {
                    if first_error.is_none() {
                        all_results.extend(results);
                    }
                }
                Err(e) => {
                    if first_error.is_none() {
                        first_error = Some(e);
                    }
                }
            }
        }
        // GC safepoint (§9.2a `thread_join`): the hyper/race join-merge
        // boundary — every batch worker has joined, its results are owned here.
        crate::gc::gc_safepoint(crate::gc::SafepointKind::ThreadJoin);
        // Sync any shared variable updates from threads back to our env
        self.sync_shared_vars_to_env();
        // Roll back this op's ephemeral env->shared migrations (see
        // `pre_shared_keys`). Dirty mutations were just synced to env, so
        // dropping their shared entries is safe; pre-existing shared vars — and
        // any concurrent sibling's updates to them — are preserved (their keys
        // are in the pre-op snapshot). This keeps a later hyper op from reading a
        // stale value for a same-named but freshly-bound lexical.
        self.retain_shared_var_keys(&pre_shared_keys);
        if let Some(e) = first_error {
            return Err(e);
        }
        Ok(all_results)
    }

    /// Sequential grep using smartmatch for non-callable matchers.
    fn exec_hyper_grep_smartmatch(
        &mut self,
        items: &[Value],
        matcher: &Value,
    ) -> Result<Vec<Value>, RuntimeError> {
        let mut results = Vec::with_capacity(items.len());
        for item in items {
            if self.vm_smart_match(item, matcher) {
                results.push(item.clone());
            }
        }
        Ok(results)
    }
}
