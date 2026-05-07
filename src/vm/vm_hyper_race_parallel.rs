use super::*;

impl VM {
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
            block,
            Value::Sub(..) | Value::WeakSub(..) | Value::Routine { .. } | Value::Mixin(..)
        );
        if !is_map && !is_callable {
            return self.exec_hyper_grep_smartmatch(items, &block);
        }
        // Cap concurrency. For small lists (<=64 items), give each item its
        // own thread so that inter-item synchronization (e.g. Promises) works.
        // The caller ensures items.len() < 1000 before calling this method.
        // TODO: store batch/degree on HyperSeq/RaceSeq so user params are used.
        let degree = if items.len() <= 64 { items.len() } else { 4 };
        let batch_size = std::cmp::max(1, items.len().div_ceil(degree));
        let batches: Vec<Vec<Value>> = items
            .chunks(batch_size)
            .map(|chunk| chunk.to_vec())
            .collect();
        let num_batches = batches.len();
        type ThreadResult = (
            crate::runtime::Interpreter,
            Result<Vec<Value>, RuntimeError>,
            String,
            String,
        );
        let mut handles: Vec<std::thread::JoinHandle<ThreadResult>> =
            Vec::with_capacity(num_batches);
        for batch in batches {
            let thread_interp = self.interpreter.clone_for_thread();
            let block_clone = block.clone();
            let is_map_flag = is_map;
            handles.push(std::thread::spawn(move || {
                let mut vm = crate::vm::VM::new(thread_interp);
                let mut results = Vec::with_capacity(batch.len());
                let mut error: Option<RuntimeError> = None;
                for item in &batch {
                    if error.is_some() {
                        break;
                    }
                    match vm.vm_call_on_value(block_clone.clone(), vec![item.clone()], None) {
                        Ok(val) => {
                            if is_map_flag {
                                if let Value::Slip(ref s) = val {
                                    results.extend(s.iter().cloned());
                                } else {
                                    results.push(val);
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
                let output = vm.interpreter.take_output();
                let stderr = vm.interpreter.take_stderr_output();
                let final_result = match error {
                    Some(e) => Err(e),
                    None => Ok(results),
                };
                (vm.interpreter, final_result, output, stderr)
            }));
        }
        let mut all_results = Vec::with_capacity(items.len());
        let mut first_error: Option<RuntimeError> = None;
        for handle in handles {
            let (thread_interp, batch_result, output, stderr) =
                handle.join().unwrap_or_else(|_| {
                    let interp = self.interpreter.clone_for_thread();
                    (
                        interp,
                        Err(RuntimeError::new("Thread panicked in hyper/race")),
                        String::new(),
                        String::new(),
                    )
                });
            self.interpreter.emit_output(&output);
            self.interpreter.emit_stderr(&stderr);
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
        // Sync any shared variable updates from threads back to our env
        self.interpreter.sync_shared_vars_to_env();
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
