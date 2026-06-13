use super::*;

impl VM {
    /// Build a backtrace string from the interpreter's routine stack.
    /// Each frame is formatted as "  in sub <name> at <file> line <N>".
    ///
    /// Each pushed frame stores the call-site (the line/file in the *caller*
    /// where this function was invoked).  To display "where each frame was
    /// executing when it called the next", we shift by one:
    ///   - innermost frame (i=0): use current ?LINE/?FILE (the die/error line)
    ///   - frame i>0: use the *next inner* frame's stored call-site
    ///     (i.e. frame[i]'s displayed line = the line where frame[i] called
    ///     frame[i-1])
    ///   - <unit> (outermost): use the outermost routine frame's stored
    ///     call-site (where <unit> called the first function)
    pub(super) fn build_backtrace_string(&self) -> String {
        let stack = self.interpreter.routine_stack();
        let current_line = self.current_source_line();
        let current_file = self.current_source_file();
        // Build reversed list: stack[last] is innermost, stack[0] is outermost
        let reversed: Vec<_> = stack.iter().rev().collect();
        let mut lines = Vec::new();
        for (i, frame) in reversed.iter().enumerate() {
            let (line, file) = if i == 0 {
                // Innermost frame: use current ?LINE/?FILE
                (current_line, current_file.clone())
            } else {
                // Outer frame: the line where this frame called the next inner frame.
                // That info is stored in the next-inner frame's call-site.
                let inner_frame = reversed[i - 1];
                (inner_frame.line, inner_frame.file.clone())
            };
            let location = Self::format_location(file.as_deref(), line);
            if frame.name.is_empty() || frame.name == "<unit>" || frame.name == "<pointy-block>" {
                lines.push(format!("  in block <unit>{}", location));
            } else {
                lines.push(format!("  in sub {}{}", frame.name, location));
            }
        }
        // Add the <unit> frame at the bottom
        if stack.is_empty() {
            let location = Self::format_location(current_file.as_deref(), current_line);
            lines.push(format!("  in block <unit>{}", location));
        } else if !stack
            .first()
            .is_some_and(|f| f.name.is_empty() || f.name == "<unit>")
        {
            // The outermost routine frame's stored call-site is where
            // <unit> called it.
            let outermost = &stack[0];
            let location = Self::format_location(outermost.file.as_deref(), outermost.line);
            lines.push(format!("  in block <unit>{}", location));
        }
        lines.join("\n")
    }

    /// Build a structured Backtrace Value from the interpreter's routine stack.
    /// Returns a `Backtrace` instance whose `frames` attribute is a list of
    /// `Backtrace::Frame` instances (each with `.subname`, `.file`, `.line`)
    /// and whose `text` attribute is the formatted backtrace string.
    pub(super) fn build_backtrace_value(&self) -> Value {
        use crate::symbol::Symbol;
        use std::collections::HashMap;

        let stack = self.interpreter.routine_stack();
        let current_line = self.current_source_line();
        let current_file = self.current_source_file();
        let reversed: Vec<_> = stack.iter().rev().collect();

        let mut frames = Vec::new();
        let mut text_lines = Vec::new();

        for (i, frame) in reversed.iter().enumerate() {
            let (line, file) = if i == 0 {
                (current_line, current_file.clone())
            } else {
                let inner_frame = reversed[i - 1];
                (inner_frame.line, inner_frame.file.clone())
            };
            let subname = if frame.name.is_empty()
                || frame.name == "<unit>"
                || frame.name == "<pointy-block>"
            {
                "<unit>".to_string()
            } else {
                frame.name.clone()
            };

            let location = Self::format_location(file.as_deref(), line);
            if subname == "<unit>" {
                text_lines.push(format!("  in block <unit>{}", location));
            } else {
                text_lines.push(format!("  in sub {}{}", subname, location));
            }

            let mut frame_attrs = HashMap::new();
            frame_attrs.insert("subname".to_string(), Value::str(subname));
            frame_attrs.insert(
                "file".to_string(),
                file.map(Value::str).unwrap_or(Value::str(String::new())),
            );
            frame_attrs.insert(
                "line".to_string(),
                line.map(|l| Value::Int(l as i64)).unwrap_or(Value::Int(0)),
            );
            frames.push(Value::make_instance(
                Symbol::intern("Backtrace::Frame"),
                frame_attrs,
            ));
        }

        // Add <unit> frame at bottom if needed
        if stack.is_empty() {
            let location = Self::format_location(current_file.as_deref(), current_line);
            text_lines.push(format!("  in block <unit>{}", location));

            let mut frame_attrs = HashMap::new();
            frame_attrs.insert("subname".to_string(), Value::str("<unit>".to_string()));
            frame_attrs.insert(
                "file".to_string(),
                current_file
                    .clone()
                    .map(Value::str)
                    .unwrap_or(Value::str(String::new())),
            );
            frame_attrs.insert(
                "line".to_string(),
                current_line
                    .map(|l| Value::Int(l as i64))
                    .unwrap_or(Value::Int(0)),
            );
            frames.push(Value::make_instance(
                Symbol::intern("Backtrace::Frame"),
                frame_attrs,
            ));
        } else if !stack
            .first()
            .is_some_and(|f| f.name.is_empty() || f.name == "<unit>")
        {
            let outermost = &stack[0];
            let location = Self::format_location(outermost.file.as_deref(), outermost.line);
            text_lines.push(format!("  in block <unit>{}", location));

            let mut frame_attrs = HashMap::new();
            frame_attrs.insert("subname".to_string(), Value::str("<unit>".to_string()));
            frame_attrs.insert(
                "file".to_string(),
                outermost
                    .file
                    .clone()
                    .map(Value::str)
                    .unwrap_or(Value::str(String::new())),
            );
            frame_attrs.insert(
                "line".to_string(),
                outermost
                    .line
                    .map(|l| Value::Int(l as i64))
                    .unwrap_or(Value::Int(0)),
            );
            frames.push(Value::make_instance(
                Symbol::intern("Backtrace::Frame"),
                frame_attrs,
            ));
        }

        let text = text_lines.join("\n");
        let mut bt_attrs = HashMap::new();
        bt_attrs.insert("frames".to_string(), Value::array(frames));
        bt_attrs.insert("text".to_string(), Value::str(text));
        Value::make_instance(Symbol::intern("Backtrace"), bt_attrs)
    }

    /// Format a " at <file> line <N>" suffix for backtrace entries.
    fn format_location(file: Option<&str>, line: Option<u32>) -> String {
        match (file, line) {
            (Some(f), Some(l)) => format!(" at {} line {}", f, l),
            (Some(f), None) => format!(" at {}", f),
            (None, Some(l)) => format!(" at line {}", l),
            (None, None) => String::new(),
        }
    }

    /// Build a Backtrace Value from a pre-formatted backtrace string.
    /// Parses the string lines to extract frame info (best-effort).
    pub(super) fn backtrace_value_from_string(bt_str: &str) -> Value {
        use crate::symbol::Symbol;
        use std::collections::HashMap;

        let mut frames = Vec::new();
        for line in bt_str.lines() {
            let trimmed = line.trim();
            // Parse lines like "  in sub foo at file.raku line 5"
            // or "  in block <unit> at -e line 1"
            let subname;
            let rest;
            if let Some(after_sub) = trimmed.strip_prefix("in sub ") {
                if let Some(at_pos) = after_sub.find(" at ") {
                    subname = after_sub[..at_pos].to_string();
                    rest = &after_sub[at_pos..];
                } else {
                    subname = after_sub.to_string();
                    rest = "";
                }
            } else if let Some(after_block) = trimmed.strip_prefix("in block ") {
                if let Some(at_pos) = after_block.find(" at ") {
                    subname = after_block[..at_pos].to_string();
                    rest = &after_block[at_pos..];
                } else {
                    subname = after_block.to_string();
                    rest = "";
                }
            } else {
                continue;
            }

            let mut file = String::new();
            let mut line_no: i64 = 0;
            if let Some(at_rest) = rest.strip_prefix(" at ") {
                if let Some(line_pos) = at_rest.rfind(" line ") {
                    file = at_rest[..line_pos].to_string();
                    if let Ok(n) = at_rest[line_pos + 6..].parse::<i64>() {
                        line_no = n;
                    }
                } else {
                    file = at_rest.to_string();
                }
            }

            let mut frame_attrs = HashMap::new();
            frame_attrs.insert("subname".to_string(), Value::str(subname));
            frame_attrs.insert("file".to_string(), Value::str(file));
            frame_attrs.insert("line".to_string(), Value::Int(line_no));
            frames.push(Value::make_instance(
                Symbol::intern("Backtrace::Frame"),
                frame_attrs,
            ));
        }

        let mut bt_attrs = HashMap::new();
        bt_attrs.insert("frames".to_string(), Value::array(frames));
        bt_attrs.insert("text".to_string(), Value::str(bt_str.to_string()));
        Value::make_instance(Symbol::intern("Backtrace"), bt_attrs)
    }

    /// If the value is a `LazyIoLines`, force it into an eager array by reading
    /// all remaining lines from the file handle. Otherwise return the value as-is.
    pub(super) fn force_if_lazy_io_lines(&mut self, val: Value) -> Result<Value, RuntimeError> {
        if let Value::LazyIoLines {
            ref handle,
            kv,
            words,
        } = val
        {
            let forced = self.interpreter.force_lazy_io_lines(handle, words)?;
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
            "Seq" | "List" | "list" | "cache" | "values" | "lazy"
        )
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

    /// Force a LazyList by running its compiled bytecode in the VM.
    /// Falls back to interpreter if no compiled code is available.
    pub(super) fn force_lazy_list_vm(
        &mut self,
        list: &LazyList,
    ) -> Result<Vec<Value>, RuntimeError> {
        // Handle scan-based lazy lists: compute elements on demand
        if list.scan_spec.is_some() {
            return self.force_scan_lazy_list(list, 200_000);
        }

        // A lazy map/grep pipeline is rooted at an infinite source, so a full
        // (strict) force can never terminate. Match raku and throw
        // X::Cannot::Lazy rather than spinning forever. Methods that know their
        // own name (e.g. `.elems`/`.sort`) raise a more specific message at the
        // dispatch site before reaching here.
        if list.lazy_pipe.is_some() {
            return Err(RuntimeError::typed_msg(
                "X::Cannot::Lazy",
                "Cannot coerce an infinite lazy list to a strict list",
            ));
        }

        // For sequence-spec lazy lists, return current cache (infinite lists
        // are never fully materialized)
        if list.sequence_spec.is_some() {
            let cache = list.cache.lock().unwrap();
            return Ok(cache.as_ref().cloned().unwrap_or_default());
        }

        // Check cache first
        if let Some(cached) = list.cache.lock().unwrap().clone() {
            return Ok(cached);
        }

        // If no compiled code, fall back to interpreter
        let (cc, fns) = match (&list.compiled_code, &list.compiled_fns) {
            (Some(cc), Some(fns)) => (cc.clone(), fns.clone()),
            _ => return self.interpreter.force_lazy_list_bridge(list),
        };

        // Save current VM state. Locals are kept coherent with env by
        // write-through (`flush_local_to_env`), so no explicit flush is needed
        // here; we restore locals directly on return.
        crate::vm::vm_stats::record_clone_env();
        let saved_env = self.interpreter.clone_env();
        let saved_locals = std::mem::take(&mut self.locals);
        let saved_stack = std::mem::take(&mut self.stack);
        let saved_env_dirty = self.env_dirty;

        // Set up the lazy list's environment as a scoped overlay's parent: the
        // gather body reads its captured lexicals through to `list.env` and its
        // own writes land in a fresh born-owned overlay (no fork of `list.env`).
        // The merge below iterates the overlay (overlay-only) = the body's writes.
        // See docs/vm-dual-store.md (Slice 6).
        *self.interpreter.env_mut() = crate::env::Env::scoped_child(list.env.flattened());

        // Push gather items collector
        let saved_gather_len = self.interpreter.gather_items_len();
        self.interpreter.push_gather_items(Vec::new());
        self.interpreter.push_gather_take_limit(None);

        // Initialize locals for the compiled code
        self.locals = vec![Value::Nil; cc.locals.len()];
        for (i, name) in cc.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(name) {
                self.locals[i] = val.clone();
            }
        }
        self.env_dirty = false;
        self.stack = Vec::new();

        // Run the compiled code using the lazy list's own compiled_fns.
        // Outer scope subs are available via the env as Value::Sub.
        let run_fns = fns.as_ref();

        let mut ip = 0;
        let mut run_result = Ok(());
        while ip < cc.ops.len() {
            match self.exec_one(&cc, &mut ip, run_fns) {
                Ok(()) => {}
                Err(e) if e.is_warn => {
                    if !self.interpreter.warning_suppressed() {
                        self.interpreter.write_warn_to_stderr(&e.message);
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
            if self.interpreter.is_halted() {
                break;
            }
        }

        // Collect gather items
        let items = self.interpreter.pop_gather_items().unwrap_or_default();
        self.interpreter.pop_gather_take_limit();

        // Clean up extra gather items if needed
        while self.interpreter.gather_items_len() > saved_gather_len {
            self.interpreter.pop_gather_items();
            self.interpreter.pop_gather_take_limit();
        }

        // Sync locals back to env before reading the result environment.
        // During VM execution, variable assignments go to self.locals, not
        // to the interpreter env. We must flush them so the merge logic below
        // can see the changes made by the gather body.
        for (i, name) in cc.locals.iter().enumerate() {
            self.interpreter
                .env_mut()
                .insert(name.clone(), self.locals[i].clone());
        }

        // Restore the outer environment, selectively merging changes from
        // the gather body. Only propagate variables that:
        // 1. Existed in the outer scope, AND
        // 2. Were actually modified during gather body execution
        //    (i.e., their value changed from the gather body's initial env).
        // This prevents nested gather closures from corrupting each other's
        // captured variables (e.g., `$n` in nested grep-div calls), while
        // still propagating genuine side effects (e.g., `$x += 1`).
        let gather_result_env = self.interpreter.env().clone();
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
        *self.interpreter.env_mut() = merged_env;

        // Check whether the merged env actually changed any outer-scope variables.
        let env_actually_changed = {
            let merged = self.interpreter.env();
            saved_env.iter().any(|(k, old_val)| {
                merged
                    .get_sym(*k)
                    .is_some_and(|new_val| new_val.to_string_value() != old_val.to_string_value())
            })
        };

        // Restore VM state
        self.locals = saved_locals;
        self.stack = saved_stack;
        self.env_dirty = if env_actually_changed {
            true
        } else {
            saved_env_dirty
        };

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
        // Check cache first
        {
            let cache = list.cache.lock().unwrap();
            if let Some(cached) = cache.as_ref()
                && cached.len() >= needed
            {
                return Ok(cached[..needed].to_vec());
            }
        }

        // For sequence-spec lazy lists, generate more elements on demand
        if let Some(ref spec) = list.sequence_spec {
            return Self::extend_sequence_cache(list, spec, needed);
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
                return self.interpreter.force_lazy_list_prefix_bridge(list, needed);
            }
        };

        // Save current VM state
        crate::vm::vm_stats::record_clone_env();
        let saved_env = self.interpreter.clone_env();
        let saved_locals = std::mem::take(&mut self.locals);
        let saved_stack = std::mem::take(&mut self.stack);
        let saved_env_dirty = self.env_dirty;

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
                *self.interpreter.env_mut() = coro.env.clone();
                self.gather_for_loop_resume = coro.for_loop_resume.clone();
                has_prior_state = true;
            } else {
                // Fresh start (scoped overlay over the gather's captured env; see
                // docs/vm-dual-store.md Slice 6). On suspend the scoped env is
                // saved into the coroutine state and restored on resume (clone
                // preserves overlay+parent+tombstones), so the body's writes
                // accumulate across takes without forking `list.env`.
                ip = 0;
                *self.interpreter.env_mut() = crate::env::Env::scoped_child(list.env.flattened());
                self.locals = vec![Value::Nil; cc.locals.len()];
                for (i, name) in cc.locals.iter().enumerate() {
                    if let Some(val) = self.interpreter.env().get(name) {
                        self.locals[i] = val.clone();
                    }
                }
                self.stack = Vec::new();
                has_prior_state = false;
            }
        } else {
            // Fresh start (no coroutine slot yet)
            ip = 0;
            *self.interpreter.env_mut() = crate::env::Env::scoped_child(list.env.flattened());
            self.locals = vec![Value::Nil; cc.locals.len()];
            for (i, name) in cc.locals.iter().enumerate() {
                if let Some(val) = self.interpreter.env().get(name) {
                    self.locals[i] = val.clone();
                }
            }
            self.stack = Vec::new();
            has_prior_state = false;
        }

        self.env_dirty = false;

        // Push gather items collector with the take limit
        let saved_gather_len = self.interpreter.gather_items_len();

        // If resuming, restore already-cached items into the gather collector
        // so that the take_value limit check accounts for them.
        let _already_cached = if has_prior_state {
            let cache = list.cache.lock().unwrap();
            let items = cache.as_ref().cloned().unwrap_or_default();
            let len = items.len();
            self.interpreter.push_gather_items(items);
            len
        } else {
            self.interpreter.push_gather_items(Vec::new());
            0
        };
        self.interpreter.push_gather_take_limit(Some(needed));

        // Run the compiled code
        let run_fns = fns.as_ref();
        let mut run_result = Ok(());
        let mut body_finished = false;

        while ip < cc.ops.len() {
            match self.exec_one(&cc, &mut ip, run_fns) {
                Ok(()) => {}
                Err(e) if e.is_warn => {
                    if !self.interpreter.warning_suppressed() {
                        self.interpreter.write_warn_to_stderr(&e.message);
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
            if self.interpreter.is_halted() {
                break;
            }
        }

        // If we exited the loop normally (ip >= cc.ops.len()), body is finished
        if run_result.is_ok() && ip >= cc.ops.len() {
            body_finished = true;
        }

        // Collect gather items
        let items = self.interpreter.pop_gather_items().unwrap_or_default();
        self.interpreter.pop_gather_take_limit();

        while self.interpreter.gather_items_len() > saved_gather_len {
            self.interpreter.pop_gather_items();
            self.interpreter.pop_gather_take_limit();
        }

        // Sync locals back to env
        for (i, name) in cc.locals.iter().enumerate() {
            self.interpreter
                .env_mut()
                .insert(name.clone(), self.locals[i].clone());
        }

        // Save coroutine state before restoring outer env
        if !body_finished && run_result.is_ok() {
            let for_loop_resume = self.gather_for_loop_resume.take();
            let coro_state = GatherCoroutineState {
                ip,
                locals: self.locals.clone(),
                stack: self.stack.clone(),
                env: self.interpreter.env().clone(),
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
        let gather_result_env = self.interpreter.env().clone();
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
        *self.interpreter.env_mut() = merged_env;

        let env_actually_changed = {
            let merged = self.interpreter.env();
            saved_env.iter().any(|(k, old_val)| {
                merged
                    .get_sym(*k)
                    .is_some_and(|new_val| new_val.to_string_value() != old_val.to_string_value())
            })
        };

        // Restore VM state
        self.locals = saved_locals;
        self.stack = saved_stack;
        self.env_dirty = if env_actually_changed {
            true
        } else {
            saved_env_dirty
        };

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
    pub(super) fn force_lazy_pipe(
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
            let (source, func, is_grep, source_idx) = {
                let spec = list.lazy_pipe.as_ref().unwrap().lock().unwrap();
                (
                    spec.source.clone(),
                    spec.func.clone(),
                    spec.is_grep,
                    spec.source_idx,
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
                Some(elem) => {
                    // Apply the stage with VM-native dispatch so the callback
                    // runs in *this* VM (keeping locals/env coherent and letting
                    // side effects reach the enclosing scope). A `grep` keeps the
                    // element when the matcher is truthy; a `map` transforms it (a
                    // `Slip` result contributes multiple elements).
                    let produced: Vec<Value> = if is_grep {
                        if self.vm_grep_item_matches(&func, &elem)? {
                            vec![elem]
                        } else {
                            Vec::new()
                        }
                    } else {
                        match self.vm_call_on_value(func, vec![elem], None)? {
                            Value::Slip(items) => items.as_ref().to_vec(),
                            v => vec![v],
                        }
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

    /// Pull the `idx`-th element of a pipeline source, or `None` when the source
    /// has fewer than `idx + 1` elements (finite source exhausted). Infinite
    /// integer ranges always produce. Nested lazy pipelines / gathers are pulled
    /// incrementally via [`Self::force_lazy_list_vm_n`].
    fn pull_source_element(
        &mut self,
        source: &Value,
        idx: usize,
    ) -> Result<Option<Value>, RuntimeError> {
        match source {
            Value::Range(a, b)
            | Value::RangeExcl(a, b)
            | Value::RangeExclStart(a, b)
            | Value::RangeExclBoth(a, b) => {
                let start = match source {
                    Value::RangeExclStart(..) | Value::RangeExclBoth(..) => a.saturating_add(1),
                    _ => *a,
                };
                let inclusive = matches!(source, Value::Range(..) | Value::RangeExclStart(..));
                let cur = match start.checked_add(idx as i64) {
                    Some(v) => v,
                    None => return Ok(None),
                };
                let in_bounds = if inclusive { cur <= *b } else { cur < *b };
                if in_bounds {
                    Ok(Some(Value::Int(cur)))
                } else {
                    Ok(None)
                }
            }
            Value::Seq(items) | Value::Slip(items) => Ok(items.get(idx).cloned()),
            Value::Array(items, _) => Ok(items.get(idx).cloned()),
            Value::LazyList(ll) => {
                let items = self.force_lazy_list_vm_n(ll, idx + 1)?;
                Ok(items.get(idx).cloned())
            }
            // Other sources (GenericRange, etc.) are not gated into the lazy
            // pipeline; materialize once and index.
            other => {
                let items = crate::runtime::value_to_list(other);
                Ok(items.get(idx).cloned())
            }
        }
    }

    /// Force a LazyList into a Seq by evaluating the gather body.
    /// Force a scan-based LazyList, computing up to `needed` elements.
    /// Elements are computed incrementally and cached in the LazyList.
    pub(super) fn force_scan_lazy_list(
        &mut self,
        list: &LazyList,
        needed: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        let scan_mutex = match &list.scan_spec {
            Some(s) => s,
            None => return Ok(Vec::new()),
        };

        // Read current state under lock, then release before calling reduction methods
        let (base_op, negate, source, mut acc, already, cached_len) = {
            let spec = scan_mutex.lock().unwrap();
            let cache_guard = list.cache.lock().unwrap();
            let cached_len = cache_guard.as_ref().map_or(0, |v| v.len());
            if cached_len >= needed {
                return Ok(cache_guard.as_ref().unwrap()[..needed].to_vec());
            }
            (
                spec.op.clone(),
                spec.negate,
                spec.source.clone(),
                spec.accumulator.clone(),
                spec.computed_count,
                cached_len,
            )
        };

        let callable = self.reduction_callable_for_op(&base_op);
        let remaining = needed - cached_len;

        // Collect new source values to iterate over
        let new_values: Vec<Value> = match &source {
            Value::Range(a, b) => {
                let start = *a + already as i64;
                let end = if *b == i64::MAX {
                    *a + needed as i64
                } else {
                    *b
                };
                (start..=end).take(remaining).map(Value::Int).collect()
            }
            Value::RangeExcl(a, b) => {
                let start = *a + already as i64;
                let end = if *b == i64::MAX {
                    *a + needed as i64
                } else {
                    *b
                };
                (start..end).take(remaining).map(Value::Int).collect()
            }
            Value::RangeExclStart(a, b) => {
                let first = *a + 1;
                let start = first + already as i64;
                let end = if *b == i64::MAX {
                    first + needed as i64
                } else {
                    *b
                };
                (start..=end).take(remaining).map(Value::Int).collect()
            }
            Value::RangeExclBoth(a, b) => {
                let first = *a + 1;
                let start = first + already as i64;
                let end = if *b == i64::MAX {
                    first + needed as i64
                } else {
                    *b
                };
                (start..end).take(remaining).map(Value::Int).collect()
            }
            Value::GenericRange {
                start,
                end,
                excl_start,
                ..
            } => {
                let end_f = end.to_f64();
                let is_infinite = end_f.is_infinite() && end_f.is_sign_positive();
                let start_i = start.as_ref().to_f64() as i64;
                let first_i = if *excl_start { start_i + 1 } else { start_i };
                let iter_start = first_i + already as i64;
                let iter_end = if is_infinite {
                    iter_start + remaining as i64
                } else {
                    (end_f as i64).min(iter_start + remaining as i64)
                };
                (iter_start..=iter_end)
                    .take(remaining)
                    .map(Value::Int)
                    .collect()
            }
            _ => {
                let items = crate::runtime::utils::value_to_list(&source);
                items.into_iter().skip(already).take(remaining).collect()
            }
        };

        // Compute new scan elements (no locks held)
        let mut new_out: Vec<Value> = Vec::new();
        let mut computed = already;

        for val in new_values {
            acc = Some(match acc.take() {
                None => {
                    new_out.push(val.clone());
                    val
                }
                Some(prev) => {
                    let call_args = vec![prev, val];
                    let v =
                        self.reduction_step_with_args(&base_op, callable.as_ref(), call_args)?;
                    let v = if negate { Value::Bool(!v.truthy()) } else { v };
                    new_out.push(v.clone());
                    v
                }
            });
            computed += 1;
        }

        // Update spec and cache under lock
        {
            let mut spec = scan_mutex.lock().unwrap();
            spec.accumulator = acc;
            spec.computed_count = computed;

            let mut cache_guard = list.cache.lock().unwrap();
            let out = cache_guard.get_or_insert_with(Vec::new);
            out.extend(new_out);

            if out.len() >= needed {
                Ok(out[..needed].to_vec())
            } else {
                Ok(out.clone())
            }
        }
    }

    fn force_lazy_if_needed(&mut self, val: Value) -> Result<Value, RuntimeError> {
        if let Value::LazyList(ll) = &val {
            let items = self.force_lazy_list_vm(ll)?;
            Ok(Value::Seq(std::sync::Arc::new(items.into())))
        } else {
            Ok(val)
        }
    }

    pub(super) fn eval_binary_with_junctions(
        &mut self,
        left: Value,
        right: Value,
        f: fn(&mut VM, Value, Value) -> Result<Value, RuntimeError>,
    ) -> Result<Value, RuntimeError> {
        // Auto-FETCH Proxy containers in binary operations
        let left = self.interpreter.auto_fetch_proxy(&left)?;
        let right = self.interpreter.auto_fetch_proxy(&right)?;
        // Decontainerize Scalar wrappers
        let left = left.descalarize().clone();
        let right = right.descalarize().clone();
        if let (
            Value::Junction {
                kind: left_kind,
                values: _,
            },
            Value::Junction {
                kind: right_kind,
                values: right_values,
            },
        ) = (&left, &right)
            && Self::thread_right_first(left_kind, right_kind)
        {
            let results: Result<Vec<Value>, RuntimeError> = right_values
                .iter()
                .cloned()
                .map(|v| self.eval_binary_with_junctions(left.clone(), v, f))
                .collect();
            return Ok(Value::junction(right_kind.clone(), results?));
        }
        if let Value::Junction { kind, values } = left {
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .cloned()
                .map(|v| self.eval_binary_with_junctions(v, right.clone(), f))
                .collect();
            return Ok(Value::junction(kind, results?));
        }
        if let Value::Junction { kind, values } = right {
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .cloned()
                .map(|v| self.eval_binary_with_junctions(left.clone(), v, f))
                .collect();
            return Ok(Value::junction(kind, results?));
        }
        // Force LazyList values before arithmetic/comparison operations
        let left = self.force_lazy_if_needed(left)?;
        let right = self.force_lazy_if_needed(right)?;
        f(self, left, right)
    }

    /// Smartmatch with junction threading but WITHOUT forcing lazy values.
    /// For `!~~` (negate=true), we compute `~~` first and then negate the
    /// collapsed result.  Raku defines `$x !~~ $y` as `not ($x ~~ $y)`,
    /// where `not` collapses junctions before negating.
    #[allow(dead_code)]
    pub(super) fn eval_smartmatch_with_junctions(
        &mut self,
        left: Value,
        right: Value,
        negate: bool,
    ) -> Result<Value, RuntimeError> {
        self.eval_smartmatch_with_junctions_ex(left, right, negate, false)
    }

    /// Extended smartmatch with junction threading.
    /// `rhs_is_match_regex` indicates the RHS was originally `m//`, which
    /// changes the failure return from Nil to False.
    pub(super) fn eval_smartmatch_with_junctions_ex(
        &mut self,
        left: Value,
        right: Value,
        negate: bool,
        rhs_is_match_regex: bool,
    ) -> Result<Value, RuntimeError> {
        // For !~~, compute ~~ first, then negate the collapsed boolean.
        if negate {
            let match_result =
                self.eval_smartmatch_with_junctions_ex(left, right, false, rhs_is_match_regex)?;
            let bool_val = match_result.truthy();
            return Ok(Value::Bool(!bool_val));
        }
        // When RHS is the Junction type object, don't auto-thread LHS.
        // $junction ~~ Junction should return True (a Junction isa Junction).
        // Also applies to Mu (the supertype of Junction).
        if matches!(&right, Value::Package(name) if matches!(name.resolve().as_str(), "Junction" | "Mu"))
            && matches!(&left, Value::Junction { .. })
        {
            return self.smart_match_op(left, right, rhs_is_match_regex);
        }
        // Helper: check if a value is a regex (for junction collapse decisions)
        let is_regex_value = |v: &Value| {
            matches!(
                v,
                Value::Regex(_)
                    | Value::RegexWithAdverbs { .. }
                    | Value::Routine { is_regex: true, .. }
            )
        };
        if let (
            Value::Junction {
                kind: left_kind,
                values: _,
            },
            Value::Junction {
                kind: right_kind,
                values: right_values,
            },
        ) = (&left, &right)
            && Self::thread_right_first(left_kind, right_kind)
        {
            let results: Result<Vec<Value>, RuntimeError> = right_values
                .iter()
                .cloned()
                .map(|v| {
                    self.eval_smartmatch_with_junctions_ex(
                        left.clone(),
                        v,
                        false,
                        rhs_is_match_regex,
                    )
                })
                .collect();
            // Smartmatch collapses junctions to Bool
            let junction = Value::junction(right_kind.clone(), results?);
            return Ok(Value::Bool(junction.truthy()));
        }
        if let Value::Junction { kind, values } = left {
            // When RHS is a non-junction regex and LHS is a junction,
            // return the Junction of Match/Nil results without collapsing.
            // For all other cases, collapse to Bool.
            let keep_junction = is_regex_value(&right);
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .cloned()
                .map(|v| {
                    self.eval_smartmatch_with_junctions_ex(
                        v,
                        right.clone(),
                        false,
                        rhs_is_match_regex,
                    )
                })
                .collect();
            let junction = Value::junction(kind, results?);
            if keep_junction {
                return Ok(junction);
            }
            return Ok(Value::Bool(junction.truthy()));
        }
        if let Value::Junction { kind, values } = right {
            // Evaluate junction elements with short-circuit semantics:
            // All: if any element is False, stop early (don't evaluate remaining).
            // Any: if any element is True, stop early.
            // One: always evaluate all (no short-circuit possible).
            let mut results = Vec::with_capacity(values.len());
            for v in values.iter().cloned() {
                let r = self.eval_smartmatch_with_junctions_ex(
                    left.clone(),
                    v,
                    false,
                    rhs_is_match_regex,
                )?;
                let is_truthy = r.truthy();
                results.push(r);
                match &kind {
                    crate::value::JunctionKind::All if !is_truthy => break, // short-circuit: All fails fast
                    crate::value::JunctionKind::Any if is_truthy => break, // short-circuit: Any succeeds fast
                    _ => {}
                }
            }
            // Smartmatch collapses junctions to Bool
            let junction = Value::junction(kind, results);
            return Ok(Value::Bool(junction.truthy()));
        }
        self.smart_match_op(left, right, rhs_is_match_regex)
    }

    pub(super) fn smart_match_op(
        &mut self,
        left: Value,
        right: Value,
        rhs_is_match_regex: bool,
    ) -> Result<Value, RuntimeError> {
        // When RHS is Whatever, autoprime: return a WhateverCode that takes
        // one argument and smartmatches LHS against it.
        // In Raku, `$x ~~ *` produces `-> $a { $x ~~ $a }`.
        if matches!(&right, Value::Whatever) {
            use crate::ast::{Expr, Stmt};
            use crate::env::Env;
            let mut env = Env::new();
            env.insert(
                "__mutsu_callable_type".to_string(),
                Value::str_from("WhateverCode"),
            );
            // Capture the LHS value in the closure environment
            env.insert("__wc_sm_lhs".to_string(), left);
            let param = "__wc_0".to_string();
            let body = vec![Stmt::Expr(Expr::Binary {
                left: Box::new(Expr::Var("__wc_sm_lhs".to_string())),
                op: crate::token_kind::TokenKind::SmartMatch,
                right: Box::new(Expr::Var(param.clone())),
            })];
            return Ok(Value::make_sub(
                Symbol::intern("GLOBAL"),
                Symbol::intern("<whatevercode-smartmatch>"),
                vec![param],
                Vec::new(),
                body,
                false,
                env,
            ));
        }
        let is_regex = matches!(
            &right,
            Value::Regex(_)
                | Value::RegexWithAdverbs { .. }
                | Value::Routine { is_regex: true, .. }
        );
        let matched = self.vm_smart_match(&left, &right);
        // Check for pending regex security error (set by regex parse/match)
        if let Some(err) = crate::runtime::Interpreter::take_pending_regex_error() {
            return Err(err);
        }
        // Check for pending dispatch error (e.g., from Any ~~ Pair method call)
        if let Some(err) = self.interpreter.take_pending_dispatch_error() {
            return Err(err);
        }
        if is_regex {
            // When $/ is a Junction (from :nth with junction argument),
            // the ~~ operator collapses the result to a Bool.
            let slash = self
                .interpreter
                .env()
                .get("/")
                .cloned()
                .unwrap_or(Value::Nil);
            if matches!(&slash, Value::Junction { .. }) {
                Ok(Value::Bool(matched))
            } else if matched {
                // For regex smartmatch, return the Match object (from $/) or Nil
                Ok(slash)
            } else if rhs_is_match_regex && matches!(&slash, Value::Nil) {
                // Failed m// (non-global) returns False, not Nil.
                // But m:g// returns an empty list from $/, so we check that
                // $/ is Nil before returning False.
                Ok(Value::Bool(false))
            } else {
                // Failed bare // returns Nil; m:g// returns $/ (empty list)
                Ok(slash)
            }
        } else {
            Ok(Value::Bool(matched))
        }
    }

    /// Resolve bound-element sentinels inside an Array value.
    ///
    /// When an array element has been bound (`@a[1] := $var`), mutsu stores a
    /// sentinel `Pair("__mutsu_bound_array_ref", var_name)` in the underlying
    /// Vec.  This method replaces every such sentinel with the current value of
    /// the referenced variable, so that callers that iterate elements (e.g.
    /// stringification, `say`, `gist`) see the live value instead of the
    /// internal marker.
    pub(super) fn resolve_bound_array_elements(&self, val: Value) -> Value {
        use super::vm_var_ops::BOUND_ARRAY_REF_SENTINEL;
        if let Value::Array(ref items, kind) = val {
            let needs_resolve = items
                .iter()
                .any(|v| matches!(v, Value::Pair(name, _) if name == BOUND_ARRAY_REF_SENTINEL));
            if !needs_resolve {
                return val;
            }
            let resolved: Vec<Value> = items
                .iter()
                .map(|v| match v {
                    Value::Pair(name, source) if name == BOUND_ARRAY_REF_SENTINEL => {
                        let source_name = source.to_string_value();
                        self.interpreter
                            .env()
                            .get(&source_name)
                            .cloned()
                            .unwrap_or(Value::Nil)
                    }
                    other => other.clone(),
                })
                .collect();
            Value::Array(std::sync::Arc::new(resolved.into()), kind)
        } else {
            val
        }
    }

    /// Extend a sequence-spec lazy list's cache to at least `needed` elements.
    /// This generates new elements using the sequence spec (arithmetic/geometric)
    /// without needing any VM or interpreter context.
    fn extend_sequence_cache(
        list: &LazyList,
        spec: &crate::value::SequenceSpec,
        needed: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        let mut cache = list.cache.lock().unwrap();
        let items = cache.get_or_insert_with(Vec::new);
        if items.len() >= needed {
            return Ok(items[..needed].to_vec());
        }
        // Generate more elements
        while items.len() < needed {
            let last = items.last().cloned().unwrap_or(Value::Int(0));
            let next = match spec {
                crate::value::SequenceSpec::Arithmetic { step, all_int } => {
                    if *all_int {
                        if let Value::Int(n) = last {
                            Value::Int(n + step)
                        } else {
                            let n = last.to_f64();
                            Value::Num(n + *step as f64)
                        }
                    } else {
                        let n = last.to_f64();
                        Value::Num(n + *step as f64)
                    }
                }
                crate::value::SequenceSpec::GeometricRat { num, den } => {
                    if let Value::Int(n) = last {
                        let result = n * num;
                        if result % den == 0 {
                            Value::Int(result / den)
                        } else {
                            Value::Rat(result * num, *den)
                        }
                    } else {
                        let n = last.to_f64();
                        Value::Num(n * (*num as f64) / (*den as f64))
                    }
                }
                crate::value::SequenceSpec::Geometric { ratio } => {
                    let n = last.to_f64();
                    Value::Num(n * ratio)
                }
            };
            items.push(next);
        }
        Ok(items[..needed].to_vec())
    }
}
