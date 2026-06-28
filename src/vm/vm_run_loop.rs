use super::*;

impl Interpreter {
    fn wrap_in_begin_time(inner: RuntimeError) -> RuntimeError {
        use std::collections::HashMap;
        let inner_exception = inner
            .exception
            .as_ref()
            .map(|e| e.as_ref().clone())
            .unwrap_or_else(|| Value::str(inner.message.clone()));
        let msg = format!(
            "An exception occurred while evaluating a CHECK\nException details:\n  {}",
            inner.message
        );
        let mut attrs = HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        attrs.insert("exception".to_string(), inner_exception);
        RuntimeError::typed("X::Comp::BeginTime", attrs)
    }

    fn validate_labels(code: &CompiledCode) -> Result<(), RuntimeError> {
        let mut seen: HashSet<String> = HashSet::new();
        for op in &code.ops {
            if let OpCode::Label(name_idx) = op {
                let label_name = Self::const_str(code, *name_idx);
                if !seen.insert(label_name.to_string()) {
                    return Err(RuntimeError::new(format!(
                        "X::Redeclaration: Label '{}' already declared",
                        label_name
                    )));
                }
            }
        }
        Ok(())
    }

    /// Run the compiled bytecode. Always returns the interpreter back
    /// (even on error) so the caller can restore it.
    ///
    /// The actual exec loop runs inside a `catch_unwind` boundary so that a Rust
    /// `panic!`/`unwrap`/index-OOB/capacity-overflow triggered by user code is
    /// converted into a catchable `X::AdHoc` `RuntimeError` (exit 1, flows through
    /// `try`/`CATCH`) instead of crashing the whole process (exit 101). This also
    /// covers EVAL and sub-VMs, which run through `Interpreter::run`. Stack overflow
    /// `abort`s rather than unwinding, so it is out of scope here.
    pub(crate) fn run_top(
        &mut self,
        code: &CompiledCode,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Option<Value>, RuntimeError> {
        self.run_inner_guarded(code, compiled_fns)
    }

    /// Run `run_inner` inside the `catch_unwind` panic->`X::AdHoc` boundary so a
    /// Rust `panic!`/overflow/index-OOB triggered by user code becomes a
    /// catchable `RuntimeError` instead of crashing the process. This is the
    /// boundary the old ping-pong `Interpreter::run` provided; both `run_top` (outermost)
    /// and `run_nested` (re-entrant carriers like `run_compiled_block`,
    /// `eval_block_value`, `run_block_raw` invoked by `dies-ok`/`try`) route
    /// through it so nested user-code panics still flow through try/CATCH.
    fn run_inner_guarded(
        &mut self,
        code: &CompiledCode,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Option<Value>, RuntimeError> {
        install_vm_panic_hook();
        // Save/restore the flag so nested boundaries (EVAL, sub-VMs) don't
        // clobber an outer boundary's state.
        let prev = IN_VM_PANIC_BOUNDARY.with(|f| f.replace(true));
        let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            self.run_inner(code, compiled_fns)
        }));
        IN_VM_PANIC_BOUNDARY.with(|f| f.set(prev));
        match caught {
            Ok(r) => r,
            Err(payload) => Err(Self::vm_panic_error(panic_payload_message(
                payload.as_ref(),
            ))),
        }
    }

    /// Build a catchable `X::AdHoc` error from a caught panic message.
    pub(crate) fn vm_panic_error(message: String) -> RuntimeError {
        let message = format!("Internal error: {message}");
        let mut err = RuntimeError::new(message.clone());
        let mut attrs = HashMap::new();
        attrs.insert("message".to_string(), Value::str(message.clone()));
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::AdHoc"),
            attrs,
        )));
        err
    }

    /// The exec loop, borrowing `&mut self` so the `catch_unwind` closure in
    /// `run` does not move `self.interpreter` out (the caller must always get the
    /// interpreter back, even on panic).
    fn run_inner(
        &mut self,
        code: &CompiledCode,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Option<Value>, RuntimeError> {
        Self::validate_labels(code)?;
        // Initialize local variable slots
        self.locals = vec![Value::Nil; code.locals.len()];
        for (i, name) in code.locals.iter().enumerate() {
            if let Some(val) = self.env().get(name) {
                self.locals[i] = val.clone();
            }
        }
        self.load_state_locals(code);
        let root_once_scope = self.next_once_scope_id();
        self.push_once_scope(root_once_scope);
        let mut ip = 0;
        while ip < code.ops.len() {
            if let Err(e) = self.exec_one(code, &mut ip, compiled_fns) {
                if e.is_goto()
                    && let Some(label) = e.label.as_deref()
                    && let Some(target_ip) = self.find_label_target(code, label)
                {
                    ip = target_ip;
                    continue;
                }
                if e.is_warn() && self.control_handler_depth == 0 {
                    if !self.warning_suppressed() {
                        self.write_warn_to_stderr(&e.message);
                    }
                    if let Some(v) = e.return_value {
                        self.stack.push(v);
                    }
                    ip += 1;
                    continue;
                }
                self.sync_state_locals(code);
                self.pop_once_scope();
                // An uncaught CX::Return signal that escapes the top-level
                // Interpreter loop means the lexical target routine was not on the
                // dynamic call stack when `return` executed, so it surfaces
                // as `X::ControlFlow::Return` with out-of-dynamic-scope set.
                // Only perform this conversion when the current dynamic call
                // stack contains no routine — otherwise the return is meant
                // for an enclosing routine that will catch it via its own
                // call-frame handling further up the stack.
                if e.is_return() && self.routine_stack().is_empty() {
                    let inner_err = RuntimeError::controlflow_return(true);
                    if self.check_phaser_depth > 0 {
                        return Err(Self::wrap_in_begin_time(inner_err));
                    }
                    return Err(inner_err);
                }
                if self.check_phaser_depth > 0 {
                    return Err(Self::wrap_in_begin_time(e));
                }
                return Err(e);
            }
            if self.is_halted() {
                break;
            }
        }
        self.sync_state_locals(code);
        self.pop_once_scope();
        // Sync local variables back to the interpreter's env so that
        // callers (e.g. eval_block_value) can observe side effects.
        self.sync_env_from_locals(code);
        let last_stack_value = self.stack.last().cloned();
        let fallback = self.last_topic_value.clone();
        Ok(last_stack_value.or(fallback))
    }

    /// CP-3 collapse PoC: run a compiled block re-entrantly on the *existing* Interpreter,
    /// without the `mem::take(self)` + `Interpreter::new(self)` + `*self = interp`
    /// ping-pong that the interpreter-side carriers (`run_compiled_block`,
    /// `run_block_raw`, `eval_precompiled_block_fast`) use today.
    ///
    /// The ping-pong only exists because those carriers live on the `Interpreter`
    /// (no live Interpreter there), so each one spins up a fresh `Interpreter` whose per-execution
    /// registers (stack/locals/call_frames/topic/…) start empty. This method
    /// reproduces that "fresh registers, shared interpreter + env" semantics in
    /// place: it saves the current per-execution registers, resets them to the
    /// `Interpreter::new` defaults, runs the block (sharing `self.interpreter`/`self.env`
    /// directly), then restores. This is the mechanism that replaces the
    /// ping-pong once the `Interpreter` struct is dissolved into the Interpreter.
    ///
    /// Shared state (interpreter fields, env, registry/io/output handles) is
    /// intentionally *not* reset — the nested block must observe and mutate the
    /// same state, exactly as the inner ping-pong Interpreter does (it shares the moved
    /// interpreter and the loaned env). Caches are gen-counted, so keeping them
    /// across the nested run is correct and avoids churn.
    pub(crate) fn run_nested(
        &mut self,
        code: &CompiledCode,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Option<Value>, RuntimeError> {
        // Use the guarded runner so a Rust panic in a re-entrant carrier
        // (run_compiled_block / eval_block_value / run_block_raw — e.g. a
        // `dies-ok { ... }` block) is caught and converted, exactly as the old
        // ping-pong `Interpreter::run` did. (The map/grep `run_reuse` loops call
        // `with_nested_registers` directly and keep their no-boundary behavior.)
        self.with_nested_registers(|me| me.run_inner_guarded(code, compiled_fns))
    }

    /// Run `f` with fresh per-execution registers (stack/locals/call_frames/topic/
    /// context flags reset to their `Interpreter::new` defaults), restoring the outer
    /// registers afterwards and flagging `env_dirty` so the outer execution
    /// re-syncs its locals from env. This is the in-place replacement for the old
    /// `mem::take(self)` + `Interpreter::new` ping-pong: shared state (env, interpreter
    /// fields, registry/io/output handles, gen-counted caches) is *not* reset, so
    /// the nested work observes and mutates the same state. Used by `run_nested`
    /// (single compiled block) and by the map/sort `run_reuse` loops (many
    /// iterations sharing one fresh-register scope).
    pub(crate) fn with_nested_registers<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        // Save the per-execution registers (the fields `Interpreter::new` initializes
        // fresh) and reset them to their fresh-Interpreter defaults for the nested run.
        let saved_stack = std::mem::take(&mut self.stack);
        let saved_locals = std::mem::take(&mut self.locals);
        let saved_upvalues = std::mem::take(&mut self.upvalues);
        let saved_call_frames = std::mem::take(&mut self.call_frames);
        let saved_resume_ip = self.resume_ip.take();
        let saved_last_topic = self.last_topic_value.take();
        let saved_topic_save_stack = std::mem::take(&mut self.topic_save_stack);
        let saved_topic_source_var = self.topic_source_var.take();
        let saved_element_source = self.element_source.take();
        let saved_container_ref_var = self.container_ref_var.take();
        let saved_container_ref_reversed = self.container_ref_reversed;
        let saved_quanthash_bind_params = std::mem::take(&mut self.quanthash_bind_params);
        let saved_for_param_restore_stack = std::mem::take(&mut self.for_param_restore_stack);
        let saved_local_bind_pairs = std::mem::take(&mut self.local_bind_pairs);
        let saved_block_declared_vars = std::mem::take(&mut self.block_declared_vars);
        let saved_loop_local_vars = std::mem::take(&mut self.loop_local_vars);
        let saved_loop_local_saved_env = std::mem::take(&mut self.loop_local_saved_env);
        let saved_outer_scope_locals = std::mem::take(&mut self.outer_scope_locals);
        let saved_pending_alias_bind_names = std::mem::take(&mut self.pending_alias_bind_names);
        let saved_in_smartmatch_rhs = self.in_smartmatch_rhs;
        let saved_transliterate = self.transliterate_in_smartmatch;
        let saved_substitution = self.substitution_in_smartmatch;
        let saved_method_dispatch_pure = self.method_dispatch_pure;
        let saved_bind_context = self.bind_context;
        let saved_scalar_bind_context = self.scalar_bind_context;
        let saved_bound_decont_active = self.bound_decont_active;
        let saved_rebind_context = self.rebind_context;
        let saved_constant_context = self.constant_context;
        let saved_array_share_context = self.array_share_context;
        let saved_array_share_source = self.array_share_source.take();
        let saved_explicit_initializer_context = self.explicit_initializer_context;
        let saved_vardecl_context = self.vardecl_context;
        let saved_loop_cond_active = self.loop_cond_active;
        let saved_state_scope_id = self.state_scope_id.take();
        let saved_gather_for_loop_resume = self.gather_for_loop_resume.take();
        let saved_rw_map_topic_capture = self.rw_map_topic_capture.take();

        self.in_smartmatch_rhs = false;
        self.transliterate_in_smartmatch = false;
        self.substitution_in_smartmatch = false;
        self.method_dispatch_pure = false;
        self.container_ref_reversed = false;
        self.bind_context = false;
        self.scalar_bind_context = false;
        self.bound_decont_active = false;
        self.rebind_context = false;
        self.constant_context = false;
        self.array_share_context = false;
        self.array_share_source = None;
        self.explicit_initializer_context = false;
        self.vardecl_context = false;
        self.loop_cond_active = false;

        let result = f(self);

        // Restore the outer execution registers.
        self.stack = saved_stack;
        self.locals = saved_locals;
        self.upvalues = saved_upvalues;
        self.call_frames = saved_call_frames;
        self.resume_ip = saved_resume_ip;
        self.last_topic_value = saved_last_topic;
        self.topic_save_stack = saved_topic_save_stack;
        self.topic_source_var = saved_topic_source_var;
        self.element_source = saved_element_source;
        self.container_ref_var = saved_container_ref_var;
        self.container_ref_reversed = saved_container_ref_reversed;
        self.quanthash_bind_params = saved_quanthash_bind_params;
        self.for_param_restore_stack = saved_for_param_restore_stack;
        self.local_bind_pairs = saved_local_bind_pairs;
        self.block_declared_vars = saved_block_declared_vars;
        self.loop_local_vars = saved_loop_local_vars;
        self.loop_local_saved_env = saved_loop_local_saved_env;
        self.outer_scope_locals = saved_outer_scope_locals;
        self.pending_alias_bind_names = saved_pending_alias_bind_names;
        self.in_smartmatch_rhs = saved_in_smartmatch_rhs;
        self.transliterate_in_smartmatch = saved_transliterate;
        self.substitution_in_smartmatch = saved_substitution;
        self.method_dispatch_pure = saved_method_dispatch_pure;
        self.bind_context = saved_bind_context;
        self.scalar_bind_context = saved_scalar_bind_context;
        self.bound_decont_active = saved_bound_decont_active;
        self.rebind_context = saved_rebind_context;
        self.constant_context = saved_constant_context;
        self.array_share_context = saved_array_share_context;
        self.array_share_source = saved_array_share_source;
        self.explicit_initializer_context = saved_explicit_initializer_context;
        self.vardecl_context = saved_vardecl_context;
        self.loop_cond_active = saved_loop_cond_active;
        self.state_scope_id = saved_state_scope_id;
        self.gather_for_loop_resume = saved_gather_for_loop_resume;
        self.rw_map_topic_capture = saved_rw_map_topic_capture;

        // The nested run shares `self.env` and may have mutated outer lexicals
        // (e.g. a deferred role-body statement writing an enclosing `my $x`).
        // Those writes land in `env`, but the restored outer `locals` slots are
        // stale, so flag the dual-store dirty to force a reload from env before
        // the outer execution next reads a local. (The ping-pong achieved this
        // implicitly: the inner Interpreter's env flowed back through the interpreter and
        // the caller re-synced from it.)

        result
    }

    /// Run compiled bytecode without consuming self.
    /// Used by map/grep to avoid Interpreter creation/destruction per iteration.
    pub(crate) fn run_reuse(
        &mut self,
        code: &CompiledCode,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        Self::validate_labels(code)?;
        self.stack.clear();
        // Initialize local variable slots
        self.locals.resize(code.locals.len(), Value::Nil);
        for (i, name) in code.locals.iter().enumerate() {
            if let Some(val) = self.env().get(name) {
                self.locals[i] = val.clone();
            } else {
                self.locals[i] = Value::Nil;
            }
        }
        self.load_state_locals(code);
        let root_once_scope = self.next_once_scope_id();
        self.push_once_scope(root_once_scope);
        let mut ip = 0;
        while ip < code.ops.len() {
            if let Err(e) = self.exec_one(code, &mut ip, compiled_fns) {
                if e.is_goto()
                    && let Some(label) = e.label.as_deref()
                    && let Some(target_ip) = self.find_label_target(code, label)
                {
                    ip = target_ip;
                    continue;
                }
                if e.is_warn() && self.control_handler_depth == 0 {
                    if !self.warning_suppressed() {
                        self.write_warn_to_stderr(&e.message);
                    }
                    if let Some(v) = e.return_value {
                        self.stack.push(v);
                    }
                    ip += 1;
                    continue;
                }
                self.sync_state_locals(code);
                self.pop_once_scope();
                return Err(e);
            }
            if self.is_halted() {
                break;
            }
        }
        self.sync_state_locals(code);
        self.pop_once_scope();
        Ok(())
    }

    /// Resolve a state variable key, applying the current closure scope if set.
    pub(crate) fn scoped_state_key(&self, key: &str) -> String {
        if let Some(id) = self.state_scope_id {
            format!("{key}#c{id}")
        } else {
            key.to_string()
        }
    }

    fn load_state_locals(&mut self, code: &CompiledCode) {
        for (slot, key) in &code.state_locals {
            if let Some(val) = self.get_state_var(key) {
                self.locals[*slot] = val.clone();
            }
        }
    }

    fn sync_state_locals(&mut self, code: &CompiledCode) {
        for (slot, key) in &code.state_locals {
            let local_name = &code.locals[*slot];
            let val = self
                .env()
                .get(local_name)
                .cloned()
                .unwrap_or_else(|| self.locals[*slot].clone());
            self.set_state_var(key.clone(), val);
        }
    }

    /// Sync only state variables whose `StateVarInit` opcode falls within
    /// the given instruction range [start..end). This avoids prematurely
    /// syncing state variables that haven't been initialized yet.
    pub(crate) fn sync_state_locals_in_range(
        &mut self,
        code: &CompiledCode,
        start: usize,
        end: usize,
    ) {
        for (slot, key) in &code.state_locals {
            // Check if this exact state variable (by key) has its StateVarInit in the range.
            // We match both slot and key_idx to avoid false matches when multiple
            // state variables share the same local slot.
            let has_init_in_range = code.ops[start..end].iter().any(|op| {
                if let OpCode::StateVarInit(s, k) = op {
                    if *s as usize != *slot {
                        return false;
                    }
                    // Verify the key constant matches
                    if let Value::Str(ref stored_key) = code.constants[*k as usize] {
                        stored_key.as_ref() == key.as_str()
                    } else {
                        false
                    }
                } else {
                    false
                }
            });
            if !has_init_in_range {
                continue;
            }
            let local_name = &code.locals[*slot];
            let val = self
                .env()
                .get(local_name)
                .cloned()
                .unwrap_or_else(|| self.locals[*slot].clone());
            self.set_state_var(key.clone(), val);
        }
    }

    /// Execute opcodes in [start..end), used by loop compound opcodes.
    /// Like `run_range`, but installs a `catch_unwind` boundary so a Rust panic
    /// (unwrap/index-OOB/overflow/...) raised anywhere inside the executed range
    /// — however deeply nested through other (unguarded) `run_range`/`run_reuse`
    /// frames — is converted into a catchable `X::AdHoc` `RuntimeError` rather
    /// than crashing the process. Used for the `try`/`CATCH` body so user error
    /// handlers can catch otherwise-fatal internal panics. The intermediate
    /// frames need no guard: Rust unwinding propagates through them up to this
    /// boundary, where it becomes a normal `Err` and flows through the existing
    /// exception machinery.
    pub(crate) fn run_range_guarded(
        &mut self,
        code: &CompiledCode,
        start: usize,
        end: usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        install_vm_panic_hook();
        let prev = IN_VM_PANIC_BOUNDARY.with(|f| f.replace(true));
        let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            self.run_range(code, start, end, compiled_fns)
        }));
        IN_VM_PANIC_BOUNDARY.with(|f| f.set(prev));
        match caught {
            Ok(r) => r,
            Err(payload) => Err(Self::vm_panic_error(panic_payload_message(
                payload.as_ref(),
            ))),
        }
    }

    pub(crate) fn run_range(
        &mut self,
        code: &CompiledCode,
        start: usize,
        end: usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let mut ip = start;
        while ip < end {
            if let Err(e) = self.exec_one(code, &mut ip, compiled_fns) {
                if e.is_goto()
                    && let Some(label) = e.label.as_deref()
                    && let Some(target_ip) = self.find_label_target(code, label)
                    && (start..end).contains(&target_ip)
                {
                    ip = target_ip;
                    continue;
                }
                // Handle warn signals inline when no CONTROL handler is active.
                if e.is_warn() && self.control_handler_depth == 0 {
                    if !self.warning_suppressed() {
                        self.write_warn_to_stderr(&e.message);
                    }
                    if let Some(v) = e.return_value {
                        self.stack.push(v);
                    }
                    // If a resume point was recorded for the original warn
                    // site (e.g., when a CONTROL block rethrew the CX::Warn),
                    // resume there so execution continues after the warn
                    // rather than past whatever op propagated the signal.
                    if let Some(resume_point) = self.resume_ip.take() {
                        ip = resume_point;
                    } else {
                        ip += 1;
                    }
                    continue;
                }
                return Err(e);
            }
            if self.is_halted() {
                break;
            }
        }
        Ok(())
    }

    /// Run LEAVE/KEEP/UNDO phaser queue with per-phaser error guarding.
    /// Each individual LEAVE phaser (delimited by `LeaveGuard` opcodes) is
    /// run independently. If one throws, the error is collected and execution
    /// continues with the next phaser. Collected exceptions are returned as
    /// an `X::PhaserExceptions` error at the end.
    pub(crate) fn run_leave_queue_guarded(
        &mut self,
        code: &CompiledCode,
        start: usize,
        end: usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        if start >= end {
            return Ok(());
        }
        // Check if there are any LeaveGuard markers in this range
        let has_guards = (start..end).any(|i| matches!(code.ops[i], OpCode::LeaveGuard { .. }));
        if !has_guards {
            return self.run_range(code, start, end, compiled_fns);
        }

        let mut collected_errors: Vec<RuntimeError> = Vec::new();
        let mut ip = start;
        while ip < end {
            match &code.ops[ip] {
                OpCode::LeaveGuard { next } => {
                    let guard_next = *next as usize;
                    // Run this phaser's body (from ip+1 to guard_next)
                    let result = self.run_range(code, ip + 1, guard_next, compiled_fns);
                    if let Err(e) = result {
                        collected_errors.push(e);
                    }
                    ip = guard_next;
                }
                _ => {
                    // Non-guarded code before the first guard; run normally
                    self.exec_one(code, &mut ip, compiled_fns)?;
                }
            }
        }

        if collected_errors.is_empty() {
            Ok(())
        } else if collected_errors.len() == 1 {
            Err(collected_errors.into_iter().next().unwrap())
        } else {
            // Create X::PhaserExceptions with all collected exceptions
            let exceptions: Vec<Value> = collected_errors
                .iter()
                .map(|e| {
                    if let Some(ex) = e.exception.as_ref() {
                        *ex.clone()
                    } else {
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("message".to_string(), Value::str(e.message.clone()));
                        Value::make_instance(crate::symbol::Symbol::intern("Exception"), attrs)
                    }
                })
                .collect();
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("exceptions".to_string(), Value::array(exceptions));
            let exception =
                Value::make_instance(crate::symbol::Symbol::intern("X::PhaserExceptions"), attrs);
            let mut err = RuntimeError::new("Multiple exceptions in LEAVE phasers".to_string());
            err.exception = Some(Box::new(exception));
            Err(err)
        }
    }

    pub(crate) fn find_label_target(&self, code: &CompiledCode, label: &str) -> Option<usize> {
        code.ops.iter().enumerate().find_map(|(i, op)| match op {
            OpCode::Label(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                if name == label { Some(i + 1) } else { None }
            }
            _ => None,
        })
    }

    /// Itemize a value read from a `$` scalar container so it behaves as a
    /// single element in list context. Arrays/Lists flip to their itemized
    /// `ArrayKind`; a Hash sets its `itemized` flag (mirroring `ArrayKind` — the
    /// value stays a `Value::Hash`, so value operations never see a wrapper and
    /// nothing leaks); a Seq is wrapped in a `Value::Scalar`. Already-itemized
    /// values and non-container scalars pass through unchanged. (Set/Bag/Mix are
    /// only itemized in the `@a = $var` path — see `ItemizeVar` — not in general
    /// `$(...)` itemization, to avoid leaking a `Scalar` wrapper into set ops.)
    pub(crate) fn itemize_value(val: Value) -> Value {
        match val {
            Value::Array(items, kind) if !kind.is_itemized() => Value::Array(items, kind.itemize()),
            Value::Hash(h) => Value::Hash(Value::hash_arc_itemized(h)),
            Value::Seq(items) => Value::Scalar(Box::new(Value::Seq(items))),
            other => other,
        }
    }

    /// De-itemize a `for … -> @a` chunk element while preserving its element
    /// type. An element-typed array (`array[int]`) keeps its type — only the
    /// itemization/scalar wrap is stripped, which is exactly the de-itemization
    /// the binding needs. Every other value flattens via the existing `.list`
    /// semantics (preserving prior behavior exactly).
    pub(crate) fn deitemize_for_bind(&mut self, val: Value) -> Result<Value, RuntimeError> {
        if let Value::Array(data, kind) = &val
            && (data.value_type.is_some() || data.declared_type.is_some())
        {
            return Ok(Value::Array(data.clone(), kind.decontainerize()));
        }
        self.call_method_with_values(val, "list", vec![])
    }
}
