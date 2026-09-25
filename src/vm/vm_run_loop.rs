use super::*;
use crate::value::ValueMap;

/// Whether any class attribute anywhere in the program has ever been
/// declared with an inline `where` constraint (`has Numeric $.lat where {
/// -90 <= $_ <= 90 }`), process-wide and monotonic. Gates
/// `Interpreter::self_attr_where_constraint` so the per-assignment
/// attribute-constraint lookup -- paid on every scalar attribute store --
/// does not additionally search `class_def.attributes` when no class in the
/// program ever declares one, which is the overwhelming common case.
static ATTR_WHERE_CONSTRAINT_SEEN: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

impl Interpreter {
    /// Record that a class attribute was declared with an inline `where`
    /// constraint. Called once per such attribute at class/role registration
    /// time (see `registration_class_body_attr.rs`, `registration_role_body.rs`,
    /// `registration_class_augment.rs`).
    pub(crate) fn mark_attr_where_constraint_seen() {
        ATTR_WHERE_CONSTRAINT_SEEN.store(true, std::sync::atomic::Ordering::Relaxed);
    }

    /// The declared `where` constraint (if any) of the scalar attribute that a
    /// local/env name refers to (`!x` / `.x` inside a method), mirroring
    /// `self_attr_type_constraint`'s MRO walk. Construction already enforces
    /// this constraint (`enforce_attribute_where_constraints`), but a later
    /// `$!x = v` inside a method reaches the plain scalar-assignment paths,
    /// which only ever consulted the attribute's declared TYPE
    /// (`scalar_attr_type_constraint`) and silently accepted any value that
    /// merely satisfied that type -- so `has Numeric $.lat where { -90 <= $_
    /// <= 90 }; method lat(Numeric $v) { $!lat = $v }` never rejected an
    /// out-of-range `$v` (ecosystem `Date::Event` t/5-lat-lon.t).
    pub(crate) fn self_attr_where_constraint(
        &self,
        attr_name: &str,
    ) -> Option<crate::opcode::DeclTraitArg> {
        if !ATTR_WHERE_CONSTRAINT_SEEN.load(std::sync::atomic::Ordering::Relaxed) {
            return None;
        }
        let self_val = self.get_env_self()?;
        let class_name = self_val.with_deref(|v| match v.view() {
            crate::value::ValueView::Instance { class_name, .. } => Some(class_name.as_str()),
            crate::value::ValueView::Mixin(inner, _) => match inner.view() {
                crate::value::ValueView::Instance { class_name, .. } => Some(class_name.as_str()),
                _ => None,
            },
            _ => None,
        })?;
        let (bare, sigil) = if let Some((bare, _)) = crate::value::attr_twigil_base(attr_name) {
            (
                bare,
                crate::value::attr_twigil_sigil(attr_name).unwrap_or('$'),
            )
        } else {
            (attr_name, '$')
        };
        self.mro_syms_readonly(class_name).iter().find_map(|cls| {
            self.registry()
                .classes
                .get(cls.as_str())
                .and_then(|class_def| {
                    class_def
                        .attributes
                        .iter()
                        .find(|attr| attr.name == bare && attr.sigil == sigil)
                        .and_then(|attr| attr.where_constraint.clone())
                })
        })
    }

    /// Check a scalar attribute's own `where` constraint (if it declared one)
    /// against a freshly assigned value, after the ordinary type check has
    /// already accepted it. No-op for `Nil` (which resets the attribute to
    /// its type object elsewhere) and for an attribute with no `where`
    /// clause. Called from the plain scalar-assignment paths
    /// (`exec_set_local_op_inner`, `exec_assign_expr_local_op_inner`, and the
    /// name-based `AssignExpr` path in `vm_misc_assign.rs`) right after their
    /// own type-constraint handling.
    pub(crate) fn check_scalar_attr_where_on_assign(
        &mut self,
        name: &str,
        val: &Value,
    ) -> Result<(), RuntimeError> {
        if val.is_nil() {
            return Ok(());
        }
        let Some(pred) = self.self_attr_where_constraint(name) else {
            return Ok(());
        };
        if self.check_attribute_where_constraint(&pred, val) {
            return Ok(());
        }
        Err(crate::runtime::utils::type_check_assignment_typed_error(
            name, "<anon>", val,
        ))
    }

    fn wrap_in_begin_time(inner: RuntimeError) -> RuntimeError {
        let inner_exception = inner
            .exception
            .as_ref()
            .map(|e| e.as_ref().clone())
            .unwrap_or_else(|| Value::str(inner.message.to_string()));
        let msg = format!(
            "An exception occurred while evaluating a CHECK\nException details:\n  {}",
            inner.message
        );
        let mut attrs = ValueMap::default();
        attrs.insert("message".to_string(), Value::str(msg));
        attrs.insert("exception".to_string(), inner_exception);
        RuntimeError::typed("X::Comp::BeginTime", attrs)
    }

    // Cost: O(1) amortized, on every `run_inner`/`run_reuse` entry (the label
    // table is built once per chunk -- `CompiledCode::duplicate_label`).
    fn validate_labels(code: &CompiledCode) -> Result<(), RuntimeError> {
        match code.duplicate_label() {
            Some(label_name) => Err(RuntimeError::new(format!(
                "X::Redeclaration: Label '{}' already declared",
                label_name
            ))),
            None => Ok(()),
        }
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
        compiled_fns: &CompiledFns,
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
        compiled_fns: &CompiledFns,
    ) -> Result<Option<Value>, RuntimeError> {
        install_vm_panic_hook();
        // Save/restore the flag so nested boundaries (EVAL, sub-VMs) don't
        // clobber an outer boundary's state.
        let prev = IN_VM_PANIC_BOUNDARY.with(|f| f.replace(true));
        // Snapshot the call-frame/stack depth so a panic caught below can be
        // recovered from — see `recover_call_frames_after_panic`.
        let entry_call_frame_depth = self.call_frames.len();
        let entry_stack_depth = self.stack.len();
        let entry_caller_env_depth = self.caller_env_stack_depth();
        let entry_let_saves_mark = self.let_saves_len();
        let entry_test_assertion_depth = self.test_assertion_line_stack_depth();
        let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            self.run_inner(code, compiled_fns)
        }));
        IN_VM_PANIC_BOUNDARY.with(|f| f.set(prev));
        match caught {
            Ok(r) => r,
            Err(payload) => {
                self.recover_call_frames_after_panic(
                    entry_call_frame_depth,
                    entry_stack_depth,
                    entry_caller_env_depth,
                    entry_let_saves_mark,
                    entry_test_assertion_depth,
                );
                Err(Self::vm_panic_error(panic_payload_message(
                    payload.as_ref(),
                )))
            }
        }
    }

    /// Build a catchable `X::AdHoc` error from a caught panic message.
    pub(crate) fn vm_panic_error(message: String) -> RuntimeError {
        let message = format!("Internal error: {message}");
        let mut err = RuntimeError::new(message.clone());
        let mut attrs = HashMap::new();
        attrs.insert("message".to_string(), Value::str(message));
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::AdHoc"),
            attrs,
        )));
        err
    }

    /// The exec loop, borrowing `&mut self` so the `catch_unwind` closure in
    /// `run` does not move `self.interpreter` out (the caller must always get the
    /// interpreter back, even on panic).
    // Cost: O(L) entry (env->locals seeding, L = the chunk's own locals; label
    // validation is O(1) amortized) and exit (state/env sync), then O(1) per dispatched op: one cached `vm_poll::armed()`
    // load (a GC safepoint / profiler sample amortized O(1)) plus `exec_one`.
    fn run_inner(
        &mut self,
        code: &CompiledCode,
        compiled_fns: &CompiledFns,
    ) -> Result<Option<Value>, RuntimeError> {
        Self::validate_labels(code)?;
        // Initialize local variable slots
        self.locals.refill_slots(code.locals.len());
        for (i, name) in code.locals.iter().enumerate() {
            if let Some(val) = self.env().get(name) {
                self.locals[i] = val.clone();
            }
        }
        self.load_state_locals(code);
        let root_once_scope = self.next_once_scope_id();
        self.push_once_scope(root_once_scope);
        // CheckPhaserStart/CheckPhaserEnd are ops inlined into `code` around a
        // CHECK/BEGIN phaser body (`compile_check_phaser`); a normal run pairs
        // every Start with a matching End. When the phaser body throws, the
        // error short-circuits this loop below and the End op is never
        // reached, so `check_phaser_depth` would otherwise leak past this
        // call — wrapping an unrelated LATER error (e.g. a die in an INIT
        // phaser evaluated by a subsequent EVAL sharing this Interpreter) in
        // X::Comp::BeginTime too. Snapshot the entry depth so every error
        // exit below can restore it after deciding whether to wrap.
        let entry_check_phaser_depth = self.check_phaser_depth;
        // ADR-0041 §9: the BEGIN-time visibility frames are pushed by
        // `CheckPhaserStart` AND by the value-position `BEGIN` opcode (which
        // does not raise `check_phaser_depth`), so unwind them by their own
        // entry depth rather than by the phaser depth.
        let entry_begin_time_depth = self.begin_time_hidden.len() as u32;
        let mut ip = 0;
        while ip < code.ops.len() {
            // VM poll (design doc §1.2): the dispatch backward edge holds no
            // container borrow, so a cycle collect may run here.
            // `vm_poll::armed()` is a single cached load (false only with
            // `MUTSU_GC=off`). Fires on worker threads too: the collector splits
            // the work by thread-safety — the dead sweep (refcount-dead
            // candidates, plain `Arc` drops) runs even while other mutators are
            // live, while the trial-deletion cycle scan first brings them to
            // quiescence via the cooperative stop-the-world (`gc::stw`, design
            // §6.1). Without in-thread sweeps, threaded mutation-heavy loops grew
            // the candidate buffer — and their dead snapshots' memory —
            // unboundedly until the post-join collect.
            if crate::vm::vm_poll::armed() {
                crate::vm::vm_poll::poll_code(
                    crate::gc::SafepointKind::Backedge,
                    ip as u32,
                    code,
                    self,
                );
            }
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
                if e.is_return() && self.routine_stack().is_empty() && self.nested_run_depth == 0 {
                    let inner_err = RuntimeError::controlflow_return(true);
                    if self.check_phaser_depth > 0 {
                        let wrapped = Self::wrap_in_begin_time(inner_err);
                        self.check_phaser_depth = entry_check_phaser_depth;
                        self.begin_time_unwind_to(entry_begin_time_depth);
                        return Err(wrapped);
                    }
                    self.check_phaser_depth = entry_check_phaser_depth;
                    self.begin_time_unwind_to(entry_begin_time_depth);
                    return Err(inner_err);
                }
                if self.check_phaser_depth > 0 {
                    let wrapped = Self::wrap_in_begin_time(e);
                    self.check_phaser_depth = entry_check_phaser_depth;
                    self.begin_time_unwind_to(entry_begin_time_depth);
                    return Err(wrapped);
                }
                self.check_phaser_depth = entry_check_phaser_depth;
                self.begin_time_unwind_to(entry_begin_time_depth);
                return Err(e);
            }
            if self.is_halted() {
                break;
            }
        }
        self.sync_state_locals(code);
        self.pop_once_scope();
        // ADR-0041 §9 safety net: a BEGIN-time region whose closing opcode was
        // skipped (a caught throw, a `halt`) would otherwise keep sub
        // declarations rolled out of the registry for the rest of the program.
        self.begin_time_unwind_to(entry_begin_time_depth);
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
        compiled_fns: &CompiledFns,
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
    /// Identity fingerprint of a CompiledCode for resume-point validation.
    /// A recorded `resume_ip` is only meaningful inside the code object whose
    /// op array it indexes; the ops pointer distinguishes frames cheaply.
    #[inline]
    pub(crate) fn resume_code_fp(code: &CompiledCode) -> usize {
        code.ops.as_ptr() as usize
    }

    /// Take the recorded resume point if (and only if) it belongs to `code`.
    /// A mismatched fingerprint means the point was recorded in a different
    /// (typically callee) frame — resuming there would jump to an arbitrary
    /// op in this frame, so it is discarded instead.
    #[inline]
    pub(crate) fn take_resume_ip_for(&mut self, code: &CompiledCode) -> Option<usize> {
        match self.resume_ip {
            Some((fp, ip)) if fp == Self::resume_code_fp(code) => {
                self.resume_ip = None;
                Some(ip)
            }
            _ => None,
        }
    }

    /// Take the pending `TagContainerRef` signal if (and only if) it was set
    /// by `code`. The tag is emitted immediately before the for/given op that
    /// consumes it, always in the same code object, so a fingerprint mismatch
    /// means the tag leaked out of a callee frame (a routine body's own
    /// tagged loop that returned before any same-frame consumer ran) — it is
    /// discarded, never applied to this frame's locals.
    #[inline]
    pub(crate) fn take_container_ref_for(
        &mut self,
        code: &CompiledCode,
    ) -> Option<(String, Option<u32>)> {
        let (name, slot, fp) = self.container_ref_var.take()?;
        (fp == Self::resume_code_fp(code)).then_some((name, slot))
    }

    pub(crate) fn with_nested_registers<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        // GC safepoint (§9.2a `nested_run`): the nested-VM entry boundary.
        crate::vm::vm_poll::poll(crate::gc::SafepointKind::NestedRun, 0);
        // Save the per-execution registers (the fields `Interpreter::new` initializes
        // fresh) and reset them to their fresh-Interpreter defaults for the nested run.
        let saved_stack = std::mem::take(&mut self.stack);
        let saved_locals_base = self.locals.push_frame(0);
        let saved_upvalues = std::mem::take(&mut self.upvalues);
        let saved_call_frames = std::mem::take(&mut self.call_frames);
        let saved_resume_ip = self.resume_ip.take();
        // ADR-0072: a nested VM run (EVAL, a `dies-ok { }` block, the map/grep
        // eager loops) is its own execution; a `die` inside it must not be
        // inline-handled by a CATCH belonging to the outer run, for the same
        // isolation reason `resume_ip` is cleared above. Such a throw keeps the
        // ordinary unwinding path.
        let saved_catch_handlers = std::mem::take(&mut self.catch_handlers);
        let saved_last_topic = self.last_topic_value.take();
        let saved_topic_save_stack = std::mem::take(&mut self.topic_save_stack);
        let saved_topic_source_var = self.topic_source_var.take();
        let saved_element_source = self.element_source.take();
        let saved_container_ref_var = self.container_ref_var.take();
        let saved_container_ref_reversed = self.container_ref_reversed;
        let saved_quanthash_bind_params = std::mem::take(&mut self.quanthash_bind_params);
        let saved_for_param_restore_stack = std::mem::take(&mut self.for_param_restore_stack);
        let saved_local_bind_pairs = std::mem::take(&mut self.local_bind_pairs);
        let saved_block_declared_vars = self.block_declared_vars.push_frame();
        let saved_loop_local_vars = self.loop_local_vars.push_frame();
        let saved_loop_local_saved_env = self.loop_local_saved_env.push_frame();
        // ADR-0027: a nested run (EVAL, dies-ok/lives-ok block, ...) starts
        // with an empty loop-owned vouch, for the same isolation rationale as
        // `active_loop_param_names` below — its own closures must not
        // inherit an unrelated enclosing loop's frozen-capture vouch.
        let saved_frame_owned = std::mem::take(&mut self.frame_owned);
        // ADR-0023: a routine called from a loop body starts with an empty
        // active-loop-param stack, so a spawn inside the callee whose free
        // variable merely shares an OUTER loop's parameter name is not
        // mistaken for that loop's own per-iteration binding.
        let saved_active_loop_param_names = self.active_loop_param_names.push_frame();
        let saved_outer_scope_locals = std::mem::take(&mut self.outer_scope_locals);
        let saved_pending_alias_bind_names = std::mem::take(&mut self.pending_alias_bind_names);
        let saved_in_smartmatch_rhs = self.in_smartmatch_rhs;
        let saved_transliterate = self.transliterate_in_smartmatch;
        let saved_substitution = self.substitution_in_smartmatch;
        let saved_method_dispatch_pure = self.method_dispatch_pure;
        // Save AND clear the whole mark-context one-shot flag family in one
        // step (`crate::runtime::mark_context`): they are a single packed
        // word plus the share-source name, so this boundary no longer spells
        // out nine `get`s here and nine `set(false)`s below. Folding them
        // also closed a gap — `param_raw_bind_context` was the one member
        // this boundary never isolated, though `MarkContextGuard` (the same
        // isolation for an ordinary call) always did.
        let (saved_mark_flags, saved_mark_share_source) = self.mark_ctx.take_all();
        let saved_loop_cond_active = self.loop_cond_active;
        let saved_state_scope_id = self.state_scope_id.take();
        // A fallback-dispatched routine body hands its registration clone id
        // across this register reset (see `pending_nested_state_scope`).
        self.state_scope_id
            .set(self.pending_nested_state_scope.take());
        let saved_gather_for_loop_resume = self.gather_for_loop_resume.take();
        let saved_rw_map_topic_capture = self.rw_map_topic_capture.take();
        // A nested-registers run (the map/grep/first eager fast paths above
        // all) executes its whole Rust-level loop over `list_items` in one
        // shot with no way to snapshot/resume mid-iteration — unlike a
        // bytecoded `ForLoop`/`while`, there is no `ForLoopResumeState` for
        // "which source index was this run resumed at" (and the line above
        // already discards any inner `gather_for_loop_resume` this run sets,
        // for exactly that reason: it can't be honored across this
        // boundary). So a `take` reaching the gather's take-limit here must
        // never raise the immediate suspend signal (`take_value`'s
        // non-deferred branch) — that would abandon every following element
        // of this run's OWN loop, silently truncating the result (#8783: a
        // `gather @list.map: *.take` consumed lazily one pull at a time only
        // ever produced the single element pulled through before the first
        // `take` hit `needed`). Deferring instead (mirroring the
        // condition-driven while/C-style loops' own boundary defer) lets the
        // run's loop keep going to its natural end, taking every element in
        // one pass — the same "whole batch in one pull" shape the coroutine
        // driver already accepts for e.g. `deepmap`.
        let saved_lazy_take_boundary_defer =
            std::mem::replace(&mut self.lazy_take_boundary_defer, true);
        let saved_gather_suspend_pending =
            std::mem::replace(&mut self.gather_suspend_pending, false);
        // `current_code` is the raw address of the *caller's* live `CompiledCode`
        // (see `exec_one`'s `self.current_code = code as *const CompiledCode as
        // usize;` and the unsafe deref in `writeback_multidim_var_to_local`).
        // `f` below runs a nested, ephemeral `CompiledCode` (e.g. a `dies-ok { }`
        // block's `compile_block_value` result, owned by a stack-local in
        // `eval_block_value`) that is dropped the instant `f` returns. Without
        // restoring the caller's address here, `current_code` is left dangling —
        // pointing at freed stack memory the next nested call's frame promptly
        // reuses — and any *later* unsafe deref of it is undefined behavior
        // (observed as a SIGABRT "slice::from_raw_parts requires the pointer to
        // be aligned and non-null" panic inside `builtin_multidim_delete`,
        // roast S32-array/multislice-6e.t, whenever a `dies-ok`/`lives-ok`/
        // `throws-like`/EVAL block ran earlier in the same program).
        let saved_current_code = self.current_code;

        self.in_smartmatch_rhs = false;
        self.transliterate_in_smartmatch = false;
        self.substitution_in_smartmatch = false;
        self.method_dispatch_pure = false;
        self.container_ref_reversed = false;
        self.accessor_ref_pending = false;
        self.loop_cond_active = false;
        self.nested_run_depth += 1;

        let result = f(self);

        self.nested_run_depth = self.nested_run_depth.saturating_sub(1);

        // Restore the outer execution registers.
        self.stack = saved_stack;
        self.locals.pop_frame(saved_locals_base);
        self.upvalues = saved_upvalues;
        self.call_frames = saved_call_frames;
        self.resume_ip = saved_resume_ip;
        self.catch_handlers = saved_catch_handlers;
        self.last_topic_value = saved_last_topic;
        self.topic_save_stack = saved_topic_save_stack;
        self.topic_source_var = saved_topic_source_var;
        self.element_source = saved_element_source;
        self.container_ref_var = saved_container_ref_var;
        self.container_ref_reversed = saved_container_ref_reversed;
        self.quanthash_bind_params = saved_quanthash_bind_params;
        self.for_param_restore_stack = saved_for_param_restore_stack;
        self.local_bind_pairs = saved_local_bind_pairs;
        self.block_declared_vars
            .pop_frame(saved_block_declared_vars);
        self.loop_local_vars.pop_frame(saved_loop_local_vars);
        self.loop_local_saved_env
            .pop_frame(saved_loop_local_saved_env);
        self.frame_owned = saved_frame_owned;
        self.active_loop_param_names
            .pop_frame(saved_active_loop_param_names);
        self.outer_scope_locals = saved_outer_scope_locals;
        self.pending_alias_bind_names = saved_pending_alias_bind_names;
        self.in_smartmatch_rhs = saved_in_smartmatch_rhs;
        self.transliterate_in_smartmatch = saved_transliterate;
        self.substitution_in_smartmatch = saved_substitution;
        self.method_dispatch_pure = saved_method_dispatch_pure;
        self.mark_ctx
            .restore_all(saved_mark_flags, saved_mark_share_source);
        self.loop_cond_active = saved_loop_cond_active;
        self.state_scope_id.set(saved_state_scope_id);
        self.gather_for_loop_resume = saved_gather_for_loop_resume;
        self.rw_map_topic_capture = saved_rw_map_topic_capture;
        self.current_code = saved_current_code;
        self.lazy_take_boundary_defer = saved_lazy_take_boundary_defer;
        self.gather_suspend_pending = saved_gather_suspend_pending;

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
    // Cost: O(L) per call, L = the body's OWN locals (seeded from env by name, as
    // MoarVM initializes a frame's registers) -- independent of the enclosing frame's
    // size; label validation is O(1) amortized -- then O(1) per op.
    pub(crate) fn run_reuse(
        &mut self,
        code: &CompiledCode,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        Self::validate_labels(code)?;
        self.stack.clear();
        // Initialize local variable slots
        self.locals.resize_slots(code.locals.len());
        for (i, name) in code.locals.iter().enumerate() {
            if let Some(val) = self.env().get(name) {
                self.locals[i] = val.clone();
            } else {
                self.locals[i] = Value::NIL;
            }
        }
        self.load_state_locals(code);
        let root_once_scope = self.next_once_scope_id();
        self.push_once_scope(root_once_scope);
        let mut ip = 0;
        while ip < code.ops.len() {
            // VM poll (design doc §1.2): the dispatch backward edge holds no
            // container borrow, so a cycle collect may run here.
            // `vm_poll::armed()` is a single cached load (false only with
            // `MUTSU_GC=off`). Fires on worker threads too: the collector splits
            // the work by thread-safety — the dead sweep (refcount-dead
            // candidates, plain `Arc` drops) runs even while other mutators are
            // live, while the trial-deletion cycle scan first brings them to
            // quiescence via the cooperative stop-the-world (`gc::stw`, design
            // §6.1). Without in-thread sweeps, threaded mutation-heavy loops grew
            // the candidate buffer — and their dead snapshots' memory —
            // unboundedly until the post-join collect.
            if crate::vm::vm_poll::armed() {
                crate::vm::vm_poll::poll_code(
                    crate::gc::SafepointKind::Backedge,
                    ip as u32,
                    code,
                    self,
                );
            }
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

    /// The `state` scope a code object's body must run under.
    ///
    /// A NAMED sub's is its REGISTRATION clone id (env
    /// `__mutsu_callable_id::Pkg::name`, refreshed on every `RegisterSub`
    /// execution) — the same id the cold named path uses — so a nested named sub
    /// re-initializes per enclosing call while a top-level sub's state persists,
    /// regardless of which dispatch path a call takes. Anonymous closures (and
    /// named subs with no registration record) keep the Sub value's identity,
    /// which is minted afresh by each `MakeClosure` and so IS the clone.
    pub(crate) fn sub_state_scope_id(&self, data: &crate::value::SubData) -> u64 {
        let name = data.name.resolve();
        if name.is_empty() {
            return data.id;
        }
        let key = crate::runtime::Interpreter::callable_id_key_for_syms(data.package, data.name);
        self.env()
            .get_sym(key)
            .and_then(|v| v.as_int())
            .filter(|i| *i != 0)
            .map_or(data.id, |i| i as u64)
    }

    /// Publish a just-written local to the state store when that slot holds a
    /// `state` variable.
    ///
    /// A `state` variable is one CONTAINER shared by every invocation of its
    /// clone, so a re-entrant call must observe a mutation the outer frame has
    /// already made. The slot is per-frame and only syncs to the store at frame
    /// exit, so without this write-through the inner frame's `load_state_locals`
    /// read the value from *before* the outer frame ran — which made
    /// `sub f { my @a = 1, (f() unless $++) }` (roast S02-types/array.t
    /// "works fine when re-entrant") recurse until the stack overflowed.
    ///
    /// Free for the overwhelmingly common state-free `CompiledCode`: one
    /// `is_empty` test. The scan is over `state_locals`, which holds a handful
    /// of entries at most.
    pub(crate) fn publish_state_local(&mut self, code: &CompiledCode, slot: u32) {
        if code.state_locals.is_empty() {
            return;
        }
        let slot = slot as usize;
        let Some(key) = code
            .state_locals
            .iter()
            .find(|(s, _)| *s == slot)
            .map(|(_, k)| *k)
        else {
            return;
        };
        let Some(val) = self.locals.get(slot).cloned() else {
            return;
        };
        let scoped = self.scoped_state_key(key);
        loan_env!(self, set_state_var(scoped, val));
    }

    /// Resolve a state variable key, applying the current closure scope if
    /// set. A Copy tuple, not a `format!`ed `String` — `publish_state_local`
    /// runs on every `SetLocal`/`SetLocalDecl` write to a `state` local
    /// (including the JIT shims, see `vm_jit_helpers::{set_local,
    /// set_local_decl}`), so this must be free.
    pub(crate) fn scoped_state_key(&self, key: Symbol) -> (Symbol, Option<u64>) {
        (key, self.state_scope_id.get())
    }

    // Both loaders/syncers resolve the key through `scoped_state_key`, matching
    // `StateVarInit`/`StateVarInitGuard`/`reset_state_locals_in_range` — the
    // store must be read and written under ONE key shape or a scope-id'd init
    // and a raw sync silently diverge (a no-op when `state_scope_id` is None,
    // which is every path that existed before the inline-map scoping).
    fn load_state_locals(&mut self, code: &CompiledCode) {
        for (slot, key) in &code.state_locals {
            let scoped_key = self.scoped_state_key(*key);
            if let Some(val) = self.get_state_var(scoped_key) {
                self.locals[*slot] = val.clone();
            }
        }
    }

    fn sync_state_locals(&mut self, code: &CompiledCode) {
        for (slot, key) in &code.state_locals {
            let local_name = &code.locals[*slot];
            // A slot-only state local has no current env mirror under the
            // precise-sync regime; an older declaration seed may still be
            // present there.  Persist the authoritative slot in that case.
            // Name-based consumers are folded into `needs_env_sync` and retain
            // the env-first path for their out-of-slot mutations.
            let val = if code.needs_env_sync.get(*slot).copied().unwrap_or(true) {
                self.env()
                    .get(local_name)
                    .cloned()
                    .unwrap_or_else(|| self.locals[*slot].clone())
            } else {
                self.locals[*slot].clone()
            };
            let scoped_key = self.scoped_state_key(*key);
            self.set_state_var(scoped_key, val);
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
        for (i, (slot, key)) in code.state_locals.iter().enumerate() {
            if !code.state_local_init_in_range(i, start, end) {
                continue;
            }
            let local_name = &code.locals[*slot];
            // Same store-selection rule as the routine-exit sibling in
            // `vm_closure_dispatch` (see its comment): read whichever store is
            // authoritative for this slot. Once ADR-0018 narrowed a for-loop
            // frame's env sync from a frame-wide blanket to the loop's own baked
            // slots, a plain `state $t` in the body no longer mirrors to env, so
            // an env-first read persisted the value the DECLARATION seeded and
            // the accumulation vanished (`gather { for 1..3 { state $t = 0; $t =
            // $t + 1; take $t } }` yielded 1 1 1). A state var that IS
            // env-synced keeps its mirror current and may have been written
            // there by name — a dynamic `s///` replacement bumping it leaves the
            // slot at the pre-replacement value — so that case still reads env
            // first.
            let slot_authoritative = !code.needs_env_sync.get(*slot).copied().unwrap_or(false);
            let val = if slot_authoritative {
                self.locals
                    .get(*slot)
                    .cloned()
                    .unwrap_or_else(|| self.env().get(local_name).cloned().unwrap_or(Value::NIL))
            } else {
                self.env()
                    .get(local_name)
                    .cloned()
                    .unwrap_or_else(|| self.locals[*slot].clone())
            };
            let scoped_key = self.scoped_state_key(*key);
            self.set_state_var(scoped_key, val);
        }
    }

    /// Reset (drop from the state store) every state variable whose
    /// `StateVarInit` falls within [start..end). Called on entry to a compound
    /// loop opcode: each execution of the loop *statement* is a fresh clone of
    /// its body block (Raku clones a block each time its enclosing block
    /// runs), so `state` declarations inside the body — including nested loop
    /// bodies — re-run their initializers. Iterations within one execution
    /// share state (the loop re-invokes the same clone), which is why this
    /// runs at statement entry, not per iteration. Callers must skip this when
    /// resuming a suspended gather coroutine into the same loop opcode.
    // Cost: O(t), t = state locals of the chunk (each init-in-range test is a
    // lookup in the chunk's `StateVarInit` index -- `crate::op_scan_index`).
    pub(crate) fn reset_state_locals_in_range(
        &mut self,
        code: &CompiledCode,
        start: usize,
        end: usize,
    ) {
        for (i, (_, key)) in code.state_locals.iter().enumerate() {
            if !code.state_local_init_in_range(i, start, end) {
                continue;
            }
            let scoped_key = self.scoped_state_key(*key);
            self.remove_state_var(scoped_key);
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
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        install_vm_panic_hook();
        let prev = IN_VM_PANIC_BOUNDARY.with(|f| f.replace(true));
        // Snapshot the call-frame/stack depth so a panic caught below can be
        // recovered from — see `recover_call_frames_after_panic`.
        let entry_call_frame_depth = self.call_frames.len();
        let entry_stack_depth = self.stack.len();
        let entry_caller_env_depth = self.caller_env_stack_depth();
        let entry_let_saves_mark = self.let_saves_len();
        let entry_test_assertion_depth = self.test_assertion_line_stack_depth();
        let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            self.run_range(code, start, end, compiled_fns)
        }));
        IN_VM_PANIC_BOUNDARY.with(|f| f.set(prev));
        match caught {
            Ok(r) => r,
            Err(payload) => {
                self.recover_call_frames_after_panic(
                    entry_call_frame_depth,
                    entry_stack_depth,
                    entry_caller_env_depth,
                    entry_let_saves_mark,
                    entry_test_assertion_depth,
                );
                Err(Self::vm_panic_error(panic_payload_message(
                    payload.as_ref(),
                )))
            }
        }
    }

    // Cost: O(1) amortized JIT-entry probe (4-slot lock-free cache; O(r) under a mutex
    // for a chunk with more hot ranges, r = its ranges) plus O(1) per op run.
    pub(crate) fn run_range(
        &mut self,
        code: &CompiledCode,
        start: usize,
        end: usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        // Hot-loop entry (ADR-0004 J4b): compound-loop bodies/conds call
        // run_range once per iteration, so a hot sub-range gets JIT-compiled
        // and runs natively. On a native-body error, the two signals the
        // interpreter loop resumes in place — goto to an in-range label and a
        // resumable warn — re-enter the interpreter loop below mid-range (all
        // state lives on the Interpreter, so continuing at the recorded ip is
        // exactly what the interpreter would have done); everything else
        // (control signals, exceptions) propagates to the caller unchanged.
        #[cfg(feature = "jit")]
        if let Some(r) = crate::vm::vm_jit::try_enter_range(self, code, start, end, compiled_fns) {
            match r {
                Ok(()) => return Ok(()),
                Err(e) => {
                    if e.is_goto()
                        && let Some(label) = e.label.as_deref()
                        && let Some(target_ip) = self.find_label_target(code, label)
                        && (start..end).contains(&target_ip)
                    {
                        return self.run_range_from(code, target_ip, start, end, compiled_fns);
                    }
                    if e.is_warn() && self.control_handler_depth == 0 {
                        if !self.warning_suppressed() {
                            self.write_warn_to_stderr(&e.message);
                        }
                        if let Some(v) = e.return_value.clone() {
                            self.stack.push(v);
                        }
                        // Every JIT shim that can surface a warn (CallFunc /
                        // CallMethod / step) records a resume point; without
                        // one there is no way to know where to continue, so
                        // propagate loudly instead of guessing.
                        if let Some(resume_point) = self.take_resume_ip_for(code) {
                            return self.run_range_from(
                                code,
                                resume_point,
                                start,
                                end,
                                compiled_fns,
                            );
                        }
                    }
                    return Err(e);
                }
            }
        }
        self.run_range_from(code, start, start, end, compiled_fns)
    }

    /// The interpreter loop of [`Self::run_range`], entered at `from` (== `start`
    /// except when resuming mid-range after a JIT'd body's goto/warn).
    // Cost: O(1) per dispatched op (cached safepoint-poll load plus `exec_one`); a `goto`
    // pays `find_label_target`, O(p), p = ops of the chunk.
    fn run_range_from(
        &mut self,
        code: &CompiledCode,
        from: usize,
        start: usize,
        end: usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let mut ip = from;
        while ip < end {
            // VM poll on the inner dispatch backedge too: compound-loop
            // ops (for/while bodies) iterate entirely inside ONE `exec_one` of
            // the outer `run` loop, so without this a tight loop never reaches
            // a safepoint and candidate-triggered collects (and the dead sweep
            // that bounds buffer memory) defer to the loop's end. Same borrow
            // argument as the outer site: between instructions no container
            // borrow is live (design doc §1.2).
            if crate::vm::vm_poll::armed() {
                crate::vm::vm_poll::poll_code(
                    crate::gc::SafepointKind::Backedge,
                    ip as u32,
                    code,
                    self,
                );
            }
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
                    if let Some(resume_point) = self.take_resume_ip_for(code) {
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
    // Cost: O(q) guard scan plus the phasers run, q = ops in the LEAVE/KEEP/UNDO queue.
    pub(crate) fn run_leave_queue_guarded(
        &mut self,
        code: &CompiledCode,
        start: usize,
        end: usize,
        compiled_fns: &CompiledFns,
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
                        attrs.insert("message".to_string(), Value::str(e.message.to_string()));
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

    // Cost: O(1) amortized (the chunk's label table -- `crate::op_scan_index`).
    pub(crate) fn find_label_target(&self, code: &CompiledCode, label: &str) -> Option<usize> {
        code.label_target(label)
    }

    /// Itemize a value read from a `$` scalar container so it behaves as a
    /// single element in list context. Arrays/Lists flip to their itemized
    /// `ArrayKind`; a Hash sets its `itemized` flag (mirroring `ArrayKind` — the
    /// value stays a `Hash` value, so value operations never see a wrapper and
    /// nothing leaks); a Seq is wrapped in a `Scalar`. Already-itemized
    /// values and non-container scalars pass through unchanged. (Set/Bag/Mix are
    /// only itemized in the `@a = $var` path — see `ItemizeVar` — not in general
    /// `$(...)` itemization, to avoid leaking a `Scalar` wrapper into set ops.)
    pub(crate) fn itemize_value(val: Value) -> Value {
        match val.view() {
            ValueView::Array(items, kind) if !kind.is_itemized() => {
                Value::array_with_kind(items.clone(), kind.itemize())
            }
            ValueView::Hash(_) => val.with_hash_itemized(true),
            // A `Slip` records the `$` container on the value too (`slip(5, 6)
            // .item.raku` is `$(slip(5, 6))`), as a second tag over the same
            // element `Arc`. It must NOT become a `Scalar` wrapper: that is the
            // "stop flattening" marker, and a `$`-held Slip still flattens
            // (`my $x = slip(5, 6); (1, $x, 2).elems` is 4).
            ValueView::Slip(_) => val.with_slip_itemized(true),
            ValueView::Seq(body) if body.view() == crate::value::SeqView::ItemList => val,
            ValueView::Seq(body) if body.view() == crate::value::SeqView::List => {
                Value::seq_body(body.as_item_list_view())
            }
            ValueView::Seq(body) => Value::scalar(Value::seq_body(body.clone())),
            // `{ ... } but R` is a Mixin wrapping the container, and the
            // itemization the `$` confers belongs to the container inside it:
            // `my $h = { a => 1 } but R; $h.raku` is `${:a(1)}`, exactly as it
            // is without the role. Itemize through the wrapper rather than
            // letting the Mixin hide the container from this match.
            ValueView::Mixin(inner, overrides) => Value::mixin_parts(
                std::sync::Arc::new(Self::itemize_value((**inner).clone())),
                overrides.clone(),
            ),
            _ => val,
        }
    }

    /// [`Self::itemize_value`] for a genuine `Array`/`Hash` ELEMENT STORE
    /// (`%h<k> = val`, `@a[i] = val` — ADR-0040 slice 1), where a `Seq`
    /// value needs different handling than `itemize_value`'s general one.
    ///
    /// `itemize_value`'s `Seq` arm wraps it in a `Value::scalar` — correct
    /// for most callers (e.g. array-LITERAL construction, `my @a =
    /// (1..5).Seq,;`, whose pinned `.raku` rendering must NOT show the `$`
    /// marker for a Seq the way it does for List/Array/Hash — measured
    /// against raku, `t/collections/array/array-single-listy-raku-comma.t`
    /// row 16). But a genuine element STORE needs the `Seq` kept as a `Seq`
    /// (tagged `ItemSeq`, like `itemize_scalar_store_value`'s plain-scalar
    /// `$x = SEQ` already does), not wrapped in `Scalar`: the wrapper drops
    /// the NaN-boxed `Kind::Seq` tag, so `is_seq_value()` (a pure tag probe)
    /// reports `false` for a value that is still logically a Seq, and the
    /// reify-before-stringify guard in `coerce_stringy_operand` never runs —
    /// `eq`/interpolation on a still-deferred Seq stored into an element then
    /// silently reads the empty not-yet-pulled generation instead of
    /// reifying first (found via Net::Netmask's `enumerate(:nets)` results
    /// compared through `Test::is`).
    ///
    /// `mark_itemized()` must run too: a bare `%h<k> .= unique;` statement
    /// discards the assignment expression's value in SINK context, and
    /// `SeqBody::sink_inner` only exempts a body the `itemized` flag names
    /// (checked separately from the `ItemSeq` view tag this itemizes to) —
    /// the same flag the plain-scalar `$s = SEQ` store sets via its own
    /// direct `mark_itemized()` call. Without it, the implicit sink poisons
    /// the SAME shared core the element just stored, so the very next read
    /// throws `X::Seq::Consumed` even though this is its first real read
    /// (`t/lang/operators/dot-eq.t`'s `%a<foo>.=unique`).
    pub(crate) fn itemize_value_for_element_store(val: Value) -> Value {
        if let ValueView::Seq(body) = val.view() {
            return match body.view() {
                crate::value::SeqView::ItemList | crate::value::SeqView::ItemSeq => val,
                crate::value::SeqView::List => Value::seq_body(body.as_item_list_view()),
                crate::value::SeqView::Seq => {
                    body.mark_itemized();
                    Value::seq_body(body.as_item_seq_view())
                }
            };
        }
        Self::itemize_value(val)
    }

    /// Itemize an aggregate VALUE stored into a `$` scalar container by
    /// assignment (`=`), so a later read reflects the Scalar container:
    /// `my $x = [1,2,3]; $x.raku` is `$[1, 2, 3]` (rakudo checks
    /// `nqp::iscont(SELF)` in `Array.raku`). Only the Value-level `ArrayKind`
    /// flips (identity of the backing `ArrayData` is preserved, so `=`-shared
    /// containers keep aliasing). Binds (`:=`) must NOT go through this — they
    /// install the value itself, not a Scalar container. The topic `_` is
    /// excluded (container-alias writeback, see `itemize_scalar_assign_result`),
    /// as are `&`-sigiled and internal `__mutsu_` names.
    pub(crate) fn itemize_scalar_store(name: &str, val: Value) -> Value {
        if Self::name_is_itemize_exempt(name) {
            return val;
        }
        Self::itemize_scalar_store_value(val)
    }

    /// True when a name is exempt from scalar-store itemization: the topic, a
    /// `&`-sigiled Callable binding, and the internal `__mutsu*` keys. Purely a
    /// property of the name, so a caller binding the same name repeatedly (a
    /// routine parameter, on every call) can settle it once -- see
    /// `CompiledFunction::param_itemize_on_bind`.
    #[inline]
    pub(crate) fn name_is_itemize_exempt(name: &str) -> bool {
        name == "_" || name.starts_with('&') || name.starts_with("__mutsu")
    }

    /// The value half of [`Self::itemize_scalar_store`], for a caller that has
    /// already settled the name half.
    pub(crate) fn itemize_scalar_store_value(val: Value) -> Value {
        match val.view() {
            ValueView::Array(items, kind) if !kind.is_itemized() => {
                Value::array_with_kind(items.clone(), kind.itemize())
            }
            // A Hash stored in a `$` scalar container is itemized per-holder:
            // `my $h = %x; $h.raku` is `${...}` while `%x.raku` stays `{...}`.
            // The per-holder itemization is a Value-level flag on the Hash
            // variant (mirroring `ArrayKind::ItemArray`): it shares the SAME
            // `HashData` Gc, so `=`-shared mutation still tracks, and the view
            // stays a plain hash so every consumer is transparent.
            ValueView::Hash(_) => val.with_hash_itemized(true),
            // A `Slip` stored in a `$` scalar container is itemized on the
            // value, by the same second-tag mechanism as the Hash arm above:
            // `my $x = slip(5, 6); $x.raku` is `$(slip(5, 6))` while a bare
            // `slip(5, 6).raku` stays `slip(5, 6)`. The view stays a `Slip`,
            // so the value keeps flattening everywhere it did before — a
            // `Scalar` wrapper would mean the opposite (stop flattening).
            ValueView::Slip(_) => val.with_slip_itemized(true),
            // A deferred `.cache` result is a List-view handle rather than a
            // real Array. Retag the handle without pulling its source, exactly
            // as the Array arm changes List -> ItemList without copying data.
            //
            // Every arm below also marks the shared reification core
            // itemized (`SeqBody::mark_itemized`): a `$`-held Seq/List
            // survives an implicit statement sink un-consumed (`my $s =
            // (1,2,3).Seq;` alone must not exhaust it), exactly like the
            // dedicated `body.mark_itemized()` call `SetLocal`'s own store
            // path (`vm_var_assign_set_local.rs`) makes beside its own call
            // into this function. Without it here too, a caller with no such
            // separate call of its own -- the attribute-accessor store
            // (`itemize_attr_store_value`) -- left the body un-itemized, so
            // `$obj.w = (1,2,3).Seq;`'s own implicit sink silently consumed
            // it before any later read (#9042).
            ValueView::Seq(body) if body.view() == crate::value::SeqView::ItemList => {
                body.mark_itemized();
                val
            }
            ValueView::Seq(body) if body.view() == crate::value::SeqView::List => {
                body.mark_itemized();
                Value::seq_body(body.as_item_list_view())
            }
            // A real `Seq` records the `$` container on the HANDLE (a second
            // `SeqView` tag over the same reification core), not as a `Scalar`
            // wrapper: rakudo's single-argument rule still flattens a `$`-held
            // `Seq` into a `+@` slurpy (`my $s = (1,2,3).Seq; map {...}, $s`
            // yields three elements, unlike a `$`-held `List`), so the value
            // must stay a `Seq` to every consumer while `.raku` renders the
            // container. Retagging pulls nothing, so a lazy source stays lazy.
            ValueView::Seq(body) if body.view() == crate::value::SeqView::Seq => {
                body.mark_itemized();
                Value::seq_body(body.as_item_seq_view())
            }
            // Unlike Arrays and Hashes, a Range has no itemized representation
            // of its own. A scalar container therefore keeps it as a Scalar
            // wrapper, making `$range` one subscript whose numeric value is the
            // range's element count rather than a slice selector.
            ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. } => val.item(),
            // Array/Hash subclasses keep their bare Instance representation so
            // type checks, lvalue writes, and subclass delegation remain
            // transparent. Record scalar itemization only as renderer metadata.
            ValueView::Instance { attributes, .. }
                if attributes.contains_key("__mutsu_array_storage")
                    || attributes.contains_key("__mutsu_hash_storage") =>
            {
                attributes.insert(
                    crate::builtins::methods_0arg::raku_repr::RAKU_SCALAR_ITEMIZED_KEY,
                    Value::TRUE,
                );
                val
            }
            // A `but`-mixed container keeps the itemization the `$` confers —
            // see the Mixin arm of `itemize_value`.
            ValueView::Mixin(inner, overrides) => Value::mixin_parts(
                std::sync::Arc::new(Self::itemize_scalar_store_value((**inner).clone())),
                overrides.clone(),
            ),
            _ => val,
        }
    }

    /// True when a scalar store is an identity restore: the incoming value is
    /// the SAME backing array with the SAME kind the slot already holds (the
    /// compiler-emitted hyper-func-op writeback re-storing an unmutated left
    /// operand). Skipping itemization then preserves a `:=`-bound bare List —
    /// re-storing what was read must not manufacture a Scalar container.
    pub(crate) fn is_identity_scalar_restore(current: &Value, val: &Value) -> bool {
        if let (ValueView::Array(cur, ck), ValueView::Array(new, nk)) = (current.view(), val.view())
        {
            crate::gc::Gc::ptr_eq(&cur, &new) && ck == nk
        } else {
            false
        }
    }

    /// The rvalue produced by a *scalar* assignment expression is the itemized
    /// container value, so a following list context sees it as one element:
    /// `@p = ($x = 3, 4)` gives `((3,4),)`. `@`/`%`/`&` names keep their value.
    ///
    /// The topic `$_` is excluded: when it aliases a whole container (`given @a {
    /// .=reverse }`) the assignment writes back through the alias, and itemizing
    /// the result would wrap the container written back to `@a`.
    pub(crate) fn itemize_scalar_assign_result(name: &str, val: Value) -> Value {
        if name.starts_with('@') || name.starts_with('%') || name.starts_with('&') || name == "_" {
            val
        } else {
            Self::itemize_value(val)
        }
    }

    /// Assigning `Nil` to an *untyped* scalar container resets it to the
    /// container's default type object, `Any` (`my $x = 5; $x = Nil` leaves
    /// `$x =:= Any`). Typed scalars reset to their own type object, handled
    /// separately. Only applies to genuine user scalar names — not `@`/`%`/`&`
    /// containers (which hold Nil as an element) nor internal `__mutsu_` temps.
    ///
    /// A scalar with an explicit `is default(X)` trait keeps its own default (the
    /// caller has already applied it), even when that default is `Nil` — e.g.
    /// `my $foo is default(Nil) = 42; $foo = Nil` leaves `$foo` as `Nil`, not
    /// `Any` — so such vars are excluded here.
    pub(crate) fn reset_nil_untyped_scalar(&self, name: &str, val: Value) -> Value {
        if val.is_nil()
            && !name.starts_with('@')
            && !name.starts_with('%')
            && !name.starts_with('&')
            && !name.contains("__mutsu")
            && self.var_default(name).is_none()
        {
            // A typed scalar attribute (`has Foo $.f; ... $!f = Nil`) resets to
            // its own type object, not Any. Attribute vars reach this untyped
            // branch because their type lives in the class registry, not in a
            // per-var constraint.
            if let Some(attr) = name.strip_prefix('!').or_else(|| name.strip_prefix('.'))
                && !attr.is_empty()
                && let Some(tc) = self.self_attr_type_constraint(name)
            {
                return Value::package(crate::symbol::Symbol::intern(&tc));
            }
            Value::package(crate::symbol::wk::any())
        } else {
            val
        }
    }

    /// The declared type of the scalar attribute that a local/env name refers
    /// to (`!x` / `.x` inside a method), for the assignment type check. The
    /// per-variable `__mutsu_type::` lane cannot carry this: it is keyed by
    /// bare name and would conflate `!n` across unrelated classes, so the
    /// constraint is resolved against the current `self`'s class instead.
    ///
    /// `None` for non-attribute names, for `@`/`%` attributes (whose declared
    /// type constrains the *elements*, checked on the container paths) and for
    /// unconstrained `Mu`/`Any` attributes, so the ordinary untyped-scalar
    /// handling is untouched.
    pub(crate) fn scalar_attr_type_constraint(&self, name: &str) -> Option<String> {
        if name.starts_with('@') || name.starts_with('%') {
            return None;
        }
        let (_, _) = crate::value::attr_twigil_base(name)?;
        let tc = self.self_attr_type_constraint(name)?;
        (!matches!(tc.as_str(), "Mu" | "Any")).then_some(tc)
    }

    /// [`Self::scalar_attr_type_constraint`] for a caller that already holds
    /// `name`'s interned form.
    // Cost: O(1) on a memo hit (see `vm_attr_type_constraint`).
    pub(crate) fn scalar_attr_type_constraint_sym(
        &self,
        name: &str,
        name_sym: crate::symbol::Symbol,
    ) -> Option<String> {
        if name.starts_with('@') || name.starts_with('%') {
            return None;
        }
        let (_, _) = crate::value::attr_twigil_base(name)?;
        let tc = self.self_attr_type_constraint_sym(name, name_sym)?;
        (!matches!(tc.as_str(), "Mu" | "Any")).then_some(tc)
    }

    /// De-itemize a `for … -> @a` chunk element while preserving its element
    /// type. An element-typed array (`array[int]`) keeps its type — only the
    /// itemization/scalar wrap is stripped, which is exactly the de-itemization
    /// the binding needs. Every other value flattens via the existing `.list`
    /// semantics (preserving prior behavior exactly).
    pub(crate) fn deitemize_for_bind(&mut self, val: Value) -> Result<Value, RuntimeError> {
        let deitemized = val.clone().deitemize_for_sigil_bind();
        let direct_aggregate = match val.view() {
            // A nested aggregate element arrives through a Scalar/container
            // holder.  The sigil binder needs that aggregate itself, not a
            // one-element list produced by calling `.list` on the holder.
            ValueView::Scalar(_) | ValueView::ContainerRef(_) => {
                matches!(
                    deitemized.view(),
                    ValueView::Array(..) | ValueView::Hash(..)
                )
            }
            // Preserve the existing typed-array fast path.  A plain outer
            // array still goes through `.list`, which retains metadata such
            // as an object-hash default during destructuring.
            ValueView::Array(data, _) => data.value_type.is_some() || data.declared_type.is_some(),
            _ => false,
        };
        if direct_aggregate {
            return Ok(deitemized);
        }
        self.call_method_with_values(val, "list", vec![])
    }
}
