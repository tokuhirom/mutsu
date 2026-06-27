use super::*;

impl Interpreter {
    /// Find the first positional argument that is a Junction and whose corresponding
    /// parameter type constraint does not accept Junction (i.e., needs auto-threading).
    /// Returns the index of that argument, or None if no auto-threading is needed.
    fn find_junction_autothread_arg_with_pointy(
        &self,
        data: &crate::value::SubData,
        args: &[Value],
        is_pointy_block: bool,
    ) -> Option<usize> {
        // Collect positional param_defs (skip named)
        let positional_params: Vec<&crate::ast::ParamDef> = data
            .param_defs
            .iter()
            .filter(|pd| !pd.named && !pd.slurpy && !pd.double_slurpy && !pd.onearg)
            .collect();

        // Also filter out named args from the args list to get positional args
        let mut positional_idx = 0usize;
        for (i, arg) in args.iter().enumerate() {
            // Skip named args (pairs where key matches a named param)
            if let Value::Pair(_, _) = arg {
                // Check if this is a named argument
                let is_named = data.param_defs.iter().any(|pd| {
                    pd.named
                        && if let Value::Pair(key, _) = arg {
                            pd.name.trim_start_matches('$').trim_start_matches(':') == key.as_str()
                        } else {
                            false
                        }
                });
                if is_named {
                    continue;
                }
            }

            if let Value::Junction { .. } = arg {
                // Check if the corresponding param accepts Junction
                if let Some(pd) = positional_params.get(positional_idx) {
                    let constraint = pd.type_constraint.as_deref().unwrap_or(
                        if is_pointy_block || pd.name.starts_with('@') || pd.name.starts_with('%') {
                            "Mu" // pointy blocks and array/hash sigils: implicit Mu
                        } else {
                            "Any" // scalar params ($x stored as "x"): implicit Any
                        },
                    );
                    // Mu and Junction accept junctions directly
                    if constraint == "Mu" || constraint == "Junction" {
                        // No auto-threading needed
                    } else {
                        // Check if constraint is a subset with Mu/Junction base
                        let resolved_base = self.resolve_subset_base_type(constraint);
                        if resolved_base != "Mu" && resolved_base != "Junction" {
                            return Some(i);
                        }
                    }
                }
                // If no param_def found (extra args), implicit Any rejects Junction
                else if positional_idx >= positional_params.len() {
                    // Slurpy or extra - don't auto-thread
                }
            }
            positional_idx += 1;
        }
        None
    }

    /// Call a compiled closure (Value::Sub with compiled_code).
    pub(super) fn call_compiled_closure(
        &mut self,
        data: &crate::value::SubData,
        cc: &CompiledCode,
        args: Vec<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        self.call_compiled_closure_with_topic(data, cc, args, None, false, compiled_fns)
    }

    /// Like [`Self::call_compiled_closure`] but with an optional explicit topic
    /// and optional rw-topic capture, both used by the native `.map` loop.
    ///
    /// `explicit_topic`: for Pair-shaped source elements, a positional `Pair`
    /// passed to the general call machinery is bound as a *named* argument (and
    /// skipped when setting the implicit `$_`), so the block would see no topic.
    /// When `Some`, the topic `$_` (and a lone positional param) is force-bound to
    /// the element value regardless of its pair-ness.
    ///
    /// `capture_rw_topic`: when true, the block's final `$_` value is stashed in
    /// `self.rw_map_topic_capture` (read from the live frame just after the body
    /// runs, before the frame is popped) so the native map loop can implement
    /// Raku's rw binding — `@a.map({ $_++ })` mutates `@a`. This captures the
    /// topic value directly rather than relying on the `__mutsu_rw_map_topic__`
    /// signal, so it also covers `$_++`/`$_--` (which the interpreter's
    /// signal-based writeback misses).
    pub(super) fn call_compiled_closure_with_topic(
        &mut self,
        data: &crate::value::SubData,
        cc: &CompiledCode,
        args: Vec<Value>,
        explicit_topic: Option<Value>,
        capture_rw_topic: bool,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        let (mut args, callsite_line) = self.sanitize_call_args(&args);
        if callsite_line.is_some() {
            loan_env!(self, set_pending_callsite_line(callsite_line));
        }
        // Slice F: clear any rw-writeback sources left undrained by a sibling
        // call so this dispatch's call site only sees this call's own sources
        // (see call_compiled_function_named for the full rationale).
        self.pending_rw_writeback_sources.clear();

        // Apply assumed args from .assuming() (same logic as tree-walker call_sub_value)
        if !data.assumed_positional.is_empty() || !data.assumed_named.is_empty() {
            let mut positional = Vec::new();
            let mut named = data.assumed_named.clone();
            let mut incoming_positional = Vec::new();
            for arg in &args {
                if let Value::Pair(key, boxed) = arg {
                    named.insert(key.clone(), *boxed.clone());
                } else {
                    incoming_positional.push(arg.clone());
                }
            }
            let mut incoming_idx = 0usize;
            for assumed in &data.assumed_positional {
                let is_placeholder = matches!(assumed, Value::Whatever)
                    || matches!(assumed, Value::Num(f) if f.is_infinite())
                    || matches!(assumed, Value::Rat(_, 0));
                if is_placeholder {
                    if incoming_idx < incoming_positional.len() {
                        positional.push(incoming_positional[incoming_idx].clone());
                        incoming_idx += 1;
                    }
                } else {
                    positional.push(assumed.clone());
                }
            }
            positional.extend(incoming_positional.into_iter().skip(incoming_idx));
            for (key, value) in named {
                positional.push(Value::Pair(key, Box::new(value)));
            }
            args = positional;
        }

        // Junction auto-threading: if any positional argument is a Junction and the
        // corresponding parameter's type constraint does not accept Junction (i.e., is
        // not Mu or Junction), call the closure once per eigenvalue and collect results
        // into a new Junction of the same kind.
        // For pointy blocks (-> { }), untyped params have implicit Mu, so skip
        // auto-threading when the param has no type constraint.
        if let Some(junction_idx) =
            self.find_junction_autothread_arg_with_pointy(data, &args, cc.is_pointy_block)
            && let Value::Junction { kind, values } = &args[junction_idx]
        {
            let kind = kind.clone();
            let values = values.clone();
            let mut results = Vec::with_capacity(values.len());
            for eigenvalue in values.iter() {
                let mut threaded_args = args.clone();
                threaded_args[junction_idx] = eigenvalue.clone();
                results.push(self.call_compiled_closure_with_topic(
                    data,
                    cc,
                    threaded_args,
                    explicit_topic.clone(),
                    capture_rw_topic,
                    compiled_fns,
                )?);
            }
            return Ok(Value::junction(kind, results));
        }

        // Check for "backdoor" private attribute access:
        // A free-standing method object (package != class) that has $!attr locals
        // (local names starting with '!') being called on an instance should throw.
        // In Raku, $!attr in a method is only valid inside the class that owns it.
        {
            let method_pkg = data.package.resolve();
            let invocant = args.first();
            if let Some(Value::Instance { class_name, .. }) = invocant {
                let class = class_name.resolve();
                // Only check free-standing methods (not defined in a class, or defined
                // in a different class than the invocant).
                let is_foreign = method_pkg != class
                    && (method_pkg.is_empty()
                        || method_pkg == "GLOBAL"
                        || !self.has_class(&method_pkg));
                if is_foreign {
                    // Check if the compiled code has any !attr locals
                    let has_attr_locals = cc
                        .locals
                        .iter()
                        .any(|name| name.starts_with('!') && name.len() > 1);
                    if has_attr_locals {
                        return Err(RuntimeError::new(format!(
                            "Cannot access private attribute from outside class {}",
                            class
                        )));
                    }
                }
            }
        }

        self.push_call_frame();
        let saved_stack_depth = self.call_frames.last().unwrap().saved_stack_depth;
        let saved_state_scope = self.state_scope_id;
        self.state_scope_id = Some(data.id);

        loan_env!(self, inject_pending_callsite_line());

        // Scoped-overlay (docs/vm-dual-store.md Slice 6): install an empty
        // born-owned overlay over the (flat) caller so the captured-env merge,
        // per-instance state, &?BLOCK / callable_id setup, param binding and the
        // body's writes all land in a fresh map instead of forking the caller env.
        // `frame.saved_env` holds the flat caller for restoration; the exit
        // writeback iterates this overlay (overlay-only) = exactly the closure's
        // own mutations, which is what it must propagate back to the caller.
        {
            let parent = self.env().clone();
            self.set_env(crate::env::Env::scoped_child(parent));
        }

        // Merge captured environment into current env (or_insert = don't overwrite existing).
        // Key directly by the captured Symbol to avoid a resolve()+re-intern per entry.
        // EXCEPTION: a `ContainerRef` captured value is a *shared container cell*
        // (box-on-capture, lever C Slice 2). It is the single source of truth for
        // that lexical, so it must OVERWRITE any stale plain value the caller env
        // currently holds (e.g. a later loop iteration's slot re-injection) — the
        // don't-overwrite default would otherwise hide this closure's own cell.
        for (k, v) in data.env.iter() {
            if matches!(v, Value::ContainerRef(_)) {
                self.env_mut().insert_sym(*k, v.clone());
            } else {
                self.env_mut().entry_or_insert_sym(*k, v.clone());
            }
        }
        for (sym, maybe_val) in cc.free_var_syms.iter().zip(data.captured_upvalues.iter()) {
            let Some(val) = maybe_val else { continue };
            if matches!(val, Value::ContainerRef(_)) {
                self.env_mut().insert_sym(*sym, val.clone());
            } else {
                self.env_mut().insert_sym(*sym, val.clone());
            }
        }
        // Per-iteration loop captures (Raku fresh-binding semantics): these free
        // variables were declared in an enclosing loop body when this closure was
        // created, so each iteration's closure froze a distinct value in its
        // captured `data.env` (copy-on-write). Overwrite the caller's current
        // value with this closure's own captured value, so calling the closure
        // *after* the loop reads its iteration's value rather than the loop's
        // final value (which the don't-overwrite merge + dual-store slot
        // re-injection would otherwise leak in). Applied *before* the
        // per-instance-state override below so a mutating loop closure's
        // accumulated state still wins on later calls. See PLAN.md lever C.
        for sym in &data.owned_captures {
            if let Some(val) = data
                .captured_upvalue(cc, *sym)
                .or_else(|| data.env.get_sym(*sym))
                .cloned()
            {
                self.env_mut().insert_sym(*sym, val);
            }
        }
        // Override with persisted per-closure-instance captured variable state.
        // This ensures that each closure instance maintains independent mutable
        // state across calls (e.g. two closures from the same factory each get
        // their own copy of captured variables).
        //
        // Only the closure's free variables can be mutated by its body, so only
        // those carry per-instance state. Iterating `cc.free_var_syms` (a handful
        // of names) instead of the whole captured env (~100 names) avoids that
        // many `format!` allocations and state-var lookups per call.
        let cap_overrides: Vec<(Symbol, Value)> = cc
            .free_var_syms
            .iter()
            .filter_map(|k| {
                // Skip box-on-capture cells: a ContainerRef-captured lexical is a
                // shared container, not per-instance frozen state, so the stale
                // closure_captured_state value must not clobber the live Arc.
                if matches!(data.env.get_sym(*k), Some(Value::ContainerRef(_))) {
                    return None;
                }
                self.get_closure_captured_state(data.id, *k)
                    .map(|val| (*k, val.clone()))
            })
            .collect();
        for (k, val) in cap_overrides {
            self.env_mut().insert_sym(k, val);
        }

        loan_env!(self, push_caller_env());

        // Push Sub value to block_stack for callframe().code
        // Also set &?BLOCK as a weak self-reference (mirrors resolution.rs)
        let block_arc = std::sync::Arc::new(crate::value::SubData {
            package: data.package,
            name: data.name,
            params: data.params.clone(),
            param_defs: data.param_defs.clone(),
            body: vec![],
            is_rw: data.is_rw,
            is_raw: data.is_raw,
            env: data.env.clone(),
            assumed_positional: data.assumed_positional.clone(),
            assumed_named: data.assumed_named.clone(),
            id: data.id,
            empty_sig: data.empty_sig,
            is_bare_block: data.is_bare_block,
            compiled_code: data.compiled_code.clone(),
            deprecated_message: data.deprecated_message.clone(),
            source_line: data.source_line,
            source_file: data.source_file.clone(),
            owned_captures: data.owned_captures.clone(),
            captured_upvalues: data.captured_upvalues.clone(),
            captured_upvalues_from_local: data.captured_upvalues_from_local.clone(),
        });
        self.env_mut().insert(
            "&?BLOCK".to_string(),
            Value::WeakSub(std::sync::Arc::downgrade(&block_arc)),
        );
        self.push_block(Value::Sub(block_arc));

        // Push routine info for leave/return/when targeting.
        // For pointy blocks, we push a special marker name so that
        // &?ROUTINE can skip it and see the enclosing routine.
        let call_line = self.current_source_line();
        let call_file = self.current_source_file();
        if cc.is_pointy_block || data.is_bare_block {
            // Bare blocks and pointy blocks are NOT routine boundaries.
            // Push a marker name so &?ROUTINE skips them and finds the
            // enclosing sub/method.
            self.push_block_routine_with_location(
                data.package.resolve(),
                "<pointy-block>".to_string(),
                call_line,
                call_file,
            );
        } else {
            self.push_block_routine_with_location(
                data.package.resolve(),
                data.name.resolve(),
                call_line,
                call_file,
            );
        }
        self.env_mut().insert(
            "__mutsu_callable_id".to_string(),
            Value::Int(data.id as i64),
        );

        if data.empty_sig && !args.is_empty() {
            self.pop_routine();
            self.pop_block();
            self.pop_caller_env();
            self.stack.truncate(saved_stack_depth);
            let frame = self.pop_call_frame();
            *self.env_mut() = frame.saved_env;
            return Err(Interpreter::reject_args_for_empty_sig(&args));
        }

        // Bind parameters
        let rw_bindings = match loan_env!(
            self,
            bind_function_args_values(&data.param_defs, &data.params, &args)
        ) {
            Ok(bindings) => bindings,
            Err(e) => {
                self.pop_routine();
                self.pop_block();
                self.pop_caller_env();
                self.stack.truncate(saved_stack_depth);
                let frame = self.pop_call_frame();
                *self.env_mut() = frame.saved_env;
                return Err(Interpreter::enhance_binding_error(
                    e,
                    &data.name.resolve(),
                    &data.param_defs,
                    &args,
                ));
            }
        };

        // Handle implicit $_ for bare blocks (no explicit params, single arg)
        let uses_positional = data.params.iter().any(|p| p != "_" && !p.starts_with(':'));
        if !uses_positional
            && !data.params.is_empty()
            && data.params.iter().all(|p| p.starts_with('$'))
        {
            // Named params with placeholders: handled by bind_function_args_values
        } else if !uses_positional && !args.is_empty() {
            if let Some(first) = args.iter().find(|v| !matches!(v, Value::Pair(_, _))) {
                self.env_mut().insert("_".to_string(), first.clone());
            }
        } else if data.params.is_empty() && args.is_empty() && data.name.is_empty() {
            let caller_topic = self.call_frames.last().unwrap().saved_env.get("_").cloned();
            if let Some(topic) = caller_topic {
                self.env_mut().insert("_".to_string(), topic);
            }
        }

        // Raku: routines get their own $_ initialized to (Any). A *block* with
        // placeholder params (`{ $^a }`) is still a block, not a routine, so it
        // keeps the enclosing lexical `$_` (mutsu compiles such a bare block as a
        // routine for `return`/`&?ROUTINE` purposes, but its topic must still be
        // the outer `$_`). Skip the reset when an explicit `$_` param is present
        // or when the signature is made of placeholder params (`$^x`).
        let has_placeholder_param = data.param_defs.iter().any(|pd| {
            pd.name.starts_with('^') || pd.name.starts_with("@^") || pd.name.starts_with("%^")
        });
        if cc.is_routine
            && !has_placeholder_param
            && !data.param_defs.iter().any(|pd| pd.name == "_")
        {
            self.env_mut().insert(
                "_".to_string(),
                Value::Package(crate::symbol::Symbol::intern("Any")),
            );
        }

        // Raku: $! is scoped per routine — fresh Nil on entry
        if !data.name.resolve().is_empty() {
            self.env_mut().insert("!".to_string(), Value::Nil);
        }

        // Explicit topic override (native `.map` over Pair-shaped elements). The
        // general call machinery binds a positional Pair as a named argument and
        // skips the implicit `$_`, so force the topic — and a lone positional
        // param — to the element value. Applied after the routine-`$_` reset so
        // it wins, and before the locals load so the slot picks it up.
        if let Some(topic) = explicit_topic {
            let env = self.env_mut();
            env.insert("_".to_string(), topic.clone());
            env.insert("$_".to_string(), topic.clone());
            // A single simple positional param consumes the topic too (e.g.
            // `-> $p { $p.key }`, which the native map call site stores as
            // `params == ["p"]` with empty `param_defs`). The call site only
            // passes a topic for blocks with no placeholder/special params, so a
            // lone plain positional is the only param to force-bind here.
            let pos_param = if let [pd] = data.param_defs.as_slice() {
                (!pd.named
                    && !pd.slurpy
                    && !pd.is_invocant
                    && !pd.name.is_empty()
                    && !pd.name.starts_with(':'))
                .then(|| pd.name.clone())
            } else if data.param_defs.is_empty() {
                match data.params.as_slice() {
                    [p] if is_plain_positional_param(p) => Some(p.clone()),
                    _ => None,
                }
            } else {
                None
            };
            if let Some(name) = pos_param {
                env.insert(name, topic);
            }
        }

        self.locals = vec![Value::Nil; cc.locals.len()];
        for (i, local_name) in cc.locals.iter().enumerate() {
            if let Some(val) = self.env().get(local_name) {
                self.locals[i] = val.clone();
            }
        }
        // Load persisted state variable values using scoped keys
        // (state_scope_id is set to data.id above, so scoped_state_key
        // will generate closure-instance-specific keys automatically).
        for (slot, key) in &cc.state_locals {
            let scoped_key = self.scoped_state_key(key);
            if let Some(val) = self.get_state_var(&scoped_key) {
                self.locals[*slot] = val.clone();
            }
        }

        // Snapshot the values of this closure's free variables so the writeback
        // below can tell whether the body actually changed anything the caller
        // cares about. The only outer state a closure can mutate is a free
        // variable -- directly (SetGlobal in its body) or transitively (a nested
        // closure that captured it from this frame and wrote it back). Both leave
        // the variable's value in this frame's env differing from entry. This is
        // a handful of values, and unlike comparing the env Arc it is immune to
        // the per-statement `?LINE` bookkeeping write that always dirties env.
        let free_at_entry: Vec<Option<Value>> = cc
            .free_var_syms
            .iter()
            .map(|k| self.env().get_sym(*k).cloned())
            .collect();
        // A captured variable that lives in a local slot is flushed to env on
        // exit (above) rather than written through env during the body, so its
        // change is not visible via `free_at_entry`; force the writeback then.
        let has_captured_local = cc
            .locals
            .iter()
            .any(|n| !n.is_empty() && data.captures_name(Some(cc), n));

        let let_mark = self.let_saves_len();
        let mut ip = 0;
        let mut result = Ok(());
        let mut explicit_return: Option<Value> = None;
        let mut fail_bypass = false;
        while ip < cc.ops.len() {
            match self.exec_one(cc, &mut ip, compiled_fns) {
                Ok(()) => {}
                Err(mut e) if e.is_leave => {
                    let routine_key = format!("{}::{}", data.package, data.name);
                    let matches_frame = if let Some(target_id) = e.leave_callable_id {
                        target_id == data.id
                    } else if let Some(target_routine) = e.leave_routine.as_ref() {
                        target_routine == &routine_key
                    } else {
                        e.label.is_none()
                    };
                    if matches_frame {
                        e.is_leave = false;
                        e.is_last = false;
                        let ret_val = e.return_value.unwrap_or(Value::Nil);
                        explicit_return = Some(ret_val.clone());
                        self.stack.truncate(saved_stack_depth);
                        self.stack.push(ret_val);
                        self.discard_let_saves(let_mark);
                        result = Ok(());
                        break;
                    }
                    loan_env!(self, restore_let_saves(let_mark));
                    result = Err(e);
                    break;
                }
                Err(e) if e.is_succeed() => {
                    // `when`/`default` succeed signals are caught at the
                    // enclosing block boundary (sub, method, or pointy block).
                    let ret_val = e.return_value.unwrap_or(Value::Nil);
                    explicit_return = Some(ret_val.clone());
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val);
                    self.discard_let_saves(let_mark);
                    result = Ok(());
                    break;
                }
                Err(mut e) if e.return_value.is_some() => {
                    // Non-routine closures (bare blocks, pointy blocks) are NOT
                    // return boundaries.  `return` inside them propagates up to
                    // the lexically enclosing routine (sub/method).
                    if !cc.is_routine {
                        // Only propagate if we can identify the target routine.
                        // If no target exists (e.g., supply block done+return),
                        // catch it locally.
                        let has_target = e.return_target_callable_id.is_some()
                            || data.env.contains_key("__mutsu_callable_id");
                        if has_target {
                            if e.return_target_callable_id.is_none()
                                && let Some(Value::Int(id)) = data.env.get("__mutsu_callable_id")
                            {
                                e.return_target_callable_id = Some(*id as u64);
                            }
                            loan_env!(self, restore_let_saves(let_mark));
                            result = Err(e);
                            break;
                        }
                        // No target: fall through to catch locally
                    }
                    // Routine closures: check if the return targets a specific
                    // callable; if so, only catch if it matches this closure.
                    if let Some(target_id) = e.return_target_callable_id
                        && target_id != data.id
                    {
                        loan_env!(self, restore_let_saves(let_mark));
                        result = Err(e);
                        break;
                    }
                    let ret_val = e.return_value.unwrap();
                    explicit_return = Some(ret_val.clone());
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val);
                    self.discard_let_saves(let_mark);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail => {
                    fail_bypass = true;
                    let failure = self.fail_error_to_failure_value(&e);
                    loan_env!(self, restore_let_saves(let_mark));
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(failure);
                    result = Ok(());
                    break;
                }
                Err(e) => {
                    loan_env!(self, restore_let_saves(let_mark));
                    result = Err(e);
                    break;
                }
            }
            if self.is_halted() {
                break;
            }
        }

        let ret_val = if result.is_ok() {
            if self.stack.len() > saved_stack_depth {
                self.stack.pop().unwrap_or(Value::Nil)
            } else {
                Value::Nil
            }
        } else {
            Value::Nil
        };

        self.stack.truncate(saved_stack_depth);

        // Capture the block's final `$_` for the native rw-`.map` writeback. Read
        // it here — right after the body ran, before the env/locals re-sync below
        // and the frame pop — so `$_++`/`$_=`/`s///` mutations are visible. The
        // local slot is authoritative when `$_` compiled to a local; otherwise the
        // env carries it (SetGlobal `_` / the `__mutsu_rw_map_topic__` signal).
        if capture_rw_topic {
            let local_topic = cc
                .locals
                .iter()
                .position(|n| n == "_")
                .map(|i| self.locals[i].clone());
            self.rw_map_topic_capture = local_topic
                .or_else(|| self.env().get("_").cloned())
                .or_else(|| self.env().get("__mutsu_rw_map_topic__").cloned());
        }

        // Sync state variables back using scoped keys
        for (slot, key) in &cc.state_locals {
            let local_name = &cc.locals[*slot];
            let val = self
                .env()
                .get(local_name)
                .cloned()
                .unwrap_or_else(|| self.locals[*slot].clone());
            let scoped_key = self.scoped_state_key(key);
            loan_env!(self, set_state_var(scoped_key, val));
        }

        // Restore the previous state scope
        self.state_scope_id = saved_state_scope;

        self.pop_routine();
        self.pop_block();

        // Sync locals back to env so captured variable changes are visible.
        // Only captured variables that also occupy a local slot need this: a
        // non-captured local is strictly frame-local and is discarded when the
        // env is restored to `saved_env` below, while a captured variable not in
        // a slot is mutated through the env directly (SetGlobal). Restricting the
        // writeback to captured locals avoids an O(env) copy-on-write deep copy
        // per call for the common read-only closure (which has no captured
        // locals to flush at all).
        for (i, local_name) in cc.locals.iter().enumerate() {
            if !local_name.is_empty() && data.captures_name(Some(cc), local_name) {
                {
                    let __v = self.locals[i].clone();
                    self.env_mut().insert(local_name.clone(), __v);
                }
            }
        }

        // Persist captured variable state so this closure instance retains
        // its own mutable state across calls (independent from other closures).
        // Mirror of `cap_overrides` above: only free variables can be mutated by
        // the body, so only those need persisting.
        for k in &cc.free_var_syms {
            if let Some(val) = self.env().get_sym(*k).cloned() {
                self.set_closure_captured_state(data.id, *k, val);
            }
        }

        // Environment writeback: merge changes back to caller
        let frame = self.pop_call_frame();
        let mut restored_env = frame.saved_env;
        self.pop_caller_env_with_writeback(&mut restored_env);
        loan_env!(
            self,
            apply_rw_bindings_to_env(&rw_bindings, &mut restored_env)
        );
        // Slice F: record the caller-source names this `is rw` writeback touched
        // so the call-site op (which holds the caller's `code`) writes each new
        // value straight through to the caller's local slot, keeping it coherent
        // without the reverse `sync_locals_from_env` pull. The values land in
        // `restored_env`, which becomes `self.env` before this function returns.
        if !rw_bindings.is_empty() {
            self.pending_rw_writeback_sources
                .extend(rw_bindings.iter().map(|(_, source)| source.clone()));
        }
        // Build set of parameter names — these are strictly local to the
        // function call and must never leak back to the caller's env, even
        // when they share a name with a captured outer variable.
        // Keyed by interned Symbol so the per-entry membership checks in the
        // writeback scan below compare Symbols directly (a u32 compare + hash)
        // instead of resolving every env key back to a &str via `with_str` --
        // that Symbol-to-string churn dominated the writeback profile.
        let mut param_names: std::collections::HashSet<Symbol> = std::collections::HashSet::new();
        for p in &data.params {
            param_names.insert(Symbol::intern(p));
        }
        // Collect names bound by subsignature parameters (e.g. `|c(Str $x)`),
        // which are also strictly call-local and must not leak to the caller.
        let mut subsig_names: std::collections::HashSet<String> = std::collections::HashSet::new();
        for pd in &data.param_defs {
            if !pd.name.is_empty() {
                param_names.insert(Symbol::intern(&pd.name));
            }
            Interpreter::collect_sub_signature_names(&pd.sub_signature, &mut subsig_names);
        }
        for name in &subsig_names {
            param_names.insert(Symbol::intern(name));
        }

        // The full-env writeback below scans the entire working env (~100
        // entries) to propagate the closure's mutations back to the caller. It
        // is only needed when the closure actually changed something visible to
        // the caller: a free variable changed value (direct or transitive write),
        // it dirtied the env with a non-bookkeeping write (e.g. a sigilless
        // alias), it has rw parameters, or it modified a captured local. A
        // read-only *leaf* closure (the common map/grep/sort block) trips none of
        // these and skips the whole scan.
        //
        // The skip is only sound for a leaf closure (one that makes no calls,
        // i.e. `!cc.has_calls`). Once the body makes a call we cannot reason
        // locally about what got mutated: a nested method/closure call can write
        // *any* captured variable back into this frame's env -- including one that
        // is captured here but is not a free variable of this closure (so it is
        // invisible to the `free_changed` check). Two regressing shapes:
        //   * `{ $*OUT.write($buf) }` (advent2011) mutates the enclosing `$output`
        //     through the dynamically-dispatched `$*OUT.write` method closing over
        //     it.
        //   * `-> $blk, $v { $blk($v) }` forwards a call to a closure that mutates
        //     an outer lexical the forwarder never names.
        // `cc.has_calls` covers *every* call opcode (CallFunc/CallMethod/ExecCall/
        // CallOnValue/CallOnCodeVar/Hyper.../CallDefined/...) -- unlike
        // `has_env_writes`, which lists only some of them and so missed
        // `CallOnCodeVar` (the `$blk($v)` case).
        let free_changed = cc
            .free_var_syms
            .iter()
            .zip(free_at_entry.iter())
            .any(|(k, old)| self.env().get_sym(*k) != old.as_ref());
        // A writable parameter (sigilless `\a`, `is rw`, or `is raw`) bound to a
        // caller-provided container writes the mutation into this frame's env
        // under the *source container's* name (e.g. the synthetic `__mutsu_*`
        // var a hyper function-op injects, or a `$x` the caller passed by
        // reference). That source name is neither a free variable nor a
        // parameter, so the `free_changed`/`rw_bindings` checks miss it; the
        // env-scan below is what propagates it back to the caller. Force the
        // scan whenever the closure has such a parameter so the write-back is
        // not silently dropped for a non-mutating-looking frame (env_dirty may
        // be clear if a prior call reset it).
        let has_writable_params = data
            .param_defs
            .iter()
            .any(|pd| pd.sigilless || pd.traits.iter().any(|t| t == "rw" || t == "raw"));
        // `cc.has_env_writes` catches a body that writes a captured-outer lexical
        // by name (a `SetGlobal`/`AssignExpr` to a var that is captured but not
        // tracked in `free_var_syms`, e.g. an anonymous role-method `gist` doing
        // `$seen = 1`) — `free_changed` misses it, so without this the writeback
        // is silently dropped. A read-only leaf block (the common map/grep body)
        // has no env-write opcode and still skips the scan.
        let needs_caller_writeback = free_changed
            || has_captured_local
            || has_writable_params
            || !rw_bindings.is_empty()
            || cc.has_calls
            || cc.has_env_writes;
        // Whether any closure-writeback metadata key (sigilless readonly/alias,
        // state-var, predictive-seq) may exist in any env. The common program
        // creates none, so this stays false and the per-call metadata scans
        // (and the `__mutsu_*` prefix checks below) are skipped entirely.
        let meta_possible = crate::env::closure_meta_keys_possible();
        if needs_caller_writeback {
            let rw_sources: std::collections::HashSet<Symbol> = rw_bindings
                .iter()
                .map(|(_, source)| Symbol::intern(source))
                .collect();
            let captured_names: std::collections::HashSet<Symbol> = data
                .env
                .keys()
                .copied()
                .chain(cc.free_var_syms.iter().copied())
                .collect();
            // Write back captured-variable changes, but NOT the closure's own
            // parameters/locals (which live in cc.locals).  Without this filter,
            // recursive &?BLOCK calls clobber the outer frame's $n, etc.
            // `cc.locals_sym` is `cc.locals` pre-interned at compile time, so
            // this avoids re-interning every local on every call.
            let local_names: std::collections::HashSet<Symbol> =
                cc.locals_sym.iter().copied().collect();
            let underscore_sym = Symbol::intern("_");
            let at_underscore_sym = Symbol::intern("@_");
            // Caller lexicals this scan writes back with a *changed* value. They are
            // recorded (after the loop, to avoid borrowing `self` while iterating
            // `self.env()`) on the retain-on-miss writeback list so the call site
            // refreshes the owner's local slot precisely — covers a nested
            // gist/method/closure that mutated an outer lexical which is NOT a direct
            // free var of THIS closure (`cap({ note … $seen = 1 })`), so the
            // free_var recording below misses it.
            let mut caller_writeback: Vec<Symbol> = Vec::new();
            for (k, v) in self.env().iter() {
                let plain_upvalue = data.captured_upvalue_from_local(cc, *k);
                if *k != underscore_sym
                    && *k != at_underscore_sym
                    && !plain_upvalue
                    && !rw_sources.contains(k)
                    && !param_names.contains(k)
                    && (restored_env.contains_key_sym(*k)
                        || captured_names.contains(k)
                        || (meta_possible
                            && (k.starts_with("__mutsu_predictive_seq_iter::")
                                || k.starts_with("__mutsu_sigilless_alias::!"))))
                    && (!local_names.contains(k) || captured_names.contains(k))
                {
                    // Don't leak captured-only variables to callers that don't have
                    // them. This prevents independent closures from sharing state
                    // via the calling env (e.g. two closures from the same factory
                    // should have their own captured variable copies).
                    if captured_names.contains(k) && !restored_env.contains_key_sym(*k) {
                        continue;
                    }
                    // A genuine caller lexical whose value changed across the call:
                    // queue its slot for a precise refresh.
                    if restored_env.contains_key_sym(*k)
                        && restored_env.get_sym(*k) != Some(v)
                        && !k.starts_with("__mutsu")
                    {
                        caller_writeback.push(*k);
                    }
                    restored_env.insert_sym(*k, v.clone());
                }
            }
            for k in caller_writeback {
                self.record_caller_var_writeback(&k.resolve());
            }
            // Slice F (env<->locals coherence): a closure that mutated a captured
            // outer variable (`@a.map({ $sum += $_ })`, `$blk()` closing over a
            // lexical) has just written the new value into `restored_env` (which
            // becomes `self.env` below). Record the changed free-var names that
            // are genuine caller lexicals so the call-site op — which holds the
            // caller's `code` — writes each value straight through to the caller's
            // local slot (`apply_pending_rw_writeback`), dropping the dependency on
            // the reverse `sync_locals_from_env` pull. Only changed free vars that
            // the caller actually has are recorded (a slot-less name is a harmless
            // no-op in the drain; a captured-only var is filtered out).
            if free_changed {
                for (k, old) in cc.free_var_syms.iter().zip(free_at_entry.iter()) {
                    let plain_upvalue = data.captured_upvalue_from_local(cc, *k);
                    if *k != underscore_sym
                        && *k != at_underscore_sym
                        && !plain_upvalue
                        && !param_names.contains(k)
                        && restored_env.contains_key_sym(*k)
                        && self.env().get_sym(*k) != old.as_ref()
                    {
                        self.pending_rw_writeback_sources
                            .push(k.resolve().to_string());
                    }
                }
            }
        }
        // Write back readonly/alias metadata for captured variables that were
        // modified inside the closure (e.g. `$a := $arg` where `$a` is captured).
        // Only free variables can be modified by the body, so restrict the
        // (format!-heavy) metadata scan to them instead of the whole captured env.
        // Skipped wholesale when no sigilless/alias/predictive metadata has ever
        // been created (the common case) -- `meta_possible` gates the per-free-var
        // `format!`s and the env-wide `merge_sigilless_alias_writes` scan.
        if meta_possible {
            for captured_sym in &cc.free_var_syms {
                let captured_name = captured_sym.resolve();
                let readonly_key = format!("__mutsu_sigilless_readonly::{}", captured_name);
                if let Some(v) = self.env().get(&readonly_key).cloned() {
                    restored_env.insert(readonly_key, v);
                }
                let alias_key = format!("__mutsu_sigilless_alias::{}", captured_name);
                if let Some(v) = self.env().get(&alias_key).cloned() {
                    restored_env.insert(alias_key, v);
                }
            }
            self.merge_sigilless_alias_writes(&mut restored_env, self.env());
        }

        // Only run the state-variable sync and cleanup when the closure
        // references captured variables that may be state vars.  This avoids the
        // per-call overhead for simple closures in hot loops.  The `meta_possible`
        // gate skips the per-free-var `format!("__mutsu_state_key::...")` lookups
        // entirely for programs with no state variables.
        if meta_possible && !cc.free_var_syms.is_empty() {
            // Update state variable storage when closures modify captured
            // state variables.  The metadata key "__mutsu_state_key::<name>"
            // maps the variable name to its state storage key (set by
            // StateVarInit in the declaring scope).
            for k in &cc.free_var_syms {
                let meta_key = format!("__mutsu_state_key::{}", k);
                if let Some(Value::Str(state_key)) = self.env().get(&meta_key).cloned()
                    && let Some(val) = self.env().get_sym(*k).cloned()
                {
                    loan_env!(self, set_state_var(state_key.to_string(), val));
                }
            }
        }

        // Clean up variables that were declared locally in this closure but
        // not captured from an outer scope.
        for local_name in cc.locals.iter() {
            if !local_name.is_empty()
                && !data.env.contains_key(local_name)
                && !param_names.contains(&Symbol::intern(local_name))
                && !local_name.starts_with("__mutsu_")
            {
                restored_env.remove(local_name);
            }
        }

        *self.env_mut() = restored_env;

        // After a closure returns, update captured envs of END phasers for
        // variables that the closure captures (and may have modified).  This
        // ensures END phasers see the final values rather than stale copies.
        // Only update keys matching the closure's captured variable names to
        // avoid overwriting unrelated captured lexicals in other END phasers.
        if self.end_phaser_count() > 0 && !data.env.is_empty() {
            let captured_strs: Vec<String> = data
                .env
                .keys()
                .copied()
                .chain(cc.free_var_syms.iter().copied())
                .map(|s| s.resolve())
                .collect();
            let captured_names: std::collections::HashSet<&str> =
                captured_strs.iter().map(|s| s.as_str()).collect();
            // Flatten: END phasers run at program exit with this captured env;
            // it must hold the full lexical view, not a transient scoped overlay.
            let current = self.clone_env();
            self.update_end_phaser_envs_for_keys(&captured_names, &current);
        }

        let return_spec = data.env.get("__mutsu_return_type").and_then(|v| match v {
            Value::Str(s) => Some(s.to_string()),
            _ => None,
        });
        let effective_return_spec = return_spec
            .as_deref()
            .map(|spec| loan_env!(self, resolved_type_capture_name(spec)));

        match result {
            Ok(()) if fail_bypass => Ok(ret_val),
            Ok(()) => {
                // For closures, absorb `return` — don't re-propagate as error.
                let base_val = explicit_return.unwrap_or(ret_val);
                loan_env!(
                    self,
                    finalize_return_with_spec(Ok(base_val), effective_return_spec.as_deref())
                )
            }
            Err(e) => Err(e),
        }
    }
}

/// True when `p` is a plain positional parameter name as stored in
/// `SubData::params` for a pointy/bare block — a bare identifier with no sigil,
/// twigil, or placeholder/named prefix. Such a param is bound by position and
/// can be force-set to the map topic; placeholder (`^a`) / named (`:x`) /
/// aggregate (`@a`,`%h`) params cannot and must fall back to the interpreter.
pub(super) fn is_plain_positional_param(p: &str) -> bool {
    p.chars()
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
        && p != "_"
}
