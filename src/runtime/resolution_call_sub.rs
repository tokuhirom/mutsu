use super::*;

/// (pre_phasers, enter_phasers, success_queue, failure_queue, post_phasers, body_main)
pub(super) type SplitPhasers = (
    Vec<Stmt>,
    Vec<Stmt>,
    Vec<Stmt>,
    Vec<Stmt>,
    Vec<Stmt>,
    Vec<Stmt>,
);

impl Interpreter {
    pub(super) fn split_block_phasers(&self, stmts: &[Stmt]) -> SplitPhasers {
        let mut pre_ph = Vec::new();
        let mut enter_ph = Vec::new();
        let mut body_main = Vec::new();
        let mut success_queue = Vec::new();
        let mut failure_queue = Vec::new();
        let mut post_ph = Vec::new();
        for stmt in stmts {
            if let Stmt::Phaser { kind, body } = stmt {
                match kind {
                    PhaserKind::Pre => pre_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Enter => enter_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Post | PhaserKind::Leave | PhaserKind::Keep | PhaserKind::Undo => {}
                    _ => body_main.push(stmt.clone()),
                }
            } else {
                body_main.push(stmt.clone());
            }
        }
        // POST phasers in reverse source order
        for stmt in stmts.iter().rev() {
            if let Stmt::Phaser { kind, body } = stmt {
                match kind {
                    PhaserKind::Leave => {
                        success_queue.push(Stmt::Block(body.clone()));
                        failure_queue.push(Stmt::Block(body.clone()));
                    }
                    PhaserKind::Keep => success_queue.push(Stmt::Block(body.clone())),
                    PhaserKind::Undo => failure_queue.push(Stmt::Block(body.clone())),
                    PhaserKind::Post => post_ph.push(Stmt::Block(body.clone())),
                    _ => {}
                }
            }
        }
        (
            pre_ph,
            enter_ph,
            success_queue,
            failure_queue,
            post_ph,
            body_main,
        )
    }

    pub(super) fn make_promise_instance(&self, status: &str, result: Value) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("status".to_string(), Value::str(status.to_string()));
        attrs.insert("result".to_string(), result);
        Value::make_instance(Symbol::intern("Promise"), attrs)
    }

    /// The declared name of a callable value, for looking it up in name-keyed
    /// registries. `None` for anonymous blocks/closures.
    pub(crate) fn callable_value_name(func: &Value) -> Option<String> {
        let (package, name) = match func.view() {
            ValueView::Routine { package, name, .. } => (package.resolve(), name.resolve()),
            ValueView::Sub(data) => (data.package.resolve(), data.name.resolve()),
            _ => return None,
        };
        if name.is_empty() {
            return None;
        }
        if package.is_empty() || package == "GLOBAL" {
            Some(name.to_string())
        } else {
            Some(format!("{package}::{name}"))
        }
    }

    /// Dispatch `name` over C FFI if it is a registered `is native` sub.
    /// `Ok(None)` means "not a native sub" — the caller continues normally.
    ///
    /// Unlike the VM opcode paths this does not write `is rw` out-parameters
    /// back to a caller *slot* (there is no `CompiledCode` here to address one);
    /// it writes them into `env` and records the name, which the enclosing VM
    /// frame drains on return.
    pub(crate) fn try_dispatch_native_by_name(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        let Some(mut spec) = self.native_call_specs.get(name).cloned().or_else(|| {
            name.rsplit_once("::")
                .and_then(|(_, short)| self.native_call_specs.get(short).cloned())
        }) else {
            return Ok(None);
        };
        self.resolve_native_ret_struct(&mut spec);
        let call_args: Vec<Value> = args
            .iter()
            .filter(|a| !Self::is_callsite_line_marker(a))
            .cloned()
            .collect();
        let (result, out_args) =
            crate::runtime::nativecall::call_native_with_out_args(&spec, &call_args)?;
        for (idx, val) in out_args {
            if let ValueView::VarRef { name, .. } = call_args[idx].view() {
                let n = name.resolve().to_string();
                self.env_mut().insert(n.clone(), val);
                self.pending_rw_writeback_sources.push(n);
            }
        }
        Ok(Some(result))
    }

    pub(crate) fn call_sub_value(
        &mut self,
        func: Value,
        args: Vec<Value>,
        merge_all: bool,
    ) -> Result<Value, RuntimeError> {
        // Upgrade WeakSub to Sub transparently
        let func = match func.view() {
            ValueView::WeakSub(weak) => match weak.upgrade() {
                Some(strong) => Value::sub_value(strong),
                None => return Err(RuntimeError::new("Callable has been freed")),
            },
            _ => func,
        };
        // NativeCall: a sub declared `is native(...)` has a `{ * }` stub for a
        // body, so reaching that body at all is a bug — it must be dispatched
        // over C FFI instead. The two VM call opcodes check `native_call_specs`
        // by name, but a call through a code object (`my &f = &dlsym; f(|c)`,
        // which is how `NativeLibs` picks between the dyncall and libffi symbol
        // lookups) resolves the callee as a value and never consulted them, so
        // it ran the stub and returned `*`.
        if !self.native_call_specs.is_empty()
            && let Some(name) = Self::callable_value_name(&func)
            && let Some(result) = self.try_dispatch_native_by_name(&name, &args)?
        {
            return Ok(result);
        }
        if let ValueView::Routine {
            package,
            name,
            is_regex,
        } = func.view()
        {
            // A token/rule method value called with a cursor (`$meth($c)`,
            // e.g. from a custom grammar HOW's `find_method` wrapper) runs
            // the token at the cursor position and returns a Match.
            if is_regex
                && let Some(res) =
                    self.try_call_token_method_value(&package.resolve(), &name.resolve(), &args)
            {
                return res;
            }
            if !package.is_empty() && package != "GLOBAL" {
                let fq = format!("{package}::{name}");
                if self.resolve_function(&fq).is_some() {
                    return self.call_function(&fq, args);
                }
            }
            let name_str = name.resolve();
            if self.resolve_function(&name_str).is_some()
                || self.has_proto(&name_str)
                || self.has_multi_candidates(&name_str)
            {
                return self.call_function(&name_str, args);
            }
            // Method dispatch fallback for &?ROUTINE.dispatcher()(self, ...)
            // Only use this when the package is a known class.
            let pkg = package.resolve();
            if !args.is_empty() && !pkg.is_empty() && pkg != "GLOBAL" && self.has_class(&pkg) {
                let invocant = args[0].clone();
                let method_args = args[1..].to_vec();
                return self.call_method_with_values(invocant, &name_str, method_args);
            }
            return self.call_function(&name_str, args);
        }
        if let ValueView::Junction { kind, values } = func.view() {
            let mut results = Vec::with_capacity(values.len());
            for callable in values.iter() {
                results.push(self.call_sub_value(callable.clone(), args.clone(), merge_all)?);
            }
            return Ok(Value::junction(kind, results));
        }
        if let ValueView::Sub(data) = func.view() {
            // Check for wrap chain — if wrappers exist, dispatch through them.
            // A callsame/callwith invocation of the original (or an inner
            // wrapper) sets `wrap_skip_once` so exactly that call runs the sub
            // directly; a fresh named call — including a *recursive* one from
            // inside the original body — re-enters the chain like Raku does.
            let skip_chain_once = self.wrap_skip_once.take() == Some(data.id);
            if !skip_chain_once
                && let Some(chain) = self.wrap_chains.get(&data.id).cloned()
                && !chain.is_empty()
                // A nextcallee-returned wrappee carries __mutsu_wrap_direct:
                // it is the inner code object and always runs directly.
                && !matches!(
                    data.env.get("__mutsu_wrap_direct").map(Value::view),
                    Some(ValueView::Bool(true))
                )
            {
                let (sanitized_args, callsite_line) = self.sanitize_call_args(&args);
                self.test_pending_callsite_line = callsite_line;
                // Build remaining list: inner wrappers then original sub
                // chain is ordered inner-to-outer (last = outermost), so:
                // outermost is last, we call it; remaining = [inner..., original]
                let outermost = chain.last().unwrap().1.clone();
                let mut remaining: Vec<Value> = Vec::new();
                // Add wrappers from second-to-last down to first (inner order)
                for i in (0..chain.len() - 1).rev() {
                    remaining.push(chain[i].1.clone());
                }
                // Add the original sub at the end
                remaining.push(Value::sub_value(data.clone()));
                let frame = super::WrapDispatchFrame {
                    sub_id: data.id,
                    remaining,
                    args: sanitized_args.clone(),
                };
                let (wrapper_id, wrapper_base_env) = if let ValueView::Sub(wd) = outermost.view() {
                    (Some(wd.id), Some(wd.env.clone()))
                } else {
                    (None, None)
                };
                self.wrap_dispatch_stack.push(frame);
                let result = self.call_sub_value(outermost, sanitized_args, false);
                self.wrap_dispatch_stack.pop();
                // Propagate closure variable mutations from the wrapper back to
                // the current env so captured variables (e.g. $seen = True) are
                // visible to the caller. Only write back keys the wrapper actually
                // *changed* relative to its captured lexical snapshot: a persisted
                // value equal to the wrapper's capture is stale (never mutated) and
                // must not clobber the caller's live value (e.g. the `$h` that holds
                // this very WrapHandle, captured as Nil mid-`my $h = &foo.wrap(...)`).
                if let Some(wid) = wrapper_id
                    && let Some(persisted) = self.closure_env_overrides.get(&wid).cloned()
                {
                    for (k, v) in persisted.iter() {
                        if !self.env.contains_key_sym(*k) {
                            continue;
                        }
                        let unchanged = wrapper_base_env
                            .as_ref()
                            .is_some_and(|base| base.get_sym(*k) == Some(v));
                        if unchanged {
                            continue;
                        }
                        self.env.insert_sym(*k, v.clone());
                    }
                }
                return result;
            }
            let (sanitized_args, callsite_line) = self.sanitize_call_args(&args);
            self.test_pending_callsite_line = callsite_line;
            let mut call_args = sanitized_args.clone();
            if !data.assumed_positional.is_empty() || !data.assumed_named.is_empty() {
                let mut positional = Vec::new();
                let mut named = data.assumed_named.clone();
                let mut incoming_positional = Vec::new();
                for arg in &sanitized_args {
                    if let ValueView::Pair(key, boxed) = arg.view() {
                        named.insert(key.clone(), boxed.clone());
                    } else {
                        incoming_positional.push(arg.clone());
                    }
                }
                // Fill bare `*` primers from incoming positional args in order.
                // Remaining positional args are appended after all fixed primers.
                let mut incoming_idx = 0usize;
                for assumed in &data.assumed_positional {
                    let is_placeholder = matches!(assumed.view(), ValueView::Whatever)
                        || matches!(assumed.view(), ValueView::Num(f) if f.is_infinite())
                        || matches!(assumed.view(), ValueView::Rat(_, 0));
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
                call_args = positional;
                for (key, value) in named {
                    call_args.push(Value::pair(key, value));
                }
            }
            // Routine wrapper from .assuming() on a multi-dispatch sub
            if let Some(ValueView::Str(routine_name)) =
                data.env.get("__mutsu_routine_name").map(Value::view)
            {
                return self.call_function(&routine_name, call_args);
            }
            // Multi-dispatch dispatcher: captured multi candidates from resolve_code_var
            if let Some((candidates_arc, _)) = data
                .env
                .get("__mutsu_multi_dispatch_candidates")
                .cloned()
                .and_then(Value::into_array)
            {
                let candidates = (*candidates_arc).clone();
                // First try to dispatch via the function table (if still in scope)
                if let Some(ValueView::Str(name)) =
                    data.env.get("__mutsu_multi_dispatch_name").map(Value::view)
                    && (self.resolve_function(&name).is_some()
                        || self.has_proto(&name)
                        || self.has_multi_candidates(&name))
                {
                    return self.call_function(&name, call_args);
                }
                // Candidates are out of scope -- dispatch through captured Subs
                for candidate in &candidates {
                    if let ValueView::Sub(cand_data) = candidate.view()
                        && self
                            .bind_function_args_values(
                                &cand_data.param_defs,
                                &cand_data.params,
                                &call_args,
                            )
                            .is_ok()
                    {
                        return self.call_sub_value(candidate.clone(), call_args, false);
                    }
                }
                // Slurpy catch-all
                for candidate in &candidates {
                    if let ValueView::Sub(cand_data) = candidate.view()
                        && cand_data.param_defs.iter().any(|pd| pd.slurpy)
                    {
                        return self.call_sub_value(candidate.clone(), call_args, false);
                    }
                }
                if let Some(candidate) = candidates.first() {
                    return self.call_sub_value(candidate.clone(), call_args, false);
                }
            }
            if let (Some(left), Some(right)) = (
                data.env.get("__mutsu_compose_left").cloned(),
                data.env.get("__mutsu_compose_right").cloned(),
            ) {
                let right_result = self.call_sub_value(right, call_args, false)?;
                let (left_params, left_param_defs) = self.callable_signature(&left);
                let left_variadic = left_param_defs.iter().any(|pd| pd.slurpy && !pd.named);
                let left_expects_single = match left.view() {
                    ValueView::Sub(left_data) if left_params.is_empty() => {
                        let (uses_positional, _) = Self::auto_signature_uses(&left_data.body);
                        !uses_positional
                    }
                    _ => !left_variadic && left_params.len() == 1,
                };
                let left_args = Self::composed_result_to_args(right_result, left_expects_single);
                return self.call_sub_value(left, left_args, false);
            }
            // Reject arguments for blocks with explicitly empty signatures
            // (e.g. `-> { ... }` which takes 0 parameters).
            if data.empty_sig && !call_args.is_empty() {
                return Err(Self::reject_args_for_empty_sig(&call_args));
            }
            let saved_env = self.env.clone();
            let saved_readonly = self.enter_readonly_frame();
            if let Some(line) = self.test_pending_callsite_line {
                self.cur_source_line = line;
            }
            self.push_caller_env();
            let persist_closure_env =
                data.name.is_empty() || !self.has_function(&data.name.resolve());
            let closure_base_env = if persist_closure_env {
                self.closure_env_overrides
                    .get(&data.id)
                    .cloned()
                    .unwrap_or_else(|| data.env.clone())
            } else {
                data.env.clone()
            };
            // Free vars this closure lexically OWNS: the creating frame declares
            // them and never mutates them after the capture op ran (the compiler's
            // `authoritative_free_vars`, plus the runtime-vouched
            // `authoritative_captures` for a closure built inside a `.map`/`.grep`
            // block). A same-named lexical in whatever frame happens to call the
            // closure is an unrelated binding and must never shadow these — that is
            // lexical scoping degrading into dynamic scoping. The VM's
            // `call_compiled_closure` already installs them with overwrite; this is
            // the interpreter-path twin, and it is what lets the `merge_all`
            // caller-priority merge below stay sound (see the comment there).
            let auth_free_vars: &[crate::symbol::Symbol] = data
                .compiled_code
                .as_ref()
                .map(|cc| cc.authoritative_free_vars.as_slice())
                .unwrap_or(&[]);
            let is_authoritative = |k: crate::symbol::Symbol| {
                auth_free_vars.contains(&k) || data.authoritative_captures.contains(&k)
            };
            let mut new_env = saved_env.clone();
            for (k, v) in &closure_base_env {
                // A captured `ContainerRef` is a shared container cell (box-on-
                // capture / lever C): the single source of truth for that lexical,
                // so it must OVERWRITE any stale value the caller env holds (a
                // value the declaring block leaked, or this closure's own prior
                // writeback). The default "don't overwrite" merge would hide the
                // live cell and make a *second* bareword `f()` call read a stale
                // value (`my &f; { my $a=3; &f=sub{$a++} }; f(); f()` returned 3
                // then 0). Mirrors the VM closure dispatch (`call_compiled_closure`).
                if matches!(v.view(), ValueView::ContainerRef(_)) {
                    new_env.insert_sym(*k, v.clone());
                    continue;
                }
                // `self` is LEXICAL in Raku: a block has no invocant of its own,
                // so `self`/`$.attr` inside it resolves to the enclosing method's
                // invocant — the one this closure captured. The merge_all
                // don't-overwrite below made it *dynamic* instead: a natively
                // invoked callback (a `whenever` body dispatched from a supplier
                // tap or the in-process connect path) ran against whatever `self`
                // the caller env last leaked — e.g. Cro::TCP::Listener.incoming's
                // whenever body read `$.nodelay` off a Cro::TCP::Replier. Mirrors
                // the VM closure dispatch's force-install; a method's own invocant
                // is bound from its args after this merge, so it still wins.
                if k.with_str(|s| s == "self") {
                    new_env.insert_sym(*k, v.clone());
                    continue;
                }
                // Caller-priority: a `Proxy` FETCH body (and the other
                // native-invoked callbacks that pass `merge_all`) must see the
                // CURRENT value of a captured lexical its STORE twin mutates
                // (substr-rw's `$str`), and a dynamic var must resolve against the
                // live caller chain. But a name the closure lexically owns is not
                // up for grabs — hence the `is_authoritative` escape hatch, which
                // covers exactly the never-mutated captures where "same name" can
                // safely be read as "the closure's own binding".
                if merge_all && !is_authoritative(*k) {
                    new_env.entry_or_insert(k.resolve(), v.clone());
                    continue;
                }
                if matches!(
                    new_env.get_sym(*k).map(Value::view),
                    Some(ValueView::Array(..))
                ) && matches!(v.view(), ValueView::Array(..))
                {
                    continue;
                }
                new_env.insert_sym(*k, v.clone());
            }
            // `self` may live in a PARENT tier of the captured env: the loop
            // above iterates the own tier only (`Env::iter` does not walk the
            // chain, unlike `get`), so the lexical-self install in the loop
            // never fires for a closure captured under a scoped overlay — a
            // supply block created in `Sink.sinker` and driven from another
            // object's method read `$!attr` off that other object. Resolve
            // through the tier-walking get and force-install (self is lexical;
            // a method's own invocant is bound from its args after this).
            if let Some(captured_self) = closure_base_env.get("self").cloned() {
                new_env.insert("self".to_string(), captured_self);
            }
            self.env = new_env.clone();
            // Check for empty signature: a pointy block `-> { }` or sub with no
            // params that doesn't use @_/%_ must reject positional arguments.
            let positional_call_args: Vec<&Value> = call_args
                .iter()
                .filter(|v| !matches!(v.view(), ValueView::Pair(_, _)))
                .collect();
            if data.empty_sig && !positional_call_args.is_empty() {
                self.pop_caller_env();
                self.env = saved_env;
                self.exit_readonly_frame(saved_readonly);
                return Err(Self::reject_args_for_empty_sig(&call_args));
            }
            let rw_bindings =
                match self.bind_function_args_values(&data.param_defs, &data.params, &call_args) {
                    Ok(bindings) => bindings,
                    Err(e) => {
                        self.pop_caller_env();
                        self.env = saved_env;
                        self.exit_readonly_frame(saved_readonly);
                        return Err(e);
                    }
                };
            new_env = self.env.clone();
            if data.params.is_empty() {
                for arg in &sanitized_args {
                    if let ValueView::Pair(name, value) = arg.view() {
                        new_env.insert(format!(":{}", name), value.clone());
                    }
                }
            }
            // Bind implicit $_ for bare blocks called with arguments
            let (uses_positional, _) = Self::auto_signature_uses(&data.body);
            if data.params.is_empty()
                && !uses_positional
                && !sanitized_args.is_empty()
                && let Some(first_positional) = sanitized_args
                    .iter()
                    .find(|v| !matches!(v.view(), ValueView::Pair(_, _)))
            {
                new_env.insert("_".to_string(), first_positional.clone());
                new_env.insert("$_".to_string(), first_positional.clone());
            } else if data.params.is_empty()
                && sanitized_args.is_empty()
                && data.name.is_empty()
                && let Some(caller_topic) = saved_env.get("_")
            {
                new_env.insert("_".to_string(), caller_topic.clone());
                new_env.insert("$_".to_string(), caller_topic.clone());
            }
            // &?BLOCK: weak self-reference to break reference cycles
            let block_arc = crate::gc::Gc::new(crate::value::SubData {
                package: data.package,
                name: data.name,
                params: data.params.clone(),
                param_defs: data.param_defs.clone(),
                body: data.body.clone(),
                is_rw: data.is_rw,
                is_raw: data.is_raw,
                env: new_env.clone(),
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
                authoritative_captures: data.authoritative_captures.clone(),
                upvalues: data.upvalues.clone(),
            });
            new_env.insert(
                "&?BLOCK".to_string(),
                Value::weak_sub(crate::gc::Gc::downgrade(&block_arc)),
            );
            let block_sub = Value::make_sub_with_id(
                data.package,
                data.name,
                vec![],
                Vec::new(),
                data.body.clone(),
                data.is_rw,
                new_env.clone(),
                data.id,
            );
            self.env = new_env;
            self.env.insert(
                "__mutsu_callable_id".to_string(),
                Value::int(data.id as i64),
            );
            self.routine_stack.push(RoutineFrame {
                package: data.package.resolve(),
                name: data.name.resolve(),
                line: None,
                file: None,
                is_method: false,
                is_block: true,
                def_file: None,
            });
            self.block_stack.push(block_sub);
            let return_spec = data
                .env
                .get("__mutsu_return_type")
                .and_then(|v| match v.view() {
                    ValueView::Str(s) => Some(s.to_string()),
                    _ => None,
                });
            self.prepare_definite_return_slot(return_spec.as_deref());
            let let_mark = self.let_saves.len();
            // Snapshot the body-entry env so the exit writeback can tell which
            // captured outer scalars the body actually *mutated* (and propagate
            // only those) from those it merely captured a stale snapshot of.
            // Without this, a callee's stale captured value (e.g. an original
            // sub's snapshot of `$h` taken before `my $h = &foo.wrap(...)`
            // assigned it) would clobber the caller's live value on return.
            // Env is copy-on-write, so this clone is O(1) until the body forks it.
            let body_entry_env = self.env.clone();
            // Runtime transitive vouching (see `Interpreter::frame_authoritative`):
            // record the free-var names this block vouches for (its own
            // `authoritative_free_vars` plus any inherited via `owned_captures`) so
            // a closure created inside its body inherits authoritative (overwrite)
            // capture. This is the interpreter-path (`.map`/`.grep` block) twin of
            // the VM's `call_compiled_closure_with_topic` set; restored right after
            // the body runs. Without it a closure created inside a map/grep block
            // loses lexical capture of an outer free var when the block is invoked
            // through a callee that has a same-named parameter.
            let saved_frame_auth = std::mem::replace(
                &mut self.frame_authoritative,
                data.compiled_code
                    .as_ref()
                    .map(|cc| {
                        super::resolution_map_grep::frame_authoritative_set(
                            cc,
                            &data.authoritative_captures,
                        )
                    })
                    .unwrap_or_default(),
            );
            // Tell the fresh-compiler body path which PLACEHOLDER params this
            // call has bound (`{ 0 <= $^p <= 5 }` called as a subset `where`
            // predicate), so the chained-comparison DoBlock's stray-placeholder
            // check does not die on an attached one. Restored right after; a
            // nested call sets and restores its own.
            let saved_eval_placeholders = std::mem::replace(
                &mut self.pending_eval_placeholder_params,
                data.params
                    .iter()
                    .filter(|p| p.trim_start_matches(['$', '@', '%', '&']).starts_with('^'))
                    .cloned()
                    .collect(),
            );
            // Package-scoped name resolution: run the body under the closure's
            // declaring package so nested-class short names and `our`-vars
            // resolve when the Sub value is invoked from a foreign frame —
            // mirrors the VM closure dispatch. A `start Transform.new(...)`
            // inside a method of the class that declares nested `class
            // Transform` ran its body on a thread whose current package was
            // the SPAWNING frame's (Cro::CompositeConnector), so the
            // suppressed nested name did not resolve.
            let saved_anon_pkg = {
                let pkg = data.package.resolve();
                if !pkg.is_empty() && pkg != "GLOBAL" && !pkg.contains("::&") {
                    let saved = self.current_package().to_string();
                    self.set_current_package(pkg.to_string());
                    Some(saved)
                } else {
                    None
                }
            };
            // Carry the compiler's supply-body mark onto the chunk
            // `eval_block_value` is about to compile from `data.body` — see
            // `pending_supply_block_body`.
            self.pending_supply_block_body = data
                .compiled_code
                .as_ref()
                .is_some_and(|cc| cc.is_supply_block_body);
            let body_result = self.eval_block_value(&data.body);
            self.pending_supply_block_body = false;
            if let Some(p) = saved_anon_pkg {
                self.set_current_package(p);
            }
            self.pending_eval_placeholder_params = saved_eval_placeholders;
            self.frame_authoritative = saved_frame_auth;
            let result = match body_result {
                Err(mut e) if e.is_leave => {
                    let routine_key = format!("{}::{}", data.package, data.name);
                    let matches_frame = if let Some(target_id) = e.leave_callable_id() {
                        target_id == data.id
                    } else if let Some(target_routine) = e.leave_routine() {
                        target_routine == routine_key
                    } else {
                        e.label.is_none()
                    };
                    if matches_frame {
                        e.is_leave = false;
                        e.control = None;
                        if e.return_value.is_none() {
                            e.return_value = Some(Value::NIL);
                        }
                        Err(e)
                    } else {
                        Err(e)
                    }
                }
                other => other,
            };
            let effective_return_spec = return_spec
                .as_deref()
                .map(|spec| self.resolved_type_capture_name(spec));
            self.block_stack.pop();
            self.routine_stack.pop();
            // Manage let saves based on sub result
            match &result {
                Ok(_) => {
                    // Successful completion — restore temps, discard lets
                    self.resolve_let_saves_on_success(let_mark, true);
                }
                Err(e) if e.return_value.is_some() => {
                    // Explicit return — restore temps, discard lets
                    self.resolve_let_saves_on_success(let_mark, true);
                }
                Err(_) => {
                    // Exception/fail — restore saves
                    self.restore_let_saves(let_mark);
                }
            }
            if persist_closure_env {
                let mut persisted_closure_env = closure_base_env.clone();
                for key in closure_base_env.keys() {
                    if let Some(value) = self.env.get_sym(*key).cloned() {
                        persisted_closure_env.insert_sym(*key, value);
                    }
                }
                self.closure_env_overrides
                    .insert(data.id, persisted_closure_env);
            }
            // Preserve map rw topic writeback across env restoration
            let rw_map_topic = self.env.get("__mutsu_rw_map_topic__").cloned();
            let mut merged = saved_env;
            self.pop_caller_env_with_writeback(&mut merged);
            // Slice 1b (captured-outer cell sharing): a nested-declared method/sub
            // dispatched through `call_sub_value` (`$d.bar` where `method bar { … }`
            // was declared in `foo`'s body) runs its body via the interpreter and
            // merges a captured-outer scalar write back into `merged` (the caller
            // env) below. But the caller's matching *local slot* is not refreshed
            // from env unless the blanket reconcile is on. Record each captured-outer
            // scalar this body actually *changed* (vs its body-entry snapshot) so the
            // call site (`apply_pending_rw_writeback`) writes the merged value straight
            // through to the caller's slot, dropping the dependency on the reverse
            // `sync_locals_from_env` pull. Mirrors `merge_method_env`'s
            // `changed_caller_locals` for the compiled method path.
            let mut captured_outer_writes: Vec<String> = Vec::new();
            // A name the body declared with its own `my` is block-private and must
            // never reach the caller, even though it differs from the body-entry
            // snapshot: that snapshot holds the CALLER's same-named value, so a
            // fresh `my` always looks like a mutation to the compares below. This
            // is the same rule `push_block_declared_keys` and the compiled-closure
            // exit merge (`vm_closure_dispatch`) already apply; only this
            // interpreter path was missing it, which is how a `supply { my $x … }`
            // body's lexical escaped into the caller's `$x`. A declared name that
            // is ALSO a free var refers to the outer binding for its
            // pre-declaration uses, so its writeback is kept.
            let body_declared = data
                .compiled_code
                .as_ref()
                .map(|cc| (&cc.my_declared_sym, &cc.free_var_syms));
            // A name the closure lexically OWNS is likewise not the caller's: it
            // was installed with overwrite on entry (`is_authoritative` above),
            // so writing it back would push the closure's private binding onto a
            // caller lexical that merely shares the name — the `whenever` body of
            // a `supply { my $x … }`, whose per-instance state lives in
            // `closure_env_overrides` instead.
            let is_body_private = |k: crate::symbol::Symbol| {
                data.authoritative_captures.contains(&k)
                    || body_declared
                        .as_ref()
                        .is_some_and(|(declared, free)| declared.contains(&k) && !free.contains(&k))
            };
            if merge_all {
                for (k, v) in self.env.iter() {
                    // Per-call-site index-rw temps are frame-internal; merging a
                    // callee's same-named entries corrupts the caller's pending
                    // post-call writeback compare (see is_index_rw_call_temp).
                    if k.with_str(crate::runtime::utils::is_index_rw_call_temp) {
                        continue;
                    }
                    if k == "_" || k == "@_" {
                        continue;
                    }
                    // The ambient callable-instance id is frame-scoped: leaking a
                    // callee's (or a nested callee's) id into the caller env makes
                    // closures created later in the caller capture the WRONG
                    // non-local-return target (a quit-handler `return` then
                    // escapes its routine instead of returning from it).
                    if k == "__mutsu_callable_id" {
                        continue;
                    }
                    if is_body_private(*k) {
                        continue;
                    }
                    if merged.contains_key_sym(*k) {
                        // Only propagate what the body actually CHANGED (vs its
                        // body-entry snapshot). An entry equal to that snapshot is
                        // the callee's own captured lexical, not a mutation, and
                        // writing it back would overwrite an unrelated caller
                        // variable that merely shares the name — `cglobal`'s FETCH
                        // captures `$libname` = 'libmariadb.so.0', its caller
                        // `try-versions` has its own `Str $libname` = 'mariadb',
                        // and every probe after the first read the clobbered value
                        // (NativeLibs, the DBIish mysql path).
                        if body_entry_env.get_sym(*k) == Some(v) {
                            continue;
                        }
                        // Unlike the non-merge_all branch below, this path is used
                        // by native-invoked callbacks (Promise/Supply/reduce/lvalue
                        // Proxy/on-switch, ...) where nothing else drains
                        // `pending_rw_writeback_sources` right after the call. Any
                        // value type the body actually changed (not just scalars)
                        // must be tracked here, or the caller's cached local slot
                        // stays stale until an unrelated later call happens to drain
                        // it (io-cathandle.t on-switch: `$args = (a, b)` assigns a
                        // List, which the old Bool/Int/Num/Str/Rat-only whitelist
                        // silently dropped).
                        captured_outer_writes.push(k.resolve().to_string());
                        merged.insert_sym(*k, v.clone());
                    } else if k.starts_with("__mutsu_var_meta::") {
                        merged.insert_sym(*k, v.clone());
                    }
                }
            } else {
                // Names bound from subsignature parameters are block-local and
                // must not be written back to the caller (otherwise a parameter
                // such as `|c(Str $x)` would clobber a caller variable that
                // happens to share the parameter's name).
                let mut subsig_names = std::collections::HashSet::new();
                for pd in &data.param_defs {
                    Self::collect_sub_signature_names(&pd.sub_signature, &mut subsig_names);
                }
                for (k, v) in self.env.iter() {
                    if k == "_" || k == "@_" || subsig_names.contains(&k.resolve()) {
                        continue;
                    }
                    // See the merge_all branch: index-rw temps never merge back.
                    if k.with_str(crate::runtime::utils::is_index_rw_call_temp) {
                        continue;
                    }
                    // See the merge_all branch: the ambient callable-instance id
                    // is frame-scoped and must never leak into the caller env.
                    if k == "__mutsu_callable_id" {
                        continue;
                    }
                    if is_body_private(*k) {
                        continue;
                    }
                    if matches!(v.view(), ValueView::Array(..)) {
                        // Arrays are Arc-shared; in-place mutations are already
                        // visible to the caller, and reassignment should propagate.
                        merged.insert_sym(*k, v.clone());
                    } else if merged.contains_key_sym(*k)
                        && matches!(
                            v.view(),
                            ValueView::Bool(_)
                                | ValueView::Int(_)
                                | ValueView::Num(_)
                                | ValueView::Str(_)
                                | ValueView::Rat(_, _)
                        )
                        // Only write a captured scalar back to the caller when the
                        // body actually changed it from its body-entry value. A
                        // value equal to the entry snapshot is a stale capture, not
                        // a mutation, and must not clobber the caller's live value.
                        && body_entry_env.get_sym(*k) != Some(v)
                    {
                        captured_outer_writes.push(k.resolve().to_string());
                        merged.insert_sym(*k, v.clone());
                    }
                }
            }
            // Record the captured-outer scalar writes for precise caller-slot
            // write-back (drained by the call-site `apply_pending_rw_writeback`).
            self.pending_rw_writeback_sources
                .extend(captured_outer_writes);
            self.merge_sigilless_alias_writes(&mut merged, &self.env);
            // Apply rw bindings after merge so they take precedence
            self.apply_rw_bindings_to_env(&rw_bindings, &mut merged);
            // Restore map rw topic tracker if it was set during block execution
            if let Some(topic_val) = rw_map_topic {
                merged.insert("__mutsu_rw_map_topic__".to_string(), topic_val);
            }
            self.env = merged;
            self.exit_readonly_frame(saved_readonly);
            if let Err(e) = &result
                && e.is_fail()
            {
                return Ok(self.fail_error_to_failure_value(e));
            }
            // Bare blocks and pointy blocks are NOT routine boundaries for `return`.
            // Propagate the return signal with the target callable ID so the
            // enclosing routine can catch it.
            let is_non_routine =
                data.is_bare_block || data.compiled_code.as_ref().is_some_and(|cc| !cc.is_routine);
            if is_non_routine
                && let Err(ref e) = result
                && e.return_value.is_some()
            {
                // Only propagate non-local return if we can identify the target
                // routine (via __mutsu_callable_id in captured env or already set).
                // If no target exists, catch it locally (e.g., supply block done+return).
                let has_target = e.return_target_callable_id().is_some()
                    || data.env.contains_key("__mutsu_callable_id");
                if has_target {
                    let mut e = result.unwrap_err();
                    if e.return_target_callable_id().is_none()
                        && let Some(ValueView::Int(id)) =
                            data.env.get("__mutsu_callable_id").map(Value::view)
                    {
                        e.set_return_target_callable_id(Some(id as u64));
                    }
                    return Err(e);
                }
            }
            let result = match result {
                Err(e) if e.is_leave => return Err(e),
                other => other,
            };
            let finalized =
                self.finalize_return_with_spec(result, effective_return_spec.as_deref());
            let fetch_rw = data.is_rw && !data.is_raw;
            return finalized.and_then(|v| {
                let v = if let ValueView::LazyList(list) = v.view() {
                    let mut env = list.env.clone();
                    env.insert(
                        "__mutsu_preserve_lazy_on_array_assign".to_string(),
                        Value::TRUE,
                    );
                    Value::lazy_list(crate::gc::Gc::new(crate::value::LazyList {
                        body: list.body.clone(),
                        env,
                        cache: std::sync::Mutex::new(list.cache.lock().unwrap().clone()),
                        compiled_code: list.compiled_code.clone(),
                        compiled_fns: list.compiled_fns.clone(),
                        elems_count: list.elems_count.clone(),
                        scan_spec: list
                            .scan_spec
                            .as_ref()
                            .map(|s| std::sync::Mutex::new(s.lock().unwrap().clone())),
                        sequence_spec: list.sequence_spec.clone(),
                        coroutine: list
                            .coroutine
                            .as_ref()
                            .map(|c| std::sync::Mutex::new(c.lock().unwrap().clone())),
                        lazy_pipe: list
                            .lazy_pipe
                            .as_ref()
                            .map(|p| std::sync::Mutex::new(p.lock().unwrap().clone())),
                        closure_seq: list
                            .closure_seq
                            .as_ref()
                            .map(|c| std::sync::Mutex::new(c.lock().unwrap().clone())),
                        walk_pending: list
                            .walk_pending
                            .as_ref()
                            .map(|w| std::sync::Mutex::new(w.lock().unwrap().clone())),
                        cat_pull: list
                            .cat_pull
                            .as_ref()
                            .map(|c| std::sync::Mutex::new(c.lock().unwrap().clone())),
                    }))
                } else {
                    v
                };
                self.maybe_fetch_rw_proxy(v, fetch_rw)
            });
        }
        Err(RuntimeError::new("Callable expected"))
    }
}
