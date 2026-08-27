use super::*;
use crate::runtime::resolution_map_grep::bind_loop_topic;
use crate::value::ValueView;

impl Interpreter {
    /// Returns the mapped result and whether any element of `list_items` was
    /// actually written back (Raku's rw binding of `$_` / an `is rw` param).
    /// The caller must only refresh the source array when that flag is set: a
    /// read-only block leaves the source untouched, and rebuilding it anyway
    /// would drop the container's per-slot metadata — most visibly the
    /// `initialized` bitmap, so a `:delete`d slot stopped reading as a hole and
    /// a later trailing-element `:delete` could no longer truncate the array
    /// (roast/S32-array/delete.t).
    pub(super) fn eval_map_over_items_rw(
        &mut self,
        func: Option<Value>,
        list_items: &mut [Value],
    ) -> Result<(Value, bool), RuntimeError> {
        // This construct handles `next`/`last`/`redo`, so a loop-control
        // statement raised anywhere in its dynamic extent has somewhere to go
        // (`runtime/loop_handler_depth.rs`). Without the guard the raise site
        // would convert the signal into a thrown `X::ControlFlow` and silently
        // break this loop.
        let _loop_handler = crate::runtime::loop_handler_depth::LoopHandlerGuard::new();
        let topic_key = "__mutsu_rw_map_topic__";
        let wrote_back = std::cell::Cell::new(false);
        if let Some(func_ref) = func.as_ref()
            && let ValueView::Sub(data) = func_ref.view()
        {
            let data = data.clone();
            let requires_full_binding = data.param_defs.iter().any(|pd| {
                pd.named
                    || pd.slurpy
                    || pd.sigilless
                    || pd.optional_marker
                    || pd.default.is_some()
                    || pd.type_constraint.is_some()
                    || pd.where_constraint.is_some()
                    || pd.sub_signature.is_some()
                    || pd.outer_sub_signature.is_some()
                    || pd.code_signature.is_some()
                    || pd.shape_constraints.is_some()
            });
            // A routine callback must run through the real call path so a
            // `return` in its body ends THAT call with the returned value
            // (routine semantics) — see the same gate in `eval_map_over_items`.
            let is_routine_callback = (!data.is_bare_block
                && data.compiled_code.as_ref().is_some_and(|cc| cc.is_routine)
                && !matches!(
                    data.env.get("__mutsu_callable_type").map(Value::view),
                    Some(ValueView::Str(kind)) if kind.as_str() == "WhateverCode"
                )
                // A placeholder block (`{ $^x.value }`) is a Block, not a
                // Routine, even though its compile path currently flags
                // is_routine (it compiles as a named-anon-sub body). It must
                // stay on the fast path: the general call machinery binds a
                // Pair element as a NAMED argument, leaving the placeholder
                // positional unbound (t/map-native-pairs.t).
                && crate::ast::collect_placeholders_shallow(&data.body).is_empty())
                // A body-less routine Sub (plan-derived, ADR-0019 C6e-3) must
                // take the real call path — see `eval_map_over_items`.
                || (data.body.is_empty() && data.compiled_routine.is_some());
            if requires_full_binding
                || is_routine_callback
                || super::resolution_map_grep::sub_is_call_carrier(&data)
            {
                // Fall through to call_sub_value path for complex cases
                let mut result = Vec::new();
                let arity = if !data.params.is_empty() {
                    let effective = data
                        .params
                        .len()
                        .saturating_sub(data.assumed_positional.len());
                    if effective == 0 { 1 } else { effective }
                } else {
                    1
                };
                // An explicit `is rw`/`is raw` scalar block param (`-> Int $x
                // is rw { $x++ }`) rw-aliases the source element's container,
                // same as a `$_`-mutating block via `topic_key` below -- but a
                // NAMED param never mirrors into `topic_key` (that mirror only
                // ever tracks `$_`/`_` writes), so it needs its own writable
                // cell, the same transient-`ContainerRef` pattern
                // `deepmap_leaf_call` uses. Only a single, non-assumed param
                // qualifies -- a multi-arity block has no one element to alias.
                let rw_param = (arity == 1)
                    .then(|| data.param_defs.get(data.assumed_positional.len()))
                    .flatten()
                    .filter(|pd| pd.traits.iter().any(|t| t == "rw" || t == "raw"));
                let mut i = 0usize;
                while i < list_items.len() {
                    if arity > 1 && i + arity > list_items.len() {
                        return Err(RuntimeError::new("Not enough elements for map block arity"));
                    }
                    let value = if rw_param.is_some() {
                        let cell = crate::gc::Gc::new(std::sync::Mutex::new(list_items[i].clone()));
                        let res = self.call_sub_value(
                            Value::sub_value(data.clone()),
                            vec![Value::container_ref(cell.clone())],
                            false,
                        )?;
                        list_items[i] = cell.lock().unwrap().clone();
                        wrote_back.set(true);
                        res.deref_container()
                    } else {
                        let chunk: Vec<Value> = if arity == 1 {
                            vec![list_items[i].clone()]
                        } else {
                            list_items[i..i + arity].to_vec()
                        };
                        self.env.remove(topic_key);
                        let v =
                            self.call_sub_value(Value::sub_value(data.clone()), chunk, false)?;
                        if arity == 1
                            && let Some(mutated) = self.env.get(topic_key).cloned()
                        {
                            list_items[i] = mutated;
                            wrote_back.set(true);
                        }
                        v
                    };
                    let value = self.reify_finite_pipe_value(value)?;
                    if let ValueView::Slip(elems) = value.view() {
                        result.extend(elems.iter().cloned());
                    } else {
                        result.push(value);
                    }
                    i += arity;
                }
                self.env.remove(topic_key);
                return Ok((Value::array(result), wrote_back.get()));
            }

            let arity = if !data.params.is_empty() {
                let effective = data
                    .params
                    .len()
                    .saturating_sub(data.assumed_positional.len());
                if effective == 0 { 1 } else { effective }
            } else {
                1
            };
            // See the rw_param comment on the call_sub_value branch above --
            // same shape check, for the untyped/unconstrained param that took
            // this env-insert fast path instead.
            let rw_param = (arity == 1)
                .then(|| data.param_defs.get(data.assumed_positional.len()))
                .flatten()
                .filter(|pd| pd.traits.iter().any(|t| t == "rw" || t == "raw"));
            let mut result = Vec::new();

            // Compile once, reuse VM for every iteration (same as eval_map_over_items).
            // Normalize a bare tail `Stmt::Call` carrying named/slip args (how an
            // imported sub call like `f(k => v)` parses) into `Stmt::Expr(Expr::Call)`
            // so its value is preserved as the block's result; otherwise it compiles
            // as a value-discarding statement and the map result wrongly falls back
            // to the topic `$_` (see `eval_map_over_items`).
            let compiler = crate::compiler::Compiler::new();
            let normalized_body =
                super::resolution_map_grep::normalize_tail_stmt_for_value(&data.body);
            let tail_is_when = super::resolution_map_grep::tail_is_when_chain(&normalized_body);
            let (code, compiled_fns) = compiler.compile(&normalized_body);

            let underscore = "_".to_string();
            let dollar_topic = "$_".to_string();

            let mut touched_keys: Vec<String> = Vec::with_capacity(data.params.len() + 1);
            for k in data.env.keys() {
                if !self.env.contains_key_sym(*k) {
                    touched_keys.push(k.resolve());
                }
            }
            for p in &data.params {
                if !touched_keys.contains(p) {
                    touched_keys.push(p.clone());
                }
            }
            if !touched_keys.iter().any(|k| k == "_") {
                touched_keys.push(underscore.clone());
            }
            if !touched_keys.iter().any(|k| k == "$_") {
                touched_keys.push(dollar_topic.clone());
            }
            super::resolution_map_grep::push_block_declared_keys(&mut touched_keys, &code);
            // `self` is lexical: the block's captured invocant wins over the
            // caller's (see `call_compiled_closure`). It is normally already in
            // the running env, so the loop above did not list it.
            if data.env.get("self").is_some() && !touched_keys.iter().any(|k| k == "self") {
                touched_keys.push("self".to_string());
            }
            let saved: Vec<(String, Option<Value>)> = touched_keys
                .iter()
                .map(|k| (k.clone(), self.env.get(k).cloned()))
                .collect();

            for (k, v) in &data.env {
                if k.with_str(|s| s == "self") || !self.env.contains_key_sym(*k) {
                    self.env.insert_sym(*k, v.clone());
                }
            }

            // A `$_`-referencing WhateverCode (`@a.map(* eq $_)`) binds the
            // element to its `*` placeholder, so `$_` must keep referring to the
            // CALLER's topic — only a bare block topicalizes `$_` to the element.
            // The List sibling (`eval_map_over_items`) and the grep loop below
            // both route their topic bind through `bind_loop_topic` for this;
            // this loop used to insert the element unconditionally, so
            // `@a.map(* eq $_)` compared each element against itself
            // (t/whatever-code-topic.t). When the topic is the caller's it is
            // NOT an alias for the element either, so it must not write back.
            let keeps_outer_topic = super::resolution_map_grep::block_keeps_outer_topic(&data);
            let outer_topic = self.env.get("_").cloned();

            // CP-3 collapse: run the rw map loop with fresh execution registers
            // (replaces the `mem::take(self)` + `VM::new` sub-VM). The closure
            // returns the loop's Result; `with_nested_registers` restores the
            // outer registers and flags env_dirty. The `saved`/`topic_key` env
            // restore is hoisted to after the call (ran on every old exit).
            // Runtime transitive vouching: see `frame_authoritative_set`.
            let block_authoritative = data
                .compiled_code
                .as_ref()
                .map(|cc| {
                    super::resolution_map_grep::frame_authoritative_set(
                        cc,
                        &data.authoritative_captures,
                    )
                })
                .unwrap_or_default();
            // ADR-0027: see the matching comment in `eval_map_over_items`
            // (`resolution_map_grep.rs`).
            let block_owned = data.owned_captures.clone();
            let loop_result: Result<Value, RuntimeError> = self.with_nested_registers(|vm| {
                // Scope `state` variables to the closure instance — the body was
                // re-compiled fresh, so two distinct blocks share compile-time
                // state keys (see the same line in `eval_map_over_items`).
                vm.state_scope_id.set(Some(data.id));
                let mut i = 0usize;
                while i < list_items.len() {
                    if arity > 1 && i + arity > list_items.len() {
                        return Err(RuntimeError::new("Not enough elements for map block arity"));
                    }
                    vm.frame_authoritative = block_authoritative.clone();
                    vm.frame_owned = block_owned.clone();
                    // Set when `rw_param` is active: the transient cell this
                    // iteration's param is bound to, read back after the call
                    // instead of `topic_key` (which never mirrors a NAMED
                    // param write, only `$_`/`_`).
                    let mut rw_cell: Option<crate::gc::Gc<std::sync::Mutex<Value>>> = None;
                    {
                        let assumed_count = data.assumed_positional.len();
                        for (idx, val) in data.assumed_positional.iter().enumerate() {
                            if let Some(p) = data.params.get(idx) {
                                vm.env_mut().insert(p.clone(), val.clone());
                            }
                        }
                        // Clear the topic tracker before each iteration
                        vm.env_mut().remove(topic_key);
                        if arity == 1 {
                            let item = list_items[i].clone();
                            if let Some(p) = data.params.get(assumed_count) {
                                if rw_param.is_some() {
                                    let cell =
                                        crate::gc::Gc::new(std::sync::Mutex::new(item.clone()));
                                    vm.env_mut()
                                        .insert(p.clone(), Value::container_ref(cell.clone()));
                                    rw_cell = Some(cell);
                                } else {
                                    vm.env_mut().insert(p.clone(), item.clone());
                                }
                            }
                            bind_loop_topic(vm.env_mut(), &item, keeps_outer_topic, &outer_topic);
                        } else {
                            for (idx, p) in data.params.iter().skip(assumed_count).enumerate() {
                                if i + idx < list_items.len() {
                                    vm.env_mut().insert(p.clone(), list_items[i + idx].clone());
                                }
                            }
                            bind_loop_topic(
                                vm.env_mut(),
                                &list_items[i],
                                keeps_outer_topic,
                                &outer_topic,
                            );
                        }
                    }
                    let writeback = |list_items: &mut [Value], vm: &Interpreter| {
                        if arity != 1 {
                            return;
                        }
                        if let Some(cell) = &rw_cell {
                            // An explicit `is rw` param aliases the element
                            // regardless of where `$_` points.
                            list_items[i] = cell.lock().unwrap().clone();
                            wrote_back.set(true);
                        } else if !keeps_outer_topic
                            && let Some(mutated) = vm.env().get(topic_key).cloned()
                        {
                            // Only a block that topicalizes `$_` to the element
                            // rw-aliases it; when `$_` is the caller's topic a
                            // write to it must not reach the source array.
                            list_items[i] = mutated;
                            wrote_back.set(true);
                        }
                    };
                    let saved_when_matched = vm.when_matched();
                    vm.when_nonmatch_value = None;
                    // This loop binds the block's param directly into `env`
                    // (above) instead of going through the normal call machinery
                    // (`bind_function_args_values`/`push_call_frame`), so
                    // `readonly_frames` is never incremented here. A compiled
                    // body that marks itself readonly at runtime -- e.g. a
                    // single-param pointy block's `Stmt::MarkReadonly` prologue
                    // (`compiler/expr_closure.rs`) -- would otherwise mark
                    // `readonly_vars` with `readonly_frames == 0`, which skips
                    // the undo journal entirely (see `mark_readonly_sym_with`)
                    // and leaks the mark PERMANENTLY into every later,
                    // unrelated same-named lexical in the program. Opening a
                    // proper (panic-safe) readonly scope per iteration — the
                    // same guard a real call frame uses — gives this body the
                    // same isolation `call_compiled_closure_with_topic` does.
                    let _readonly_guard =
                        crate::vm::vm_call_state_guard::ReadonlyFrameGuard::new(vm);
                    match vm.run_reuse(&code, &compiled_fns) {
                        Ok(()) => {
                            let val = vm
                                .last_stack_value()
                                .cloned()
                                .or_else(|| {
                                    tail_is_when.then(|| {
                                        vm.when_nonmatch_value.take().unwrap_or(Value::FALSE)
                                    })
                                })
                                .or_else(|| vm.env().get("_").cloned())
                                .unwrap_or(Value::NIL);
                            writeback(list_items, vm);
                            let val = vm.reify_finite_pipe_value(val)?;
                            if let ValueView::Slip(elems) = val.view() {
                                result.extend(elems.iter().cloned());
                            } else {
                                result.push(val);
                            }
                        }
                        Err(e) if e.is_next() => {
                            writeback(list_items, vm);
                        }
                        Err(e) if e.is_last() => {
                            writeback(list_items, vm);
                            break;
                        }
                        // A matched `when`/`default` escapes as a succeed
                        // signal instead of returning normally — absorb it the
                        // same way the `Ok` arm does.
                        Err(e) if e.is_succeed() => {
                            vm.set_when_matched(saved_when_matched);
                            let val = e.return_value.unwrap_or(Value::NIL);
                            writeback(list_items, vm);
                            let val = vm.reify_finite_pipe_value(val)?;
                            if let ValueView::Slip(elems) = val.view() {
                                result.extend(elems.iter().cloned());
                            } else {
                                result.push(val);
                            }
                        }
                        Err(e) => {
                            return Err(e);
                        }
                    }
                    drop(_readonly_guard);
                    i += arity;
                }

                Ok(Value::array(result))
            });

            for (k, orig) in saved {
                match orig {
                    Some(v) => self.env.insert(k, v),
                    None => self.env.remove(&k),
                };
            }
            self.env.remove(topic_key);
            return loop_result.map(|v| (v, wrote_back.get()));
        }
        // Non-Sub func: delegate to regular map (which never writes back)
        self.eval_map_over_items(func, list_items.to_vec())
            .map(|v| (v, false))
    }

    pub(super) fn eval_grep_over_items_with_mutated(
        &mut self,
        func: Option<Value>,
        mut list_items: Vec<Value>,
    ) -> Result<(Value, Vec<Value>), RuntimeError> {
        // This construct handles `next`/`last`/`redo`, so a loop-control
        // statement raised anywhere in its dynamic extent has somewhere to go
        // (`runtime/loop_handler_depth.rs`). Without the guard the raise site
        // would convert the signal into a thrown `X::ControlFlow` and silently
        // break this loop.
        let _loop_handler = crate::runtime::loop_handler_depth::LoopHandlerGuard::new();
        // Look through a role-mixed callable (`&foo but R1`) so the
        // compile-once fast path below (which requires a bare `Sub`) still
        // takes it, instead of silently falling through to smartmatch-style
        // filtering (which never truthily matches a Mixin, dropping every
        // element) -- see `todo/tickets/map-rejects-role-mixed-sub-as-callable.md`.
        let func = func.map(Self::unwrap_callable_mixin);
        if let Some(func_ref) = func.as_ref()
            && let ValueView::Sub(data) = func_ref.view()
        {
            let data = data.clone();
            let mut result = Vec::new();
            // A destructuring sub-signature (`grep -> [ \a, \u, \v ] { u %% v }`) has to go
            // through the real binder. The fast path below inserts each parameter into the
            // env *by name*, which cannot take an element apart, so the inner names stayed
            // unbound. `map` already routes such signatures to `call_sub_value`.
            let needs_full_binding = data
                .param_defs
                .iter()
                .any(|pd| pd.sub_signature.is_some() || pd.outer_sub_signature.is_some())
                // A body-less routine Sub (plan-derived, ADR-0019 C6e-3)
                // carries only bytecode — the compile-the-AST fast path below
                // would evaluate an empty predicate; run the real call path.
                || (data.body.is_empty() && data.compiled_routine.is_some());
            if needs_full_binding {
                for item in &list_items {
                    let pred = self.call_sub_value(
                        Value::sub_value(data.clone()),
                        vec![item.clone()],
                        false,
                    )?;
                    if pred.truthy() {
                        result.push(item.clone());
                    }
                }
                return Ok((Value::array(result), list_items));
            }
            let arity = if !data.params.is_empty() {
                let effective = data
                    .params
                    .len()
                    .saturating_sub(data.assumed_positional.len());
                if effective == 0 { 1 } else { effective }
            } else {
                1
            };
            // Carrier Subs (.assuming wrapper, composed callable, multi-candidate
            // dispatcher) — delegate to call_sub_value which resolves the markers.
            if super::resolution_map_grep::sub_is_call_carrier(&data) {
                let mut i = 0usize;
                while i < list_items.len() {
                    if arity > 1 && i + arity > list_items.len() {
                        break;
                    }
                    let chunk: Vec<Value> = if arity == 1 {
                        vec![list_items[i].clone()]
                    } else {
                        list_items[i..i + arity].to_vec()
                    };
                    let pred =
                        self.call_sub_value(Value::sub_value(data.clone()), chunk.clone(), false)?;
                    if pred.truthy() {
                        if arity == 1 {
                            result.push(chunk[0].clone());
                        } else {
                            result.push(Value::array(chunk));
                        }
                    }
                    i += arity;
                }
                return Ok((Value::array(result), list_items));
            }

            // Compile once, reuse VM for every iteration (and reuse a cached
            // compile across repeated calls to this same closure literal —
            // see `compile_loop_block_cached`). `return` inside this block
            // should propagate up to the lexically enclosing routine (if
            // any); `compile_loop_block_cached` marks the compiler as
            // lexically nested in a routine whenever one is currently on the
            // dynamic call stack.
            let normalized_body =
                super::resolution_map_grep::normalize_tail_stmt_for_value(&data.body);
            let tail_is_when = super::resolution_map_grep::tail_is_when_chain(&normalized_body);
            let (code, compiled_fns) = self.compile_loop_block_cached(&data, &normalized_body);

            let underscore = "_".to_string();
            let dollar_topic = "$_".to_string();

            let mut touched_keys: Vec<String> = Vec::with_capacity(data.params.len() + 2);
            for k in data.env.keys() {
                if !self.env.contains_key_sym(*k) {
                    touched_keys.push(k.resolve());
                }
            }
            for p in &data.params {
                if !touched_keys.contains(p) {
                    touched_keys.push(p.clone());
                }
            }
            if !touched_keys.iter().any(|k| k == "_") {
                touched_keys.push(underscore.clone());
            }
            if !touched_keys.iter().any(|k| k == "$_") {
                touched_keys.push(dollar_topic.clone());
            }
            let topic_source_key = "__mutsu_grep_topic_source".to_string();
            if !touched_keys.iter().any(|k| k == &topic_source_key) {
                touched_keys.push(topic_source_key.clone());
            }
            super::resolution_map_grep::push_block_declared_keys(&mut touched_keys, &code);
            // The pre-insert below overwrites every captured name, `self`
            // included (it is lexical — see `call_compiled_closure`); it is
            // normally already in the running env, so list it for restoration.
            if data.env.get("self").is_some() && !touched_keys.iter().any(|k| k == "self") {
                touched_keys.push("self".to_string());
            }
            let saved: Vec<(String, Option<Value>)> = touched_keys
                .iter()
                .map(|k| (k.clone(), self.env.get(k).cloned()))
                .collect();

            // Pre-insert closure env
            for (k, v) in &data.env {
                self.env.insert_sym(*k, v.clone());
            }

            let keeps_outer_topic = super::resolution_map_grep::block_keeps_outer_topic(&data);
            let outer_topic = self.env.get("_").cloned();

            // CP-3 collapse: run the grep loop with fresh execution registers
            // (replaces the `mem::take(self)` + `VM::new` sub-VM). The closure
            // returns Ok(()) / Err on the loop; `with_nested_registers` restores
            // the outer registers and flags env_dirty. The `saved` env restore is
            // hoisted to after the call (ran on every old exit path).
            // Runtime transitive vouching: see `frame_authoritative_set`.
            let block_authoritative = data
                .compiled_code
                .as_ref()
                .map(|cc| {
                    super::resolution_map_grep::frame_authoritative_set(
                        cc,
                        &data.authoritative_captures,
                    )
                })
                .unwrap_or_default();
            // ADR-0027: see the matching comment in `eval_map_over_items`
            // (`resolution_map_grep.rs`).
            let block_owned = data.owned_captures.clone();
            let loop_result: Result<(), RuntimeError> = self.with_nested_registers(|vm| {
                // Scope `state` variables to the closure instance (see
                // `eval_map_over_items`).
                vm.state_scope_id.set(Some(data.id));
                let mut i = 0usize;
                let mut stop = false;
                while i < list_items.len() {
                    if arity > 1 && i + arity > list_items.len() {
                        break;
                    }
                    let chunk: Vec<Value> = if arity == 1 {
                        vec![list_items[i].clone()]
                    } else {
                        list_items[i..i + arity].to_vec()
                    };
                    'body_redo: loop {
                        vm.frame_authoritative = block_authoritative.clone();
                        vm.frame_owned = block_owned.clone();
                        {
                            let assumed_count = data.assumed_positional.len();
                            for (idx, val) in data.assumed_positional.iter().enumerate() {
                                if let Some(p) = data.params.get(idx) {
                                    vm.env_mut().insert(p.clone(), val.clone());
                                }
                            }
                            if arity == 1 {
                                if let Some(p) = data.params.get(assumed_count) {
                                    vm.env_mut().insert(p.clone(), chunk[0].clone());
                                }
                                bind_loop_topic(
                                    vm.env_mut(),
                                    &chunk[0],
                                    keeps_outer_topic,
                                    &outer_topic,
                                );
                                if !keeps_outer_topic {
                                    vm.env_mut()
                                        .insert(topic_source_key.clone(), chunk[0].clone());
                                }
                            } else {
                                for (idx, p) in data.params.iter().skip(assumed_count).enumerate() {
                                    if idx < chunk.len() {
                                        vm.env_mut().insert(p.clone(), chunk[idx].clone());
                                    }
                                }
                                bind_loop_topic(
                                    vm.env_mut(),
                                    &chunk[0],
                                    keeps_outer_topic,
                                    &outer_topic,
                                );
                            }
                        }
                        // `$_` holding the caller's topic is not an alias for the
                        // element, so it must not write back to it.
                        vm.set_topic_source_var(
                            (arity == 1 && !keeps_outer_topic).then_some(topic_source_key.clone()),
                        );
                        let saved_when_matched = vm.when_matched();
                        vm.when_nonmatch_value = None;
                        match vm.run_reuse(&code, &compiled_fns) {
                            Ok(()) => {
                                let pred = vm
                                    .last_stack_value()
                                    .cloned()
                                    .or_else(|| {
                                        tail_is_when.then(|| {
                                            vm.when_nonmatch_value.take().unwrap_or(Value::FALSE)
                                        })
                                    })
                                    .or_else(|| vm.env().get("_").cloned())
                                    .unwrap_or(Value::NIL);
                                let updated_item = if arity == 1 {
                                    vm.env()
                                        .get(&topic_source_key)
                                        .cloned()
                                        .unwrap_or_else(|| chunk[0].clone())
                                } else {
                                    chunk[0].clone()
                                };
                                if arity == 1 {
                                    list_items[i] = updated_item.clone();
                                }
                                if pred.truthy() {
                                    if arity == 1 {
                                        result.push(updated_item);
                                    } else {
                                        result.push(Value::array(chunk));
                                    }
                                }
                                break 'body_redo;
                            }
                            Err(e) if e.is_redo() => continue 'body_redo,
                            Err(e) if e.is_next() => break 'body_redo,
                            Err(e) if e.is_last() => {
                                stop = true;
                                break 'body_redo;
                            }
                            // A matched `when`/`default` escapes as a succeed
                            // signal instead of returning normally — its value
                            // is the predicate result, same as the `Ok` arm.
                            Err(e) if e.is_succeed() => {
                                vm.set_when_matched(saved_when_matched);
                                let pred = e.return_value.unwrap_or(Value::NIL);
                                let updated_item = if arity == 1 {
                                    vm.env()
                                        .get(&topic_source_key)
                                        .cloned()
                                        .unwrap_or_else(|| chunk[0].clone())
                                } else {
                                    chunk[0].clone()
                                };
                                if arity == 1 {
                                    list_items[i] = updated_item.clone();
                                }
                                if pred.truthy() {
                                    if arity == 1 {
                                        result.push(updated_item);
                                    } else {
                                        result.push(Value::array(chunk));
                                    }
                                }
                                break 'body_redo;
                            }
                            Err(e) => {
                                return Err(e);
                            }
                        }
                    }
                    if stop {
                        break;
                    }
                    i += arity;
                }
                Ok(())
            });

            for (k, orig) in saved {
                match orig {
                    Some(v) => {
                        self.env.insert(k, v);
                    }
                    None => {
                        self.env.remove(&k);
                    }
                }
            }
            if loop_result.is_ok() {
                self.record_eager_block_free_var_writeback(&code, &data.params);
            }
            loop_result?;
            return Ok((Value::array(result), list_items));
        }
        if let Some(pattern) = func {
            if matches!(pattern.view(), ValueView::Bool(_)) {
                return Err(RuntimeError::new("X::Match::Bool"));
            }
            let mut result = Vec::new();
            for item in &list_items {
                if self.smart_match(item, &pattern) {
                    result.push(item.clone());
                }
            }
            return Ok((Value::array(result), list_items));
        }
        if let Some(func) = func {
            let mut result = Vec::new();
            for item in &list_items {
                let pred = self.call_sub_value(func.clone(), vec![item.clone()], false)?;
                if pred.truthy() {
                    result.push(item.clone());
                }
            }
            return Ok((Value::array(result), list_items));
        }
        Ok((Value::array(list_items.clone()), list_items))
    }
}
