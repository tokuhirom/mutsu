use super::*;
use crate::runtime::map_grep_plan::{InlineLoopKind, MapGrepPlanSlot};
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
    ///
    /// `slot` keeps the callback's loop plan across calls: a deferred `.map`
    /// pulled a chunk at a time reuses it on every pull
    /// (`runtime/map_grep_plan.rs`).
    // Cost: one callback call per element (per `arity` elements for a
    // multi-parameter block).
    pub(crate) fn eval_map_over_items_rw(
        &mut self,
        func: Option<Value>,
        list_items: &mut [Value],
        slot: &mut MapGrepPlanSlot,
    ) -> Result<(Value, bool), RuntimeError> {
        // This construct handles `next`/`last`/`redo`, so a loop-control
        // statement raised anywhere in its dynamic extent has somewhere to go
        // (`runtime/loop_handler_depth.rs`). Without the guard the raise site
        // would convert the signal into a thrown `X::ControlFlow` and silently
        // break this loop.
        let _loop_handler = crate::runtime::loop_handler_depth::LoopHandlerGuard::new();
        let topic_key = crate::symbol::wk::rw_map_topic();
        let wrote_back = std::cell::Cell::new(false);
        if let Some(func_ref) = func.as_ref()
            && let ValueView::Sub(data) = func_ref.view()
        {
            // Nothing to iterate: the block is never invoked, so no element can
            // be produced and nothing can be written back. Both branches below
            // reach exactly this value for an empty input — returning here just
            // skips the setup they would do first (block compile lookup, the
            // env save/restore of every key the block captured, the
            // nested-register frame). `@!resources.map(*.flat)` over an empty
            // attribute array is the shape a `TWEAK` body uses, and paying that
            // setup per construction was the single largest component of
            // bench-ctor. It also stops an inner empty map from removing the
            // enclosing map's `topic_key` mid-iteration.
            if list_items.is_empty() {
                return Ok((Value::array(Vec::new()), false));
            }
            let data = data.clone();
            // A plan in `slot` means an earlier chunk of this same Seq already
            // classified the callback as inline-loop material: the checks
            // below are a pure function of the callback.
            let cached_plan = self.cached_inline_loop_plan(&data, InlineLoopKind::MapRw, slot);
            let needs_call_path = cached_plan.is_none() && {
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
                    && !super::resolution_map_grep::sub_is_whatever_code(&data)
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
                requires_full_binding
                    || is_routine_callback
                    || super::resolution_map_grep::sub_is_call_carrier(&data)
                    || super::resolution_map_grep::sub_reads_block_var(&data)
            };
            if needs_call_path {
                // Fall through to call_sub_value path for complex cases
                let keeps_outer_topic = super::resolution_map_grep::block_keeps_outer_topic(&data);
                let mut result = Vec::new();
                let arity = crate::runtime::map_grep_plan::inline_loop_arity(&data);
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
                    let value = if rw_param.is_some() {
                        let cell = crate::gc::Gc::new(crate::value::ContainerCell::new(
                            list_items[i].clone(),
                        ));
                        let res = self.call_sub_value(
                            Value::sub_value(data.clone()),
                            vec![Value::container_ref(cell.clone())],
                            false,
                        )?;
                        list_items[i] = cell.lock().unwrap().clone();
                        wrote_back.set(true);
                        res.deref_container()
                    } else {
                        // A short final chunk (fewer than `arity` elements
                        // remain) is not an error here — an optional trailing
                        // parameter (`-> $a, $b? {...}`) binds its missing
                        // slot to the default/`Any` via the normal call
                        // machinery below, the same way the List sibling's
                        // batch loop (`eval_map_over_items`) already handles
                        // it. A block whose trailing params are all mandatory
                        // still raises "Too few positionals" from that same
                        // call, matching raku.
                        let chunk: Vec<Value> = if arity == 1 {
                            vec![list_items[i].clone()]
                        } else {
                            list_items[i..(i + arity).min(list_items.len())].to_vec()
                        };
                        self.env.remove_sym(topic_key);
                        // A block with its own parameters binds the element to
                        // them, never to `$_`, so no topic write-back is read
                        // from it: run its compiled body as any closure call
                        // does instead of the carrier, which re-evaluates the
                        // body from its AST under a rebuilt env per element.
                        let v = if keeps_outer_topic {
                            self.vm_call_on_value(Value::sub_value(data.clone()), chunk, None)?
                        } else {
                            self.call_sub_value(Value::sub_value(data.clone()), chunk, false)?
                        };
                        if arity == 1
                            && !keeps_outer_topic
                            && let Some(mutated) = self.env.get_sym(topic_key).cloned()
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
                self.env.remove_sym(topic_key);
                return Ok((Value::array(result), wrote_back.get()));
            }

            let arity = crate::runtime::map_grep_plan::inline_loop_arity(&data);
            // See the rw_param comment on the call_sub_value branch above --
            // same shape check, for the untyped/unconstrained param that took
            // this env-insert fast path instead.
            let rw_param = (arity == 1)
                .then(|| data.param_defs.get(data.assumed_positional.len()))
                .flatten()
                .filter(|pd| pd.traits.iter().any(|t| t == "rw" || t == "raw"));
            let mut result = Vec::new();

            // Compile once, reuse VM for every iteration (and reuse a cached
            // compile across repeated calls to this same closure literal —
            // see `compile_loop_block_cached`). Without the cache every single
            // `@a.map(...)` call re-ran the whole compiler on the block's AST:
            // `@!resources.map(*.flat)` inside a TWEAK cost ~19us per
            // construction on an EMPTY array, which was the largest single
            // component of bench-ctor. The plan also holds the capture merge's
            // classification, so a deferred map pulled one element at a time
            // (a `for` loop, #9936) pays none of this per element
            // (`runtime/map_grep_plan.rs`).
            //
            // ADR-0058 §9.4: same capture-priority rule as the plain loop, and
            // for the same reason — this loop is no longer run inside the
            // frame that created the block. Once `@a.map` deferred, the pull
            // happens wherever the Seq is consumed, so a same-named lexical
            // live in THAT frame silently shadowed the block's own free
            // variable (`sub p($f) { my $c = C.new($f); @data.map: { $c.use }
            // }` read the unit's `$c` — caught by the bundled-library gate on
            // `Text::CSV`'s `66_formula.t`).
            let plan = match cached_plan {
                Some(plan) => plan,
                None => self.inline_loop_plan(&data, InlineLoopKind::MapRw, slot),
            };
            let (code, compiled_fns) = (&plan.code, &plan.fns);
            let saved = self.enter_inline_loop_env(&data, &plan);

            // A `$_`-referencing WhateverCode (`@a.map(* eq $_)`) binds the
            // element to its `*` placeholder, so `$_` must keep referring to the
            // CALLER's topic — only a bare block topicalizes `$_` to the element.
            // The List sibling (`eval_map_over_items`) and the grep loop below
            // both route their topic bind through `bind_loop_topic` for this;
            // this loop used to insert the element unconditionally, so
            // `@a.map(* eq $_)` compared each element against itself
            // (t/whatever-code-topic.t). When the topic is the caller's it is
            // NOT an alias for the element either, so it must not write back.
            let keeps_outer_topic = plan.keeps_outer_topic;
            let outer_topic = self.env.get_sym(crate::symbol::wk::topic()).cloned();
            // See `CompiledCode::immutable_topic` / `set_loop_topic_readonly`.
            let immutable_topic = plan.immutable_topic;

            // CP-3 collapse: run the rw map loop with fresh execution registers
            // (replaces the `mem::take(self)` + `VM::new` sub-VM). The closure
            // returns the loop's Result; `with_nested_registers` restores the
            // outer registers and flags env_dirty. The `saved`/`topic_key` env
            // restore is hoisted to after the call (ran on every old exit).
            // Runtime transitive vouching: see `frame_authoritative_set`.
            let block_authoritative = &plan.block_authoritative;
            // ADR-0027: see the matching comment in `eval_map_over_items`
            // (`resolution_map_grep.rs`).
            let block_owned = &data.owned_captures;
            let loop_result: Result<Value, RuntimeError> = self.with_nested_registers(|vm| {
                // Scope `state` variables to the closure instance — the body was
                // re-compiled fresh, so two distinct blocks share compile-time
                // state keys (see the same line in `eval_map_over_items`).
                vm.lexicals.state_scope_id.set(Some(data.id));
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
                    let mut rw_cell: Option<crate::gc::Gc<crate::value::ContainerCell>> = None;
                    {
                        let assumed_count = data.assumed_positional.len();
                        for (idx, val) in data.assumed_positional.iter().enumerate() {
                            if let Some(&p) = plan.param_syms.get(idx) {
                                vm.env_mut().insert_sym_noting(p, val.clone());
                            }
                        }
                        // Clear the topic tracker before each iteration
                        vm.env_mut().remove_sym(topic_key);
                        if arity == 1 {
                            let item = list_items[i].clone();
                            if let Some(&p) = plan.param_syms.get(assumed_count) {
                                if rw_param.is_some() {
                                    let cell = crate::gc::Gc::new(
                                        crate::value::ContainerCell::new(item.clone()),
                                    );
                                    vm.env_mut()
                                        .insert_sym_noting(p, Value::container_ref(cell.clone()));
                                    rw_cell = Some(cell);
                                } else {
                                    vm.env_mut().insert_sym_noting(p, item.clone());
                                }
                            }
                            bind_loop_topic(vm.env_mut(), &item, keeps_outer_topic, &outer_topic);
                        } else {
                            for (idx, &p) in plan.param_syms.iter().skip(assumed_count).enumerate()
                            {
                                if i + idx < list_items.len() {
                                    vm.env_mut()
                                        .insert_sym_noting(p, list_items[i + idx].clone());
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
                            && let Some(mutated) = vm.env().get_sym(topic_key).cloned()
                        {
                            // Only a block that topicalizes `$_` to the element
                            // rw-aliases it; when `$_` is the caller's topic a
                            // write to it must not reach the source array.
                            list_items[i] = mutated;
                            wrote_back.set(true);
                        }
                    };
                    let saved_when_matched = vm.when_matched();
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
                    vm.mark_placeholder_params_readonly(&data.params);
                    super::resolution_map_grep::set_loop_topic_readonly(vm, immutable_topic);
                    match vm.run_reuse(code, compiled_fns) {
                        Ok(()) => {
                            let val = vm
                                .last_stack_value()
                                .cloned()
                                .or_else(|| vm.env().get_sym(crate::symbol::wk::topic()).cloned())
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
                            vm.async_state.map_grep_last_depth =
                                Some(crate::runtime::loop_handler_depth::loop_handler_depth());
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

            self.leave_inline_loop_env(saved);
            self.env.remove_sym(topic_key);
            return loop_result.map(|v| (v, wrote_back.get()));
        }
        // Non-Sub func: delegate to regular map (which never writes back)
        self.eval_map_over_items(func, list_items.to_vec())
            .map(|v| (v, false))
    }
}
