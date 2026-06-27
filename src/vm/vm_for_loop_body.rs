use super::vm_control_ops::ForLoopSpec;
use super::*;

impl Interpreter {
    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_for_loop_body(
        &mut self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        items: &[Value],
        body_start: usize,
        loop_end: usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
        resume_index: usize,
    ) -> Result<(), RuntimeError> {
        let param_name = spec
            .param_idx
            .map(|idx| match &code.constants[idx as usize] {
                Value::Str(s) => s.to_string(),
                _ => unreachable!("ForLoop param must be a string constant"),
            });

        let arity = spec.arity.max(1) as usize;
        let writes_back_topic =
            spec.param_idx.is_none() && spec.param_local.is_none() && spec.arity <= 1;
        let mut rw_writeback = spec.do_writeback;
        // A plain (non-rw, non-copy) named loop variable aliases the source
        // element in Raku: `for @m -> @row { @row.push(9) }` and
        // `for @m -> $row { $row.push(9) }` both mutate `@m` (a scalar binding a
        // container can still mutate the container, though not rebind it).
        // Mirror the topic writeback for it. `is copy` sets `is_rw` (suppressing
        // this), and `<->` rw uses `rw_writeback` instead. The unchanged-value
        // guard in `write_back_for_topic_item` keeps read-only loops O(n).
        let writes_back_named_param =
            !spec.is_rw && !rw_writeback && spec.arity <= 1 && param_name.is_some();
        // `.pairs`/`.antipairs` loop variables are `Pair`s wrapping the element,
        // not the element itself — writing one back would overwrite the source
        // element with the Pair (S32-array/pairs.t 14). The Pair's rw `.value`
        // alias handles propagation (and immutability for Mix), so suppress the
        // plain writeback here while keeping the source tag.
        let writes_back_loop_var =
            (writes_back_topic || writes_back_named_param) && !spec.loop_var_wraps_element;
        let chunked_items: Vec<Value> = if arity > 1 {
            items
                .chunks(arity)
                .map(|chunk| Value::array(chunk.to_vec()))
                .collect()
        } else {
            items.to_vec()
        };
        let stack_base = if spec.collect {
            Some(self.stack.len())
        } else {
            None
        };
        let mut collected = if spec.collect { Some(Vec::new()) } else { None };
        let mut deferred_container_refs: Vec<(usize, String)> = Vec::new();
        let saved_topic = spec
            .restore_topic
            .then(|| self.env().get("_").cloned())
            .flatten();
        let saved_topic_source = self.topic_source_var.take();
        let saved_quanthash_bind = std::mem::take(&mut self.quanthash_bind_params);
        let container_binding = self.container_ref_var.take();
        // A sigilless/`is rw` for-param aliases the source element, but an
        // *immutable* Mix/Set/Bag yields immutable weights — assigning to the
        // alias must throw X::Assignment::RO, and no writeback may run (it would
        // corrupt the immutable collection). Mutable MixHash/BagHash/SetHash and
        // arrays/hashes are unaffected. Detect the immutable-QuantHash source at
        // runtime (the compiler cannot know mutability) and force the params
        // read-only with writeback suppressed.
        let source_immutable_quant = container_binding.as_ref().is_some_and(|name| {
            matches!(
                self.get_env_with_main_alias(name),
                Some(Value::Mix(_, false))
                    | Some(Value::Set(_, false))
                    | Some(Value::Bag(_, false))
            )
        });
        if source_immutable_quant {
            rw_writeback = false;
        }
        // A *mutable* QuantHash (MixHash/BagHash/SetHash) source iterated via
        // `.values`/`.kv`/`.pairs` aliases its weights: `$_ = X for $b.values`,
        // `for $b.kv -> \k,\v { v = X }` and `.value = X for $b.pairs` all mutate
        // the QuantHash. Detect it so the topic stays writable (not readonly) and
        // the writeback paths can update the weight by key order, coercing the
        // assigned value (X::Str::Numeric on a bad string; weight 0 removes the key).
        let source_mutable_quant = container_binding.as_ref().is_some_and(|name| {
            matches!(
                self.get_env_with_main_alias(name),
                Some(Value::Mix(_, true)) | Some(Value::Set(_, true)) | Some(Value::Bag(_, true))
            )
        });
        let container_reversed = self.container_ref_reversed;
        self.container_ref_reversed = false;
        // Capture hash key order before the loop so writeback uses the
        // original key order even after the hash is mutated during iteration.
        // Needed for the rw-param `%h.values -> $v is rw` writeback and for the
        // plain topic `$_ = X for %h.values` writeback (values_mode).
        let hash_keys_for_writeback: Option<Vec<String>> =
            if rw_writeback || (writes_back_loop_var && spec.values_mode) {
                container_binding.as_ref().and_then(|source| {
                    // Deref a `ContainerRef` cell (a `:=`-bound hash) so the key
                    // order is captured from the inner Hash (Stage 1).
                    match self
                        .get_env_with_main_alias(source)
                        .as_ref()
                        .map(|v| v.deref_container())
                    {
                        Some(Value::Hash(hash_items)) if source.starts_with('%') => {
                            Some(hash_items.keys().cloned().collect())
                        }
                        // A mutable QuantHash bound to a scalar: capture the weight
                        // map's key order so `.values`/`.kv` writeback lands on the
                        // same key `.values()`/`.kv` yielded (same unmodified map →
                        // identical iteration order).
                        Some(Value::Bag(b, true)) => Some(b.keys().cloned().collect()),
                        Some(Value::Mix(m, true)) => Some(m.keys().cloned().collect()),
                        Some(Value::Set(s, true)) => Some(s.elements.iter().cloned().collect()),
                        _ => None,
                    }
                })
            } else {
                None
            };
        // Save multi-param values and readonly state so they can be restored
        // after the loop (inner loops must not clobber outer scope bindings).
        let saved_multi_params: Vec<(String, Option<Value>, bool, Option<Value>)> = spec
            .multi_param_names
            .iter()
            .map(|name| {
                let val = self.env().get(name).cloned();
                let was_readonly = self.readonly_vars().contains(name);
                let sigilless_key = format!("__mutsu_sigilless_readonly::{}", name);
                let sigilless_ro = self.env().get(&sigilless_key).cloned();
                (name.clone(), val, was_readonly, sigilless_ro)
            })
            .collect();
        // Save the single named loop param (`for ... -> $x`) too, so a loop in a
        // called sub that reuses the same variable name does not clobber an outer
        // loop's binding of that name (the env keys these by bare name). Skip
        // `@`/`%` sigils, which bind a shared mutable container the body may
        // legitimately reassign, and skip the rw case (handled via writeback).
        let saved_param: Option<(String, Option<Value>)> = param_name
            .as_ref()
            .filter(|n| !n.starts_with('@') && !n.starts_with('%'))
            .map(|name| (name.clone(), self.env().get(name).cloned()));
        // Track loop-body declarations for per-iteration closure capture
        // (owned_captures). Balanced by pop on every exit.
        self.push_loop_local_scope();
        // Determine if the implicit topic ($_) should be read-only.
        // Only mark $_ readonly when iterating over an *immutable* collection
        // (Mix/Set/Bag, the `(_, false)` variants). This blocks `.value = ...`
        // and `$_ = ...` mutations on values/pairs from immutable collections
        // while keeping $_ writable for a *mutable* QuantHash (MixHash/BagHash —
        // `for $b.values { $_ = X }` / `.value = X for $b.pairs` must write back),
        // expression results, multi-param loops, and Scalar containers.
        let topic_readonly =
            !spec.is_rw && param_name.is_none() && spec.multi_param_names.is_empty() && {
                match &container_binding {
                    None => false,
                    Some(name) => {
                        if let Some(val) = self.get_env_with_main_alias(name) {
                            matches!(
                                val,
                                Value::Mix(_, false) | Value::Set(_, false) | Value::Bag(_, false)
                            )
                        } else {
                            false
                        }
                    }
                }
            };
        let total_items = chunked_items.len();
        'for_loop: for (idx, item) in chunked_items.into_iter().enumerate().skip(resume_index) {
            // `topic_source_var` drives the whole-topic writeback for a scalar
            // source (`for $x { $_[1] = ... }` writes the mutated `$_` back to
            // `$x`). For a `.values` loop over a mutable QuantHash the topic is a
            // *weight*, not the container — wholesale-overwriting `$b` with the
            // weight would clobber the MixHash/BagHash. The per-element
            // `write_back_quanthash_value_item` handles that source, so suppress
            // the whole-topic writeback here.
            self.topic_source_var =
                if writes_back_topic && !(spec.values_mode && source_mutable_quant) {
                    container_binding.clone()
                } else {
                    None
                };
            // Only set $_ when no named parameter is given (for @list { ... })
            // When -> $k is used, $_ should remain from the enclosing scope
            if param_name.is_none() {
                self.env_mut().insert("_".to_string(), item.clone());
            }
            if let Some(ref name) = param_name {
                self.env_mut().insert(name.clone(), item.clone());
                // Create non-twigil alias for placeholder params: $^a → $a
                if let Some(bare) = name.strip_prefix("&^") {
                    self.env_mut().insert(format!("&{}", bare), item.clone());
                } else if let Some(bare) = name.strip_prefix('^') {
                    self.env_mut().insert(bare.to_string(), item.clone());
                }
            }
            if let Some(slot) = spec.param_local {
                self.locals[slot as usize] = item.clone();
            }
            // Mark implicit $_ readonly when source is immutable.
            // Also set a deep-readonly flag so that method-lvalue
            // assignments like .value = ... are blocked too.
            if topic_readonly {
                self.mark_readonly("_");
                self.env_mut()
                    .insert("__mutsu_deep_readonly::_".to_string(), Value::Bool(true));
            }
            // Mark named params readonly when not in rw mode.
            // Skip @-sigil and %-sigil params: they bind to a mutable
            // Array/Hash container, so assignments like `@a = values` must
            // be allowed (matching Raku semantics).
            if !spec.is_rw
                && let Some(ref name) = param_name
                && !name.starts_with('@')
                && !name.starts_with('%')
            {
                self.mark_readonly(name);
            }
            // `%`-sigil for-loop bindings preserve a QuantHash value (and keep
            // its type across a `%a = ...pairs` reset) instead of coercing it to
            // a plain Hash — Raku binds params, it does not assign-coerce them.
            self.quanthash_bind_params = spec
                .multi_param_names
                .iter()
                .chain(param_name.iter())
                .filter(|n| n.starts_with('%'))
                .cloned()
                .collect();
            // Temporarily clear readonly flags for multi-param names
            // so the bind stmts (Stmt::Assign) at the start of the body can
            // re-bind variables that may be readonly from an outer scope.
            for mp_name in &spec.multi_param_names {
                // Clear regular readonly flag
                self.unmark_readonly(mp_name);
                // Clear sigilless readonly flag
                let key = format!("__mutsu_sigilless_readonly::{}", mp_name);
                self.env_mut().insert(key, Value::Bool(false));
            }
            'body_redo: loop {
                let mut body_result = self.run_range(code, body_start, loop_end, compiled_fns);
                // An immutable Mix/Set/Bag source yields immutable weights: if the
                // body modified a sigilless/rw alias, Raku throws X::Assignment::RO.
                // Detect it here (writeback is already suppressed above) and convert
                // a successful body into the same error, so the shared Err arm runs
                // its readonly/topic cleanup before propagating.
                if body_result.is_ok()
                    && source_immutable_quant
                    && let Some(err) = self.immutable_quant_param_mutation(
                        &param_name,
                        &spec.multi_param_names,
                        &item,
                    )
                {
                    body_result = Err(err);
                }
                // `$_ = X for $b.values` / `for $b.values -> $v { $v = X }` where
                // `$b` is a mutable QuantHash: write the aliased weight back here,
                // before the match, so a coercion failure (X::Str::Numeric on a
                // non-numeric string) flows through the shared Err-arm cleanup. The
                // Ok arm's `write_back_for_topic_item` skips scalar quant sources.
                if body_result.is_ok()
                    && writes_back_loop_var
                    && spec.values_mode
                    && source_mutable_quant
                    && let Some(ref source) = container_binding
                    && let Err(err) = self.write_back_quanthash_value_item(
                        code,
                        source,
                        &param_name,
                        idx,
                        &hash_keys_for_writeback,
                    )
                {
                    body_result = Err(err);
                }
                // The rw-param sibling of the above: `for $b.kv -> \k, \v { v = X }`
                // and `for $b.values -> $v is rw { $v = X }` over a mutable QuantHash.
                // Handled pre-match (coercion may raise X::Str::Numeric) so the
                // Ok-arm `write_back_for_rw_param` (which no-ops on scalar quant
                // sources) need not change.
                if body_result.is_ok()
                    && rw_writeback
                    && source_mutable_quant
                    && (spec.kv_mode || spec.values_mode)
                    && let Some(ref source) = container_binding
                    && let Err(err) = self.write_back_quanthash_rw(
                        code,
                        source,
                        &spec.rw_param_names,
                        &param_name,
                        idx,
                        spec.kv_mode,
                        &hash_keys_for_writeback,
                    )
                {
                    body_result = Err(err);
                }
                match body_result {
                    Ok(()) => {
                        // Sync state variables modified in this iteration so
                        // that StateVarInit in the next iteration sees the
                        // updated values.
                        if !code.state_locals.is_empty() {
                            self.sync_state_locals_in_range(code, body_start, loop_end);
                        }
                        if writes_back_loop_var {
                            self.write_back_for_topic_item(
                                code,
                                &container_binding,
                                &param_name,
                                idx,
                                container_reversed,
                                total_items,
                                spec.values_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        if rw_writeback {
                            self.write_back_for_rw_param(
                                code,
                                &container_binding,
                                &param_name,
                                &spec.rw_param_names,
                                idx,
                                arity,
                                spec.kv_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        self.write_back_to_source_var(
                            code,
                            &spec.source_var_names,
                            &param_name,
                            idx,
                        );
                        if let Some(ref mut coll) = collected {
                            let base = stack_base.unwrap();
                            if self.stack.len() > base {
                                let val = self.stack.pop().unwrap();
                                let deferred_ref = self.container_ref_var.take();
                                let coll_start_len = coll.len();
                                Self::collect_loop_value(coll, val);
                                if let Some(name) = deferred_ref
                                    && coll.len() == coll_start_len + 1
                                {
                                    deferred_container_refs.push((coll_start_len, name));
                                }
                            }
                            // Drain any extra values pushed during this iteration
                            self.stack.truncate(base);
                        }
                        break 'body_redo;
                    }
                    Err(e) if e.is_succeed() => {
                        if writes_back_loop_var {
                            self.write_back_for_topic_item(
                                code,
                                &container_binding,
                                &param_name,
                                idx,
                                container_reversed,
                                total_items,
                                spec.values_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        if rw_writeback {
                            self.write_back_for_rw_param(
                                code,
                                &container_binding,
                                &param_name,
                                &spec.rw_param_names,
                                idx,
                                arity,
                                spec.kv_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        self.write_back_to_source_var(
                            code,
                            &spec.source_var_names,
                            &param_name,
                            idx,
                        );
                        break 'body_redo;
                    }
                    Err(e) if e.is_redo() && Self::label_matches(&e.label, &spec.label) => {
                        if param_name.is_none() {
                            self.env_mut().insert("_".to_string(), item.clone());
                        }
                        if let Some(ref name) = param_name {
                            self.env_mut().insert(name.clone(), item.clone());
                        }
                        if let Some(slot) = spec.param_local {
                            self.locals[slot as usize] = item.clone();
                        }
                        continue 'body_redo;
                    }
                    Err(e)
                        if e.is_leave
                            && e.leave_callable_id.is_none()
                            && e.leave_routine.is_none()
                            && Self::label_matches(&e.label, &spec.label) =>
                    {
                        if writes_back_loop_var {
                            self.write_back_for_topic_item(
                                code,
                                &container_binding,
                                &param_name,
                                idx,
                                container_reversed,
                                total_items,
                                spec.values_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        if rw_writeback {
                            self.write_back_for_rw_param(
                                code,
                                &container_binding,
                                &param_name,
                                &spec.rw_param_names,
                                idx,
                                arity,
                                spec.kv_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        self.write_back_to_source_var(
                            code,
                            &spec.source_var_names,
                            &param_name,
                            idx,
                        );
                        if let Some(v) = e.return_value {
                            if let Some(ref mut coll) = collected {
                                Self::collect_loop_value(coll, v.clone());
                            } else {
                                self.env_mut().insert("_".to_string(), v.clone());
                                // Push return value on stack so enclosing compiled
                                // closures can see it as the block result.
                                self.stack.push(v);
                            }
                        }
                        break 'for_loop;
                    }
                    Err(e) if e.is_last() && Self::label_matches(&e.label, &spec.label) => {
                        if writes_back_loop_var {
                            self.write_back_for_topic_item(
                                code,
                                &container_binding,
                                &param_name,
                                idx,
                                container_reversed,
                                total_items,
                                spec.values_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        if rw_writeback {
                            self.write_back_for_rw_param(
                                code,
                                &container_binding,
                                &param_name,
                                &spec.rw_param_names,
                                idx,
                                arity,
                                spec.kv_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        self.write_back_to_source_var(
                            code,
                            &spec.source_var_names,
                            &param_name,
                            idx,
                        );
                        break 'for_loop;
                    }
                    Err(e) if e.is_next() && Self::label_matches(&e.label, &spec.label) => {
                        if writes_back_loop_var {
                            self.write_back_for_topic_item(
                                code,
                                &container_binding,
                                &param_name,
                                idx,
                                container_reversed,
                                total_items,
                                spec.values_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        if rw_writeback {
                            self.write_back_for_rw_param(
                                code,
                                &container_binding,
                                &param_name,
                                &spec.rw_param_names,
                                idx,
                                arity,
                                spec.kv_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        self.write_back_to_source_var(
                            code,
                            &spec.source_var_names,
                            &param_name,
                            idx,
                        );
                        break 'body_redo;
                    }
                    Err(e)
                        if e.message
                            == crate::runtime::Interpreter::LAZY_GATHER_TAKE_LIMIT_SIGNAL =>
                    {
                        // Save for-loop state for gather coroutine resumption.
                        self.gather_for_loop_resume =
                            Some(crate::value::ForLoopResumeState::List {
                                items: items.to_vec(),
                                next_index: idx + 1,
                            });
                        if topic_readonly {
                            self.unmark_readonly("_");
                            self.env_mut().remove("__mutsu_deep_readonly::_");
                        }
                        if !spec.is_rw
                            && let Some(ref name) = param_name
                        {
                            self.readonly_vars_mut().remove(name);
                        }
                        self.topic_source_var = saved_topic_source;
                        self.quanthash_bind_params = saved_quanthash_bind.clone();
                        if spec.restore_topic {
                            match saved_topic {
                                Some(v) => {
                                    self.env_mut().insert("_".to_string(), v);
                                }
                                None => {
                                    self.env_mut().remove("_");
                                }
                            }
                        }
                        self.pop_loop_local_scope(code);
                        return Err(e);
                    }
                    Err(e) => {
                        // Unmark readonly before propagating error
                        if topic_readonly {
                            self.unmark_readonly("_");
                            self.env_mut().remove("__mutsu_deep_readonly::_");
                        }
                        if !spec.is_rw
                            && let Some(ref name) = param_name
                        {
                            self.readonly_vars_mut().remove(name);
                        }
                        if spec.restore_topic {
                            match saved_topic.clone() {
                                Some(v) => {
                                    self.env_mut().insert("_".to_string(), v);
                                }
                                None => {
                                    self.env_mut().remove("_");
                                }
                            }
                        }
                        self.pop_loop_local_scope(code);
                        return Err(e);
                    }
                }
            }
            if self.is_halted() {
                break;
            }
        }
        // Unmark readonly topic after loop completion
        if topic_readonly {
            self.unmark_readonly("_");
            self.env_mut().remove("__mutsu_deep_readonly::_");
        }
        // Unmark readonly params after loop completion
        if !spec.is_rw
            && let Some(ref name) = param_name
        {
            self.readonly_vars_mut().remove(name);
        }
        // Restore saved multi-param values and readonly state
        for (name, saved_val, was_readonly, sigilless_ro) in saved_multi_params {
            if let Some(v) = saved_val {
                // Slice F (env<->locals coherence): a *sigilless* multi-param loop
                // variable (`-> \value`) shares its bare name — and therefore its
                // local slot — with an enclosing binding of the same name (an
                // outer `\value` re-bound by an inner `for (...) -> $string,
                // \value`). Restoring only the env binding leaves the local slot
                // clobbered with the last iteration value, so a later read of the
                // outer name (with the reverse env->locals pull disabled) sees
                // stale data. Write the restored value through to the local slot
                // too. Restricted to sigilless names: a sigil'd param (`$a`/`@a`/
                // `%a`) keeps its own slot and may hold a live rw/element alias the
                // env-only restore must not overwrite — those cases were already
                // coherent without the write-through.
                if !name.starts_with(['$', '@', '%', '&'])
                    && let Some(slot) = self.find_local_slot(code, &name)
                {
                    self.locals[slot] = v.clone();
                }
                self.env_mut().insert(name.clone(), v);
            } else {
                self.env_mut().remove(&name);
            }
            if was_readonly {
                self.mark_readonly(&name);
            } else {
                self.unmark_readonly(&name);
            }
            let sigilless_key = format!("__mutsu_sigilless_readonly::{}", name);
            if let Some(ro_val) = sigilless_ro {
                self.env_mut().insert(sigilless_key, ro_val);
            } else {
                self.env_mut().remove(&sigilless_key);
            }
        }
        // Defer restoring the single named loop param's prior binding until
        // after the loop's LAST/post phasers have run — they must still observe
        // the param at its final iteration value (e.g.
        // `for 1,2 -> $x { LAST { say $x } }` must see 2). The paired
        // `RestoreForParam` opcode (emitted right after the post phasers) pops
        // this and applies it. Only pushed here on normal completion, which
        // keeps it balanced with that opcode; an early return/exception from the
        // body exits before this point, so no entry is pushed and the matching
        // opcode is likewise skipped as the frame unwinds.
        if let Some(entry) = saved_param {
            self.for_param_restore_stack.push(entry);
        }
        self.pop_loop_local_scope(code);
        // Slice F (env<->locals coherence, docs/env-locals-coherence.md): a
        // `.value = X` / `.value--` in the body of `for $b.pairs` over a mutable
        // QuantHash writes the new weight back to the source `$b` *by name*
        // (`quanthash_set_weight`, reached via `topic_source_var` from the lvalue
        // builtin, called with an empty `CompiledCode` because that path lacks
        // the bytecode — see methods_mut.rs). With the real loop `code` in hand,
        // write the final env value of the source straight through to its local
        // slot so the post-loop read sees the mutation without the reverse pull
        // (skipping a live `HashEntryRef` binding slot, as the reverse pull does).
        if source_mutable_quant
            && let Some(ref source) = container_binding
            && let Some(slot) = self.find_local_slot(code, source)
            && !matches!(self.locals[slot], Value::HashEntryRef { .. })
            && let Some(val) = self.env().get(source).cloned()
        {
            self.locals[slot] = val;
        }
        self.topic_source_var = saved_topic_source;
        self.quanthash_bind_params = saved_quanthash_bind.clone();
        if spec.restore_topic {
            match saved_topic {
                Some(v) => {
                    self.env_mut().insert("_".to_string(), v);
                }
                None => {
                    self.env_mut().remove("_");
                }
            }
        }
        if let Some(coll) = collected {
            let mut coll = coll;
            for (idx, name) in deferred_container_refs {
                if idx < coll.len()
                    && let Some(v) = self.get_env_with_main_alias(&name)
                {
                    coll[idx] = v;
                }
            }
            self.stack.push(Value::array(coll));
        }
        Ok(())
    }
}
