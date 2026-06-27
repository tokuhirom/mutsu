use super::*;

impl Interpreter {
    pub(super) fn eval_map_over_items(
        &mut self,
        func: Option<Value>,
        list_items: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Some(Value::Sub(data)) = func {
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
            if requires_full_binding {
                let mut result = Vec::new();
                for item in list_items {
                    let value = self.call_sub_value(Value::Sub(data.clone()), vec![item], false)?;
                    match value {
                        Value::Slip(elems) => result.extend(elems.iter().cloned()),
                        v => result.push(v),
                    }
                }
                return Ok(Value::array(result));
            }
            let arity = if !data.params.is_empty() {
                // Account for assumed positional args (from .assuming)
                let effective = data
                    .params
                    .len()
                    .saturating_sub(data.assumed_positional.len());
                if effective == 0 { 1 } else { effective }
            } else {
                1
            };
            // Routine wrapper from .assuming() on a builtin — delegate to call_sub_value
            // which knows how to resolve __mutsu_routine_name.
            if data.env.contains_key("__mutsu_routine_name") {
                let mut result = Vec::new();
                let mut i = 0usize;
                while i < list_items.len() {
                    if arity > 1 && i + arity > list_items.len() {
                        return Err(RuntimeError::new("Not enough elements for map block arity"));
                    }
                    let chunk: Vec<Value> = if arity == 1 {
                        vec![list_items[i].clone()]
                    } else {
                        list_items[i..i + arity].to_vec()
                    };
                    let value = self.call_sub_value(Value::Sub(data.clone()), chunk, false)?;
                    match value {
                        Value::Slip(elems) => result.extend(elems.iter().cloned()),
                        v => result.push(v),
                    }
                    i += arity;
                }
                return Ok(Value::array(result));
            }
            if data.env.contains_key("__mutsu_compose_left")
                && data.env.contains_key("__mutsu_compose_right")
            {
                let mut result = Vec::new();
                let mut i = 0usize;
                while i < list_items.len() {
                    if arity > 1 && i + arity > list_items.len() {
                        return Err(RuntimeError::new("Not enough elements for map block arity"));
                    }
                    let chunk: Vec<Value> = if arity == 1 {
                        vec![list_items[i].clone()]
                    } else {
                        list_items[i..i + arity].to_vec()
                    };
                    let value = self.call_sub_value(Value::Sub(data.clone()), chunk, false)?;
                    match value {
                        Value::Slip(elems) => result.extend(elems.iter().cloned()),
                        v => result.push(v),
                    }
                    i += arity;
                }
                return Ok(Value::array(result));
            }
            let mut result = Vec::new();

            // Extract LAST phaser bodies so they can be run after the map loop
            let mut last_phaser_bodies: Vec<Vec<crate::ast::Stmt>> = Vec::new();
            for stmt in &data.body {
                if let crate::ast::Stmt::Phaser {
                    kind: crate::ast::PhaserKind::Last,
                    body,
                } = stmt
                {
                    last_phaser_bodies.push(body.clone());
                }
            }

            // Compile once, reuse VM for every iteration.
            // `return` inside this block should propagate up to the
            // lexically enclosing routine (if any), so mark the fresh
            // compiler as being lexically nested inside a routine whenever
            // a routine is currently on the dynamic call stack.
            let mut compiler = crate::compiler::Compiler::new();
            compiler.lexically_in_routine = !self.routine_stack.is_empty();
            let (code, compiled_fns) = compiler.compile(&data.body);

            let underscore = "_".to_string();
            let dollar_topic = "$_".to_string();

            // Save/restore only temporary bindings introduced by map itself.
            // Captured lexical vars (in data.env) must keep mutations done inside
            // the mapper block (e.g. `{ $a++ }`).
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
            let saved: Vec<(String, Option<Value>)> = touched_keys
                .iter()
                .map(|k| (k.clone(), self.env.get(k).cloned()))
                .collect();

            // Pre-insert closure env
            for (k, v) in &data.env {
                if !self.env.contains_key_sym(*k) {
                    self.env.insert_sym(*k, v.clone());
                }
            }

            // CP-3 collapse: run the map loop with fresh execution registers
            // (replaces the `mem::take(self)` + `VM::new` sub-VM, reusing one
            // register scope across all iterations). The closure returns the
            // loop's Result; `with_nested_registers` restores the outer registers
            // and flags env_dirty. The temporary-binding env restore (`saved`) is
            // hoisted to after the call — it ran on every old exit path.
            let loop_result: Result<Value, RuntimeError> = self.with_nested_registers(|vm| {
                let mut i = 0usize;
                while i < list_items.len() {
                    if arity > 1 && i + arity > list_items.len() {
                        return Err(RuntimeError::new("Not enough elements for map block arity"));
                    }
                    {
                        let assumed_count = data.assumed_positional.len();
                        // Bind assumed positional args first
                        for (idx, val) in data.assumed_positional.iter().enumerate() {
                            if let Some(p) = data.params.get(idx) {
                                vm.env_mut().insert(p.clone(), val.clone());
                            }
                        }
                        if arity == 1 {
                            let item = list_items[i].clone();
                            if let Some(p) = data.params.get(assumed_count) {
                                vm.env_mut().insert(p.clone(), item.clone());
                            }
                            vm.env_mut().insert(underscore.clone(), item.clone());
                            vm.env_mut().insert(dollar_topic.clone(), item);
                        } else {
                            for (idx, p) in data.params.iter().skip(assumed_count).enumerate() {
                                if i + idx < list_items.len() {
                                    vm.env_mut().insert(p.clone(), list_items[i + idx].clone());
                                }
                            }
                            vm.env_mut()
                                .insert(underscore.clone(), list_items[i].clone());
                            vm.env_mut()
                                .insert(dollar_topic.clone(), list_items[i].clone());
                        }
                    }
                    match vm.run_reuse(&code, &compiled_fns) {
                        Ok(()) => {
                            let val = vm
                                .last_stack_value()
                                .cloned()
                                .or_else(|| vm.env().get("_").cloned())
                                .unwrap_or(Value::Nil);
                            match val {
                                Value::Slip(elems) => result.extend(elems.iter().cloned()),
                                v => result.push(v),
                            }
                        }
                        Err(e) if e.is_next() => {}
                        Err(e) if e.is_last() => break,
                        Err(mut e) => {
                            // A `return` inside the map block targets the routine
                            // that lexically encloses the block. Stamp the signal
                            // with that routine's callable id (captured in the
                            // closure env) so propagation/out-of-dynamic-scope
                            // detection works when the map is forced lazily after
                            // the enclosing routine has exited.
                            if e.is_return()
                                && e.return_target_callable_id.is_none()
                                && let Some(Value::Int(id)) = data.env.get("__mutsu_callable_id")
                            {
                                e.return_target_callable_id = Some(*id as u64);
                            }
                            return Err(e);
                        }
                    }
                    i += arity;
                }

                // Run LAST phasers after the map loop completes (natural end or `last`)
                if !last_phaser_bodies.is_empty() && i > 0 {
                    for phaser_body in &last_phaser_bodies {
                        let phaser_compiler = crate::compiler::Compiler::new();
                        let (phaser_code, phaser_fns) = phaser_compiler.compile(phaser_body);
                        // Ignore errors from LAST phasers (best-effort)
                        let _ = vm.run_reuse(&phaser_code, &phaser_fns);
                        // Record the phaser's captured-outer writes (`LAST $x = True`)
                        // so the enclosing call site drains them into the caller's
                        // local slots — the map body's own writeback (below) only
                        // covers `code`, not the separately-compiled phaser body.
                        vm.record_eager_block_free_var_writeback(&phaser_code, &[]);
                    }
                }

                Ok(Value::array(result))
            });

            // Restore original values (was done on every exit of the old loop).
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
            return loop_result;
        }
        if let Some(func) = func {
            let mut result = Vec::new();
            for item in list_items {
                let value = self.call_sub_value(func.clone(), vec![item], false)?;
                match value {
                    Value::Slip(elems) => result.extend(elems.iter().cloned()),
                    v => result.push(v),
                }
            }
            return Ok(Value::array(result));
        }
        Ok(Value::array(list_items))
    }

    /// Like `eval_map_over_items` but writes back `$_` mutations to the source
    /// list elements, implementing Raku's rw binding semantics for map.
    /// Uses the same VM fast path as `eval_map_over_items` but checks for
    /// `__mutsu_rw_map_topic__` after each iteration to capture mutations.
    pub(super) fn eval_grep_over_items(
        &mut self,
        func: Option<Value>,
        list_items: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let (result, _) = self.eval_grep_over_items_with_mutated(func, list_items)?;
        Ok(result)
    }

    pub(super) fn find_first_match_over_items(
        &mut self,
        func: Option<Value>,
        list_items: &[Value],
        from_end: bool,
    ) -> Result<Option<(usize, Value)>, RuntimeError> {
        let mut matcher = InterpFirstMatcher(self);
        find_first_match_generic(&mut matcher, func.as_ref(), list_items, from_end)
    }
}

/// Engine-agnostic matcher for `.first` / `first`: decides whether a single
/// element matches the pattern. The only engine-specific part is invoking a
/// `Sub` matcher block (the genuine Category-B fork); `smart_match` for non-block
/// patterns is the interpreter's single shared implementation. Implemented for
/// both the interpreter ([`InterpFirstMatcher`]) and the VM (`VmFirstMatcher`),
/// so the iteration / `:end` / `pair_as_positional` logic lives once in
/// [`find_first_match_generic`].
pub(crate) trait FirstMatcher {
    fn item_matches(&mut self, pattern: &Value, item: &Value) -> Result<bool, RuntimeError>;
}

/// [`FirstMatcher`] backed by the tree-walking interpreter.
pub(crate) struct InterpFirstMatcher<'a>(pub(crate) &'a mut Interpreter);

impl FirstMatcher for InterpFirstMatcher<'_> {
    fn item_matches(&mut self, pattern: &Value, item: &Value) -> Result<bool, RuntimeError> {
        if matches!(pattern, Value::Sub(_)) {
            // Pass the element positionally: a Hash-sourced `Value::Pair` would
            // otherwise bind as a named arg, leaving the block with zero
            // positionals (`%h.first({ .value > 1 })`).
            let call_item = crate::runtime::utils::pair_as_positional(item);
            Ok(self
                .0
                .call_sub_value(pattern.clone(), vec![call_item], true)?
                .truthy())
        } else {
            Ok(self.0.smart_match(item, pattern))
        }
    }
}

/// Shared `.first` / `first` scan: return the first (or, with `from_end`, last)
/// `(index, value)` whose element matches `pattern` (or any element when
/// `pattern` is `None`). Engines supply matching through `matcher`.
pub(crate) fn find_first_match_generic(
    matcher: &mut dyn FirstMatcher,
    pattern: Option<&Value>,
    list_items: &[Value],
    from_end: bool,
) -> Result<Option<(usize, Value)>, RuntimeError> {
    if list_items.is_empty() {
        return Ok(None);
    }
    let len = list_items.len();
    for idx in 0..len {
        let actual_idx = if from_end { len - 1 - idx } else { idx };
        let item = list_items[actual_idx].clone();
        let matched = match pattern {
            Some(p) => matcher.item_matches(p, &item)?,
            None => true,
        };
        if matched {
            return Ok(Some((actual_idx, item)));
        }
    }
    Ok(None)
}
