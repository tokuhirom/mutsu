use super::*;

/// Convert a `CallArg` to an `Expr` for expression-level call compilation,
/// preserving named (`k => v`) and slip (`|v`) args. Mirrors the compiler's
/// `call_args_to_expr_args`.
fn call_arg_to_expr(arg: &crate::ast::CallArg) -> crate::ast::Expr {
    use crate::ast::{CallArg, Expr};
    use crate::token_kind::TokenKind;
    match arg {
        CallArg::Positional(e) | CallArg::Invocant(e) => e.clone(),
        CallArg::Named {
            name,
            value: Some(e),
        } => Expr::Binary {
            left: Box::new(Expr::Literal(Value::str(name.clone()))),
            op: TokenKind::FatArrow,
            right: Box::new(e.clone()),
        },
        CallArg::Named { name, value: None } => Expr::Binary {
            left: Box::new(Expr::Literal(Value::str(name.clone()))),
            op: TokenKind::FatArrow,
            right: Box::new(Expr::Literal(Value::TRUE)),
        },
        CallArg::Slip(e) => Expr::Unary {
            op: TokenKind::Pipe,
            expr: Box::new(e.clone()),
        },
    }
}

/// Normalize the last non-`SetLine` statement of a `.map`/`.grep` block `body`
/// so the re-compiled block leaves its value on the stack (otherwise the map/grep
/// result wrongly falls back to the topic `$_`). Two statement shapes compile as
/// value-discarding statements and need rewriting into their expression form:
///
/// - a bare `Stmt::Call` carrying named/slip args (`f(k => v)`) → `Stmt::Expr(Call)`
/// - a plain `Stmt::Assign` to an already-declared variable (`$x = 9`) →
///   `Stmt::Expr(AssignExpr)`, so `(1, 2, 3).map({ $x = 9 })` yields the assigned
///   value, not the topic. Using the assignment *expression* keeps the normal
///   store (readonly / type-constraint checks) intact.
pub(super) fn normalize_tail_stmt_for_value(body: &[crate::ast::Stmt]) -> Vec<crate::ast::Stmt> {
    use crate::ast::{AssignOp, Expr, Stmt};
    let Some(last_idx) = body.iter().rposition(|s| !matches!(s, Stmt::SetLine(_))) else {
        return body.to_vec();
    };
    match &body[last_idx] {
        Stmt::Call { name, args } => {
            let expr_args = args.iter().map(call_arg_to_expr).collect();
            let mut out = body.to_vec();
            out[last_idx] = Stmt::Expr(Expr::Call {
                name: *name,
                args: expr_args,
            });
            out
        }
        // A plain assignment to an existing variable; compound ops desugar the
        // operator into the RHS, so `op` is always `Assign` here. A `Feed`-RHS
        // assignment (`@x = SOURCE ==> SINK`) is left alone — it has its own
        // sink-context statement lowering.
        Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
        } if !matches!(expr, Expr::Feed { .. }) => {
            let mut out = body.to_vec();
            out[last_idx] = Stmt::Expr(Expr::AssignExpr {
                name: name.clone(),
                expr: Box::new(expr.clone()),
                is_bind: false,
            });
            out
        }
        _ => body.to_vec(),
    }
}

impl Interpreter {
    pub(super) fn eval_map_over_items(
        &mut self,
        func: Option<Value>,
        list_items: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Some(ValueView::Sub(data)) = func.as_ref().map(Value::view) {
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
            // A routine callback (`map $f, @xs` / `@xs.map($f)` where `$f` is a
            // `sub`) must run through the real call path so a `return` in its
            // body ends THAT call with the returned value (routine semantics).
            // The inline fast path below runs the body as a bare block, so the
            // return signal would escape the map VM and surface as
            // X::ControlFlow::Return (99problems-21-to-30.t P23 `$compress`).
            // A WhateverCode is expression-shaped (it cannot contain `return`)
            // and needs the fast path's outer-topic handling for its `$_`.
            let is_routine_callback = !data.is_bare_block
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
                && crate::ast::collect_placeholders_shallow(&data.body).is_empty();
            if requires_full_binding || is_routine_callback {
                // `map` batches the source by the block's `.count` (the number of
                // positional parameters it will bind): a multi-positional block
                // such as `-> \a, \b { }` consumes a chunk of that many elements
                // per call. A slurpy parameter makes `.count` infinite, so the
                // batch falls back to the required-positional arity (min 1). The
                // previous code always passed a single element here, so sigilless
                // / typed / defaulted multi-param blocks were mis-called with one
                // argument (`Too few positionals passed`).
                let assumed = data.assumed_positional.len();
                let positional: Vec<&crate::ast::ParamDef> = data
                    .param_defs
                    .iter()
                    .filter(|pd| !pd.named && !pd.is_invocant)
                    .collect();
                let batch = if positional.is_empty() {
                    data.params.len().saturating_sub(assumed).max(1)
                } else if positional
                    .iter()
                    .any(|pd| pd.slurpy || pd.is_capture_subsignature())
                {
                    positional
                        .iter()
                        .filter(|pd| {
                            !pd.slurpy
                                && !pd.optional_marker
                                && pd.default.is_none()
                                && !pd.is_capture_subsignature()
                        })
                        .count()
                        .saturating_sub(assumed)
                        .max(1)
                } else {
                    positional.len().saturating_sub(assumed).max(1)
                };
                let mut result = Vec::new();
                let mut i = 0usize;
                while i < list_items.len() {
                    let end = (i + batch).min(list_items.len());
                    let chunk: Vec<Value> = list_items[i..end].to_vec();
                    // A short final chunk of a required-arity block raises
                    // "Too few positionals" (matching raku); an optional trailing
                    // parameter binds the missing slot to its default / `Any`.
                    let value =
                        self.call_sub_value(Value::sub_value(data.clone()), chunk, false)?;
                    let value = self.reify_finite_pipe_value(value)?;
                    match value.view() {
                        ValueView::Slip(elems) => result.extend(elems.iter().cloned()),
                        _ => result.push(value),
                    }
                    i = end;
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
                    let value =
                        self.call_sub_value(Value::sub_value(data.clone()), chunk, false)?;
                    let value = self.reify_finite_pipe_value(value)?;
                    match value.view() {
                        ValueView::Slip(elems) => result.extend(elems.iter().cloned()),
                        _ => result.push(value),
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
                    let value =
                        self.call_sub_value(Value::sub_value(data.clone()), chunk, false)?;
                    let value = self.reify_finite_pipe_value(value)?;
                    match value.view() {
                        ValueView::Slip(elems) => result.extend(elems.iter().cloned()),
                        _ => result.push(value),
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
            // The map value is taken from the block's tail-expression result (or
            // the topic `$_` if none was left on the stack). A bare tail
            // `Stmt::Call` carrying named/slip args (how an imported sub call like
            // `f(k => v)` parses) is compiled as a value-discarding statement, so
            // the result would wrongly fall back to the topic. Normalize such a
            // tail into `Stmt::Expr(Expr::Call)` so its value is preserved.
            let normalized_body = normalize_tail_stmt_for_value(&data.body);
            let (code, compiled_fns) = compiler.compile(&normalized_body);

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

            // A WhateverCode whose body references `$_` (`*.new($_)`) compiles its
            // `*` placeholder to a synthetic `__wc_N` param so the element binds
            // there, NOT to `$_`; the body's `$_` must stay the caller's topic
            // (S02 "no scoping issues when using topic variables":
            // `do { $_ = 42; (Int).map(*.new($_)) }` -> `Int.new(42)`). So for such
            // a WhateverCode, instead of topicalizing `$_`/`_` to the element, hold
            // them at the caller's outer topic for every iteration (re-set so a
            // prior iteration's tail value can't leak into the next one's `$_`).
            // A plain `*.abs` (no `$_` in the body) keeps `_` AS its placeholder
            // param, so it must still receive the element — hence the
            // `_`-is-not-a-param guard.
            let is_whatever_code = matches!(
                data.env.get("__mutsu_callable_type").map(Value::view),
                Some(ValueView::Str(kind)) if kind.as_str() == "WhateverCode"
            ) && !data.params.iter().any(|p| p == "_");
            let outer_topic = self.env.get("_").cloned();

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
                            if is_whatever_code {
                                match &outer_topic {
                                    Some(t) => {
                                        vm.env_mut().insert(underscore.clone(), t.clone());
                                        vm.env_mut().insert(dollar_topic.clone(), t.clone());
                                    }
                                    None => {
                                        vm.env_mut().remove(&underscore);
                                        vm.env_mut().remove(&dollar_topic);
                                    }
                                }
                            } else {
                                vm.env_mut().insert(underscore.clone(), item.clone());
                                vm.env_mut().insert(dollar_topic.clone(), item);
                            }
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
                                .unwrap_or(Value::NIL);
                            // A callback that returns a finite lazy `.map`/`.grep`
                            // pipe (e.g. `{ gather {...}.grep(...) }`) must reify
                            // it here — the result array's static readers (`.flat`,
                            // `for`) can't run the VM to force a nested pipe.
                            let val = vm.reify_finite_pipe_value(val)?;
                            match val.view() {
                                ValueView::Slip(elems) => result.extend(elems.iter().cloned()),
                                _ => result.push(val),
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
                                && e.return_target_callable_id().is_none()
                                && let Some(ValueView::Int(id)) =
                                    data.env.get("__mutsu_callable_id").map(Value::view)
                            {
                                e.set_return_target_callable_id(Some(id as u64));
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
                let value = self.reify_finite_pipe_value(value)?;
                match value.view() {
                    ValueView::Slip(elems) => result.extend(elems.iter().cloned()),
                    _ => result.push(value),
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
        if let Some(func_ref) = func.as_ref()
            && let Some(res) = self.try_first_match_batched(func_ref, list_items, from_end)
        {
            return res;
        }
        let mut matcher = InterpFirstMatcher(self);
        find_first_match_generic(&mut matcher, func.as_ref(), list_items, from_end)
    }

    /// Batched `.first` over a `Sub` matcher: one compile + closure-env setup,
    /// then a bare `run_reuse` per element with an early exit on the first
    /// match — the same setup-once shape as `eval_grep_over_items_with_mutated`.
    /// The per-element full call machinery (`call_sub_value` /
    /// `call_compiled_closure`: call frame, scoped overlay, captured-env merge,
    /// `&?BLOCK` construction, full binder) costs ~25x more per element, which
    /// made `.first(*.contains(...))` the single hottest line of zef's
    /// Ecosystems populate.
    ///
    /// Returns `None` when the matcher needs the full per-element machinery
    /// (destructuring sub-signature, composed callable, `.assuming` partials,
    /// multi-param blocks), so the caller falls back unchanged.
    pub(crate) fn try_first_match_batched(
        &mut self,
        func: &Value,
        list_items: &[Value],
        from_end: bool,
    ) -> Option<Result<Option<(usize, Value)>, RuntimeError>> {
        let ValueView::Sub(data) = func.view() else {
            return None;
        };
        if list_items.is_empty() {
            return Some(Ok(None));
        }
        let data = data.clone();
        if data
            .param_defs
            .iter()
            .any(|pd| pd.sub_signature.is_some() || pd.outer_sub_signature.is_some())
        {
            return None;
        }
        if !data.assumed_positional.is_empty() || !data.assumed_named.is_empty() {
            return None;
        }
        if data.env.contains_key("__mutsu_compose_left")
            && data.env.contains_key("__mutsu_compose_right")
        {
            return None;
        }
        // A multi-param matcher block would take element tuples; `.first` has
        // no chunking semantics — keep it on the per-element generic path.
        if data.params.len() > 1 {
            return None;
        }

        // Compile once (mirrors grep: normalize the tail statement so the last
        // expression lands on the stack as the predicate value).
        let mut compiler = crate::compiler::Compiler::new();
        compiler.lexically_in_routine = !self.routine_stack.is_empty();
        let normalized_body = normalize_tail_stmt_for_value(&data.body);
        let (code, compiled_fns) = compiler.compile(&normalized_body);

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
        let saved: Vec<(String, Option<Value>)> = touched_keys
            .iter()
            .map(|k| (k.clone(), self.env.get(k).cloned()))
            .collect();

        // Pre-insert closure env
        for (k, v) in &data.env {
            self.env.insert_sym(*k, v.clone());
        }

        let mut found: Option<(usize, Value)> = None;
        let loop_result: Result<(), RuntimeError> = self.with_nested_registers(|vm| {
            vm.set_topic_source_var(None);
            let len = list_items.len();
            for scan in 0..len {
                let idx = if from_end { len - 1 - scan } else { scan };
                let item = &list_items[idx];
                // A Hash-sourced `Pair` element binds positionally (same as
                // the generic path's `pair_as_positional`).
                let call_item = crate::runtime::utils::pair_as_positional(item);
                'body_redo: loop {
                    if let Some(p) = data.params.first() {
                        vm.env_mut().insert(p.clone(), call_item.clone());
                    }
                    vm.env_mut().insert(underscore.clone(), call_item.clone());
                    vm.env_mut().insert(dollar_topic.clone(), call_item.clone());
                    match vm.run_reuse(&code, &compiled_fns) {
                        Ok(()) => {
                            let pred = vm
                                .last_stack_value()
                                .cloned()
                                .or_else(|| vm.env().get("_").cloned())
                                .unwrap_or(Value::NIL);
                            if pred.truthy() {
                                found = Some((idx, item.clone()));
                            }
                            break 'body_redo;
                        }
                        Err(e) if e.is_redo() => continue 'body_redo,
                        // `next` skips the element; `last` stops the scan and
                        // returns the CURRENT element as the match (Rakudo
                        // behaviour, mirrored from `find_first_match_generic`).
                        Err(e) if e.is_next() => break 'body_redo,
                        Err(e) if e.is_last() => {
                            found = Some((idx, item.clone()));
                            return Ok(());
                        }
                        Err(e) => return Err(e),
                    }
                }
                if found.is_some() {
                    return Ok(());
                }
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
        // The matcher block may mutate a captured-outer lexical (`.first({
        // $count++; ... })` — S32-list/first-kv.t "matcher got only executed
        // once"). Its `$count++` landed in the shared `env` during the loop,
        // but the caller's stale `locals` slot must be refreshed on return.
        // Queue those free-var writes for the CallMethod op to drain, exactly
        // as `eval_grep_over_items_with_mutated` does.
        if loop_result.is_ok() {
            self.record_eager_block_free_var_writeback(&code, &data.params);
        }
        match loop_result {
            Ok(()) => Some(Ok(found)),
            Err(e) => Some(Err(e)),
        }
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
        if matches!(pattern.view(), ValueView::Sub(_)) {
            // Pass the element positionally: a Hash-sourced `Pair` would
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
            // A block matcher may run `next`/`last` (loop control). `next` skips
            // the current element (treat as non-matching); `last` stops the scan
            // and returns the CURRENT element as the match (Rakudo behaviour, e.g.
            // `(5,1,2).first({ last if $_ == 1; $_ > 10 })` returns 1).
            Some(p) => match matcher.item_matches(p, &item) {
                Ok(m) => m,
                Err(e) if e.is_next() => false,
                Err(e) if e.is_last() => return Ok(Some((actual_idx, item))),
                Err(e) => return Err(e),
            },
            None => true,
        };
        if matched {
            return Ok(Some((actual_idx, item)));
        }
    }
    Ok(None)
}
