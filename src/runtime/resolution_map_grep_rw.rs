use super::*;
use crate::value::ValueView;

impl Interpreter {
    pub(super) fn eval_map_over_items_rw(
        &mut self,
        func: Option<Value>,
        list_items: &mut [Value],
    ) -> Result<Value, RuntimeError> {
        let topic_key = "__mutsu_rw_map_topic__";
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
            if requires_full_binding
                || is_routine_callback
                || data.env.contains_key("__mutsu_routine_name")
                || data.env.contains_key("__mutsu_compose_left")
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
                    self.env.remove(topic_key);
                    let value =
                        self.call_sub_value(Value::sub_value(data.clone()), chunk, false)?;
                    if arity == 1
                        && let Some(mutated) = self.env.get(topic_key).cloned()
                    {
                        list_items[i] = mutated;
                    }
                    let value = self.reify_finite_pipe_value(value)?;
                    if let ValueView::Slip(elems) = value.view() {
                        result.extend(elems.iter().cloned());
                    } else {
                        result.push(value);
                    }
                    i += arity;
                }
                self.env.remove(topic_key);
                return Ok(Value::array(result));
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
            let saved: Vec<(String, Option<Value>)> = touched_keys
                .iter()
                .map(|k| (k.clone(), self.env.get(k).cloned()))
                .collect();

            for (k, v) in &data.env {
                if !self.env.contains_key_sym(*k) {
                    self.env.insert_sym(*k, v.clone());
                }
            }

            // CP-3 collapse: run the rw map loop with fresh execution registers
            // (replaces the `mem::take(self)` + `VM::new` sub-VM). The closure
            // returns the loop's Result; `with_nested_registers` restores the
            // outer registers and flags env_dirty. The `saved`/`topic_key` env
            // restore is hoisted to after the call (ran on every old exit).
            let loop_result: Result<Value, RuntimeError> = self.with_nested_registers(|vm| {
                let mut i = 0usize;
                while i < list_items.len() {
                    if arity > 1 && i + arity > list_items.len() {
                        return Err(RuntimeError::new("Not enough elements for map block arity"));
                    }
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
                                .unwrap_or(Value::NIL);
                            // Write back topic mutation if it happened
                            if arity == 1
                                && let Some(mutated) = vm.env().get(topic_key).cloned()
                            {
                                list_items[i] = mutated;
                            }
                            let val = vm.reify_finite_pipe_value(val)?;
                            if let ValueView::Slip(elems) = val.view() {
                                result.extend(elems.iter().cloned());
                            } else {
                                result.push(val);
                            }
                        }
                        Err(e) if e.is_next() => {
                            if arity == 1
                                && let Some(mutated) = vm.env().get(topic_key).cloned()
                            {
                                list_items[i] = mutated;
                            }
                        }
                        Err(e) if e.is_last() => {
                            if arity == 1
                                && let Some(mutated) = vm.env().get(topic_key).cloned()
                            {
                                list_items[i] = mutated;
                            }
                            break;
                        }
                        Err(e) => {
                            return Err(e);
                        }
                    }
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
            return loop_result;
        }
        // Non-Sub func: delegate to regular map
        self.eval_map_over_items(func, list_items.to_vec())
    }

    pub(super) fn eval_grep_over_items_with_mutated(
        &mut self,
        func: Option<Value>,
        mut list_items: Vec<Value>,
    ) -> Result<(Value, Vec<Value>), RuntimeError> {
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
                .any(|pd| pd.sub_signature.is_some() || pd.outer_sub_signature.is_some());
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
            if data.env.contains_key("__mutsu_compose_left")
                && data.env.contains_key("__mutsu_compose_right")
            {
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

            // Compile once, reuse VM for every iteration.
            // `return` inside this block should propagate up to the
            // lexically enclosing routine (if any), so mark the fresh
            // compiler as being lexically nested inside a routine whenever
            // a routine is currently on the dynamic call stack.
            let mut compiler = crate::compiler::Compiler::new();
            compiler.lexically_in_routine = !self.routine_stack.is_empty();
            let normalized_body =
                super::resolution_map_grep::normalize_tail_stmt_for_value(&data.body);
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
            let topic_source_key = "__mutsu_grep_topic_source".to_string();
            if !touched_keys.iter().any(|k| k == &topic_source_key) {
                touched_keys.push(topic_source_key.clone());
            }
            let saved: Vec<(String, Option<Value>)> = touched_keys
                .iter()
                .map(|k| (k.clone(), self.env.get(k).cloned()))
                .collect();

            // Pre-insert closure env
            for (k, v) in &data.env {
                self.env.insert_sym(*k, v.clone());
            }

            // CP-3 collapse: run the grep loop with fresh execution registers
            // (replaces the `mem::take(self)` + `VM::new` sub-VM). The closure
            // returns Ok(()) / Err on the loop; `with_nested_registers` restores
            // the outer registers and flags env_dirty. The `saved` env restore is
            // hoisted to after the call (ran on every old exit path).
            let loop_result: Result<(), RuntimeError> = self.with_nested_registers(|vm| {
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
                                vm.env_mut().insert(underscore.clone(), chunk[0].clone());
                                vm.env_mut().insert(dollar_topic.clone(), chunk[0].clone());
                                vm.env_mut()
                                    .insert(topic_source_key.clone(), chunk[0].clone());
                            } else {
                                for (idx, p) in data.params.iter().skip(assumed_count).enumerate() {
                                    if idx < chunk.len() {
                                        vm.env_mut().insert(p.clone(), chunk[idx].clone());
                                    }
                                }
                                vm.env_mut().insert(underscore.clone(), chunk[0].clone());
                                vm.env_mut().insert(dollar_topic.clone(), chunk[0].clone());
                            }
                        }
                        vm.set_topic_source_var((arity == 1).then_some(topic_source_key.clone()));
                        match vm.run_reuse(&code, &compiled_fns) {
                            Ok(()) => {
                                let pred = vm
                                    .last_stack_value()
                                    .cloned()
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
