use super::*;

impl VM {
    /// Strip hyper operator delimiters (>>...<<, >>...>>, <<...<<, <<...>>)
    /// and their Unicode variants, returning the inner operator if found.
    fn strip_hyper_delimiters_str(s: &str) -> Option<&str> {
        let after_left = s
            .strip_prefix(">>")
            .or_else(|| s.strip_prefix("<<"))
            .or_else(|| s.strip_prefix('\u{00BB}'))
            .or_else(|| s.strip_prefix('\u{00AB}'))?;
        let inner = after_left
            .strip_suffix(">>")
            .or_else(|| after_left.strip_suffix("<<"))
            .or_else(|| after_left.strip_suffix('\u{00BB}'))
            .or_else(|| after_left.strip_suffix('\u{00AB}'))?;
        if inner.is_empty() {
            return None;
        }
        Some(inner)
    }

    pub(super) fn eval_reduction_operator_values(
        &mut self,
        op: &str,
        left: &Value,
        right: &Value,
    ) -> Result<Value, RuntimeError> {
        if let Some(inner_op) = op.strip_prefix('R')
            && !inner_op.is_empty()
        {
            return self.eval_reduction_operator_values(inner_op, right, left);
        }
        // Bare Z: zip two lists into tuples (used by [Z] reduction).
        // When left elements are already lists (from a prior Z fold), flatten them
        // so that [Z] (a,b,c),(d,e,f),(g,h,i) produces (a d g), (b e h), (c f i).
        if op == "Z" {
            let left_list = runtime::value_to_list(left);
            let right_list = runtime::value_to_list(right);
            let len = left_list.len().min(right_list.len());
            let mut results = Vec::new();
            for i in 0..len {
                let mut tuple = match &left_list[i] {
                    Value::Array(items, kind) if !kind.is_itemized() => items.to_vec(),
                    other => vec![other.clone()],
                };
                tuple.push(right_list[i].clone());
                results.push(Value::array(tuple));
            }
            return Ok(Value::array(results));
        }
        // Z-prefixed meta-operator: zip two lists element-wise with the inner op.
        if let Some(inner_op) = op.strip_prefix('Z')
            && !inner_op.is_empty()
        {
            let left_list = runtime::value_to_list(left);
            let right_list = runtime::value_to_list(right);
            let len = left_list.len().min(right_list.len());
            let mut results = Vec::new();
            for i in 0..len {
                results.push(self.eval_reduction_operator_values(
                    inner_op,
                    &left_list[i],
                    &right_list[i],
                )?);
            }
            return Ok(Value::array(results));
        }
        // Hyper operator forms: >>op<<, >>op>>, <<op<<, <<op>>
        // Apply inner op element-wise to two lists.
        if let Some(inner_op) = Self::strip_hyper_delimiters_str(op) {
            let left_list = runtime::value_to_list(left);
            let right_list = runtime::value_to_list(right);
            let dwim_left = op.starts_with("<<") || op.starts_with('\u{00AB}');
            let dwim_right = op.ends_with(">>") || op.ends_with('\u{00BB}');
            let len = if dwim_left && dwim_right {
                left_list.len().max(right_list.len())
            } else if dwim_left {
                right_list.len()
            } else if dwim_right {
                left_list.len()
            } else {
                left_list.len().max(right_list.len())
            };
            let mut results = Vec::with_capacity(len);
            for i in 0..len {
                let l = if left_list.is_empty() {
                    &Value::Int(0.into())
                } else {
                    &left_list[i % left_list.len()]
                };
                let r = if right_list.is_empty() {
                    &Value::Int(0.into())
                } else {
                    &right_list[i % right_list.len()]
                };
                results.push(self.eval_reduction_operator_values(inner_op, l, r)?);
            }
            return Ok(Value::array(results));
        }
        // Thread junctions through arithmetic/comparison reduction ops
        if matches!(left, Value::Junction { .. }) || matches!(right, Value::Junction { .. }) {
            return self.eval_reduction_op_with_junctions(op, left.clone(), right.clone());
        }
        let normalized_op = if op == "\u{2218}" { "o" } else { op };
        match Interpreter::apply_reduction_op(normalized_op, left, right) {
            Ok(v) => Ok(v),
            Err(err) if err.message.starts_with("Unsupported reduction operator:") => {
                let args = vec![left.clone(), right.clone()];
                if let Some(name) = normalized_op.strip_prefix('&') {
                    let callable = self.interpreter.resolve_code_var(name);
                    if matches!(
                        callable,
                        Value::Sub(_)
                            | Value::WeakSub(_)
                            | Value::Routine { .. }
                            | Value::Instance { .. }
                    ) {
                        return self.vm_call_on_value(callable, args, None);
                    }
                } else {
                    let infix_name = format!("infix:<{}>", normalized_op);
                    if let Some(v) = self.try_user_infix(&infix_name, left, right)? {
                        return Ok(v);
                    }
                    if let Some(callable) = self
                        .interpreter
                        .env()
                        .get(&format!("&{}", infix_name))
                        .cloned()
                    {
                        return self.vm_call_on_value(callable, args.clone(), None);
                    }
                    if let Some(callable) = self
                        .interpreter
                        .env()
                        .get(&format!("&{}", normalized_op))
                        .cloned()
                    {
                        return self.vm_call_on_value(callable, args.clone(), None);
                    }
                }
                Err(err)
            }
            Err(err) => Err(err),
        }
    }

    pub(super) fn coerce_numeric_bridge_value(
        &mut self,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        if !matches!(value, Value::Instance { .. }) {
            return Ok(value);
        }
        // Unhandled Failure: throw the stored exception
        if let Some(err) = self
            .interpreter
            .failure_to_runtime_error_if_unhandled(&value)
        {
            return Err(err);
        }
        // Match coerces to Numeric via its matched string
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &value
            && class_name == "Match"
            && let Some(str_val) = attributes.get("str")
        {
            let s = str_val.to_string_value();
            let s = s.trim();
            if let Ok(i) = s.parse::<i64>() {
                return Ok(Value::Int(i));
            }
            if let Ok(f) = s.parse::<f64>() {
                return Ok(Value::Num(f));
            }
            return Ok(Value::Int(0));
        }
        // Check if type is known to be Real/Numeric, OR if the class has a
        // user-defined Numeric method (for classes like `class Blue { method Numeric { 3 } }`)
        let known_numeric = self.interpreter.type_matches_value("Real", &value)
            || self.interpreter.type_matches_value("Numeric", &value);
        let has_numeric_method = if let Value::Instance { ref class_name, .. } = value {
            let cn = class_name.to_string();
            self.interpreter.has_user_method(&cn, "Numeric")
        } else {
            false
        };
        if !known_numeric && !has_numeric_method {
            return Ok(value);
        }
        self.try_compiled_method_or_interpret(value.clone(), "Numeric", vec![])
            .or_else(|_| self.try_compiled_method_or_interpret(value.clone(), "Bridge", vec![]))
            .or(Ok(value))
    }

    pub(super) fn coerce_numeric_bridge_pair(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<(Value, Value), RuntimeError> {
        Ok((
            self.coerce_numeric_bridge_value(left)?,
            self.coerce_numeric_bridge_value(right)?,
        ))
    }

    /// Evaluate truthiness of a value, including dispatch to user-defined Bool methods.
    /// For Package (type objects) and Instance values, checks if the class defines
    /// a custom Bool method and calls it. Falls back to Value::truthy() otherwise.
    pub(super) fn eval_truthy(&mut self, val: &Value) -> bool {
        match val {
            Value::Package(name) => {
                let class_name = name.resolve().to_string();
                if self
                    .interpreter
                    .resolve_method_with_owner(&class_name, "Bool", &[])
                    .is_some()
                    && let Ok(result) =
                        self.try_compiled_method_or_interpret(val.clone(), "Bool", vec![])
                {
                    return result.truthy();
                }
                val.truthy()
            }
            Value::Instance { class_name, .. } => {
                let cn = class_name.resolve().to_string();
                if self
                    .interpreter
                    .resolve_method_with_owner(&cn, "Bool", &[])
                    .is_some()
                    && let Ok(result) =
                        self.try_compiled_method_or_interpret(val.clone(), "Bool", vec![])
                {
                    return result.truthy();
                }
                val.truthy()
            }
            Value::Regex(_)
            | Value::RegexWithAdverbs { .. }
            | Value::Routine { is_regex: true, .. } => {
                let topic = self
                    .interpreter
                    .env()
                    .get("_")
                    .cloned()
                    .unwrap_or(Value::Nil);
                self.vm_smart_match(&topic, val)
            }
            _ => val.truthy(),
        }
    }

    /// VM-native dispatch for calling a value (Sub, Routine, Junction, etc.).
    ///
    /// This avoids the interpreter's `eval_call_on_value` for common cases:
    /// - Value::Sub with compiled_code -> call_compiled_closure
    /// - Value::Sub without compiled_code -> compile on-the-fly, then call_compiled_closure
    /// - Value::Routine -> resolve to function name and dispatch
    /// - Value::Junction -> thread over values
    /// - Value::WeakSub -> upgrade to Sub and recurse
    ///
    /// Falls back to interpreter for Mixin (CALL-ME from roles) and Instance (CALL-ME).
    pub(super) fn vm_call_on_value(
        &mut self,
        target: Value,
        args: Vec<Value>,
        compiled_fns: Option<&HashMap<String, CompiledFunction>>,
    ) -> Result<Value, RuntimeError> {
        // Upgrade WeakSub to Sub transparently
        let target = if let Value::WeakSub(ref weak) = target {
            match weak.upgrade() {
                Some(strong) => Value::Sub(strong),
                None => return Err(RuntimeError::new("Callable has been freed")),
            }
        } else {
            target
        };

        // Fast path: Sub with compiled_code
        if let Value::Sub(ref data) = target
            && let Some(ref cc) = data.compiled_code
        {
            let cc = cc.clone();
            let data = data.clone();
            let empty_fns = HashMap::new();
            let fns = compiled_fns.unwrap_or(&empty_fns);
            return self.call_compiled_closure(&data, &cc, args, fns);
        }

        // Sub without compiled_code: compile on-the-fly then dispatch via VM
        if let Value::Sub(ref data) = target
            && !data.body.is_empty()
        {
            let cc = {
                let mut compiler = crate::compiler::Compiler::new();
                // Use routine closure body so `return` inside the sub works correctly
                compiler.compile_routine_closure_body(&data.params, &data.param_defs, &data.body)
            };
            let data = data.clone();
            let empty_fns = HashMap::new();
            let fns = compiled_fns.unwrap_or(&empty_fns);
            return self.call_compiled_closure(&data, &cc, args, fns);
        }

        // Routine: resolve to function name and dispatch
        // Keep using interpreter.call_function here because Routine values may
        // reference builtin functions (e.g. &SETTING::not resolves to Routine{name:"not"})
        // and call_function correctly prioritizes builtins over user-defined functions.
        if let Value::Routine { package, name, .. } = &target {
            let pkg = package.resolve();
            let name_str = name.resolve();
            if !pkg.is_empty() && pkg != "GLOBAL" {
                let fq = format!("{pkg}::{name_str}");
                if self.interpreter.has_function(&fq) {
                    return self.interpreter.call_function(&fq, args);
                }
            }
            if self.interpreter.has_function(&name_str)
                || self.interpreter.has_proto(&name_str)
                || self.interpreter.has_multi_candidates(&name_str)
            {
                return self.interpreter.call_function(&name_str, args);
            }
            // Method dispatch fallback for &?ROUTINE.dispatcher()(self, ...)
            if !args.is_empty() && !pkg.is_empty() && pkg != "GLOBAL" {
                let invocant = args[0].clone();
                let method_args = args[1..].to_vec();
                return self
                    .interpreter
                    .call_method_with_values(invocant, &name_str, method_args);
            }
            return self.interpreter.call_function(&name_str, args);
        }

        // Junction: thread over values
        if let Value::Junction { kind, values } = target {
            let mut results = Vec::with_capacity(values.len());
            for callable in values.iter() {
                results.push(self.vm_call_on_value(
                    callable.clone(),
                    args.clone(),
                    compiled_fns,
                )?);
            }
            return Ok(Value::junction(kind, results));
        }

        // Mixin wrapping a Sub/Routine: try inner callable first
        if let Value::Mixin(ref inner, ref mixins) = target {
            // Check if any mixed-in role provides CALL-ME
            for key in mixins.keys() {
                if let Some(role_name) = key.strip_prefix("__mutsu_role__")
                    && self.interpreter.role_has_method(role_name, "CALL-ME")
                {
                    // TODO: complex case -- fall back to interpreter for CALL-ME on Mixin
                    return self.try_compiled_method_or_interpret(target, "CALL-ME", args);
                }
            }
            // Delegate to inner callable
            return self.vm_call_on_value(inner.as_ref().clone(), args, compiled_fns);
        }

        // Instance or Package (type object): CALL-ME -- try compiled method path first
        if matches!(target, Value::Instance { .. } | Value::Package(_)) {
            return self.try_compiled_method_or_interpret(target, "CALL-ME", args);
        }

        // Sub with empty body (no-op closure): call directly via interpreter's
        // call_sub_value, avoiding the eval_call_on_value indirection since we
        // already know the target is a Sub.
        if matches!(target, Value::Sub(_)) {
            return self.interpreter.call_sub_value(target, args, true);
        }

        Ok(Value::Nil)
    }

    /// Force a lazy thunk: evaluate the sub on first access, cache and return the result.
    pub(crate) fn force_lazy_thunk(
        &mut self,
        thunk_data: &std::sync::Arc<crate::value::LazyThunkData>,
    ) -> Result<Value, RuntimeError> {
        // Check cache first
        {
            let cache = thunk_data.cache.lock().unwrap();
            if let Some(ref cached) = *cache {
                return Ok(cached.clone());
            }
        }
        // Evaluate the thunk (call the sub with no args)
        let result = self
            .interpreter
            .call_sub_value(thunk_data.thunk.clone(), vec![], true)?;
        // Cache the result
        {
            let mut cache = thunk_data.cache.lock().unwrap();
            *cache = Some(result.clone());
        }
        Ok(result)
    }

    /// Thread a reduction operator through junctions.
    fn eval_reduction_op_with_junctions(
        &mut self,
        op: &str,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        if let Value::Junction { kind, values } = left {
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .cloned()
                .map(|v| self.eval_reduction_op_with_junctions(op, v, right.clone()))
                .collect();
            return Ok(Value::junction(kind, results?));
        }
        if let Value::Junction { kind, values } = right {
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .cloned()
                .map(|v| self.eval_reduction_op_with_junctions(op, left.clone(), v))
                .collect();
            return Ok(Value::junction(kind, results?));
        }
        let normalized_op = if op == "\u{2218}" { "o" } else { op };
        Interpreter::apply_reduction_op(normalized_op, &left, &right)
    }
}
