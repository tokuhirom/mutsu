use super::*;

impl Interpreter {
    /// A plain, eager, list-like target — `Array`/`List`, `Seq`, `Slip`, or any
    /// `Range`. These are the shapes the native `.sort` / `.min` / `.max` /
    /// `.minmax` / `.first` paths handle without falling back to the interpreter
    /// (`Shaped`/`Lazy`/itemized arrays, `Instance`/`Supply`, etc. do not).
    /// `Hash` is intentionally excluded — callers that also accept it
    /// (`.sort` / `.first`) test for it separately.
    pub(super) fn is_plain_eager_list(target: &crate::value::Value) -> bool {
        use crate::value::{ArrayKind, Value};
        matches!(
            target,
            Value::Array(_, ArrayKind::Array | ArrayKind::List)
                | Value::Seq(_)
                | Value::Slip(_)
                | Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::GenericRange { .. }
        )
    }

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
        // A reduction operator used as the inner op of a hyper op, e.g.
        // `(1,2) >>[+]<< (100,200)`. Reducing the base op over the two operands
        // is just the base op applied once.
        if let Some(inner) = op.strip_prefix('[')
            && let Some(inner_op) = inner.strip_suffix(']')
            && !inner_op.is_empty()
        {
            return self.eval_reduction_operator_values(inner_op, left, right);
        }
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
            return Ok(Value::Seq(std::sync::Arc::new(results)));
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
        // Normalize Unicode operator aliases to their ASCII forms so they work
        // in reduction / hyper / cross / zip meta-ops (`[×]`, `»×»`, `Z×`), just
        // as they do as plain infixes: ∘→o, ×→*, ÷→/, −(U+2212)→-, ≤→<=, ≥→>=,
        // ≠→!=.
        let normalized_op = match op {
            "\u{2218}" => "o",
            "\u{00D7}" => "*",
            "\u{00F7}" => "/",
            "\u{2212}" => "-",
            "\u{2264}" => "<=",
            "\u{2265}" => ">=",
            "\u{2260}" => "!=",
            other => other,
        };
        match Interpreter::apply_reduction_op(normalized_op, left, right) {
            Ok(v) => Ok(v),
            Err(err) if err.message.starts_with("Unsupported reduction operator:") => {
                let args = vec![left.clone(), right.clone()];
                if let Some(name) = normalized_op.strip_prefix('&') {
                    let callable = loan_env!(self, resolve_code_var(name));
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
                    if let Some(callable) = self.env().get(&format!("&{}", infix_name)).cloned() {
                        return self.vm_call_on_value(callable, args.clone(), None);
                    }
                    if let Some(callable) = self.env().get(&format!("&{}", normalized_op)).cloned()
                    {
                        return self.vm_call_on_value(callable, args.clone(), None);
                    }
                }
                Err(err)
            }
            Err(err) => Err(err),
        }
    }

    /// Coerce an Instance operand to a numeric value via its `Numeric`/`Bridge`
    /// method. Delegates to the single authoritative implementation on the
    /// interpreter (`Interpreter::coerce_infix_operand_numeric`) so the
    /// Instance->numeric bridge logic is not duplicated between the Interpreter and the
    /// interpreter. Non-Instance values (the hot Int/Num/Rat path) return early
    /// inside the helper without any method dispatch.
    pub(super) fn coerce_numeric_bridge_value(
        &mut self,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        // Slice F (compiled-method redispatch coherence): a user `Numeric`/`Bridge`
        // method run by this internal coercion can mutate a captured-outer caller
        // lexical (`my $c; method Numeric { $c++; ... }`). `call_compiled_method`
        // records that write into `pending_rw_writeback_sources`, but — unlike an
        // explicit `$obj.Numeric` call op — this internal redispatch has no
        // surrounding op to drain it, so the caller's local slot stays stale.
        // Capture the caller frame's code before the dispatch (which clobbers
        // `current_code`) and reconcile after, mirroring `say`/`note`'s `.gist`
        // closure handling. No-op when the coercion ran no user method (pending
        // list stays empty).
        let caller_code = self.current_code;
        let r = loan_env!(self, coerce_infix_operand_numeric(value));
        self.reconcile_caller_after_internal_dispatch(caller_code);
        r
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

    /// Like [`coerce_numeric_bridge_pair`], but additionally raises
    /// X::Str::Numeric when either operand is a non-numeric string. Used by the
    /// genuinely-numeric operators (`+ - * / % **`, `== != < > <= >= <=>`); the
    /// generic comparators (`cmp`, `before`/`after`) use the plain bridge so they
    /// keep comparing strings as strings.
    pub(super) fn coerce_numeric_bridge_pair_strict(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<(Value, Value), RuntimeError> {
        crate::runtime::utils::check_str_numeric(&left)?;
        crate::runtime::utils::check_str_numeric(&right)?;
        self.coerce_numeric_bridge_pair(left, right)
    }

    /// Evaluate truthiness of a value, including dispatch to user-defined Bool methods.
    /// For Package (type objects) and Instance values, checks if the class defines
    /// a custom Bool method and calls it. Falls back to Value::truthy() otherwise.
    pub(super) fn eval_truthy(&mut self, val: &Value) -> bool {
        match val {
            Value::Package(name) => {
                let class_name = name.resolve().to_string();
                if loan_env!(self, resolve_method_with_owner(&class_name, "Bool", &[])).is_some() {
                    // Slice F: a user `Bool` method run by this internal coercion
                    // can mutate a captured-outer caller lexical; drain its
                    // writeback to the caller's slot (see coerce_numeric_bridge_value).
                    let caller_code = self.current_code;
                    let result = self.try_compiled_method_or_interpret(val.clone(), "Bool", vec![]);
                    self.reconcile_caller_after_internal_dispatch(caller_code);
                    if let Ok(result) = result {
                        return result.truthy();
                    }
                }
                val.truthy()
            }
            Value::Instance { class_name, .. } => {
                let cn = class_name.resolve().to_string();
                if loan_env!(self, resolve_method_with_owner(&cn, "Bool", &[])).is_some() {
                    let caller_code = self.current_code;
                    let result = self.try_compiled_method_or_interpret(val.clone(), "Bool", vec![]);
                    self.reconcile_caller_after_internal_dispatch(caller_code);
                    if let Ok(result) = result {
                        return result.truthy();
                    }
                }
                val.truthy()
            }
            Value::Regex(_)
            | Value::RegexWithAdverbs { .. }
            | Value::Routine { is_regex: true, .. } => {
                let topic = self.env().get("_").cloned().unwrap_or(Value::Nil);
                self.vm_smart_match(&topic, val)
            }
            _ => val.truthy(),
        }
    }

    /// Call a plain `Value::Sub` map block, optionally with an explicit topic
    /// (Pair elements) and/or rw-topic capture (`$_`-mutating blocks).
    ///
    /// Used by the native `.map` loop (see [`Self::call_compiled_closure_with_topic`]).
    /// The block is always a plain `Sub` here (the native map path rejects
    /// assuming/compose/Routine wrappers), so only the two `Sub` fast-paths of
    /// [`Self::vm_call_on_value`] are needed. When `capture_rw_topic` is set the
    /// block's final `$_` lands in `self.rw_map_topic_capture`.
    pub(super) fn vm_call_map_block(
        &mut self,
        block: &Value,
        args: Vec<Value>,
        explicit_topic: Option<Value>,
        capture_rw_topic: bool,
    ) -> Result<Value, RuntimeError> {
        let Value::Sub(data) = block else {
            return self.vm_call_on_value(block.clone(), args, None);
        };
        let empty_fns = HashMap::new();
        if let Some(cc) = &data.compiled_code {
            let cc = cc.clone();
            let data = data.clone();
            return self.call_compiled_closure_with_topic(
                &data,
                &cc,
                args,
                explicit_topic,
                capture_rw_topic,
                &empty_fns,
            );
        }
        // Sub without compiled_code: compile on-the-fly (mirrors vm_call_on_value).
        let cc = {
            let mut compiler = crate::compiler::Compiler::new();
            compiler.compile_routine_closure_body(&data.params, &data.param_defs, &data.body)
        };
        let data = data.clone();
        self.call_compiled_closure_with_topic(
            &data,
            &cc,
            args,
            explicit_topic,
            capture_rw_topic,
            &empty_fns,
        )
    }

    /// Interpreter-native dispatch for calling a value (Sub, Routine, Junction, etc.).
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

        // A WalkList is invoked (`$x.WALK(...)()`) by calling each candidate on
        // the original invocant, forwarding any arguments.
        if let Value::Instance { class_name, .. } = &target
            && class_name.resolve() == "WalkList"
        {
            return self.walk_list_invoke_direct(&target, args);
        }

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

        // Sub without compiled_code: compile on-the-fly then dispatch via Interpreter
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

        // Routine value dispatch (ledger §2, ③ PR-1). Resolve to a function name
        // and route through the Interpreter's unified compiled-first entry
        // (`call_function_compiled_first`): user-defined subs/multi/proto run as
        // compiled bytecode, native builtins fall through to `native_function`, and
        // only genuine carriers (EVAL/pseudo-package) reach the interpreter terminal.
        // This replaces the raw `interpreter.call_function` fallbacks; builtin
        // priority is preserved because a bare builtin Routine (e.g. `&SETTING::not`
        // -> Routine{GLOBAL, "not"}) is not a declared user function, so it skips the
        // `has_function` branches and resolves natively in compiled-first.
        if let Value::Routine { package, name, .. } = &target {
            let pkg = package.resolve();
            let name_str = name.resolve();
            let empty_fns = HashMap::new();
            let fns = compiled_fns.unwrap_or(&empty_fns);
            if !pkg.is_empty() && pkg != "GLOBAL" {
                let fq = format!("{pkg}::{name_str}");
                if self.has_function(&fq) {
                    return self.call_function_compiled_first(&fq, args, fns);
                }
            }
            if self.has_function(&name_str)
                || self.has_proto(&name_str)
                || self.has_multi_candidates(&name_str)
            {
                // A Routine whose name is also a builtin (e.g. `&SETTING::...::not`
                // resolves to Routine{GLOBAL, "not"}, accessors.rs) intentionally
                // refers to the builtin, not a user sub that shadows the name. Keep
                // builtin priority via `call_function` for those (a plain user `&not`
                // is a `Value::Sub` and never reaches this Routine branch). Otherwise
                // route user subs/multi/proto through compiled-first.
                if crate::runtime::Interpreter::is_builtin_function(&name_str) {
                    return self.vm_call_function(&name_str, args);
                }
                return self.call_function_compiled_first(&name_str, args, fns);
            }
            // Method dispatch fallback for &?ROUTINE.dispatcher()(self, ...)
            // Only use this when the package is a known class.
            if !args.is_empty() && !pkg.is_empty() && pkg != "GLOBAL" && self.has_class(&pkg) {
                let invocant = args[0].clone();
                let method_args = args[1..].to_vec();
                // Route through the Interpreter's unified compiled-first dispatch (ledger §1):
                // user-defined methods run as compiled bytecode, native fall back.
                return self.try_compiled_method_or_interpret(invocant, &name_str, method_args);
            }
            return self.call_function_compiled_first(&name_str, args, fns);
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
                    && self.role_has_method(role_name, "CALL-ME")
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
            return self.vm_call_sub_value(target, args, true);
        }

        // Any remaining value (Int, Str, Num, ...) is not Callable. Invoking it
        // with `()` resolves the postfix-call to a `CALL-ME` method, which these
        // types do not provide, so this raises X::Method::NotFound (method
        // 'CALL-ME', typename = the value's type) — matching Raku.
        self.try_compiled_method_or_interpret(target, "CALL-ME", args)
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
        let result = self.call_sub_value(thunk_data.thunk.clone(), vec![], true)?;
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
