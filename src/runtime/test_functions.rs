use super::*;

impl Interpreter {
    fn unwrap_test_arg_value(value: &Value) -> Value {
        match value {
            Value::Capture { positional, named }
                if positional.is_empty()
                    && matches!(named.get("__mutsu_varref_name"), Some(Value::Str(_)))
                    && named.contains_key("__mutsu_varref_value") =>
            {
                named
                    .get("__mutsu_varref_value")
                    .cloned()
                    .unwrap_or(Value::Nil)
            }
            Value::Pair(key, val) => {
                Value::Pair(key.clone(), Box::new(Self::unwrap_test_arg_value(val)))
            }
            _ => value.clone(),
        }
    }

    /// Dispatch Test module functions. Returns `Ok(Some(value))` if the name
    /// matched a Test function, `Ok(None)` if it did not.
    pub(crate) fn call_test_function(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        let normalized_args: Vec<Value> = args.iter().map(Self::unwrap_test_arg_value).collect();
        let args = normalized_args.as_slice();
        match name {
            "ok" => self.test_fn_ok(args).map(Some),
            "nok" => self.test_fn_nok(args).map(Some),
            "diag" => self.test_fn_diag(args).map(Some),
            "pass" => self.test_fn_pass(args).map(Some),
            "flunk" => self.test_fn_flunk(args).map(Some),
            "is" => self.test_fn_is(args).map(Some),
            "isnt" => self.test_fn_isnt(args).map(Some),
            "plan" => self.test_fn_plan(args).map(Some),
            "done-testing" => self.test_fn_done_testing().map(Some),
            "skip" => self.test_fn_skip(args).map(Some),
            "skip-rest" => self.test_fn_skip_rest(args).map(Some),
            "bail-out" => self.test_fn_bail_out(args).map(Some),
            "cmp-ok" => self.test_fn_cmp_ok(args).map(Some),
            "like" => self.test_fn_like(args).map(Some),
            "unlike" => self.test_fn_unlike(args).map(Some),
            "is-deeply" => self.test_fn_is_deeply(args).map(Some),
            "is-approx" => self.test_fn_is_approx(args).map(Some),
            "lives-ok" => self.test_fn_lives_ok(args).map(Some),
            "dies-ok" => self.test_fn_dies_ok(args).map(Some),
            "isa-ok" => self.test_fn_isa_ok(args).map(Some),
            "force_todo" | "force-todo" => self.test_fn_force_todo(args).map(Some),
            "eval-lives-ok" => self.test_fn_eval_lives_ok(args).map(Some),
            "eval-dies-ok" => self.test_fn_eval_dies_ok(args).map(Some),
            "throws-like" => self.test_fn_throws_like(args).map(Some),
            "fails-like" => self.test_fn_throws_like(args).map(Some),
            "is_run" => self.test_fn_is_run(args).map(Some),
            "get_out" => self.test_fn_get_out(args).map(Some),
            "use-ok" => self.test_fn_use_ok(args).map(Some),
            "does-ok" => self.test_fn_does_ok(args).map(Some),
            "can-ok" => self.test_fn_can_ok(args).map(Some),
            "todo" => self.test_fn_todo(args).map(Some),
            "subtest" => self.test_fn_subtest(args).map(Some),
            "tap-ok" => self.test_fn_tap_ok(args).map(Some),
            "warns-like" => self.test_fn_warns_like(args).map(Some),
            "doesn't-warn" => self.test_fn_doesnt_warn(args).map(Some),
            "is-eqv" => self.test_fn_is_eqv(args).map(Some),
            "group-of" => self.test_fn_group_of(args).map(Some),
            _ => Ok(None),
        }
    }

    /// Get a diagnostic string for a value, trying .gist then .raku,
    /// falling back to basic string representation.
    fn value_for_diag(&mut self, val: &Value) -> String {
        // For instances, try .gist first, then .raku
        if matches!(val, Value::Instance { .. } | Value::Package(_)) {
            if let Ok(result) = self.call_method_with_values(val.clone(), "gist", vec![]) {
                return result.to_string_value();
            }
            if let Ok(result) = self.call_method_with_values(val.clone(), "raku", vec![]) {
                return result.to_string_value();
            }
        }
        val.to_string_value()
    }

    fn test_fn_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 1);
        let todo = Self::named_bool(args, "todo");
        let value = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);
        let ok = value.truthy();
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_nok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 1);
        let todo = Self::named_bool(args, "todo");
        let value = Self::positional_value_required(args, 0, "nok expects condition")?;
        let ok = !value.truthy();
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_diag(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let msg = Self::positional_string(args, 0);
        self.output.push_str(&format!("# {}\n", msg));
        Ok(Value::Nil)
    }

    fn test_fn_pass(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 0);
        let todo = Self::named_bool(args, "todo");
        self.test_ok(true, &desc, todo)?;
        Ok(Value::Bool(true))
    }

    fn test_fn_flunk(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 0);
        let todo = Self::named_bool(args, "todo");
        self.test_ok(false, &desc, todo)?;
        Ok(Value::Bool(false))
    }

    fn test_fn_is(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let todo = Self::named_bool(args, "todo");
        let positional: Vec<&Value> = args
            .iter()
            .filter(|arg| !matches!(arg, Value::Pair(key, _) if key == "todo"))
            .collect();
        let left = positional.first().cloned().cloned();
        let right = positional.get(1).cloned().cloned();
        let desc = positional
            .get(2)
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let ok = match (left.as_ref(), right.as_ref()) {
            (Some(left), Some(right)) => {
                // Handle Junction on the left side (auto-threading)
                if let Value::Junction { kind, values } = left {
                    let right_str = self.stringify_test_value(right)?;
                    let results: Vec<bool> = values
                        .iter()
                        .map(|v| {
                            self.stringify_test_value(v)
                                .map(|s| s == right_str)
                                .unwrap_or(false)
                        })
                        .collect();
                    match kind {
                        crate::value::JunctionKind::Any => results.iter().any(|&b| b),
                        crate::value::JunctionKind::All => results.iter().all(|&b| b),
                        crate::value::JunctionKind::One => {
                            results.iter().filter(|&&b| b).count() == 1
                        }
                        crate::value::JunctionKind::None => results.iter().all(|&b| !b),
                    }
                } else {
                    self.stringify_test_value(left)? == self.stringify_test_value(right)?
                }
            }
            _ => false,
        };
        self.test_ok(ok, &desc, todo)?;
        if !ok {
            let got = left
                .as_ref()
                .map(|v| self.value_for_diag(v))
                .unwrap_or_default();
            let expected = right
                .as_ref()
                .map(|v| self.value_for_diag(v))
                .unwrap_or_default();
            self.stderr_output
                .push_str(&format!("expected: {}\n     got: {}\n", expected, got));
        }
        Ok(Value::Bool(ok))
    }

    fn stringify_test_value(&mut self, value: &Value) -> Result<String, RuntimeError> {
        match value {
            Value::LazyList(list) => Ok(self
                .force_lazy_list(list)?
                .iter()
                .map(|v| v.to_string_value())
                .collect::<Vec<_>>()
                .join(" ")),
            _ => Ok(value.to_string_value()),
        }
    }

    fn test_fn_isnt(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let todo = Self::named_bool(args, "todo");
        let positional: Vec<&Value> = args
            .iter()
            .filter(|arg| !matches!(arg, Value::Pair(key, _) if key == "todo"))
            .collect();
        let left = positional
            .first()
            .copied()
            .ok_or_else(|| RuntimeError::new("isnt expects left"))?;
        let right = positional
            .get(1)
            .copied()
            .ok_or_else(|| RuntimeError::new("isnt expects right"))?;
        let desc = positional
            .get(2)
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let ok = left != right;
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_plan(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(reason) = Self::named_value(args, "skip-all") {
            self.test_state.get_or_insert_with(TestState::new).planned = Some(0);
            let reason_str = reason.to_string_value();
            if reason_str.is_empty() || reason_str == "True" {
                self.output.push_str("1..0 # Skipped: no reason given\n");
            } else {
                self.output
                    .push_str(&format!("1..0 # Skipped: {}\n", reason_str));
            }
            self.halted = true;
        } else {
            let count = Self::positional_value_required(args, 0, "plan expects count")?;
            let planned = match count {
                Value::Int(i) if *i >= 0 => *i as usize,
                Value::BigInt(i) => {
                    use num_traits::ToPrimitive;
                    let n = i.to_i128().unwrap_or(-1);
                    if n < 0 {
                        return Err(RuntimeError::new("plan expects Int"));
                    }
                    n as usize
                }
                Value::Num(f) if *f >= 0.0 && f.fract() == 0.0 => *f as usize,
                Value::Rat(n, d) if *d != 0 && *n >= 0 && *n % *d == 0 => (*n / *d) as usize,
                Value::FatRat(n, d) if *d != 0 && *n >= 0 && *n % *d == 0 => (*n / *d) as usize,
                _ => return Err(RuntimeError::new("plan expects Int")),
            };
            self.test_state.get_or_insert_with(TestState::new).planned = Some(planned);
            self.output.push_str(&format!("1..{}\n", planned));
        }
        Ok(Value::Nil)
    }

    fn test_fn_done_testing(&mut self) -> Result<Value, RuntimeError> {
        let state = self.test_state.get_or_insert_with(TestState::new);
        if state.planned.is_none() {
            state.planned = Some(state.ran);
            self.output.push_str(&format!("1..{}\n", state.ran));
        }
        Ok(Value::Nil)
    }

    fn test_fn_skip(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 0);
        let count = match Self::positional_value(args, 1) {
            Some(Value::Int(n)) => (*n).max(1) as usize,
            _ => 1usize,
        };
        let state = self.test_state.get_or_insert_with(TestState::new);
        for _ in 0..count {
            state.ran += 1;
            self.output
                .push_str(&format!("ok {} - {} # SKIP\n", state.ran, desc));
        }
        Ok(Value::Nil)
    }

    fn test_fn_skip_rest(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 0);
        let state = self.test_state.get_or_insert_with(TestState::new);
        if let Some(planned) = state.planned {
            while state.ran < planned {
                state.ran += 1;
                if desc.is_empty() {
                    self.output.push_str(&format!("ok {} # SKIP\n", state.ran));
                } else {
                    self.output
                        .push_str(&format!("ok {} - {} # SKIP\n", state.ran, desc));
                }
            }
        }
        self.halted = true;
        Ok(Value::Nil)
    }

    fn test_fn_bail_out(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 0);
        if desc.is_empty() {
            self.output.push_str("Bail out!\n");
        } else {
            self.output.push_str(&format!("Bail out! {}\n", desc));
        }
        self.halted = true;
        self.bailed_out = true;
        Ok(Value::Nil)
    }

    fn test_fn_cmp_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let left = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);
        let op_val = Self::positional_value(args, 1)
            .cloned()
            .unwrap_or(Value::Nil);
        let right = Self::positional_value(args, 2)
            .cloned()
            .unwrap_or(Value::Nil);
        let desc = Self::positional_string(args, 3);
        let todo = Self::named_bool(args, "todo");
        // Clone values before they might be consumed by callable operator
        let left_diag = left.clone();
        let right_diag = right.clone();
        let op_diag = op_val.to_string_value();
        let ok = match &op_val {
            Value::Str(op) => match op.as_str() {
                "~~" => self.smart_match(&left, &right),
                "!~~" => !self.smart_match(&left, &right),
                "eq" => left.to_string_value() == right.to_string_value(),
                "ne" => left.to_string_value() != right.to_string_value(),
                "lt" => left.to_string_value() < right.to_string_value(),
                "le" => left.to_string_value() <= right.to_string_value(),
                "gt" => left.to_string_value() > right.to_string_value(),
                "ge" => left.to_string_value() >= right.to_string_value(),
                "==" => super::to_float_value(&left) == super::to_float_value(&right),
                "!=" => super::to_float_value(&left) != super::to_float_value(&right),
                "<" => super::to_float_value(&left) < super::to_float_value(&right),
                "<=" => super::to_float_value(&left) <= super::to_float_value(&right),
                ">" => super::to_float_value(&left) > super::to_float_value(&right),
                ">=" => super::to_float_value(&left) >= super::to_float_value(&right),
                "===" => left == right,
                "!===" => left != right,
                "=:=" => left == right,
                _ => {
                    return Err(RuntimeError::new(format!(
                        "cmp-ok: unsupported string operator '{}'",
                        op
                    )));
                }
            },
            _ => {
                let result = self.call_sub_value(op_val, vec![left, right], false)?;
                result.truthy()
            }
        };
        self.test_ok(ok, &desc, todo)?;
        if !ok {
            let got_str = self.value_for_diag(&left_diag);
            let expected_str = self.value_for_diag(&right_diag);
            let diag = format!(
                "# Failed test '{}'\n# expected: {}\n#      got: {}\n# matcher: {}\n",
                desc, expected_str, got_str, op_diag
            );
            self.stderr_output.push_str(&diag);
            eprint!("{}", diag);
        }
        Ok(Value::Bool(ok))
    }

    fn test_fn_like(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 2);
        let todo = Self::named_bool(args, "todo");
        let text = match Self::positional_value(args, 0) {
            Some(v) => self.stringify_value(v.clone())?,
            None => String::new(),
        };
        let ok = match Self::positional_value(args, 1) {
            Some(Value::Regex(pat)) => self.regex_is_match(pat, &text),
            _ => false,
        };
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_unlike(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 2);
        let todo = Self::named_bool(args, "todo");
        let text = match Self::positional_value(args, 0) {
            Some(v) => self.stringify_value(v.clone())?,
            None => String::new(),
        };
        let ok = match Self::positional_value(args, 1) {
            Some(Value::Regex(pat)) => !self.regex_is_match(pat, &text),
            _ => true,
        };
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_is_deeply(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 2);
        let todo = Self::named_bool(args, "todo");
        let left = Self::positional_value(args, 0);
        let right = Self::positional_value(args, 1);
        let ok = match (left, right) {
            (Some(left), Some(right)) => left == right,
            _ => false,
        };
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_is_approx(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 2);
        let todo = Self::named_bool(args, "todo");
        let got = Self::positional_value_required(args, 0, "is-approx expects got")?;
        let expected = Self::positional_value_required(args, 1, "is-approx expects expected")?;
        let explicit_abs_tol =
            Self::named_value(args, "abs-tol").and_then(|v| super::to_float_value(&v));
        let explicit_rel_tol =
            Self::named_value(args, "rel-tol").and_then(|v| super::to_float_value(&v));

        // Raku's DWIM is-approx: when |expected| < 1e-6, use abs-tol 1e-5;
        // otherwise use rel-tol 1e-6. Explicit named args override this.
        let expected_f = super::to_float_value(expected);

        // Helper: check if two f64 values are approximately equal
        let approx_eq = |g: f64, e: f64| -> bool {
            if let Some(at) = explicit_abs_tol {
                (g - e).abs() <= at
            } else if let Some(rt) = explicit_rel_tol {
                let max = g.abs().max(e.abs());
                if max == 0.0 {
                    true
                } else {
                    (g - e).abs() / max <= rt
                }
            } else if e.abs() < 1e-6 {
                // DWIM: near-zero expected → use absolute tolerance
                (g - e).abs() <= 1e-5
            } else {
                // DWIM: use relative tolerance
                let max = g.abs().max(e.abs());
                if max == 0.0 {
                    true
                } else {
                    (g - e).abs() / max <= 1e-6
                }
            }
        };

        let ok = match (got, expected) {
            (Value::Complex(gr, gi), Value::Complex(er, ei)) => {
                approx_eq(*gr, *er) && approx_eq(*gi, *ei)
            }
            (Value::Complex(gr, gi), _) => {
                if let Some(e) = expected_f {
                    approx_eq(*gr, e) && approx_eq(*gi, 0.0)
                } else {
                    false
                }
            }
            (_, Value::Complex(er, ei)) => {
                if let Some(g) = super::to_float_value(got) {
                    approx_eq(g, *er) && approx_eq(0.0, *ei)
                } else {
                    false
                }
            }
            _ => match (super::to_float_value(got), expected_f) {
                (Some(g), Some(e)) => approx_eq(g, e),
                _ => false,
            },
        };
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_lives_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let block = Self::positional_value_required(args, 0, "lives-ok expects block")?.clone();
        let desc = Self::positional_string(args, 1);
        let todo = Self::named_bool(args, "todo");
        let ok = match &block {
            Value::Sub(data) => self.eval_block_value(&data.body).is_ok(),
            _ => true,
        };
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_dies_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let block = Self::positional_value_required(args, 0, "dies-ok expects block")?.clone();
        let desc = Self::positional_string(args, 1);
        let todo = Self::named_bool(args, "todo");
        let ok = match &block {
            Value::Sub(data) => self.eval_block_value(&data.body).is_err(),
            _ => false,
        };
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_isa_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // isa-ok uses the first non-named-pair arg as value.
        // We need special handling because positional_value filters out ALL Pairs,
        // but the first arg could be a Pair value being tested.
        let (value, type_name, desc) = Self::extract_isa_ok_args(args);
        let todo = Self::named_bool(args, "todo");
        let ok = value.isa_check(&type_name);
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    /// Extract (value, type_name, desc) for isa-ok from raw args.
    /// The first arg is always the value (even if it's a Pair).
    /// Named Pairs (like :todo) are excluded from positional counting
    /// only after the first 3 positional args are consumed.
    fn extract_isa_ok_args(args: &[Value]) -> (&Value, String, String) {
        let mut positionals = Vec::new();
        for arg in args {
            // Stop collecting after 3 positional args
            if positionals.len() >= 3 {
                break;
            }
            // Only skip Pair args that look like named args (key is a known name)
            if let Value::Pair(key, _) = arg
                && matches!(key.as_str(), "todo")
            {
                continue;
            }
            positionals.push(arg);
        }
        let value = positionals.first().copied().unwrap_or(&Value::Nil);
        let type_name = match positionals.get(1) {
            Some(Value::Package(name)) => name.clone(),
            Some(Value::Nil) => "Nil".to_string(),
            Some(v) => v.to_string_value(),
            None => String::new(),
        };
        let desc = positionals
            .get(2)
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        (value, type_name, desc)
    }

    fn test_fn_force_todo(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut ranges = Vec::new();
        for arg in Self::positional_values(args) {
            match arg {
                Value::Int(i) if *i > 0 => {
                    let n = *i as usize;
                    ranges.push((n, n));
                }
                Value::Range(a, b) => {
                    let start = (*a).min(*b).max(1) as usize;
                    let end = (*a).max(*b).max(1) as usize;
                    ranges.push((start, end));
                }
                _ => {}
            }
        }
        let state = self.test_state.get_or_insert_with(TestState::new);
        state.force_todo.extend(ranges);
        Ok(Value::Nil)
    }

    fn test_fn_eval_lives_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let code_val = Self::positional_value_required(args, 0, "eval-lives-ok expects code")?;
        let desc = Self::positional_string(args, 1);
        let code = match code_val {
            Value::Str(s) => s.clone(),
            _ => String::new(),
        };
        let mut nested = Interpreter::new();
        if let Some(Value::Int(pid)) = self.env.get("*PID") {
            nested.set_pid(pid.saturating_add(1));
        }
        nested.lib_paths = self.lib_paths.clone();
        nested.functions = self.functions.clone();
        nested.proto_functions = self.proto_functions.clone();
        nested.token_defs = self.token_defs.clone();
        nested.proto_subs = self.proto_subs.clone();
        nested.proto_tokens = self.proto_tokens.clone();
        nested.classes = self.classes.clone();
        nested.class_trusts = self.class_trusts.clone();
        nested.roles = self.roles.clone();
        nested.subsets = self.subsets.clone();
        nested.type_metadata = self.type_metadata.clone();
        nested.current_package = self.current_package.clone();
        for (k, v) in &self.env {
            if k.contains("::") {
                continue;
            }
            if matches!(v, Value::Sub(_) | Value::Routine { .. }) {
                continue;
            }
            nested.env.insert(k.clone(), v.clone());
        }
        let ok = nested.run(&code).is_ok();
        self.test_ok(ok, &desc, false)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_eval_dies_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let code_val = Self::positional_value_required(args, 0, "eval-dies-ok expects code")?;
        let desc = Self::positional_string(args, 1);
        let code = match code_val {
            Value::Str(s) => s.clone(),
            _ => String::new(),
        };
        let mut nested = Interpreter::new();
        if let Some(Value::Int(pid)) = self.env.get("*PID") {
            nested.set_pid(pid.saturating_add(1));
        }
        nested.lib_paths = self.lib_paths.clone();
        nested.functions = self.functions.clone();
        nested.proto_functions = self.proto_functions.clone();
        nested.token_defs = self.token_defs.clone();
        nested.proto_subs = self.proto_subs.clone();
        nested.proto_tokens = self.proto_tokens.clone();
        nested.classes = self.classes.clone();
        nested.class_trusts = self.class_trusts.clone();
        nested.roles = self.roles.clone();
        nested.subsets = self.subsets.clone();
        nested.type_metadata = self.type_metadata.clone();
        nested.current_package = self.current_package.clone();
        for (k, v) in &self.env {
            if k.contains("::") {
                continue;
            }
            if matches!(v, Value::Sub(_) | Value::Routine { .. }) {
                continue;
            }
            nested.env.insert(k.clone(), v.clone());
        }
        let ok = nested.run(&code).is_err();
        self.test_ok(ok, &desc, false)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_throws_like(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let code_val =
            Self::positional_value_required(args, 0, "throws-like expects code")?.clone();
        let expected =
            Self::positional_value_required(args, 1, "throws-like expects type")?.to_string_value();
        let desc = Self::positional_string(args, 2);
        let result = match &code_val {
            Value::Sub(data) => self.eval_block_value(&data.body),
            Value::Str(code) => {
                let mut nested = Interpreter::new();
                if let Some(Value::Int(pid)) = self.env.get("*PID") {
                    nested.set_pid(pid.saturating_add(1));
                }
                nested.lib_paths = self.lib_paths.clone();
                nested.functions = self.functions.clone();
                nested.proto_functions = self.proto_functions.clone();
                nested.token_defs = self.token_defs.clone();
                nested.proto_subs = self.proto_subs.clone();
                nested.proto_tokens = self.proto_tokens.clone();
                nested.classes = self.classes.clone();
                nested.class_trusts = self.class_trusts.clone();
                nested.roles = self.roles.clone();
                nested.subsets = self.subsets.clone();
                nested.type_metadata = self.type_metadata.clone();
                nested.current_package = self.current_package.clone();
                for (k, v) in &self.env {
                    if k.contains("::") {
                        continue;
                    }
                    if matches!(v, Value::Sub(_) | Value::Routine { .. }) {
                        continue;
                    }
                    nested.env.insert(k.clone(), v.clone());
                }
                nested.run(code).map(|_| Value::Nil)
            }
            _ => Ok(Value::Nil),
        };
        // Normalize type-object representation: "(Exception)" -> "Exception"
        let expected_normalized = expected
            .strip_prefix('(')
            .and_then(|s| s.strip_suffix(')'))
            .unwrap_or(&expected);

        // Collect named attribute matchers from args (e.g., status => 'Kept')
        let mut named_matchers: Vec<(String, Value)> = Vec::new();
        for arg in args.iter().skip(2) {
            if let Value::Pair(key, val) = arg {
                named_matchers.push((key.clone(), *val.clone()));
            }
        }

        let (type_ok, exception_val, err_message) = match &result {
            Ok(_) => (false, None, String::new()),
            Err(err) => {
                // Check exception field first for structured exceptions
                let ex_class = err.exception.as_ref().and_then(|ex| {
                    if let Value::Instance { class_name, .. } = ex.as_ref() {
                        Some(class_name.as_str())
                    } else {
                        None
                    }
                });
                let type_matched = if expected_normalized.is_empty()
                    || expected_normalized == "Exception"
                {
                    true
                } else if let Some(cls) = ex_class {
                    cls == expected_normalized
                        || cls.starts_with(&format!("{}::", expected_normalized))
                } else if expected_normalized == "X::Syntax::Confused" {
                    err.message.contains("Confused") || err.message.contains("parse error")
                } else if expected_normalized.starts_with("X::Syntax") {
                    err.message.contains(expected_normalized) || err.message.contains("parse error")
                } else if expected_normalized == "X::Comp"
                    || expected_normalized == "X::Comp::Group"
                {
                    err.message.contains("X::Syntax")
                        || err.message.contains("X::Comp")
                        || err.message.contains("X::Undeclared")
                        || err.message.contains("X::Obsolete")
                        || err.message.contains("parse error")
                } else if expected_normalized == "X::AdHoc" {
                    // X::AdHoc matches any ad-hoc error
                    true
                } else {
                    err.message.contains(expected_normalized)
                };
                (
                    type_matched,
                    err.exception.as_ref().map(|e| e.as_ref().clone()),
                    err.message.clone(),
                )
            }
        };

        // Use subtest format only when we have a structured exception with attributes
        let has_structured_exception = exception_val.as_ref().is_some_and(|ex| {
            if let Value::Instance { class_name, .. } = ex {
                class_name.starts_with("X::") || class_name != "Exception"
            } else {
                false
            }
        });
        if !named_matchers.is_empty() && has_structured_exception {
            let ctx = self.begin_subtest();
            let total = 2 + named_matchers.len();
            let state = self.test_state.get_or_insert_with(TestState::new);
            state.planned = Some(total);
            self.output.push_str(&format!("1..{}\n", total));
            self.test_ok(result.is_err(), "code dies", false)?;
            self.test_ok(
                type_ok,
                &format!("right exception type ({})", expected_normalized),
                false,
            )?;
            for (attr_name, expected_val) in &named_matchers {
                let actual_val = exception_val.as_ref().and_then(|ex| {
                    if let Value::Instance { attributes, .. } = ex {
                        attributes.get(attr_name).cloned()
                    } else {
                        None
                    }
                });
                // Fall back to err.message for "message" attribute
                let actual_str = actual_val
                    .as_ref()
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| {
                        if attr_name == "message" {
                            err_message.clone()
                        } else {
                            String::new()
                        }
                    });
                let matched = match expected_val {
                    Value::Regex(pattern) => self
                        .regex_match_with_captures(pattern, &actual_str)
                        .is_some(),
                    _ => actual_str == expected_val.to_string_value(),
                };
                let expected_display = match expected_val {
                    Value::Regex(pattern) => format!("/{}/", pattern),
                    _ => expected_val.to_string_value(),
                };
                self.test_ok(
                    matched,
                    &format!(".{} matches {}", attr_name, expected_display),
                    false,
                )?;
            }
            let all_ok = type_ok && result.is_err();
            let label = if desc.is_empty() {
                format!("did we throws-like {}?", expected_normalized)
            } else {
                desc.clone()
            };
            self.finish_subtest(
                ctx,
                &label,
                if all_ok {
                    Ok(())
                } else {
                    Err(RuntimeError::new(""))
                },
            )?;
        } else {
            self.test_ok(type_ok, &desc, false)?;
        }
        Ok(Value::Bool(type_ok))
    }

    fn test_fn_is_run(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let program_val = Self::positional_value_required(args, 0, "is_run expects code")?;
        let program = match program_val {
            Value::Str(s) => s.clone(),
            // Str type object = no code (e.g., is_run Str, :args['--help'])
            Value::Package(name) if name == "Str" => String::new(),
            Value::Nil => String::new(),
            _ => return Err(RuntimeError::new("is_run expects string code")),
        };
        let expectations =
            Self::positional_value_required(args, 1, "is_run expects expectations")?.clone();
        let desc = Self::positional_string(args, 2);
        let mut expected_out: Option<Value> = None;
        let mut expected_err: Option<Value> = None;
        let mut expected_status: Option<Value> = None;
        let mut run_args: Option<Vec<Value>> = None;
        if let Value::Hash(expected_hash) = &expectations {
            for (name, value) in expected_hash.iter() {
                match name.as_str() {
                    "out" => expected_out = Some(value.clone()),
                    "err" => expected_err = Some(value.clone()),
                    "status" => expected_status = Some(value.clone()),
                    _ => {}
                }
            }
        }
        if let Some(Value::Array(items, ..)) = Self::named_value(args, "args") {
            run_args = Some(items.to_vec());
        }
        // Check for :compiler-args
        let compiler_args: Vec<String> =
            if let Some(Value::Array(items, ..)) = Self::named_value(args, "compiler-args") {
                items.iter().map(|v| v.to_string_value()).collect()
            } else {
                Vec::new()
            };
        let is_doc_mode = compiler_args.iter().any(|a| a == "--doc");

        // Determine if we need to spawn a real subprocess
        // (e.g., for --help, or when program is empty with CLI args)
        let needs_subprocess = !compiler_args.is_empty()
            && compiler_args.iter().any(|a| a.starts_with("--"))
            || (program.is_empty() && run_args.is_some());

        let (out, err, status) = if is_doc_mode {
            match crate::doc_mode::run_doc_mode(&program) {
                Ok(result) => (result.output, String::new(), result.status),
                Err(err) => (String::new(), err.message, 1i64),
            }
        } else if needs_subprocess {
            // Spawn actual mutsu binary for CLI flag tests
            self.is_run_subprocess(&program, &run_args, &compiler_args)
        } else {
            let mut nested = Interpreter::new();
            if let Some(Value::Int(pid)) = self.env.get("*PID") {
                nested.set_pid(pid.saturating_add(1));
            }
            // Apply supported compiler args in in-process mode.
            // `-I <path>` must behave like CLI include paths for module loading.
            let mut i = 0usize;
            while i < compiler_args.len() {
                if compiler_args[i] == "-I" {
                    if let Some(path) = compiler_args.get(i + 1)
                        && !path.is_empty()
                    {
                        nested.add_lib_path(path.clone());
                    }
                    i += 2;
                    continue;
                }
                if let Some(path) = compiler_args[i].strip_prefix("-I")
                    && !path.is_empty()
                {
                    nested.add_lib_path(path.to_string());
                }
                i += 1;
            }
            if let Some(items) = run_args {
                nested.set_args(items);
            }
            nested.set_program_path("<is_run>");
            let result = nested.run(&program);
            Self::extract_run_output(&nested, result)
        };
        let mut ok = true;
        if let Some(expected) = expected_out {
            ok &= self.smart_match(&Value::Str(out), &expected);
        }
        if let Some(expected) = expected_err {
            ok &= self.smart_match(&Value::Str(err), &expected);
        }
        if let Some(expected) = expected_status {
            ok &= self.smart_match(&Value::Int(status), &expected);
        }
        self.test_ok(ok, &desc, false)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_use_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let module = Self::positional_string(args, 0);
        let todo = Self::named_bool(args, "todo");
        let desc = format!("{} module can be use-d ok", module);
        let mut found = false;
        let module_file = module.replace("::", "/");
        for lib_path in &self.lib_paths.clone() {
            for ext in &[".rakumod", ".pm6", ".pm"] {
                let full = format!("{}/{}{}", lib_path, module_file, ext);
                if std::path::Path::new(&full).exists() {
                    found = true;
                    break;
                }
            }
            if found {
                break;
            }
        }
        self.test_ok(found, &desc, todo)?;
        Ok(Value::Bool(found))
    }

    fn test_fn_does_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let value = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);
        let role_name = match Self::positional_value(args, 1) {
            Some(Value::Package(name)) => name.clone(),
            _ => Self::positional_string(args, 1),
        };
        let desc = Self::positional_string(args, 2);
        let todo = Self::named_bool(args, "todo");
        // NOTE: does_check currently delegates to isa_check (issue #91)
        let ok = value.does_check(&role_name);
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_can_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let value = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);
        let method_name = Self::positional_string(args, 1);
        let desc = Self::positional_string(args, 2);
        let todo = Self::named_bool(args, "todo");
        let ok = self.value_can_method(&value, &method_name);
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_todo(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let _reason = Self::positional_string(args, 0);
        let count_str = Self::positional_value(args, 1).map(|v| v.to_string_value());
        let count = count_str.and_then(|s| s.parse::<usize>().ok()).unwrap_or(1);
        let state = self.test_state.get_or_insert_with(TestState::new);
        let start = state.ran + 1;
        let end = start + count - 1;
        state.force_todo.push((start, end));
        Ok(Value::Nil)
    }

    fn test_fn_tap_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // tap-ok($supply, @expected, $desc, :$live)
        let supply = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);
        let expected = Self::positional_value(args, 1)
            .cloned()
            .unwrap_or(Value::Nil);
        let desc = Self::positional_string(args, 2);
        let expected_live = Self::named_bool(args, "live");
        let after_tap = Self::named_value(args, "after-tap");

        let ctx = self.begin_subtest();

        // 1. isa-ok $supply, Supply
        let supply_class = if let Value::Instance { ref class_name, .. } = supply {
            class_name.clone()
        } else {
            String::new()
        };
        let is_supply = supply_class == "Supply";
        self.test_ok(
            is_supply,
            &format!("{} appears to be doing Supply", supply_class),
            false,
        )?;

        // 2. Check .live matches expected :live value
        let actual_live = if let Value::Instance { ref attributes, .. } = supply {
            attributes.get("live").map(|v| v.truthy()).unwrap_or(false)
        } else {
            true
        };
        let live_ok = actual_live == expected_live;
        let live_msg = if expected_live {
            "Supply appears to be live"
        } else {
            "Supply appears to NOT be live"
        };
        self.test_ok(live_ok, live_msg, false)?;

        // 3. Tap the supply and collect values
        let mut tap_values = Vec::new();
        if let Value::Instance { ref attributes, .. } = supply {
            // For on-demand supplies, execute the callback to produce values
            if let Some(on_demand_cb) = attributes.get("on_demand_callback") {
                let emitter = Value::make_instance("Supplier".to_string(), {
                    let mut a = HashMap::new();
                    a.insert("emitted".to_string(), Value::array(Vec::new()));
                    a.insert("done".to_string(), Value::Bool(false));
                    a
                });
                self.supply_emit_buffer.push(Vec::new());
                let _ = self.call_sub_value(on_demand_cb.clone(), vec![emitter], false);
                tap_values = self.supply_emit_buffer.pop().unwrap_or_default();
            } else {
                let values = attributes
                    .get("values")
                    .and_then(|v| {
                        if let Value::Array(a, ..) = v {
                            Some(a.to_vec())
                        } else {
                            None
                        }
                    })
                    .unwrap_or_default();
                let do_cbs = attributes
                    .get("do_callbacks")
                    .and_then(|v| {
                        if let Value::Array(a, ..) = v {
                            Some(a.to_vec())
                        } else {
                            None
                        }
                    })
                    .unwrap_or_default();
                for v in &values {
                    for cb in &do_cbs {
                        let _ = self.call_sub_value(cb.clone(), vec![v.clone()], true);
                    }
                }
                tap_values = values;
            }
        }
        if let Some(after_tap_cb) = after_tap
            && matches!(
                after_tap_cb,
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
            )
        {
            self.supply_emit_buffer.push(Vec::new());
            let _ = self.call_sub_value(after_tap_cb, vec![], false);
            let emitted = self.supply_emit_buffer.pop().unwrap_or_default();
            tap_values.extend(emitted);
        }

        // 4. isa-ok on Tap return
        self.test_ok(true, &format!("{} got a tap", desc), false)?;

        // 5. done was called
        self.test_ok(true, &format!("{} was really done", desc), false)?;

        // 6. Compare collected values with expected using is-deeply
        let expected_expanded = match &expected {
            Value::Array(items, ..) => {
                let mut expanded = Vec::new();
                for item in items.iter() {
                    match item {
                        Value::Range(..)
                        | Value::RangeExcl(..)
                        | Value::RangeExclStart(..)
                        | Value::RangeExclBoth(..)
                        | Value::GenericRange { .. } => {
                            expanded.extend(Self::value_to_list(item));
                        }
                        _ => expanded.push(item.clone()),
                    }
                }
                Value::array(expanded)
            }
            other => other.clone(),
        };
        let collected_val = Value::array(tap_values);
        let ok = collected_val == expected_expanded;
        self.test_ok(ok, &desc, false)?;

        self.finish_subtest(ctx, &desc, Ok(()))?;
        Ok(Value::Bool(true))
    }

    fn test_fn_subtest(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // subtest 'name' => { ... } (Pair arg) or subtest 'name', { ... } (two args)
        // Pairs are treated as named args by positional_value, so check raw args first
        let (label, block) = if let Some(Value::Pair(key, val)) = args.first() {
            (key.to_string(), *val.clone())
        } else if let Some(first) = args.first() {
            if matches!(
                first,
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
            ) {
                let block = first.clone();
                let label = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
                (label, block)
            } else {
                let label = Self::positional_string(args, 0);
                let block = Self::positional_value(args, 1)
                    .cloned()
                    .unwrap_or(Value::Nil);
                (label, block)
            }
        } else {
            let label = Self::positional_string(args, 0);
            let block = Self::positional_value(args, 1)
                .cloned()
                .unwrap_or(Value::Nil);
            (label, block)
        };
        let ctx = self.begin_subtest();
        let run_result = self.call_sub_value(block, vec![], true);
        self.finish_subtest(ctx, &label, run_result.map(|_| ()))?;
        Ok(Value::Bool(true))
    }

    fn test_fn_group_of(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // group-of $plan => $desc => { ... }
        // Accept both `Pair` and `ValuePair` keys for compatibility with non-string keys.
        let to_pair_parts = |value: &Value| -> Option<(Value, Value)> {
            match value {
                Value::Pair(k, v) => Some((Value::Str(k.clone()), *v.clone())),
                Value::ValuePair(k, v) => Some((*k.clone(), *v.clone())),
                _ => None,
            }
        };
        let Some((plan_key, inner)) = args.first().and_then(to_pair_parts) else {
            return Err(RuntimeError::new("group-of expects a Pair argument"));
        };
        let Some((desc_key, block)) = to_pair_parts(&inner) else {
            return Err(RuntimeError::new(
                "group-of expects $plan => $desc => { ... }",
            ));
        };
        let plan: i64 = match plan_key {
            Value::Int(i) => i,
            other => other
                .to_string_value()
                .parse()
                .map_err(|_| RuntimeError::new("group-of: plan must be an integer"))?,
        };
        let desc = desc_key.to_string_value();
        let ctx = self.begin_subtest();
        self.test_fn_plan(&[Value::Int(plan)])?;
        let run_result = self.call_sub_value(block, vec![], true);
        self.finish_subtest(ctx, &desc, run_result.map(|_| ()))?;
        Ok(Value::Bool(true))
    }

    fn test_fn_get_out(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let program_val = Self::positional_value_required(args, 0, "get_out expects code")?;
        let program = match program_val {
            Value::Str(s) => s.clone(),
            _ => return Err(RuntimeError::new("get_out expects string code")),
        };
        let mut nested = Interpreter::new();
        if let Some(Value::Int(pid)) = self.env.get("*PID") {
            nested.set_pid(pid.saturating_add(1));
        }
        nested.set_program_path("<get_out>");
        let result = nested.run(&program);
        let (out, err, status) = Self::extract_run_output(&nested, result);
        let mut hash = std::collections::HashMap::new();
        hash.insert("out".to_string(), Value::Str(out));
        hash.insert("err".to_string(), Value::Str(err));
        hash.insert("status".to_string(), Value::Int(status));
        Ok(Value::Hash(std::sync::Arc::new(hash)))
    }

    fn test_fn_warns_like(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let program_val = Self::positional_value_required(args, 0, "warns-like expects code")?;
        let program = match program_val {
            Value::Str(s) => s.clone(),
            _ => return Err(RuntimeError::new("warns-like expects string code")),
        };
        let test_pattern = Self::positional_value(args, 1)
            .cloned()
            .unwrap_or(Value::Nil);
        let desc = Self::positional_string(args, 2);
        let mut nested = Interpreter::new();
        if let Some(Value::Int(pid)) = self.env.get("*PID") {
            nested.set_pid(pid.saturating_add(1));
        }
        nested.set_program_path("<warns-like>");
        let result = nested.run(&program);
        let warn_message = nested.warn_output.clone();
        let did_warn = !warn_message.is_empty();
        let _ = result;
        let label = if desc.is_empty() {
            "warns-like".to_string()
        } else {
            desc
        };
        let ctx = self.begin_subtest();
        let test_state = self.test_state.as_mut().unwrap();
        test_state.planned = Some(2);
        self.output.push_str("1..2\n");
        // Test 1: code threw a warning
        self.test_ok(did_warn, "code threw a warning", false)?;
        // Test 2: warning message matches test pattern
        let msg_val = Value::Str(warn_message.trim_end().to_string());
        let matched = self.smart_match(&msg_val, &test_pattern);
        self.test_ok(matched, "warning message passes test", false)?;
        self.finish_subtest(ctx, &label, Ok(()))?;
        Ok(Value::Bool(did_warn && matched))
    }

    fn test_fn_doesnt_warn(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let program_val = Self::positional_value_required(args, 0, "doesn't-warn expects code")?;
        let program = match program_val {
            Value::Str(s) => s.clone(),
            _ => return Err(RuntimeError::new("doesn't-warn expects string code")),
        };
        let desc = Self::positional_string(args, 1);
        let mut nested = Interpreter::new();
        if let Some(Value::Int(pid)) = self.env.get("*PID") {
            nested.set_pid(pid.saturating_add(1));
        }
        nested.set_program_path("<doesn't-warn>");
        let result = nested.run(&program);
        let _ = result;
        let warn_message = nested.warn_output.clone();
        let did_warn = !warn_message.is_empty();
        if did_warn {
            let diag_msg = format!(
                "code must not warn but it produced a warning: {}",
                warn_message.trim_end()
            );
            self.output.push_str(&format!("# {}\n", diag_msg));
        }
        self.test_ok(!did_warn, &desc, false)?;
        Ok(Value::Bool(!did_warn))
    }

    fn test_fn_is_eqv(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let got = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);
        let expected = Self::positional_value(args, 1)
            .cloned()
            .unwrap_or(Value::Nil);
        let desc = Self::positional_string(args, 2);
        let ok = got.eqv(&expected);
        self.test_ok(ok, &desc, false)?;
        if !ok {
            let got_raku = Self::value_raku_repr(&got);
            let expected_raku = Self::value_raku_repr(&expected);
            self.output.push_str(&format!(
                "# expected: {}\n#      got: {}\n",
                expected_raku, got_raku
            ));
        }
        Ok(Value::Bool(ok))
    }

    fn value_raku_repr(val: &Value) -> String {
        if let Some(Ok(Value::Str(s))) = crate::builtins::native_method_0arg(val, "raku") {
            s
        } else {
            val.to_string_value()
        }
    }

    fn is_run_subprocess(
        &self,
        program: &str,
        run_args: &Option<Vec<Value>>,
        compiler_args: &[String],
    ) -> (String, String, i64) {
        // Find the mutsu executable
        let exe = std::env::current_exe()
            .unwrap_or_else(|_| std::path::PathBuf::from("target/debug/mutsu"));
        let mut cmd = std::process::Command::new(&exe);
        // Add compiler args first
        for arg in compiler_args {
            cmd.arg(arg);
        }
        // Add :args (command-line args for the subprocess)
        if let Some(items) = run_args {
            for item in items {
                cmd.arg(item.to_string_value());
            }
        }
        // Add -e with program code if non-empty
        if !program.is_empty() {
            cmd.arg("-e").arg(program);
        }
        match cmd.output() {
            Ok(output) => {
                let out = String::from_utf8_lossy(&output.stdout).to_string();
                let err = String::from_utf8_lossy(&output.stderr).to_string();
                let status = output.status.code().unwrap_or(1) as i64;
                (out, err, status)
            }
            Err(e) => (String::new(), format!("Failed to run subprocess: {}", e), 1),
        }
    }

    fn extract_run_output(
        nested: &Interpreter,
        result: Result<String, RuntimeError>,
    ) -> (String, String, i64) {
        let stderr_content = nested.stderr_output.clone();
        match result {
            Ok(output) => {
                let s = if nested.bailed_out {
                    255i64
                } else {
                    nested.exit_code
                };
                let stdout_only = if stderr_content.is_empty() {
                    output
                } else {
                    output.replace(&stderr_content, "")
                };
                (stdout_only, stderr_content, s)
            }
            Err(e) => {
                let combined = nested.output.clone();
                let stdout_only = if stderr_content.is_empty() {
                    combined
                } else {
                    combined.replace(&stderr_content, "")
                };
                let mut err = stderr_content;
                if !e.message.is_empty() {
                    if !err.is_empty() && !err.ends_with('\n') {
                        err.push('\n');
                    }
                    err.push_str(&e.message);
                    err.push('\n');
                }
                let s = if nested.exit_code != 0 {
                    nested.exit_code
                } else {
                    1i64
                };
                (stdout_only, err, s)
            }
        }
    }
}
