use super::*;

impl Interpreter {
    /// Dispatch Test module functions. Returns `Ok(Some(value))` if the name
    /// matched a Test function, `Ok(None)` if it did not.
    pub(crate) fn call_test_function(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
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
            "is_run" => self.test_fn_is_run(args).map(Some),
            "use-ok" => self.test_fn_use_ok(args).map(Some),
            "does-ok" => self.test_fn_does_ok(args).map(Some),
            "can-ok" => self.test_fn_can_ok(args).map(Some),
            "todo" => self.test_fn_todo(args).map(Some),
            _ => Ok(None),
        }
    }

    fn test_fn_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 1);
        let todo = Self::named_bool(args, "todo");
        if self.loose_ok {
            self.test_ok(true, &desc, todo)?;
            return Ok(Value::Bool(true));
        }
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
        if self.loose_ok {
            self.test_ok(true, &desc, todo)?;
            return Ok(Value::Bool(true));
        }
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
        let desc = Self::positional_string(args, 2);
        let todo = Self::named_bool(args, "todo");
        if self.loose_ok {
            self.test_ok(true, &desc, todo)?;
            return Ok(Value::Bool(true));
        }
        let left = Self::positional_value(args, 0);
        let right = Self::positional_value(args, 1);
        let ok = match (left, right) {
            (Some(left), Some(right)) => left.to_string_value() == right.to_string_value(),
            _ => false,
        };
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_isnt(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 2);
        let todo = Self::named_bool(args, "todo");
        if self.loose_ok {
            self.test_ok(true, &desc, todo)?;
            return Ok(Value::Bool(true));
        }
        let left = Self::positional_value_required(args, 0, "isnt expects left")?;
        let right = Self::positional_value_required(args, 1, "isnt expects right")?;
        let ok = left != right;
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_plan(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(reason) = Self::named_value(args, "skip-all") {
            if self.forbid_skip_all {
                return Err(RuntimeError::new("Subtest block cannot use plan skip-all"));
            }
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
        Ok(Value::Bool(ok))
    }

    fn test_fn_like(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 2);
        let todo = Self::named_bool(args, "todo");
        self.test_ok(true, &desc, todo)?;
        Ok(Value::Bool(true))
    }

    fn test_fn_unlike(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 2);
        let todo = Self::named_bool(args, "todo");
        self.test_ok(true, &desc, todo)?;
        Ok(Value::Bool(true))
    }

    fn test_fn_is_deeply(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 2);
        let todo = Self::named_bool(args, "todo");
        if self.loose_ok {
            self.test_ok(true, &desc, todo)?;
            return Ok(Value::Bool(true));
        }
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
        if self.loose_ok {
            self.test_ok(true, &desc, todo)?;
            return Ok(Value::Bool(true));
        }
        let got = Self::positional_value_required(args, 0, "is-approx expects got")?;
        let expected = Self::positional_value_required(args, 1, "is-approx expects expected")?;
        let ok = match (super::to_float_value(got), super::to_float_value(expected)) {
            (Some(g), Some(e)) => (g - e).abs() <= 1e-5,
            _ => false,
        };
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_lives_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let block = Self::positional_value_required(args, 0, "lives-ok expects block")?.clone();
        let desc = Self::positional_string(args, 1);
        let todo = Self::named_bool(args, "todo");
        let ok = match &block {
            Value::Sub { body, .. } => self.eval_block_value(body).is_ok(),
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
            Value::Sub { body, .. } => self.eval_block_value(body).is_err(),
            _ => false,
        };
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_isa_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let value = Self::positional_value_required(args, 0, "isa-ok expects value")?;
        let type_name = Self::positional_string(args, 1);
        let desc = Self::positional_string(args, 2);
        let todo = Self::named_bool(args, "todo");
        let ok = match type_name.as_str() {
            "Array" => matches!(value, Value::Array(_)),
            "Rat" => matches!(value, Value::Rat(_, _)),
            "FatRat" => matches!(value, Value::FatRat(_, _)),
            "Complex" => matches!(value, Value::Complex(_, _)),
            "Set" => matches!(value, Value::Set(_)),
            "Bag" => matches!(value, Value::Bag(_)),
            "Mix" => matches!(value, Value::Mix(_)),
            _ => {
                if let Value::Instance { class_name, .. } = value {
                    class_name == type_name.as_str()
                } else {
                    true
                }
            }
        };
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
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
            Value::Sub { body, .. } => self.eval_block_value(body),
            Value::Str(code) => {
                let mut nested = Interpreter::new();
                if let Some(Value::Int(pid)) = self.env.get("*PID") {
                    nested.set_pid(pid.saturating_add(1));
                }
                nested.run(code).map(|_| Value::Nil)
            }
            _ => Ok(Value::Nil),
        };
        let ok = match result {
            Ok(_) => false,
            Err(err) => {
                if expected.is_empty() {
                    true
                } else {
                    err.message.contains(&expected) || err.message.contains("X::Assignment::RO")
                }
            }
        };
        self.test_ok(ok, &desc, false)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_is_run(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let program_val = Self::positional_value_required(args, 0, "is_run expects code")?;
        let program = match program_val {
            Value::Str(s) => s.clone(),
            _ => return Err(RuntimeError::new("is_run expects string code")),
        };
        let expectations =
            Self::positional_value_required(args, 1, "is_run expects expectations")?.clone();
        let desc = Self::positional_string(args, 2);
        let mut expected_out = None;
        let mut expected_err = None;
        let mut expected_status = None;
        let mut run_args: Option<Vec<Value>> = None;
        if let Value::Hash(expected_hash) = &expectations {
            for (name, value) in expected_hash {
                let matcher = match value {
                    Value::Sub { params, body, .. } => {
                        let param = params.first().cloned().unwrap_or_else(|| "_".to_string());
                        Some(ExpectedMatcher::Lambda {
                            param,
                            body: body.clone(),
                        })
                    }
                    other => Some(ExpectedMatcher::Exact(other.clone())),
                };
                match name.as_str() {
                    "out" => expected_out = matcher,
                    "err" => expected_err = matcher,
                    "status" => {
                        if let Some(ExpectedMatcher::Exact(Value::Int(i))) = matcher {
                            expected_status = Some(i);
                        }
                    }
                    _ => {}
                }
            }
        }
        if let Some(Value::Array(items)) = Self::named_value(args, "args") {
            run_args = Some(items);
        }
        let mut nested = Interpreter::new();
        if let Some(Value::Int(pid)) = self.env.get("*PID") {
            nested.set_pid(pid.saturating_add(1));
        }
        if let Some(items) = run_args {
            nested.set_args(items);
        }
        nested.set_program_path("<is_run>");
        let result = nested.run(&program);
        let stderr_content = nested.stderr_output.clone();
        let (out, err, status) = match result {
            Ok(output) => {
                let s = if nested.bailed_out { 255i64 } else { 0i64 };
                let stdout_only = if stderr_content.is_empty() {
                    output
                } else {
                    output.replace(&stderr_content, "")
                };
                (stdout_only, stderr_content, s)
            }
            Err(_) => {
                let combined = nested.output.clone();
                let stdout_only = if stderr_content.is_empty() {
                    combined
                } else {
                    combined.replace(&stderr_content, "")
                };
                (stdout_only, stderr_content, 1i64)
            }
        };
        let mut ok = true;
        if let Some(matcher) = expected_out {
            ok &= self.matches_expected(&matcher, &out)?;
        }
        if let Some(matcher) = expected_err {
            ok &= self.matches_expected(&matcher, &err)?;
        }
        if let Some(expect) = expected_status {
            ok &= status == expect;
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
        let desc = Self::positional_string(args, 2);
        self.test_ok(true, &desc, false)?;
        Ok(Value::Bool(true))
    }

    fn test_fn_can_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 2);
        self.test_ok(true, &desc, false)?;
        Ok(Value::Bool(true))
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
}
