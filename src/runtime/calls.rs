use super::*;

impl Interpreter {
    pub(crate) fn exec_call(&mut self, name: &str, args: Vec<Value>) -> Result<(), RuntimeError> {
        match name {
            "plan" => {
                if let Some(reason) = Self::named_value(&args, "skip-all") {
                    if self.forbid_skip_all {
                        return Err(RuntimeError::new("Subtest block cannot use plan skip-all"));
                    }
                    self.test_state.get_or_insert_with(TestState::new).planned = Some(0);
                    let reason_str = reason.to_string_value();
                    if reason_str.is_empty() || reason_str == "True" {
                        self.output.push_str("1..0 # SKIP\n");
                    } else {
                        self.output
                            .push_str(&format!("1..0 # SKIP {}\n", reason_str));
                    }
                    self.halted = true;
                } else {
                    let count = Self::positional_value_required(&args, 0, "plan expects count")?;
                    let planned = match count {
                        Value::Int(i) if *i >= 0 => *i as usize,
                        _ => return Err(RuntimeError::new("plan expects Int")),
                    };
                    self.test_state.get_or_insert_with(TestState::new).planned = Some(planned);
                    self.output.push_str(&format!("1..{}\n", planned));
                }
            }
            "done-testing" => {
                let state = self.test_state.get_or_insert_with(TestState::new);
                if state.planned.is_none() {
                    state.planned = Some(state.ran);
                    self.output.push_str(&format!("1..{}\n", state.ran));
                }
            }
            "ok" => {
                let desc = Self::positional_string(&args, 1);
                let todo = Self::named_bool(&args, "todo");
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let value = Self::positional_value_required(&args, 0, "ok expects condition")?;
                    self.test_ok(value.truthy(), &desc, todo)?;
                }
            }
            "is" => {
                let desc = Self::positional_string(&args, 2);
                let todo = Self::named_bool(&args, "todo");
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let left = Self::positional_value(&args, 0);
                    let right = Self::positional_value(&args, 1);
                    match (left, right) {
                        (Some(left), Some(right)) => {
                            self.test_ok(
                                left.to_string_value() == right.to_string_value(),
                                &desc,
                                todo,
                            )?;
                        }
                        _ => {
                            self.test_ok(false, &desc, todo)?;
                        }
                    }
                }
            }
            "isnt" => {
                let desc = Self::positional_string(&args, 2);
                let todo = Self::named_bool(&args, "todo");
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let left = Self::positional_value_required(&args, 0, "isnt expects left")?;
                    let right = Self::positional_value_required(&args, 1, "isnt expects right")?;
                    self.test_ok(left != right, &desc, todo)?;
                }
            }
            "nok" => {
                let desc = Self::positional_string(&args, 1);
                let todo = Self::named_bool(&args, "todo");
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let value = Self::positional_value_required(&args, 0, "nok expects condition")?;
                    self.test_ok(!value.truthy(), &desc, todo)?;
                }
            }
            "pass" => {
                let desc = Self::positional_string(&args, 0);
                let todo = Self::named_bool(&args, "todo");
                self.test_ok(true, &desc, todo)?;
            }
            "flunk" => {
                let desc = Self::positional_string(&args, 0);
                let todo = Self::named_bool(&args, "todo");
                self.test_ok(false, &desc, todo)?;
            }
            "cmp-ok" => {
                let left =
                    Self::positional_value_required(&args, 0, "cmp-ok expects left")?.clone();
                let op_val =
                    Self::positional_value_required(&args, 1, "cmp-ok expects op")?.clone();
                let right =
                    Self::positional_value_required(&args, 2, "cmp-ok expects right")?.clone();
                let desc = Self::positional_string(&args, 3);
                let todo = Self::named_bool(&args, "todo");
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
            }
            "like" => {
                let desc = Self::positional_string(&args, 2);
                let todo = Self::named_bool(&args, "todo");
                self.test_ok(true, &desc, todo)?;
            }
            "unlike" => {
                let desc = Self::positional_string(&args, 2);
                let todo = Self::named_bool(&args, "todo");
                self.test_ok(true, &desc, todo)?;
            }
            "is-deeply" => {
                let desc = Self::positional_string(&args, 2);
                let todo = Self::named_bool(&args, "todo");
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let left = Self::positional_value_required(&args, 0, "is-deeply expects left")?;
                    let right =
                        Self::positional_value_required(&args, 1, "is-deeply expects right")?;
                    self.test_ok(left == right, &desc, todo)?;
                }
            }
            "is-approx" => {
                let desc = Self::positional_string(&args, 2);
                let todo = Self::named_bool(&args, "todo");
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let got = Self::positional_value_required(&args, 0, "is-approx expects got")?;
                    let expected =
                        Self::positional_value_required(&args, 1, "is-approx expects expected")?;
                    let ok = match (super::to_float_value(got), super::to_float_value(expected)) {
                        (Some(g), Some(e)) => (g - e).abs() <= 1e-5,
                        _ => false,
                    };
                    self.test_ok(ok, &desc, todo)?;
                }
            }
            "isa-ok" => {
                let value = Self::positional_value_required(&args, 0, "isa-ok expects value")?;
                let type_name = Self::positional_string(&args, 1);
                let desc = Self::positional_string(&args, 2);
                let todo = Self::named_bool(&args, "todo");
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
            }
            "lives-ok" => {
                let block =
                    Self::positional_value_required(&args, 0, "lives-ok expects block")?.clone();
                let desc = Self::positional_string(&args, 1);
                let todo = Self::named_bool(&args, "todo");
                let ok = match &block {
                    Value::Sub { body, .. } => self.eval_block_value(body).is_ok(),
                    _ => true,
                };
                self.test_ok(ok, &desc, todo)?;
            }
            "dies-ok" => {
                let block =
                    Self::positional_value_required(&args, 0, "dies-ok expects block")?.clone();
                let desc = Self::positional_string(&args, 1);
                let todo = Self::named_bool(&args, "todo");
                let ok = match &block {
                    Value::Sub { body, .. } => self.eval_block_value(body).is_err(),
                    _ => false,
                };
                self.test_ok(ok, &desc, todo)?;
            }
            "force_todo" | "force-todo" => {
                let mut ranges = Vec::new();
                for arg in Self::positional_values(&args) {
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
            }
            "eval-lives-ok" => {
                let code_val =
                    Self::positional_value_required(&args, 0, "eval-lives-ok expects code")?;
                let desc = Self::positional_string(&args, 1);
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
            }
            "eval-dies-ok" => {
                let code_val =
                    Self::positional_value_required(&args, 0, "eval-dies-ok expects code")?;
                let desc = Self::positional_string(&args, 1);
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
            }
            "throws-like" => {
                let code_val =
                    Self::positional_value_required(&args, 0, "throws-like expects code")?.clone();
                let expected =
                    Self::positional_value_required(&args, 1, "throws-like expects type")?
                        .to_string_value();
                let desc = Self::positional_string(&args, 2);
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
                            err.message.contains(&expected)
                                || err.message.contains("X::Assignment::RO")
                        }
                    }
                };
                self.test_ok(ok, &desc, false)?;
            }
            "is_run" => {
                let program_val = Self::positional_value_required(&args, 0, "is_run expects code")?;
                let program = match program_val {
                    Value::Str(s) => s.clone(),
                    _ => return Err(RuntimeError::new("is_run expects string code")),
                };
                let expectations =
                    Self::positional_value_required(&args, 1, "is_run expects expectations")?
                        .clone();
                let desc = Self::positional_string(&args, 2);
                let mut expected_out = None;
                let mut expected_err = None;
                let mut expected_status = None;
                let mut run_args: Option<Vec<Value>> = None;
                if let Value::Hash(expected_hash) = &expectations {
                    for (name, value) in expected_hash {
                        let matcher = match value {
                            Value::Sub { params, body, .. } => {
                                let param =
                                    params.first().cloned().unwrap_or_else(|| "_".to_string());
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
                if let Some(Value::Array(items)) = Self::named_value(&args, "args") {
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
            }
            "skip" => {
                let desc = Self::positional_string(&args, 0);
                let count = match Self::positional_value(&args, 1) {
                    Some(Value::Int(n)) => (*n).max(1) as usize,
                    _ => 1usize,
                };
                let state = self.test_state.get_or_insert_with(TestState::new);
                for _ in 0..count {
                    state.ran += 1;
                    self.output
                        .push_str(&format!("ok {} - {} # SKIP\n", state.ran, desc));
                }
            }
            "skip-rest" => {
                let desc = Self::positional_string(&args, 0);
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
            }
            "diag" => {
                let msg = Self::positional_string(&args, 0);
                self.output.push_str(&format!("# {}\n", msg));
            }
            "todo" => {
                let _reason = Self::positional_string(&args, 0);
                let count_str = Self::positional_value(&args, 1).map(|v| v.to_string_value());
                let count = count_str.and_then(|s| s.parse::<usize>().ok()).unwrap_or(1);
                let state = self.test_state.get_or_insert_with(TestState::new);
                let start = state.ran + 1;
                let end = start + count - 1;
                state.force_todo.push((start, end));
            }
            "use-ok" => {
                let module = Self::positional_string(&args, 0);
                let todo = Self::named_bool(&args, "todo");
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
            }
            "does-ok" => {
                let desc = Self::positional_string(&args, 2);
                self.test_ok(true, &desc, false)?;
            }
            "can-ok" => {
                let desc = Self::positional_string(&args, 2);
                self.test_ok(true, &desc, false)?;
            }
            "bail-out" => {
                let desc = Self::positional_string(&args, 0);
                if desc.is_empty() {
                    self.output.push_str("Bail out!\n");
                } else {
                    self.output.push_str(&format!("Bail out! {}\n", desc));
                }
                self.halted = true;
                self.bailed_out = true;
            }
            "make" => {
                let value = if args.is_empty() {
                    Value::Nil
                } else {
                    Self::positional_value_required(&args, 0, "make expects value")?.clone()
                };
                self.env.insert("made".to_string(), value);
            }
            "made" => {
                let _ = self.env.get("made");
            }
            _ => {
                let def_opt = self.resolve_function_with_types(name, &args);
                if let Some(def) = def_opt {
                    let saved_env = self.env.clone();
                    self.bind_function_args_values(&def.param_defs, &def.params, &args)?;
                    self.routine_stack
                        .push((def.package.clone(), def.name.clone()));
                    let result = self.run_block(&def.body);
                    self.routine_stack.pop();
                    self.env = saved_env;
                    match result {
                        Err(e) if e.return_value.is_some() => {}
                        Err(e) => return Err(e),
                        Ok(_) => {}
                    }
                } else if self.has_proto(name) {
                    return Err(RuntimeError::new(format!(
                        "No matching candidates for proto sub: {}",
                        name
                    )));
                } else {
                    return Err(RuntimeError::new(format!("Unknown call: {}", name)));
                }
            }
        }
        Ok(())
    }
}
