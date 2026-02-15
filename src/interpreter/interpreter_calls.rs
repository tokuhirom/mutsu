use super::*;

impl Interpreter {
    pub(crate) fn exec_call_values(
        &mut self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        match self.call_function(name, args.clone()) {
            Ok(_) => Ok(()),
            Err(e)
                if e.message
                    .starts_with("Unknown function (call_function fallback disabled):") =>
            {
                let call_args: Vec<CallArg> = args
                    .into_iter()
                    .map(|v| CallArg::Positional(Expr::Literal(v)))
                    .collect();
                self.exec_call(name, &call_args)
            }
            Err(e) => Err(e),
        }
    }

    pub(crate) fn exec_call_pairs_values(
        &mut self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        let rebuilt: Vec<CallArg> = args
            .into_iter()
            .map(|arg| match arg {
                Value::Pair(key, value) => CallArg::Named {
                    name: key,
                    value: Some(Expr::Literal(*value)),
                },
                other => CallArg::Positional(Expr::Literal(other)),
            })
            .collect();
        self.exec_call(name, &rebuilt)
    }

    pub(super) fn positional_arg<'a>(
        &self,
        args: &'a [CallArg],
        index: usize,
        message: &str,
    ) -> Result<&'a Expr, RuntimeError> {
        let mut count = 0;
        for arg in args {
            if let CallArg::Positional(expr) = arg {
                if count == index {
                    return Ok(expr);
                }
                count += 1;
            }
        }
        Err(RuntimeError::new(message))
    }

    pub(super) fn positional_arg_value(
        &mut self,
        args: &[CallArg],
        index: usize,
    ) -> Result<String, RuntimeError> {
        let mut count = 0;
        for arg in args {
            if let CallArg::Positional(expr) = arg {
                if count == index {
                    return Ok(self.eval_expr(expr)?.to_string_value());
                }
                count += 1;
            }
        }
        Ok(String::new())
    }

    pub(super) fn named_arg_bool(
        &mut self,
        args: &[CallArg],
        name: &str,
    ) -> Result<bool, RuntimeError> {
        for arg in args {
            if let CallArg::Named {
                name: arg_name,
                value,
            } = arg
                && arg_name == name
            {
                if let Some(expr) = value {
                    return Ok(self.eval_expr(expr)?.truthy());
                }
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub(super) fn named_arg_value(
        &mut self,
        args: &[CallArg],
        name: &str,
    ) -> Result<Option<String>, RuntimeError> {
        for arg in args {
            if let CallArg::Named {
                name: arg_name,
                value,
            } = arg
                && arg_name == name
            {
                if let Some(expr) = value {
                    return Ok(Some(self.eval_expr(expr)?.to_string_value()));
                }
                return Ok(Some(String::new()));
            }
        }
        Ok(None)
    }

    pub(super) fn test_ok(
        &mut self,
        success: bool,
        desc: &str,
        todo: bool,
    ) -> Result<(), RuntimeError> {
        let state = self.test_state.get_or_insert_with(TestState::new);
        state.ran += 1;
        let forced = state
            .force_todo
            .iter()
            .any(|(start, end)| state.ran >= *start && state.ran <= *end);
        let todo = todo || forced;
        if !success && !todo {
            state.failed += 1;
        }
        let mut line = String::new();
        if success {
            line.push_str("ok ");
        } else {
            line.push_str("not ok ");
        }
        line.push_str(&state.ran.to_string());
        if !desc.is_empty() {
            line.push_str(" - ");
            line.push_str(desc);
        }
        if todo {
            line.push_str(" # TODO");
        }
        line.push('\n');
        self.output.push_str(&line);
        Ok(())
    }

    pub(super) fn rewrite_call_expr(name: &str, args: &[Expr]) -> Option<Expr> {
        if name == "indir"
            && args.len() >= 2
            && let Expr::Block(body) = &args[1]
        {
            let mut rewritten = args.to_vec();
            rewritten[1] = Expr::AnonSub(body.clone());
            return Some(Expr::Call {
                name: name.to_string(),
                args: rewritten,
            });
        }
        None
    }
    pub(crate) fn exec_call(&mut self, name: &str, args: &[CallArg]) -> Result<(), RuntimeError> {
        match name {
            "plan" => {
                if let Some(reason) = self.named_arg_value(args, "skip-all")? {
                    if self.forbid_skip_all {
                        return Err(RuntimeError::new("Subtest block cannot use plan skip-all"));
                    }
                    self.test_state.get_or_insert_with(TestState::new).planned = Some(0);
                    if reason.is_empty() {
                        self.output.push_str("1..0 # SKIP\n");
                    } else {
                        self.output.push_str(&format!("1..0 # SKIP {}\n", reason));
                    }
                    self.halted = true;
                } else {
                    let count =
                        self.eval_expr(self.positional_arg(args, 0, "plan expects count")?)?;
                    let planned = match count {
                        Value::Int(i) if i >= 0 => i as usize,
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
                let desc = self.positional_arg_value(args, 1)?;
                let todo = self.named_arg_bool(args, "todo")?;
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let value =
                        self.eval_expr(self.positional_arg(args, 0, "ok expects condition")?)?;
                    self.test_ok(value.truthy(), &desc, todo)?;
                }
            }
            "is" => {
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let left_res = self
                        .positional_arg(args, 0, "is expects left")
                        .and_then(|e| self.eval_expr(e));
                    let right_res = self
                        .positional_arg(args, 1, "is expects right")
                        .and_then(|e| self.eval_expr(e));
                    match (left_res, right_res) {
                        (Ok(left), Ok(right)) => {
                            // Raku's `is` compares using string semantics (eq)
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
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let left =
                        self.eval_expr(self.positional_arg(args, 0, "isnt expects left")?)?;
                    let right =
                        self.eval_expr(self.positional_arg(args, 1, "isnt expects right")?)?;
                    self.test_ok(left != right, &desc, todo)?;
                }
            }
            "nok" => {
                let desc = self.positional_arg_value(args, 1)?;
                let todo = self.named_arg_bool(args, "todo")?;
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let value =
                        self.eval_expr(self.positional_arg(args, 0, "nok expects condition")?)?;
                    self.test_ok(!value.truthy(), &desc, todo)?;
                }
            }
            "pass" => {
                let desc = self.positional_arg_value(args, 0)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(true, &desc, todo)?;
            }
            "flunk" => {
                let desc = self.positional_arg_value(args, 0)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(false, &desc, todo)?;
            }
            "cmp-ok" => {
                let left =
                    self.eval_expr(self.positional_arg(args, 0, "cmp-ok expects left")?)?;
                let op_val =
                    self.eval_expr(self.positional_arg(args, 1, "cmp-ok expects op")?)?;
                let right =
                    self.eval_expr(self.positional_arg(args, 2, "cmp-ok expects right")?)?;
                let desc = self.positional_arg_value(args, 3)?;
                let todo = self.named_arg_bool(args, "todo")?;
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
                        "==" => {
                            crate::runtime::to_float_value(&left)
                                == crate::runtime::to_float_value(&right)
                        }
                        "!=" => {
                            crate::runtime::to_float_value(&left)
                                != crate::runtime::to_float_value(&right)
                        }
                        "<" => {
                            crate::runtime::to_float_value(&left)
                                < crate::runtime::to_float_value(&right)
                        }
                        "<=" => {
                            crate::runtime::to_float_value(&left)
                                <= crate::runtime::to_float_value(&right)
                        }
                        ">" => {
                            crate::runtime::to_float_value(&left)
                                > crate::runtime::to_float_value(&right)
                        }
                        ">=" => {
                            crate::runtime::to_float_value(&left)
                                >= crate::runtime::to_float_value(&right)
                        }
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
                let _ = self.positional_arg(args, 0, "like expects value")?;
                let _ = self.positional_arg(args, 1, "like expects pattern")?;
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(true, &desc, todo)?;
            }
            "unlike" => {
                let _ = self.positional_arg(args, 0, "unlike expects value")?;
                let _ = self.positional_arg(args, 1, "unlike expects pattern")?;
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(true, &desc, todo)?;
            }
            "is-deeply" => {
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let left =
                        self.eval_expr(self.positional_arg(args, 0, "is-deeply expects left")?)?;
                    let right =
                        self.eval_expr(self.positional_arg(args, 1, "is-deeply expects right")?)?;
                    self.test_ok(left == right, &desc, todo)?;
                }
            }
            "is-approx" => {
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let got =
                        self.eval_expr(self.positional_arg(args, 0, "is-approx expects got")?)?;
                    let expected = self.eval_expr(self.positional_arg(
                        args,
                        1,
                        "is-approx expects expected",
                    )?)?;
                    let ok = match (
                        crate::runtime::to_float_value(&got),
                        crate::runtime::to_float_value(&expected),
                    ) {
                        (Some(g), Some(e)) => (g - e).abs() <= 1e-5,
                        _ => false,
                    };
                    self.test_ok(ok, &desc, todo)?;
                }
            }
            "isa-ok" => {
                let value =
                    self.eval_expr(self.positional_arg(args, 0, "isa-ok expects value")?)?;
                let type_name = self.positional_arg_value(args, 1)?;
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                let ok = match type_name.as_str() {
                    "Array" => matches!(value, Value::Array(_)),
                    "Rat" => matches!(value, Value::Rat(_, _)),
                    "FatRat" => matches!(value, Value::FatRat(_, _)),
                    "Complex" => matches!(value, Value::Complex(_, _)),
                    "Set" => matches!(value, Value::Set(_)),
                    "Bag" => matches!(value, Value::Bag(_)),
                    "Mix" => matches!(value, Value::Mix(_)),
                    _ => {
                        if let Value::Instance { class_name, .. } = &value {
                            class_name == type_name.as_str()
                        } else {
                            true
                        }
                    }
                };
                self.test_ok(ok, &desc, todo)?;
            }
            "lives-ok" => {
                let block = self.positional_arg(args, 0, "lives-ok expects block")?;
                let desc = self.positional_arg_value(args, 1)?;
                let todo = self.named_arg_bool(args, "todo")?;
                let ok = match block {
                    Expr::Block(body)
                    | Expr::AnonSub(body)
                    | Expr::AnonSubParams { body, .. }
                    | Expr::Lambda { body, .. } => self.eval_block_value(body).is_ok(),
                    Expr::Literal(Value::Sub { body, .. }) => self.eval_block_value(body).is_ok(),
                    _ => self.eval_expr(block).is_ok(),
                };
                self.test_ok(ok, &desc, todo)?;
            }
            "dies-ok" => {
                let block = self.positional_arg(args, 0, "dies-ok expects block")?;
                let desc = self.positional_arg_value(args, 1)?;
                let todo = self.named_arg_bool(args, "todo")?;
                let ok = match block {
                    Expr::Block(body)
                    | Expr::AnonSub(body)
                    | Expr::AnonSubParams { body, .. }
                    | Expr::Lambda { body, .. } => self.eval_block_value(body).is_err(),
                    Expr::Literal(Value::Sub { body, .. }) => self.eval_block_value(body).is_err(),
                    _ => self.eval_expr(block).is_err(),
                };
                self.test_ok(ok, &desc, todo)?;
            }
            "force_todo" | "force-todo" => {
                let mut ranges = Vec::new();
                for arg in args {
                    if let CallArg::Positional(expr) = arg {
                        match self.eval_expr(expr)? {
                            Value::Int(i) if i > 0 => {
                                let n = i as usize;
                                ranges.push((n, n));
                            }
                            Value::Range(a, b) => {
                                let start = a.min(b).max(1) as usize;
                                let end = a.max(b).max(1) as usize;
                                ranges.push((start, end));
                            }
                            _ => {}
                        }
                    }
                }
                let state = self.test_state.get_or_insert_with(TestState::new);
                state.force_todo.extend(ranges);
            }
            "eval-lives-ok" => {
                let code_expr = self.positional_arg(args, 0, "eval-lives-ok expects code")?;
                let desc = self.positional_arg_value(args, 1)?;
                let code = match self.eval_expr(code_expr)? {
                    Value::Str(s) => s,
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
                let code_expr = self.positional_arg(args, 0, "eval-dies-ok expects code")?;
                let desc = self.positional_arg_value(args, 1)?;
                let code = match self.eval_expr(code_expr)? {
                    Value::Str(s) => s,
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
                let code_expr = self.positional_arg(args, 0, "throws-like expects code")?;
                let expected_expr = self.positional_arg(args, 1, "throws-like expects type")?;
                let desc = self.positional_arg_value(args, 2)?;
                let expected = match self.eval_expr(expected_expr)? {
                    Value::Str(s) => s,
                    _ => String::new(),
                };
                let result = match code_expr {
                    Expr::Block(body) | Expr::AnonSub(body) | Expr::AnonSubParams { body, .. } => {
                        self.eval_block_value(body)
                    }
                    Expr::Literal(Value::Sub { body, .. }) => self.eval_block_value(body),
                    _ => {
                        let code = match self.eval_expr(code_expr)? {
                            Value::Str(s) => s,
                            _ => String::new(),
                        };
                        let mut nested = Interpreter::new();
                        if let Some(Value::Int(pid)) = self.env.get("*PID") {
                            nested.set_pid(pid.saturating_add(1));
                        }
                        nested.run(&code).map(|_| Value::Nil)
                    }
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
                let program_expr = self.positional_arg(args, 0, "is_run expects code")?;
                let program = match self.eval_expr(program_expr)? {
                    Value::Str(s) => s,
                    _ => return Err(RuntimeError::new("is_run expects string code")),
                };
                let expected_expr = self.positional_arg(args, 1, "is_run expects expectations")?;
                let desc = self.positional_arg_value(args, 2)?;
                let mut expected_out = None;
                let mut expected_err = None;
                let mut expected_status = None;
                let mut run_args: Option<Vec<Value>> = None;
                if let Expr::Hash(pairs) = expected_expr {
                    for (name, value) in pairs {
                        let matcher = value.as_ref().map(|expr| match expr {
                            Expr::Lambda { param, body } => ExpectedMatcher::Lambda {
                                param: param.clone(),
                                body: body.clone(),
                            },
                            Expr::AnonSub(body) => ExpectedMatcher::Lambda {
                                param: "_".to_string(),
                                body: body.clone(),
                            },
                            _ => ExpectedMatcher::Exact(self.eval_expr(expr).unwrap_or(Value::Nil)),
                        });
                        match name.as_str() {
                            "out" => expected_out = matcher,
                            "err" => expected_err = matcher,
                            "status" => {
                                if let Some(Expr::Literal(Value::Int(i))) = value {
                                    expected_status = Some(*i);
                                } else if let Some(expr) = value
                                    && let Ok(Value::Int(i)) = self.eval_expr(expr)
                                {
                                    expected_status = Some(i);
                                }
                            }
                            _ => {}
                        }
                    }
                } else if let Value::Hash(expected_hash) = self.eval_expr(expected_expr)? {
                    for (name, value) in expected_hash {
                        let matcher = match value {
                            Value::Sub { params, body, .. } => {
                                let param =
                                    params.first().cloned().unwrap_or_else(|| "_".to_string());
                                Some(ExpectedMatcher::Lambda { param, body })
                            }
                            other => Some(ExpectedMatcher::Exact(other)),
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
                let mut nested = Interpreter::new();
                if let Some(Value::Int(pid)) = self.env.get("*PID") {
                    nested.set_pid(pid.saturating_add(1));
                }
                for arg in args {
                    if let CallArg::Named { name, value } = arg
                        && name == "args"
                        && let Some(expr) = value
                        && let Ok(Value::Array(items)) = self.eval_expr(expr)
                    {
                        run_args = Some(items);
                    }
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
                        // Separate stdout from combined output by removing stderr content
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
                let desc = self.positional_arg_value(args, 0)?;
                let count = {
                    let mut positional_count = 0;
                    let mut skip_count = 1usize;
                    for arg in args {
                        if let CallArg::Positional(expr) = arg {
                            if positional_count == 1
                                && let Ok(Value::Int(n)) = self.eval_expr(expr)
                            {
                                skip_count = n.max(1) as usize;
                            }
                            positional_count += 1;
                        }
                    }
                    skip_count
                };
                let state = self.test_state.get_or_insert_with(TestState::new);
                for _ in 0..count {
                    state.ran += 1;
                    self.output
                        .push_str(&format!("ok {} - {} # SKIP\n", state.ran, desc));
                }
            }
            "skip-rest" => {
                let desc = self.positional_arg_value(args, 0)?;
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
                let msg = self.positional_arg_value(args, 0)?;
                self.output.push_str(&format!("# {}\n", msg));
            }
            "todo" => {
                let _reason = self.positional_arg_value(args, 0).unwrap_or_default();
                let count_str = self.positional_arg_value(args, 1).ok();
                let count = count_str.and_then(|s| s.parse::<usize>().ok()).unwrap_or(1);
                let state = self.test_state.get_or_insert_with(TestState::new);
                let start = state.ran + 1;
                let end = start + count - 1;
                state.force_todo.push((start, end));
            }
            "use-ok" => {
                let module = self.positional_arg_value(args, 0)?;
                let todo = self.named_arg_bool(args, "todo")?;
                let desc = format!("{} module can be use-d ok", module);
                // Try to find the module file in lib_paths
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
                let _ = self.positional_arg(args, 0, "does-ok expects value")?;
                let _ = self.positional_arg(args, 1, "does-ok expects role")?;
                let desc = self.positional_arg_value(args, 2)?;
                self.test_ok(true, &desc, false)?;
            }
            "can-ok" => {
                let _ = self.positional_arg(args, 0, "can-ok expects value")?;
                let _ = self.positional_arg(args, 1, "can-ok expects method")?;
                let desc = self.positional_arg_value(args, 2)?;
                self.test_ok(true, &desc, false)?;
            }
            "bail-out" => {
                let desc = self.positional_arg_value(args, 0)?;
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
                    let expr = self.positional_arg(args, 0, "make expects value")?;
                    self.eval_expr(expr)?
                };
                self.env.insert("made".to_string(), value);
            }
            "made" => {
                let _ = self.env.get("made");
            }
            _ => {
                // Try user-defined function with type-based dispatch
                let mut arg_values = Vec::new();
                for arg in args {
                    if let CallArg::Positional(expr) = arg {
                        arg_values.push(self.eval_expr(expr)?);
                    }
                }
                let def_opt = self.resolve_function_with_types(name, &arg_values);
                if let Some(def) = def_opt {
                    let saved_env = self.env.clone();
                    self.bind_function_args_values(&def.param_defs, &def.params, &arg_values)?;
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
