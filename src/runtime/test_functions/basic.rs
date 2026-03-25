use super::super::*;

impl Interpreter {
    pub(crate) fn test_fn_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 1);
        let todo = Self::named_bool(args, "todo");
        let value = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);
        let ok = value.truthy();
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    pub(crate) fn test_fn_nok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 1);
        let todo = Self::named_bool(args, "todo");
        let value = Self::positional_value_required(args, 0, "nok expects condition")?;
        let ok = !value.truthy();
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    pub(crate) fn test_fn_diag(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let msg = Self::positional_string(args, 0);
        self.emit_output(&format!("# {}\n", msg));
        Ok(Value::Nil)
    }

    pub(crate) fn test_fn_pass(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 0);
        let todo = Self::named_bool(args, "todo");
        self.test_ok(true, &desc, todo)?;
        Ok(Value::Bool(true))
    }

    pub(crate) fn test_fn_flunk(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 0);
        let todo = Self::named_bool(args, "todo");
        self.test_ok(false, &desc, todo)?;
        Ok(Value::Bool(false))
    }

    pub(crate) fn test_fn_is(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
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
                if matches!(left, Value::Junction { .. }) || matches!(right, Value::Junction { .. })
                {
                    Self::eqv_with_junctions(left, right).truthy()
                } else if matches!(
                    left,
                    Value::Range(..)
                        | Value::RangeExcl(..)
                        | Value::RangeExclStart(..)
                        | Value::RangeExclBoth(..)
                ) || matches!(
                    right,
                    Value::Range(..)
                        | Value::RangeExcl(..)
                        | Value::RangeExclStart(..)
                        | Value::RangeExclBoth(..)
                ) {
                    crate::runtime::value_to_list(left) == crate::runtime::value_to_list(right)
                } else if crate::vm::VM::is_buf_value(left) && crate::vm::VM::is_buf_value(right) {
                    crate::vm::VM::extract_buf_bytes(left)
                        == crate::vm::VM::extract_buf_bytes(right)
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

    pub(crate) fn stringify_test_value(&mut self, value: &Value) -> Result<String, RuntimeError> {
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

    pub(crate) fn test_fn_isnt(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
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

    pub(crate) fn test_fn_plan(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(reason) = Self::named_value(args, "skip-all") {
            self.test_state.get_or_insert_with(TestState::new).planned = Some(0);
            let reason_str = reason.to_string_value();
            if reason_str.is_empty() || reason_str == "True" {
                self.emit_output("1..0 # Skipped: no reason given\n");
            } else {
                self.emit_output(&format!("1..0 # Skipped: {}\n", reason_str));
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
            self.emit_output(&format!("1..{}\n", planned));
        }
        Ok(Value::Nil)
    }

    pub(crate) fn test_fn_done_testing(&mut self) -> Result<Value, RuntimeError> {
        let already_planned = {
            let state = self.test_state.get_or_insert_with(TestState::new);
            state.planned.is_some()
        };
        if !already_planned {
            let ran = {
                let state = self.test_state.get_or_insert_with(TestState::new);
                state.planned = Some(state.ran);
                state.ran
            };
            self.emit_output(&format!("1..{}\n", ran));
        }
        // Return True if all tests passed and plan matches, False otherwise
        let state = self.test_state.as_ref().unwrap();
        let plan_matches = match state.planned {
            Some(planned) => planned == state.ran,
            None => true,
        };
        let all_passed = state.failed == 0 && plan_matches;
        Ok(Value::Bool(all_passed))
    }

    pub(crate) fn test_fn_skip(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 0);
        let count = match Self::positional_value(args, 1) {
            Some(Value::Int(n)) => (*n).max(1) as usize,
            _ => 1usize,
        };
        // Raku TAP format: `ok N - # SKIP reason` with `#` in reason escaped as ` \#`
        let escaped_desc = desc.replace('#', " \\#");
        let mut lines = Vec::with_capacity(count);
        let state = self.test_state.get_or_insert_with(TestState::new);
        for _ in 0..count {
            state.ran += 1;
            if escaped_desc.is_empty() {
                lines.push(format!("ok {} - # SKIP {}\n", state.ran, desc));
            } else {
                lines.push(format!("ok {} - # SKIP {}\n", state.ran, escaped_desc));
            }
        }
        for line in lines {
            self.emit_output(&line);
        }
        Ok(Value::Nil)
    }

    pub(crate) fn test_fn_skip_rest(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 0);
        let escaped_desc = desc.replace('#', " \\#");
        let mut lines = Vec::new();
        {
            let state = self.test_state.get_or_insert_with(TestState::new);
            if let Some(planned) = state.planned {
                while state.ran < planned {
                    state.ran += 1;
                    if escaped_desc.is_empty() {
                        lines.push(format!("ok {} - # SKIP\n", state.ran));
                    } else {
                        lines.push(format!("ok {} - # SKIP {}\n", state.ran, escaped_desc));
                    }
                }
            }
        }
        for line in &lines {
            self.emit_output(line);
        }
        self.halted = true;
        Ok(Value::Nil)
    }

    pub(crate) fn test_fn_bail_out(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 0);
        if desc.is_empty() {
            self.emit_output("Bail out!\n");
        } else {
            self.emit_output(&format!("Bail out! {}\n", desc));
        }
        self.halted = true;
        self.bailed_out = true;
        Ok(Value::Nil)
    }

    pub(crate) fn test_fn_todo(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let reason = Self::positional_string(args, 0);
        let count = Self::positional_value(args, 1)
            .map(|v| v.to_string_value())
            .and_then(|s| s.parse::<usize>().ok())
            .unwrap_or(1);
        let state = self.test_state.get_or_insert_with(TestState::new);
        let start = state.ran + 1;
        let end = start + count - 1;
        state.force_todo.push(TodoRange { start, end, reason });
        Ok(Value::Nil)
    }
}
