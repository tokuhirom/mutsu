use super::super::*;

impl Interpreter {
    pub(crate) fn test_fn_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 1);
        let todo = Self::named_bool(args, "todo");
        let value = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::NIL);
        let ok = value.truthy();
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::truth(ok))
    }

    pub(crate) fn test_fn_nok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 1);
        let todo = Self::named_bool(args, "todo");
        let value = Self::positional_value_required(args, 0, "nok expects condition")?;
        let ok = !value.truthy();
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::truth(ok))
    }

    pub(crate) fn test_fn_diag(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let msg = Self::positional_string(args, 0);
        self.emit_output(&format!("# {}\n", msg));
        Ok(Value::NIL)
    }

    pub(crate) fn test_fn_pass(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 0);
        let todo = Self::named_bool(args, "todo");
        self.test_ok(true, &desc, todo)?;
        Ok(Value::TRUE)
    }

    pub(crate) fn test_fn_flunk(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 0);
        let todo = Self::named_bool(args, "todo");
        self.test_ok(false, &desc, todo)?;
        Ok(Value::FALSE)
    }

    pub(crate) fn test_fn_is(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let todo = Self::named_bool(args, "todo");
        let positional: Vec<&Value> = args
            .iter()
            .filter(|arg| !matches!(arg.view(), ValueView::Pair(key, _) if key == "todo"))
            .collect();
        let left = positional.first().cloned().cloned();
        let right = positional.get(1).cloned().cloned();
        let desc = positional
            .get(2)
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let ok = match (left.as_ref(), right.as_ref()) {
            (Some(left), Some(right)) => {
                if matches!(left.view(), ValueView::Junction { .. })
                    || matches!(right.view(), ValueView::Junction { .. })
                {
                    Self::eq_with_junctions(left, right).truthy()
                } else if matches!(
                    left.view(),
                    ValueView::Range(..)
                        | ValueView::RangeExcl(..)
                        | ValueView::RangeExclStart(..)
                        | ValueView::RangeExclBoth(..)
                ) || matches!(
                    right.view(),
                    ValueView::Range(..)
                        | ValueView::RangeExcl(..)
                        | ValueView::RangeExclStart(..)
                        | ValueView::RangeExclBoth(..)
                ) {
                    crate::runtime::value_to_list(left) == crate::runtime::value_to_list(right)
                } else if crate::runtime::Interpreter::is_buf_value(left)
                    && crate::runtime::Interpreter::is_buf_value(right)
                {
                    crate::runtime::Interpreter::extract_buf_bytes(left)
                        == crate::runtime::Interpreter::extract_buf_bytes(right)
                } else {
                    self.stringify_test_value(left)? == self.stringify_test_value(right)?
                }
            }
            _ => false,
        };
        let detail = if ok {
            Vec::new()
        } else {
            let got = left
                .as_ref()
                .map(|v| self.value_for_diag(v))
                .unwrap_or_default();
            let expected = right
                .as_ref()
                .map(|v| self.value_for_diag(v))
                .unwrap_or_default();
            // Match raku's diagnostic: `#`-prefixed and single-quoted, routed
            // with the `# Failed test` block (stdout for a TODO test, else stderr).
            vec![
                format!("# expected: '{}'", expected),
                format!("#      got: '{}'", got),
            ]
        };
        self.test_ok_with_diag(ok, &desc, todo, &detail)?;
        Ok(Value::truth(ok))
    }

    pub(crate) fn stringify_test_value(&mut self, value: &Value) -> Result<String, RuntimeError> {
        match value.view() {
            ValueView::LazyList(list) => Ok(self
                .force_lazy_list(&list)?
                .iter()
                .map(|v| v.to_string_value())
                .collect::<Vec<_>>()
                .join(" ")),
            // A lazy IO lines iterator (e.g. `$fh.lines`) must be forced before
            // comparison so it stringifies as its contents rather than "(...)".
            ValueView::LazyIoLines { handle, words, .. } => {
                Ok(self.force_lazy_io_lines(handle, words)?.to_string_value())
            }
            // `is $got, $expected` compares via Raku's `eq` (Str coercion), so an
            // object whose class defines a user `Stringy`/`Str` must be compared
            // by that string value rather than its `.gist`.
            ValueView::Instance { class_name, .. } | ValueView::Package(class_name) => {
                let cn = class_name.resolve().to_string();
                let method = if self.has_user_method(&cn, "Stringy") {
                    Some("Stringy")
                } else if self.has_user_method(&cn, "Str") {
                    Some("Str")
                } else {
                    None
                };
                match method {
                    Some(m) => Ok(self
                        .call_method_with_values(value.clone(), m, vec![])?
                        .to_string_value()),
                    None => Ok(value.to_string_value()),
                }
            }
            _ => Ok(value.to_string_value()),
        }
    }

    pub(crate) fn test_fn_isnt(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let todo = Self::named_bool(args, "todo");
        let positional: Vec<&Value> = args
            .iter()
            .filter(|arg| !matches!(arg.view(), ValueView::Pair(key, _) if key == "todo"))
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
        Ok(Value::truth(ok))
    }

    pub(crate) fn test_fn_plan(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(reason) = Self::named_value(args, "skip-all") {
            // In Raku, `plan skip-all` inside a subtest uses `return` to exit the
            // callable. `return` only works in a Sub or Method, not a Block.
            // If we're inside a subtest with a Block callable, die with an error.
            if self.tap.subtest_depth() > 0
                && self.tap.subtest_callable_is_sub_last() == Some(false)
            {
                let msg = "Must give `subtest` a (Sub) or a (Method) to be able to use \
                           `skip-all` plan inside, but you gave a (Block)";
                self.output_sink_mut().stderr_output.push_str(msg);
                self.output_sink_mut().stderr_output.push('\n');
                self.exit_code = 1;
                self.halted = true;
                return Err(RuntimeError::new(msg));
            }
            self.tap.ensure_state().planned = Some(0);
            let reason_str = reason.to_string_value();
            if reason_str.is_empty() || reason_str == "True" {
                self.emit_output("1..0 # Skipped: no reason given\n");
            } else {
                self.emit_output(&format!("1..0 # Skipped: {}\n", reason_str));
            }
            self.halted = true;
        } else {
            let count = Self::positional_value_required(args, 0, "plan expects count")?;
            // `plan *` (a Whatever): the test count is unknown up front, so emit
            // no `1..N` header. Leave the plan unset (like never calling `plan`) —
            // the file's trailing `done-testing` emits `1..ran`, and without it no
            // header is emitted, matching Rakudo. Used by files that compute their
            // assertion count only as they run.
            if matches!(count.view(), ValueView::Whatever) {
                self.tap.ensure_state();
                return Ok(Value::NIL);
            }
            let planned = match count.view() {
                ValueView::Int(i) if i >= 0 => i as usize,
                ValueView::BigInt(i) => {
                    use num_traits::ToPrimitive;
                    let n = i.to_i128().unwrap_or(-1);
                    if n < 0 {
                        return Err(RuntimeError::new("plan expects Int"));
                    }
                    n as usize
                }
                ValueView::Num(f) if f >= 0.0 && f.fract() == 0.0 => f as usize,
                ValueView::Rat(n, d) if d != 0 && n >= 0 && n % d == 0 => (n / d) as usize,
                ValueView::FatRat(n, d) if d != 0 && n >= 0 && n % d == 0 => (n / d) as usize,
                _ => return Err(RuntimeError::new("plan expects Int")),
            };
            self.tap.ensure_state().planned = Some(planned);
            self.emit_output(&format!("1..{}\n", planned));
        }
        Ok(Value::NIL)
    }

    pub(crate) fn test_fn_done_testing(&mut self) -> Result<Value, RuntimeError> {
        let already_planned = {
            let state = self.tap.ensure_state();
            state.planned.is_some()
        };
        if !already_planned {
            let ran = {
                let state = self.tap.ensure_state();
                state.planned = Some(state.ran);
                state.ran
            };
            self.emit_output(&format!("1..{}\n", ran));
        }
        // Return True if all tests passed and plan matches, False otherwise
        let state = self.tap.state().unwrap();
        let plan_matches = match state.planned {
            Some(planned) => planned == state.ran,
            None => true,
        };
        let all_passed = state.failed == 0 && plan_matches;
        Ok(Value::truth(all_passed))
    }

    pub(crate) fn test_fn_skip(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 0);
        let count = match Self::positional_value(args, 1).map(Value::view) {
            Some(ValueView::Int(n)) => n.max(1) as usize,
            _ => 1usize,
        };
        // Raku TAP format: `ok N - # SKIP reason` with `#` in reason escaped as ` \#`
        let escaped_desc = desc.replace('#', " \\#");
        let mut lines = Vec::with_capacity(count);
        let state = self.tap.ensure_state();
        for _ in 0..count {
            state.next_ran();
            if escaped_desc.is_empty() {
                lines.push(format!("ok {} - # SKIP {}\n", state.ran, desc));
            } else {
                lines.push(format!("ok {} - # SKIP {}\n", state.ran, escaped_desc));
            }
        }
        for line in lines {
            self.emit_output(&line);
        }
        Ok(Value::NIL)
    }

    pub(crate) fn test_fn_skip_rest(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 0);
        let escaped_desc = desc.replace('#', " \\#");
        let mut lines = Vec::new();
        {
            let state = self.tap.ensure_state();
            if let Some(planned) = state.planned {
                while state.ran < planned {
                    state.next_ran();
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
        Ok(Value::NIL)
    }

    pub(crate) fn test_fn_bail_out(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 0);
        if desc.is_empty() {
            self.emit_output("Bail out!\n");
        } else {
            self.emit_output(&format!("Bail out! {}\n", desc));
        }
        self.halted = true;
        self.tap.set_bailed_out();
        Ok(Value::NIL)
    }

    pub(crate) fn test_fn_todo(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let reason = Self::positional_string(args, 0);
        let count = Self::positional_value(args, 1)
            .map(|v| v.to_string_value())
            .and_then(|s| s.parse::<usize>().ok())
            .unwrap_or(1);
        let state = self.tap.ensure_state();
        let start = state.ran + 1;
        let end = start + count - 1;
        state.force_todo.push(TodoRange { start, end, reason });
        Ok(Value::NIL)
    }
}
