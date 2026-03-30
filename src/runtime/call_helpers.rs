use super::*;

const TEST_CALLSITE_LINE_KEY: &str = "__mutsu_test_callsite_line";
const BACKEND_TODO_PREFIX: &str = "__mutsu_backend_todo__:";

impl Interpreter {
    pub(crate) fn set_pending_call_arg_sources(&mut self, sources: Option<Vec<Option<String>>>) {
        self.pending_call_arg_sources = sources;
    }

    pub(crate) fn take_pending_call_arg_sources(&mut self) -> Option<Vec<Option<String>>> {
        self.pending_call_arg_sources.take()
    }

    pub(crate) fn exec_call_values(
        &mut self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        match self.call_function(name, args.clone()) {
            Ok(_) => Ok(()),
            Err(e)
                if e.message
                    .contains("Unknown function (call_function fallback disabled):") =>
            {
                self.exec_call(name, args)
            }
            Err(e) => Err(e),
        }
    }

    pub(crate) fn exec_call_pairs_values(
        &mut self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        self.exec_call(name, args)
    }

    pub(crate) fn test_ok(
        &mut self,
        success: bool,
        desc: &str,
        todo: bool,
    ) -> Result<(), RuntimeError> {
        let mut line = String::new();
        let (record_failure, effective_todo) = {
            let state = self.test_state.get_or_insert_with(TestState::new);
            state.next_ran();
            let forced_reason = state
                .force_todo
                .iter()
                .find(|range| state.ran >= range.start && state.ran <= range.end)
                .map(|range| range.reason.as_str());
            let is_backend_todo =
                forced_reason.is_some_and(|reason| reason.starts_with(BACKEND_TODO_PREFIX));
            let todo = todo || forced_reason.is_some();
            if !success && !todo {
                state.failed += 1;
            }
            if success {
                line.push_str("ok ");
            } else {
                line.push_str("not ok ");
            }
            line.push_str(&state.ran.to_string());
            line.push_str(" - ");
            // Split description on newlines: first line goes on the TAP line,
            // remaining lines become `# continuation` lines (Raku TAP escaping).
            let mut desc_lines = desc.splitn(2, '\n');
            let first_desc_line = desc_lines.next().unwrap_or("");
            let rest_desc_lines = desc_lines.next(); // remaining after first \n
            if !first_desc_line.is_empty() {
                line.push_str(first_desc_line);
            }
            let show_todo = todo && (!is_backend_todo || !success);
            if show_todo {
                match forced_reason {
                    Some(reason) if !reason.is_empty() => {
                        let reason = reason.strip_prefix(BACKEND_TODO_PREFIX).unwrap_or(reason);
                        line.push_str(" # TODO ");
                        line.push_str(reason);
                    }
                    _ => line.push_str(" # TODO"),
                }
            }
            line.push('\n');
            // Append continuation lines for newlines in description
            if let Some(rest) = rest_desc_lines {
                for cont in rest.split('\n') {
                    line.push_str("# ");
                    line.push_str(cont);
                    line.push('\n');
                }
            }
            (!success, todo)
        };
        self.emit_output(&line);
        if record_failure {
            let to_stderr = !effective_todo && self.subtest_depth == 0;
            self.emit_test_failure_diag(desc, to_stderr);
            if !effective_todo && self.subtest_depth == 0 && self.raku_test_die_on_fail_enabled() {
                self.stderr_output
                    .push_str("Stopping test suite because of RAKU_TEST_DIE_ON_FAIL\n");
                self.exit_code = 255;
                self.halted = true;
            }
        }
        Ok(())
    }

    pub(super) fn emit_test_summary_diag(
        &mut self,
        planned: Option<usize>,
        ran: usize,
        failed: usize,
    ) {
        if let Some(planned) = planned
            && planned != ran
        {
            self.stderr_output
                .push_str(&format!("# Planned {} tests, but ran {}\n", planned, ran));
        }
        let plural = if failed == 1 { "" } else { "s" };
        self.stderr_output.push_str(&format!(
            "# You failed {} test{} of {}\n",
            failed, plural, ran
        ));
    }

    fn emit_test_failure_diag(&mut self, desc: &str, to_stderr: bool) {
        let line_no = self.current_test_failure_line();
        let at_line = if let Some(path) = &self.program_path {
            format!("at {} line {}", path, line_no)
        } else {
            format!("at line {}", line_no)
        };
        let mut emit = |msg: String| {
            if to_stderr {
                self.stderr_output.push_str(&msg);
                eprint!("{}", msg);
            } else {
                self.emit_output(&msg);
            }
        };
        if desc.is_empty() {
            emit(format!("# Failed test {}\n", at_line));
        } else {
            emit(format!("# Failed test '{}'\n", desc));
            emit(format!("# {}\n", at_line));
        }
    }

    pub(crate) fn sanitize_call_args(&self, args: &[Value]) -> (Vec<Value>, Option<i64>) {
        let mut out = Vec::with_capacity(args.len());
        let mut callsite_line = None;
        for arg in args {
            match arg {
                Value::Pair(key, value) => {
                    if key == TEST_CALLSITE_LINE_KEY {
                        callsite_line = match value.as_ref() {
                            Value::Int(i) => Some(*i),
                            Value::BigInt(i) => i.to_string().parse::<i64>().ok(),
                            Value::Num(n) => Some(*n as i64),
                            Value::Str(s) => s.parse::<i64>().ok(),
                            _ => None,
                        };
                        continue;
                    }
                    out.push(arg.clone());
                }
                Value::ValuePair(key, value) => {
                    if let Value::Str(name) = key.as_ref()
                        && name.as_str() == TEST_CALLSITE_LINE_KEY
                    {
                        callsite_line = match value.as_ref() {
                            Value::Int(i) => Some(*i),
                            Value::BigInt(i) => i.to_string().parse::<i64>().ok(),
                            Value::Num(n) => Some(*n as i64),
                            Value::Str(s) => s.parse::<i64>().ok(),
                            _ => None,
                        };
                        continue;
                    }
                    // ValuePair is kept as-is (positional pair, e.g. from (:a(3)))
                    out.push(arg.clone());
                }
                _ => out.push(arg.clone()),
            }
        }
        (out, callsite_line)
    }

    pub(crate) fn set_pending_callsite_line(&mut self, line: Option<i64>) {
        self.test_pending_callsite_line = line;
    }

    pub(crate) fn inject_pending_callsite_line(&mut self) {
        if let Some(line) = self.test_pending_callsite_line {
            // Only insert if ?LINE differs, to avoid triggering Arc::make_mut
            // deep clone on the CoW env in tight function call loops.
            let needs_update = self
                .env
                .get("?LINE")
                .is_none_or(|v| !matches!(v, Value::Int(l) if *l == line));
            if needs_update {
                self.env.insert("?LINE".to_string(), Value::Int(line));
            }
        }
    }

    pub(crate) fn push_test_assertion_context(&mut self, is_test_assertion: bool) -> bool {
        if !is_test_assertion {
            return false;
        }
        let line = self
            .test_assertion_line_stack
            .last()
            .copied()
            .or(self.test_pending_callsite_line)
            .unwrap_or(1);
        self.test_assertion_line_stack.push(line);
        true
    }

    pub(crate) fn pop_test_assertion_context(&mut self, pushed: bool) {
        if pushed {
            self.test_assertion_line_stack.pop();
        }
    }

    fn current_test_failure_line(&self) -> i64 {
        self.test_assertion_line_stack
            .last()
            .copied()
            .or(self.test_pending_callsite_line)
            .unwrap_or(1)
    }

    pub(crate) fn routine_is_test_assertion_by_name(&mut self, name: &str, args: &[Value]) -> bool {
        self.resolve_function_with_alias(name, args)
            .map(|def| def.is_test_assertion)
            .unwrap_or(false)
    }

    fn env_value(&self, key: &str) -> Option<Value> {
        if let Some(Value::Hash(env_hash)) = self.env.get("%*ENV")
            && let Some(val) = env_hash.get(key)
        {
            return Some(val.clone());
        }
        std::env::var_os(key).map(|v| Value::str(v.to_string_lossy().to_string()))
    }

    fn raku_test_die_on_fail_enabled(&self) -> bool {
        let Some(val) = self.env_value("RAKU_TEST_DIE_ON_FAIL") else {
            return false;
        };
        match val {
            Value::Nil => false,
            Value::Bool(b) => b,
            Value::Int(i) => i != 0,
            Value::BigInt(i) => *i.as_ref() != 0.into(),
            Value::Num(n) => n != 0.0,
            Value::Rat(n, d) | Value::FatRat(n, d) => d != 0 && n != 0,
            Value::Str(s) => !s.is_empty() && s.as_str() != "0",
            _ => val.truthy(),
        }
    }

    pub(super) fn positional_values(args: &[Value]) -> Vec<&Value> {
        args.iter()
            .filter(|v| !matches!(v, Value::Pair(_, _)))
            .collect()
    }

    pub(super) fn positional_value(args: &[Value], index: usize) -> Option<&Value> {
        let mut count = 0;
        for arg in args {
            if !matches!(arg, Value::Pair(_, _)) {
                if count == index {
                    return Some(arg);
                }
                count += 1;
            }
        }
        None
    }

    pub(super) fn positional_value_required<'a>(
        args: &'a [Value],
        index: usize,
        message: &str,
    ) -> Result<&'a Value, RuntimeError> {
        Self::positional_value(args, index).ok_or_else(|| RuntimeError::new(message))
    }

    pub(super) fn positional_string(args: &[Value], index: usize) -> String {
        Self::positional_value(args, index)
            .map(|v| v.to_string_value())
            .unwrap_or_default()
    }

    pub(super) fn named_bool(args: &[Value], name: &str) -> bool {
        for arg in args {
            if let Value::Pair(key, value) = arg
                && key == name
            {
                return value.truthy();
            }
        }
        false
    }

    pub(super) fn named_value(args: &[Value], name: &str) -> Option<Value> {
        for arg in args {
            if let Value::Pair(key, value) = arg
                && key == name
            {
                return Some(value.as_ref().clone());
            }
        }
        None
    }

    /// Convert a Failure value into an Err (simulating sink-context behavior).
    /// In Raku, a Failure in sink context throws its wrapped exception.
    /// This is used by throws-like to detect Failures returned by code blocks.
    pub(super) fn sink_failure_to_error(val: Value) -> Result<Value, RuntimeError> {
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &val
            && class_name == "Failure"
            && let Some(exception) = attributes.get("exception")
        {
            let message = if let Value::Instance {
                attributes: ex_attrs,
                ..
            } = exception
            {
                ex_attrs
                    .get("message")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default()
            } else {
                exception.to_string_value()
            };
            let mut err = RuntimeError::new(message);
            err.exception = Some(Box::new(exception.clone()));
            return Err(err);
        }
        Ok(val)
    }

    /// Sink a Proc with non-zero exitcode: throw X::Proc::Unsuccessful.
    pub(super) fn sink_proc_to_error(val: Value) -> Result<Value, RuntimeError> {
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &val
            && class_name.resolve() == "Proc"
        {
            let exitcode = match attributes.get("exitcode") {
                Some(Value::Int(i)) => *i,
                _ => 0,
            };
            if exitcode != 0 {
                let signal = match attributes.get("signal") {
                    Some(Value::Int(i)) => *i,
                    _ => 0,
                };
                let command = attributes
                    .get("command")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let msg = format!(
                    "The spawned command '{}' exited unsuccessfully (exit code: {}, signal: {})",
                    command, exitcode, signal
                );
                let mut ex_attrs = std::collections::HashMap::new();
                ex_attrs.insert("message".to_string(), Value::str(msg.clone()));
                ex_attrs.insert("proc".to_string(), val);
                let exception = Value::make_instance(
                    crate::symbol::Symbol::intern("X::Proc::Unsuccessful"),
                    ex_attrs,
                );
                let mut err = RuntimeError::new(msg);
                err.exception = Some(Box::new(exception));
                return Err(err);
            }
        }
        Ok(val)
    }
}
