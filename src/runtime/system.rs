use super::*;

impl Interpreter {
    fn eval_result_is_unresolved_bareword(&self, stmts: &[Stmt], result: &Value) -> bool {
        let [Stmt::Expr(Expr::BareWord(name))] = stmts else {
            return false;
        };
        matches!(result, Value::Str(s) if s == name)
            && !self.env().contains_key(name)
            && !self.has_class(name)
            && !self.has_function(name)
            && !self.has_multi_function(name)
            && !matches!(name.as_str(), "NaN" | "Inf" | "Empty")
    }

    /// Collect operator sub names from the current environment for EVAL pre-registration.
    /// Only collects circumfix/postcircumfix operators since they require parser support
    /// to recognize their delimiter syntax. Other operator categories (prefix, postfix,
    /// infix, term) work through runtime dispatch without parser pre-registration.
    fn collect_operator_sub_names(&self) -> Vec<String> {
        let mut names = Vec::new();
        for key in self.functions.keys() {
            // Strip package prefix if present (e.g. "GLOBAL::circumfix:<⌊ ⌋>")
            let name = if let Some(pos) = key.rfind("::") {
                &key[pos + 2..]
            } else {
                key.as_str()
            };
            if name.starts_with("circumfix:") || name.starts_with("postcircumfix:") {
                names.push(name.to_string());
            }
        }
        // Also check env for operator subs stored as variables
        for key in self.env.keys() {
            if key.starts_with("circumfix:") || key.starts_with("postcircumfix:") {
                names.push(key.clone());
            }
        }
        names
    }

    pub(super) fn eval_eval_string(&mut self, code: &str) -> Result<Value, RuntimeError> {
        let routine_snapshot = self.snapshot_routine_registry();
        let trimmed = code.trim();
        let previous_pod = self.env.get("=pod").cloned();
        self.collect_pod_blocks(trimmed);
        // Collect operator sub names so the parser recognizes them in EVAL context
        let op_names = self.collect_operator_sub_names();
        // General case: parse and evaluate as Raku code
        let mut result = crate::parser::parse_program_with_operators(trimmed, &op_names).and_then(
            |(stmts, _)| {
                let value = self.eval_block_value(&stmts)?;
                if self.eval_result_is_unresolved_bareword(&stmts, &value) {
                    return Err(RuntimeError::new("X::Undeclared::Symbols"));
                }
                Ok(value)
            },
        );
        // Fallback: parser still rejects forms like `~< foo bar >`.
        // Rewrite to an equivalent parenthesized form and try again.
        if result.is_err()
            && let Some(rewritten) = rewrite_prefixed_angle_list(trimmed)
        {
            result = parse_dispatch::parse_source(&rewritten).and_then(|(stmts, _)| {
                let value = self.eval_block_value(&stmts)?;
                if self.eval_result_is_unresolved_bareword(&stmts, &value) {
                    return Err(RuntimeError::new("X::Undeclared::Symbols"));
                }
                Ok(value)
            });
        }
        // Accept parenthesized statement lists like `(6;)` in EVAL.
        if result.is_err()
            && let Some(inner) = unwrap_parenthesized_statements(trimmed)
        {
            result = parse_dispatch::parse_source(inner).and_then(|(stmts, _)| {
                let value = self.eval_block_value(&stmts)?;
                if self.eval_result_is_unresolved_bareword(&stmts, &value) {
                    return Err(RuntimeError::new("X::Undeclared::Symbols"));
                }
                Ok(value)
            });
        }
        // EVAL should accept routine declarations in snippet context.
        // If unit-scope parsing rejects a declaration, retry inside an implicit block.
        if result.is_err()
            && trimmed.contains("sub ")
            && let Some(err) = result.as_ref().err()
            && err.message.contains("X::UnitScope::Invalid")
        {
            let wrapped = format!("{{ {}; }}", trimmed);
            result = parse_dispatch::parse_source(&wrapped).and_then(|(stmts, _)| {
                let value = self.eval_block_value(&stmts)?;
                if self.eval_result_is_unresolved_bareword(&stmts, &value) {
                    return Err(RuntimeError::new("X::Undeclared::Symbols"));
                }
                Ok(value)
            });
        }
        if let Some(saved) = previous_pod {
            self.env.insert("=pod".to_string(), saved);
        } else {
            self.env.remove("=pod");
        }
        self.restore_routine_registry(routine_snapshot);
        result
    }

    pub(super) fn callframe_value(&self, depth: usize) -> Option<Value> {
        self.routine_stack
            .len()
            .checked_sub(1 + depth)
            .and_then(|idx| self.routine_stack.get(idx))
            .map(|(package, name)| {
                let mut info = HashMap::new();
                info.insert("package".to_string(), Value::Str(package.clone()));
                info.insert("name".to_string(), Value::Str(name.clone()));
                info.insert("depth".to_string(), Value::Int(depth as i64));
                Value::hash(info)
            })
    }

    pub(super) fn seconds_from_value(val: Option<Value>) -> Option<f64> {
        val.and_then(|v| super::to_float_value(&v))
    }

    pub(super) fn duration_from_seconds(secs: Option<f64>) -> Duration {
        let secs = secs.unwrap_or(0.0).max(0.0);
        Duration::from_secs_f64(secs)
    }

    pub(super) fn system_time_from_value(val: Option<Value>) -> Option<SystemTime> {
        let secs = Self::seconds_from_value(val)?;
        let secs = secs.max(0.0);
        Some(UNIX_EPOCH + Duration::from_secs_f64(secs))
    }

    pub(super) fn hostname() -> String {
        env::var("HOSTNAME")
            .ok()
            .or_else(|| env::var("COMPUTERNAME").ok())
            .or_else(|| {
                Command::new("hostname")
                    .output()
                    .ok()
                    .and_then(|output| String::from_utf8(output.stdout).ok())
                    .map(|s| s.trim().to_string())
            })
            .unwrap_or_else(|| "localhost".to_string())
    }

    pub(super) fn resolve_host(name: &str) -> Vec<String> {
        format!("{}:0", name)
            .to_socket_addrs()
            .map(|addrs| {
                addrs
                    .map(|addr| addr.ip().to_string())
                    .filter(|ip| !ip.is_empty())
                    .collect()
            })
            .unwrap_or_default()
    }

    pub(super) fn make_os_name_value(name: String, mut addresses: Vec<String>) -> Value {
        if addresses.is_empty() {
            addresses.push("127.0.0.1".to_string());
        }
        let mut info = HashMap::new();
        info.insert("name".to_string(), Value::Str(name.clone()));
        info.insert("addr".to_string(), Value::Str(addresses[0].clone()));
        info.insert("aliases".to_string(), Value::array(Vec::new()));
        let addrs_values = addresses.into_iter().map(Value::Str).collect();
        info.insert("addrs".to_string(), Value::array(addrs_values));
        Value::hash(info)
    }

    pub(super) fn get_login_name() -> Option<String> {
        env::var("LOGNAME")
            .ok()
            .or_else(|| env::var("USER").ok())
            .or_else(|| env::var("USERNAME").ok())
    }

    pub(super) fn send_signal(pid: i64, signal: i64) -> bool {
        if pid == 0 {
            return false;
        }
        let pid_str = pid.to_string();
        #[cfg(unix)]
        {
            let mut cmd = Command::new("kill");
            cmd.arg(format!("-{}", signal));
            cmd.arg(&pid_str);
            cmd.status().map(|status| status.success()).unwrap_or(false)
        }
        #[cfg(windows)]
        {
            let mut cmd = Command::new("taskkill");
            cmd.args(["/PID", &pid_str, "/F"]);
            cmd.status().map(|status| status.success()).unwrap_or(false)
        }
        #[cfg(not(any(unix, windows)))]
        {
            false
        }
    }
}

fn rewrite_prefixed_angle_list(code: &str) -> Option<String> {
    let (prefix, rest) = if let Some(rest) = code.strip_prefix('~') {
        ('~', rest)
    } else if let Some(rest) = code.strip_prefix('+') {
        ('+', rest)
    } else if let Some(rest) = code.strip_prefix('?') {
        ('?', rest)
    } else {
        return None;
    };
    let inner = rest.trim_start();
    if !inner.starts_with('<') || !inner.ends_with('>') {
        return None;
    }
    Some(format!("{}({})", prefix, inner))
}

fn unwrap_parenthesized_statements(code: &str) -> Option<&str> {
    if !code.starts_with('(') || !code.ends_with(')') {
        return None;
    }
    let mut depth = 0usize;
    for (i, ch) in code.char_indices() {
        if ch == '(' {
            depth += 1;
        } else if ch == ')' {
            if depth == 0 {
                return None;
            }
            depth -= 1;
            if depth == 0 && i + ch.len_utf8() != code.len() {
                return None;
            }
        }
    }
    if depth != 0 {
        return None;
    }
    Some(&code[1..code.len() - 1])
}
