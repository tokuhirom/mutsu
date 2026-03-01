use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    fn parse_and_eval_with_operators(
        &mut self,
        src: &str,
        op_names: &[String],
        op_assoc: &HashMap<String, String>,
        imported_names: &[String],
    ) -> Result<Value, RuntimeError> {
        crate::parser::parse_program_with_operators(src, op_names, op_assoc, imported_names)
            .and_then(|(stmts, _)| {
                let value = self.eval_block_value(&stmts)?;
                if self.eval_result_is_unresolved_bareword(&stmts, &value) {
                    return Err(RuntimeError::new("X::Undeclared::Symbols"));
                }
                Ok(value)
            })
    }

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
            let key_s = key.resolve();
            // Strip package prefix if present (e.g. "GLOBAL::circumfix:<⌊ ⌋>")
            let name = if let Some(pos) = key_s.rfind("::") {
                &key_s[pos + 2..]
            } else {
                key_s.as_str()
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

    fn collect_operator_assoc_map(&self) -> HashMap<String, String> {
        let mut assoc = HashMap::new();
        for (key, value) in &self.operator_assoc {
            let name = if let Some(pos) = key.rfind("::") {
                &key[pos + 2..]
            } else {
                key.as_str()
            };
            if name.starts_with("infix:<") {
                assoc.insert(name.to_string(), value.clone());
            }
        }
        assoc
    }

    fn collect_eval_imported_function_names(&self) -> Vec<String> {
        const TEST_EXPORTS: &[&str] = &[
            "ok",
            "nok",
            "is",
            "isnt",
            "is-deeply",
            "is-approx",
            "cmp-ok",
            "like",
            "unlike",
            "isa-ok",
            "does-ok",
            "can-ok",
            "lives-ok",
            "dies-ok",
            "eval-lives-ok",
            "eval-dies-ok",
            "throws-like",
            "fails-like",
            "pass",
            "flunk",
            "skip",
            "skip-rest",
            "todo",
            "diag",
            "plan",
            "done-testing",
            "bail-out",
            "subtest",
            "use-ok",
            "force_todo",
            "force-todo",
            "tap-ok",
        ];
        TEST_EXPORTS
            .iter()
            .map(|name| (*name).to_string())
            .collect()
    }

    pub(super) fn eval_eval_string(&mut self, code: &str) -> Result<Value, RuntimeError> {
        let routine_snapshot = self.snapshot_routine_registry();
        let trimmed = code.trim();
        if trimmed == "<>" || trimmed == "<STDIN>" {
            return Err(RuntimeError::new(
                "X::Obsolete: The degenerate case <> and old angle forms like <STDIN> are disallowed.",
            ));
        }
        let previous_pod = self.env.get("=pod").cloned();
        self.collect_pod_blocks(trimmed);
        // Collect operator sub names so the parser recognizes them in EVAL context
        let op_names = self.collect_operator_sub_names();
        let op_assoc = self.collect_operator_assoc_map();
        let imported_names = self.collect_eval_imported_function_names();
        let bracketed_stmt_inner = unwrap_bracketed_statements(trimmed)
            .filter(|inner| looks_like_bracketed_statement_list(inner));
        // General case: parse and evaluate as Raku code
        let mut result = if let Some(inner) = bracketed_stmt_inner {
            // EVAL q[[ ... ]] can yield one wrapper [] around statement lists.
            self.parse_and_eval_with_operators(inner, &op_names, &op_assoc, &imported_names)
                .or_else(|_| {
                    self.parse_and_eval_with_operators(
                        trimmed,
                        &op_names,
                        &op_assoc,
                        &imported_names,
                    )
                })
        } else {
            self.parse_and_eval_with_operators(trimmed, &op_names, &op_assoc, &imported_names)
        };
        for warning in crate::parser::take_parse_warnings() {
            self.write_warn_to_stderr(&warning);
        }
        // Fallback: parser still rejects forms like `~< foo bar >`.
        // Rewrite to an equivalent parenthesized form and try again.
        if result.is_err()
            && let Some(rewritten) = rewrite_prefixed_angle_list(trimmed)
        {
            result = self.parse_and_eval_with_operators(
                &rewritten,
                &op_names,
                &op_assoc,
                &imported_names,
            );
        }
        // Accept parenthesized statement lists like `(6;)` in EVAL.
        if result.is_err()
            && let Some(inner) = unwrap_parenthesized_statements(trimmed)
        {
            result =
                self.parse_and_eval_with_operators(inner, &op_names, &op_assoc, &imported_names);
        }
        // EVAL q[[ ... ]] sometimes carries one outer statement-list bracket pair.
        if result.is_err()
            && bracketed_stmt_inner.is_none()
            && let Some(inner) = unwrap_bracketed_statements(trimmed)
        {
            result =
                self.parse_and_eval_with_operators(inner, &op_names, &op_assoc, &imported_names);
        }
        // EVAL should accept routine declarations in snippet context.
        // If unit-scope parsing rejects a declaration, retry inside an implicit block.
        if result.is_err()
            && trimmed.contains("sub ")
            && let Some(err) = result.as_ref().err()
            && err.message.contains("X::UnitScope::Invalid")
        {
            let wrapped = format!("{{ {}; }}", trimmed);
            result =
                self.parse_and_eval_with_operators(&wrapped, &op_names, &op_assoc, &imported_names);
        }
        if let Some(saved) = previous_pod {
            self.env.insert("=pod".to_string(), saved);
        } else {
            self.env.remove("=pod");
        }
        self.restore_routine_registry(routine_snapshot);
        result
    }

    /// Build a CallFrame Instance for the given depth.
    /// `callsite_line` is the line where `callframe()` was called (from hidden arg).
    pub(super) fn callframe_value(
        &self,
        depth: usize,
        callsite_line: Option<i64>,
    ) -> Option<Value> {
        let file = self
            .env
            .get("?FILE")
            .map(|v| v.to_string_value())
            .unwrap_or_default();

        if depth == 0 {
            // Current frame: use current env, current file/line, current code
            let line = callsite_line.unwrap_or(0);
            let code = self.current_routine_sub_value();
            let my_hash = self.build_lexical_hash(&self.env, None);
            let mut attrs = HashMap::new();
            attrs.insert("line".to_string(), Value::Int(line));
            attrs.insert("file".to_string(), Value::Str(file));
            attrs.insert("code".to_string(), code);
            attrs.insert("my".to_string(), my_hash);
            attrs.insert("inline".to_string(), Value::Bool(false));
            attrs.insert("__depth".to_string(), Value::Int(0));
            attrs.insert("annotations".to_string(), self.build_annotations(&attrs));
            return Some(Value::make_instance(Symbol::intern("CallFrame"), attrs));
        }

        // depth >= 1: walk up the caller env stack
        let stack_len = self.callframe_stack.len();
        if depth > stack_len {
            return None;
        }
        let entry = &self.callframe_stack[stack_len - depth];
        let code = entry.code.clone().unwrap_or(Value::Nil);
        let my_hash = self.build_lexical_hash(&entry.env, Some(depth));
        let mut attrs = HashMap::new();
        attrs.insert("line".to_string(), Value::Int(entry.line));
        attrs.insert("file".to_string(), Value::Str(entry.file.clone()));
        attrs.insert("code".to_string(), code);
        attrs.insert("my".to_string(), my_hash);
        attrs.insert("inline".to_string(), Value::Bool(false));
        attrs.insert("__depth".to_string(), Value::Int(depth as i64));
        attrs.insert("annotations".to_string(), self.build_annotations(&attrs));
        Some(Value::make_instance(Symbol::intern("CallFrame"), attrs))
    }

    fn current_routine_sub_value(&self) -> Value {
        // Try to find the current routine as a Sub value from the block stack
        for v in self.block_stack.iter().rev() {
            if matches!(v, Value::Sub(_)) {
                return v.clone();
            }
        }
        // Fallback: look at the most recent callframe stack entry for the code
        if let Some(entry) = self.callframe_stack.last()
            && let Some(ref code) = entry.code
        {
            return code.clone();
        }
        Value::Nil
    }

    fn build_lexical_hash(
        &self,
        env: &HashMap<String, Value>,
        callframe_depth: Option<usize>,
    ) -> Value {
        let mut hash = HashMap::new();
        for (k, v) in env {
            // Skip internal keys and special variables
            if k.starts_with("__") || k.starts_with('?') || k.starts_with('*') || k.starts_with('=')
            {
                continue;
            }
            // Skip type names, enum values, and signal names
            if k.chars().next().is_some_and(|c| c.is_uppercase()) {
                continue;
            }
            // Add sigil prefix based on the env key convention:
            // - Keys starting with '@' or '%' already have sigils (array/hash vars)
            // - Other keys are scalar variables that need '$' prefix
            let sigiled = if k.starts_with('@') || k.starts_with('%') || k.starts_with('&') {
                k.clone()
            } else {
                format!("${}", k)
            };
            hash.insert(sigiled, v.clone());
        }
        // Store callframe depth so assignments can write back to the caller env
        if let Some(depth) = callframe_depth {
            hash.insert("__callframe_depth".to_string(), Value::Int(depth as i64));
        }
        Value::hash(hash)
    }

    fn build_annotations(&self, attrs: &HashMap<String, Value>) -> Value {
        let mut map = HashMap::new();
        if let Some(file) = attrs.get("file") {
            map.insert("file".to_string(), file.clone());
        }
        if let Some(line) = attrs.get("line") {
            map.insert("line".to_string(), line.clone());
        }
        Value::hash(map)
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
    let inner = &code[1..code.len() - 1];
    // Restrict this fallback to statement-list snippets like `(6;)`.
    // Plain parenthesized expressions should keep normal parse behavior.
    if !inner.contains(';') {
        return None;
    }
    Some(inner)
}

fn unwrap_bracketed_statements(code: &str) -> Option<&str> {
    if !code.starts_with('[') || !code.ends_with(']') {
        return None;
    }
    let mut depth = 0usize;
    for (i, ch) in code.char_indices() {
        if ch == '[' {
            depth += 1;
        } else if ch == ']' {
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

fn looks_like_bracketed_statement_list(inner: &str) -> bool {
    let trimmed = inner.trim_start();
    if !inner.contains(';') {
        return false;
    }
    matches!(
        trimmed.split_whitespace().next(),
        Some(
            "my" | "our"
                | "state"
                | "sub"
                | "multi"
                | "proto"
                | "class"
                | "role"
                | "grammar"
                | "module"
                | "unit"
                | "use"
                | "need"
        )
    )
}
