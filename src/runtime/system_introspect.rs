use super::*;

impl Interpreter {
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
            attrs.insert("file".to_string(), Value::str(file));
            Self::insert_callframe_code_attrs(&mut attrs, &code);
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
        attrs.insert("file".to_string(), Value::str(entry.file.clone()));
        Self::insert_callframe_code_attrs(&mut attrs, &code);
        attrs.insert("code".to_string(), code);
        attrs.insert("my".to_string(), my_hash);
        attrs.insert("inline".to_string(), Value::Bool(false));
        attrs.insert("__depth".to_string(), Value::Int(depth as i64));
        attrs.insert("annotations".to_string(), self.build_annotations(&attrs));
        Some(Value::make_instance(Symbol::intern("CallFrame"), attrs))
    }

    /// Extract subname, package, subtype, and sub attributes from a code value
    /// and insert them into the CallFrame attributes map.
    fn insert_callframe_code_attrs(attrs: &mut HashMap<String, Value>, code: &Value) {
        match code {
            Value::Sub(sd) => {
                let name = sd.name.resolve();
                attrs.insert("subname".to_string(), Value::str(name.to_string()));
                let pkg = sd.package.resolve();
                attrs.insert("package".to_string(), Value::str(pkg.to_string()));
                attrs.insert("subtype".to_string(), Value::str("SubRoutine".to_string()));
                attrs.insert("sub".to_string(), code.clone());
            }
            _ => {
                attrs.insert("subname".to_string(), Value::str(String::new()));
                attrs.insert("package".to_string(), Value::str(String::new()));
                attrs.insert("subtype".to_string(), Value::str(String::new()));
                attrs.insert("sub".to_string(), Value::Nil);
            }
        }
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

    fn build_lexical_hash(&self, env: &Env, callframe_depth: Option<usize>) -> Value {
        let mut hash = HashMap::new();
        for (k, v) in env.iter() {
            // Skip internal keys and special variables
            if k.starts_with("__") || k.starts_with("?") || k.starts_with("*") || k.starts_with("=")
            {
                continue;
            }
            // Skip type names, enum values, and signal names
            k.with_str(|s| {
                if s.chars().next().is_some_and(|c| c.is_uppercase()) {
                    return;
                }
                // Add sigil prefix based on the env key convention:
                // - Keys starting with '@' or '%' already have sigils (array/hash vars)
                // - Other keys are scalar variables that need '$' prefix
                let sigiled = if s.starts_with('@') || s.starts_with('%') || s.starts_with('&') {
                    s.to_string()
                } else {
                    format!("${}", s)
                };
                hash.insert(sigiled, v.clone());
            });
        }
        // Store callframe depth so assignments can write back to the caller env
        if let Some(depth) = callframe_depth {
            hash.insert("__callframe_depth".to_string(), Value::Int(depth as i64));
        }
        Value::hash(hash)
    }

    pub(crate) fn build_annotations(&self, attrs: &HashMap<String, Value>) -> Value {
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
        info.insert("name".to_string(), Value::str(name.clone()));
        info.insert("addr".to_string(), Value::str(addresses[0].clone()));
        info.insert("aliases".to_string(), Value::array(Vec::new()));
        let addrs_values = addresses.into_iter().map(Value::str).collect();
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
