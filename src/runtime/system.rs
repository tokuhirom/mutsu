use super::*;

impl Interpreter {
    pub(super) fn eval_eval_string(&mut self, code: &str) -> Result<Value, RuntimeError> {
        let trimmed = code.trim();
        // Handle angle-bracket word lists: <a b c>, ~<a b>, +<a b>, ?<a b>
        let (prefix, rest) = if let Some(pos) = trimmed.find('<') {
            (trimmed.chars().next().unwrap_or(' '), &trimmed[pos..])
        } else {
            (' ', trimmed)
        };
        let start = rest.find('<');
        let end = rest.rfind('>');
        if (prefix != ' ' || (start.is_some() && trimmed.starts_with('<')))
            && let (Some(s), Some(e)) = (start, end)
        {
            let inner = &rest[s + 1..e];
            let words: Vec<&str> = inner.split_whitespace().collect();
            return Ok(match prefix {
                '~' => Value::Str(words.join(" ")),
                '+' => Value::Int(words.len() as i64),
                '?' => Value::Bool(!words.is_empty()),
                _ => Value::Str(words.join(" ")),
            });
        }
        // General case: parse and evaluate as Raku code
        let mut lexer = Lexer::new(trimmed);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            let end = matches!(token.kind, TokenKind::Eof);
            tokens.push(token);
            if end {
                break;
            }
        }
        let mut parser = Parser::new(tokens);
        match parser.parse_program() {
            Ok(stmts) => self.eval_block_value(&stmts),
            Err(e) => Err(e),
        }
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
                Value::Hash(info)
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
        info.insert("aliases".to_string(), Value::Array(Vec::new()));
        let addrs_values = addresses.into_iter().map(Value::Str).collect();
        info.insert("addrs".to_string(), Value::Array(addrs_values));
        Value::Hash(info)
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
