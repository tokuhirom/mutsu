use super::*;

impl Interpreter {
    pub(super) fn builtin_gethost(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let host_str = args.first().map(|v| v.to_string_value());
        let hostname = host_str.unwrap_or_else(Self::hostname);
        let addrs = Self::resolve_host(&hostname);
        Ok(Self::make_os_name_value(hostname, addrs))
    }

    pub(super) fn builtin_chroot(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let path_str = args
            .first()
            .map(|v| v.to_string_value())
            .or_else(|| {
                self.get_dynamic_string("$*CWD")
                    .or_else(|| self.get_dynamic_string("$*CHROOT"))
            })
            .unwrap_or_else(|| ".".to_string());
        let path_buf = PathBuf::from(path_str.clone());
        if !path_buf.is_dir() {
            return Ok(Value::Bool(false));
        }
        let canonical = path_buf.canonicalize().unwrap_or_else(|_| path_buf.clone());
        if std::env::set_current_dir(&canonical).is_err() {
            return Ok(Value::Bool(false));
        }
        self.chroot_root = Some(canonical.clone());
        let repr = Self::stringify_path(&canonical);
        self.env
            .insert("$*CHROOT".to_string(), Value::Str(repr.clone()));
        self.env
            .insert("*CHROOT".to_string(), Value::Str(repr.clone()));
        self.env
            .insert("$*CWD".to_string(), Value::Str(repr.clone()));
        self.env.insert("*CWD".to_string(), Value::Str(repr));
        Ok(Value::Bool(true))
    }

    /// Helper to create a Proc instance from process execution results.
    fn make_proc_instance(exitcode: i64, signal: i64, pid: i64, command: Value) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("exitcode".to_string(), Value::Int(exitcode));
        attrs.insert("signal".to_string(), Value::Int(signal));
        attrs.insert("pid".to_string(), Value::Int(pid));
        attrs.insert("command".to_string(), command);
        Value::make_instance("Proc".to_string(), attrs)
    }

    /// Extract named options (cwd, env) from arguments.
    fn extract_proc_options(
        args: &[Value],
        skip: usize,
    ) -> (Option<String>, HashMap<String, String>) {
        let mut cwd_opt: Option<String> = None;
        let mut env_opts: HashMap<String, String> = HashMap::new();
        for value in args.iter().skip(skip) {
            if let Value::Hash(map) = value {
                for (key, inner) in map {
                    match key.as_str() {
                        "cwd" => {
                            cwd_opt = Some(inner.to_string_value());
                        }
                        "env" => {
                            if let Value::Hash(env_map) = inner {
                                for (ek, ev) in env_map {
                                    env_opts.insert(ek.clone(), ev.to_string_value());
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            if let Value::Pair(key, inner) = value
                && key == "cwd"
            {
                cwd_opt = Some(inner.to_string_value());
            }
        }
        (cwd_opt, env_opts)
    }

    pub(super) fn builtin_run(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Ok(Self::make_proc_instance(
                -1,
                0,
                0,
                Value::Str(String::new()),
            ));
        }

        // Collect positional string args and named options
        let mut positional: Vec<String> = Vec::new();
        let mut named_start = args.len();
        for (i, arg) in args.iter().enumerate() {
            match arg {
                Value::Hash(_) | Value::Pair(_, _) => {
                    if named_start == args.len() {
                        named_start = i;
                    }
                }
                _ => {
                    positional.push(arg.to_string_value());
                }
            }
        }

        let (cwd_opt, env_opts) = Self::extract_proc_options(args, 0);

        if positional.is_empty() {
            return Ok(Self::make_proc_instance(
                -1,
                0,
                0,
                Value::Str(String::new()),
            ));
        }

        let program = &positional[0];
        let rest_args = &positional[1..];

        // Build the command tuple for .command attribute
        let command_val = Value::Array(positional.iter().map(|s| Value::Str(s.clone())).collect());

        let mut cmd = Command::new(program);
        cmd.args(rest_args);
        // Suppress stdout/stderr by default (like Raku's run)
        cmd.stdout(std::process::Stdio::null());
        cmd.stderr(std::process::Stdio::null());

        if let Some(cwd) = cwd_opt {
            cmd.current_dir(cwd);
        }
        for (k, v) in env_opts {
            cmd.env(k, v);
        }

        match cmd.spawn() {
            Ok(mut child) => {
                let pid = child.id() as i64;
                match child.wait() {
                    Ok(status) => {
                        let exitcode = status.code().unwrap_or(-1) as i64;
                        #[cfg(unix)]
                        let signal = {
                            use std::os::unix::process::ExitStatusExt;
                            status.signal().unwrap_or(0) as i64
                        };
                        #[cfg(not(unix))]
                        let signal = 0i64;
                        Ok(Self::make_proc_instance(exitcode, signal, pid, command_val))
                    }
                    Err(_) => Ok(Self::make_proc_instance(-1, 0, pid, command_val)),
                }
            }
            Err(_) => {
                // Process failed to start
                Ok(Self::make_proc_instance(-1, 0, 0, command_val))
            }
        }
    }

    pub(super) fn builtin_shell(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let command_str = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        if command_str.is_empty() {
            return Ok(Self::make_proc_instance(
                -1,
                0,
                0,
                Value::Str(String::new()),
            ));
        }

        let (cwd_opt, env_opts) = Self::extract_proc_options(args, 1);

        let command_val = Value::Str(command_str.clone());

        let mut command = if cfg!(windows) {
            let mut cmd = Command::new("cmd");
            cmd.arg("/C").arg(&command_str);
            cmd
        } else {
            let mut cmd = Command::new("sh");
            cmd.arg("-c").arg(&command_str);
            cmd
        };
        // Suppress stdout/stderr by default
        command.stdout(std::process::Stdio::null());
        command.stderr(std::process::Stdio::null());

        if let Some(cwd) = cwd_opt {
            command.current_dir(cwd);
        }
        for (k, v) in env_opts {
            command.env(k, v);
        }

        match command.spawn() {
            Ok(mut child) => {
                let pid = child.id() as i64;
                match child.wait() {
                    Ok(status) => {
                        let exitcode = status.code().unwrap_or(-1) as i64;
                        #[cfg(unix)]
                        let signal = {
                            use std::os::unix::process::ExitStatusExt;
                            status.signal().unwrap_or(0) as i64
                        };
                        #[cfg(not(unix))]
                        let signal = 0i64;
                        Ok(Self::make_proc_instance(exitcode, signal, pid, command_val))
                    }
                    Err(_) => Ok(Self::make_proc_instance(-1, 0, pid, command_val)),
                }
            }
            Err(_) => {
                // Process failed to start
                Ok(Self::make_proc_instance(-1, 0, 0, command_val))
            }
        }
    }

    pub(super) fn builtin_kill(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let signal = args.first().map(super::to_int).unwrap_or(15);
        let mut success = true;
        let mut had_pid = false;
        for val in args.iter().skip(1) {
            had_pid = true;
            let pid = super::to_int(val);
            success &= Self::send_signal(pid, signal);
        }
        if !had_pid {
            success = false;
        }
        Ok(Value::Bool(success))
    }

    pub(super) fn builtin_syscall(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let Some(val) = args.first() else {
            return Ok(Value::Nil);
        };
        let num = super::to_int(val);
        if num == 0 {
            let pid = self
                .env
                .get("*PID")
                .and_then(|v| match v {
                    Value::Int(i) => Some(*i),
                    _ => None,
                })
                .unwrap_or_else(|| std::process::id() as i64);
            return Ok(Value::Int(pid));
        }
        Ok(Value::Int(-1))
    }

    pub(super) fn builtin_sleep(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let duration = Self::duration_from_seconds(Self::seconds_from_value(args.first().cloned()));
        thread::sleep(duration);
        Ok(Value::Nil)
    }

    pub(super) fn builtin_sleep_timer(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let duration = Self::duration_from_seconds(Self::seconds_from_value(args.first().cloned()));
        let start = Instant::now();
        thread::sleep(duration);
        let elapsed = start.elapsed();
        let remaining = duration.checked_sub(elapsed).unwrap_or_default();
        Ok(Value::Num(remaining.as_secs_f64()))
    }

    pub(super) fn builtin_sleep_till(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(target_time) = Self::system_time_from_value(args.first().cloned()) {
            let now = SystemTime::now();
            if target_time <= now {
                return Ok(Value::Bool(false));
            }
            if let Ok(diff) = target_time.duration_since(now) {
                thread::sleep(diff);
                return Ok(Value::Bool(true));
            }
        }
        Ok(Value::Bool(false))
    }
}
