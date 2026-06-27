use super::builtins_system::*;
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
            .insert("$*CHROOT".to_string(), Value::str(repr.clone()));
        self.env
            .insert("*CHROOT".to_string(), Value::str(repr.clone()));
        let cwd_val = self.make_io_path_instance(&repr);
        self.env.insert("$*CWD".to_string(), cwd_val.clone());
        self.env.insert("*CWD".to_string(), cwd_val);
        Ok(Value::Bool(true))
    }

    /// Helper to create a Proc instance from process execution results.
    pub(super) fn make_proc_instance(
        exitcode: i64,
        signal: i64,
        pid: i64,
        command: Value,
        captured_err: Option<String>,
        captured_out: Option<String>,
    ) -> Value {
        Self::make_proc_instance_bin(
            exitcode,
            signal,
            pid,
            command,
            captured_err,
            captured_out,
            false,
        )
    }

    pub(super) fn make_proc_instance_bin(
        exitcode: i64,
        signal: i64,
        pid: i64,
        command: Value,
        captured_err: Option<String>,
        captured_out: Option<String>,
        bin: bool,
    ) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("exitcode".to_string(), Value::Int(exitcode));
        attrs.insert("signal".to_string(), Value::Int(signal));
        attrs.insert("pid".to_string(), Value::Int(pid));
        attrs.insert("command".to_string(), command);
        if bin {
            attrs.insert("bin".to_string(), Value::Bool(true));
        }
        let mut pipe_ids = Vec::new();
        if let Some(err_content) = captured_err {
            let pipe = Self::make_io_pipe_bin(err_content, bin);
            if let Value::Instance {
                attributes: ref a, ..
            } = pipe
                && let Some(Value::Int(id)) = a.as_map().get("pipe-id")
            {
                pipe_ids.push(*id);
            }
            attrs.insert("err".to_string(), pipe);
        }
        if let Some(out_content) = captured_out {
            let pipe = Self::make_io_pipe_bin(out_content, bin);
            if let Value::Instance {
                attributes: ref a, ..
            } = pipe
                && let Some(Value::Int(id)) = a.as_map().get("pipe-id")
            {
                pipe_ids.push(*id);
            }
            attrs.insert("out".to_string(), pipe);
        }
        let proc_val = Value::make_instance(Symbol::intern("Proc"), attrs);
        if !pipe_ids.is_empty()
            && let Ok(mut map) = pipe_proc_map().lock()
        {
            for id in pipe_ids {
                map.insert(id, proc_val.clone());
            }
        }
        proc_val
    }

    /// Create an IO::Pipe instance wrapping captured content.
    pub(super) fn make_io_pipe(content: String) -> Value {
        Self::make_io_pipe_bin(content, false)
    }

    pub(super) fn make_io_pipe_bin(content: String, bin: bool) -> Value {
        use std::sync::atomic::{AtomicI64, Ordering};
        static NEXT_PIPE_ID: AtomicI64 = AtomicI64::new(1);
        let id = NEXT_PIPE_ID.fetch_add(1, Ordering::SeqCst);
        if let Ok(mut map) = io_pipe_state_map().lock() {
            map.insert(
                id,
                IoPipeState {
                    content: content.clone(),
                    cursor: 0,
                    closed: false,
                },
            );
        }
        let mut attrs = HashMap::new();
        attrs.insert("content".to_string(), Value::str(content));
        attrs.insert("pipe-id".to_string(), Value::Int(id));
        if bin {
            attrs.insert("bin".to_string(), Value::Bool(true));
        }
        Value::make_instance(Symbol::intern("IO::Pipe"), attrs)
    }

    /// Extract named options (cwd, env, :err, :out) from arguments.
    pub(super) fn extract_proc_options(args: &[Value], skip: usize) -> ProcOptions {
        let mut opts = ProcOptions {
            cwd: None,
            env: HashMap::new(),
            capture_err: false,
            capture_out: false,
            capture_in: false,
            in_pipe_pid: None,
            in_pipe_content: None,
            bin: false,
            win_verbatim_args: false,
            out_handle_id: None,
            merge: false,
        };
        for value in args.iter().skip(skip) {
            if let Value::Hash(map) = value {
                for (key, inner) in map.iter() {
                    match key.as_str() {
                        "cwd" => {
                            opts.cwd = Some(inner.to_string_value());
                        }
                        "env" => {
                            if let Value::Hash(env_map) = inner {
                                for (ek, ev) in env_map.iter() {
                                    opts.env.insert(ek.clone(), ev.to_string_value());
                                }
                            }
                        }
                        "err" => {
                            opts.capture_err = inner.truthy();
                        }
                        "out" => {
                            Self::extract_out_option(inner, &mut opts);
                        }
                        "in" => {
                            Self::extract_in_option(inner, &mut opts);
                        }
                        "bin" => {
                            opts.bin = inner.truthy();
                        }
                        "merge" => {
                            opts.merge = inner.truthy();
                        }
                        "win-verbatim-args" => {
                            opts.win_verbatim_args = inner.truthy();
                        }
                        _ => {}
                    }
                }
            }
            if let Value::Pair(key, inner) = value {
                match key.as_str() {
                    "cwd" => opts.cwd = Some(inner.to_string_value()),
                    "env" => {
                        if let Value::Hash(env_map) = inner.as_ref() {
                            for (ek, ev) in env_map.iter() {
                                opts.env.insert(ek.clone(), ev.to_string_value());
                            }
                        }
                    }
                    "err" => opts.capture_err = inner.truthy(),
                    "out" => Self::extract_out_option(inner, &mut opts),
                    "in" => Self::extract_in_option(inner, &mut opts),
                    "bin" => opts.bin = inner.truthy(),
                    "merge" => opts.merge = inner.truthy(),
                    "win-verbatim-args" => opts.win_verbatim_args = inner.truthy(),
                    _ => {}
                }
            }
        }
        opts
    }

    fn extract_in_option(value: &Value, opts: &mut ProcOptions) {
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = value
            && class_name.resolve() == "IO::Pipe"
        {
            opts.capture_in = true;
            if let Some(Value::Int(pid)) = attributes.as_map().get("live-pid") {
                opts.in_pipe_pid = Some(*pid);
                return;
            }
            if let Some(Value::Int(pid)) = attributes.as_map().get("proc-pid") {
                opts.in_pipe_pid = Some(*pid);
                return;
            }
            let content = if let Some(Value::Int(id)) = attributes.as_map().get("pipe-id")
                && let Ok(mut map) = io_pipe_state_map().lock()
                && let Some(state) = map.get_mut(id)
            {
                let c = state.content[state.cursor..].to_string();
                state.cursor = state.content.len();
                c
            } else {
                attributes
                    .as_map()
                    .get("content")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default()
            };
            opts.in_pipe_content = Some(content);
            return;
        }
        opts.capture_in = value.truthy();
    }

    fn extract_out_option(value: &Value, opts: &mut ProcOptions) {
        // :out can be an IO::Handle (redirect stdout to that file handle)
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = value
            && class_name.resolve() == "IO::Handle"
            && let Some(Value::Int(id)) = attributes.as_map().get("handle")
        {
            opts.out_handle_id = Some(*id as usize);
            return;
        }
        opts.capture_out = value.truthy();
    }

    #[cfg(windows)]
    pub(super) fn apply_run_args(cmd: &mut Command, args: &[String], win_verbatim_args: bool) {
        use std::os::windows::process::CommandExt;

        if win_verbatim_args {
            if !args.is_empty() {
                // Raku's :win-verbatim-args forwards argv payload without Win32 quoting.
                cmd.raw_arg(args.join(" "));
            }
        } else {
            cmd.args(args);
        }
    }

    #[cfg(not(windows))]
    pub(super) fn apply_run_args(cmd: &mut Command, args: &[String], _win_verbatim_args: bool) {
        cmd.args(args);
    }

    pub(super) fn builtin_qx(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let command = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();

        let shell_args = vec![
            Value::str(command),
            Value::Pair("out".to_string(), Box::new(Value::Bool(true))),
            Value::Pair("err".to_string(), Box::new(Value::Bool(true))),
        ];
        let proc_value = self.builtin_shell(&shell_args)?;

        if let Value::Instance { attributes, .. } = proc_value {
            if let Some(err_pipe) = attributes.as_map().get("err") {
                let stderr = self.call_method_with_values(err_pipe.clone(), "slurp", vec![])?;
                let stderr_text = stderr.to_string_value();
                if !stderr_text.is_empty() {
                    eprint!("{}", stderr_text);
                    let _ = std::io::Write::flush(&mut std::io::stderr());
                }
            }
            if let Some(out_pipe) = attributes.as_map().get("out") {
                return self.call_method_with_values(out_pipe.clone(), "slurp", vec![]);
            }
        }

        Ok(Value::str(String::new()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extract_proc_options_reads_win_verbatim_pair() {
        let args = vec![Value::Pair(
            "win-verbatim-args".to_string(),
            Box::new(Value::Bool(true)),
        )];
        let opts = Interpreter::extract_proc_options(&args, 0);
        assert!(opts.win_verbatim_args);
    }

    #[test]
    fn extract_proc_options_reads_win_verbatim_hash() {
        let mut map = HashMap::new();
        map.insert("win-verbatim-args".to_string(), Value::Bool(true));
        let args = vec![Value::hash(map)];
        let opts = Interpreter::extract_proc_options(&args, 0);
        assert!(opts.win_verbatim_args);
    }
}
