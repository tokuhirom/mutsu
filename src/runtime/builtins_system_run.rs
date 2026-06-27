use super::builtins_system::*;
use super::*;

impl Interpreter {
    pub(super) fn builtin_run(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Ok(Self::make_proc_instance(
                -1,
                0,
                0,
                Value::str(String::new()),
                None,
                None,
            ));
        }

        // Collect positional string args and named options
        let mut positional: Vec<String> = Vec::new();
        let mut first_arg_io_path = false;
        for (idx, arg) in args.iter().enumerate() {
            let arg = match arg {
                Value::Capture { named, .. }
                    if matches!(named.get("__mutsu_varref_name"), Some(Value::Str(_)))
                        && named.contains_key("__mutsu_varref_value") =>
                {
                    named.get("__mutsu_varref_value").unwrap_or(arg)
                }
                _ => arg,
            };
            match arg {
                Value::Hash(_) | Value::Pair(_, _) => {}
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if idx == 0 && class_name.resolve() == "IO::Path" => {
                    first_arg_io_path = true;
                    if let Some(path) = attributes.as_map().get("path").map(Value::to_string_value)
                    {
                        positional.push(path);
                    } else {
                        positional.push(arg.to_string_value());
                    }
                }
                Value::Array(elems, _) => {
                    for elem in elems.iter() {
                        positional.push(Value::to_string_value(elem));
                    }
                }
                _ => {
                    positional.push(arg.to_string_value());
                }
            }
        }

        let opts = Self::extract_proc_options(args, 0);

        if positional.is_empty() {
            return Ok(Self::make_proc_instance(
                -1,
                0,
                0,
                Value::str(String::new()),
                opts.capture_err.then(String::new),
                opts.capture_out.then(String::new),
            ));
        }

        let program = &positional[0];
        let rest_args = &positional[1..];

        // Build the command tuple for .command attribute
        let command_val = Value::array(positional.iter().map(|s| Value::str(s.clone())).collect());
        // run() defaults the child's working directory to the dynamic $*CWD
        // (set by `indir`), not the interpreter process's actual cwd.
        let opts_cwd = opts
            .cwd
            .clone()
            .or_else(|| self.get_dynamic_string("$*CWD"));
        let opts_env = opts.env.clone();

        let mut cmd = Command::new(program);
        Self::apply_run_args(&mut cmd, rest_args, opts.win_verbatim_args);

        // Handle :out with IO::Handle — redirect stdout to that file
        let mut stdout_file_for_merge: Option<std::fs::File> = None;
        if let Some(handle_id) = opts.out_handle_id {
            let table = self.io_handles();
            let state = table
                .map
                .get(&handle_id)
                .ok_or_else(|| RuntimeError::new("Invalid IO::Handle for :out"))?;
            let file = state
                .file
                .as_ref()
                .ok_or_else(|| RuntimeError::new(":out IO::Handle has no file"))?;
            let cloned = file
                .try_clone()
                .map_err(|e| RuntimeError::new(format!("Cannot dup file for :out: {e}")))?;
            if opts.merge {
                stdout_file_for_merge =
                    Some(cloned.try_clone().map_err(|e| {
                        RuntimeError::new(format!("Cannot dup file for :merge: {e}"))
                    })?);
            }
            cmd.stdout(std::process::Stdio::from(cloned));
        } else if opts.capture_out {
            cmd.stdout(std::process::Stdio::piped());
        } else {
            cmd.stdout(std::process::Stdio::null());
        }
        if opts.merge {
            if let Some(file) = stdout_file_for_merge {
                cmd.stderr(std::process::Stdio::from(file));
            } else {
                // :merge without :out(file) — stderr goes to same pipe as stdout
                cmd.stderr(std::process::Stdio::piped());
            }
        } else if opts.capture_err {
            cmd.stderr(std::process::Stdio::piped());
        } else {
            cmd.stderr(std::process::Stdio::null());
        }
        let mut piped_from_live = false;
        if let Some(src_pid) = opts.in_pipe_pid {
            let mut child_stdout: Option<std::process::ChildStdout> = None;
            if let Ok(mut map) = live_proc_map().lock()
                && let Some(state) = map.get_mut(&src_pid)
            {
                child_stdout = state.child.stdout.take();
            }
            if let Some(stdout) = child_stdout {
                cmd.stdin(std::process::Stdio::from(stdout));
                piped_from_live = true;
            } else {
                cmd.stdin(std::process::Stdio::piped());
            }
        } else if opts.capture_in || opts.in_pipe_content.is_some() {
            cmd.stdin(std::process::Stdio::piped());
        }

        if let Some(cwd) = opts_cwd.clone() {
            cmd.current_dir(cwd);
        }
        for (k, v) in &opts_env {
            cmd.env(k, v);
        }

        match cmd.spawn() {
            Ok(mut child) => {
                let pid = child.id() as i64;

                if let Some(content) = &opts.in_pipe_content
                    && let Some(mut stdin) = child.stdin.take()
                {
                    use std::io::Write;
                    let _ = stdin.write_all(content.as_bytes());
                }

                let needs_live =
                    (opts.capture_in && opts.in_pipe_content.is_none()) || piped_from_live;
                if needs_live {
                    let stdin_handle = child.stdin.take();
                    // Store child in global map for later access
                    if let Ok(mut map) = live_proc_map().lock() {
                        map.insert(
                            pid,
                            LiveProcState {
                                child,
                                capture_out: opts.capture_out,
                                capture_err: opts.capture_err,
                            },
                        );
                    }
                    // Create an IO::Pipe for stdin
                    let in_pipe = if let Some(stdin) = stdin_handle {
                        // Store in proc_stdin_map for .print/.close
                        if let Ok(mut map) = super::native_methods::proc_stdin_map().lock() {
                            map.insert(pid as u32, Arc::new(std::sync::Mutex::new(Some(stdin))));
                        }
                        let mut in_attrs = HashMap::new();
                        in_attrs.insert("proc-pid".to_string(), Value::Int(pid));
                        Value::make_instance(Symbol::intern("IO::Pipe"), in_attrs)
                    } else {
                        Value::Nil
                    };
                    let mut attrs = HashMap::new();
                    attrs.insert("exitcode".to_string(), Value::Int(-1));
                    attrs.insert("signal".to_string(), Value::Int(0));
                    attrs.insert("pid".to_string(), Value::Int(pid));
                    attrs.insert("command".to_string(), command_val);
                    attrs.insert("in".to_string(), in_pipe);
                    attrs.insert("live".to_string(), Value::Bool(true));
                    if opts.bin {
                        attrs.insert("bin".to_string(), Value::Bool(true));
                    }
                    let proc = Value::make_instance(Symbol::intern("Proc"), attrs);
                    if let Ok(mut map) = proc_by_pid_map().lock() {
                        map.insert(pid, proc.clone());
                    }
                    return Ok(proc);
                }

                let captured_out = if opts.capture_out {
                    child.stdout.take().map(|mut s| {
                        let mut buf = String::new();
                        use std::io::Read;
                        let _ = s.read_to_string(&mut buf);
                        buf
                    })
                } else {
                    None
                };
                let captured_err = if opts.capture_err {
                    child.stderr.take().map(|mut s| {
                        let mut buf = String::new();
                        use std::io::Read;
                        let _ = s.read_to_string(&mut buf);
                        buf
                    })
                } else {
                    None
                };
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
                        Ok(Self::make_proc_instance_bin(
                            exitcode,
                            signal,
                            pid,
                            command_val,
                            captured_err,
                            captured_out,
                            opts.bin,
                        ))
                    }
                    Err(_) => Ok(Self::make_proc_instance_bin(
                        -1,
                        0,
                        pid,
                        command_val,
                        captured_err,
                        captured_out,
                        opts.bin,
                    )),
                }
            }
            Err(err) => {
                let os_error = err.to_string();
                // Fallback for cases where $*EXECUTABLE is passed as an IO::Path-ish value
                // that stringifies ambiguously. Retry with current_exe.
                if first_arg_io_path || program == "$*EXECUTABLE" || program.ends_with("mutsu") {
                    let fallback = Some(
                        Self::resolved_current_executable_path()
                            .to_string_lossy()
                            .to_string(),
                    );
                    if let Some(exe) = fallback {
                        let mut retry = Command::new(exe);
                        Self::apply_run_args(&mut retry, rest_args, opts.win_verbatim_args);
                        if opts.capture_out {
                            retry.stdout(std::process::Stdio::piped());
                        } else {
                            retry.stdout(std::process::Stdio::null());
                        }
                        if opts.capture_err {
                            retry.stderr(std::process::Stdio::piped());
                        } else {
                            retry.stderr(std::process::Stdio::null());
                        }
                        if let Some(cwd) = opts_cwd.clone() {
                            retry.current_dir(cwd);
                        }
                        for (k, v) in &opts_env {
                            retry.env(k, v);
                        }
                        if let Ok(mut child) = retry.spawn() {
                            let pid = child.id() as i64;
                            let captured_out = if opts.capture_out {
                                child.stdout.take().map(|mut s| {
                                    let mut buf = String::new();
                                    use std::io::Read;
                                    let _ = s.read_to_string(&mut buf);
                                    buf
                                })
                            } else {
                                None
                            };
                            let captured_err = if opts.capture_err {
                                child.stderr.take().map(|mut s| {
                                    let mut buf = String::new();
                                    use std::io::Read;
                                    let _ = s.read_to_string(&mut buf);
                                    buf
                                })
                            } else {
                                None
                            };
                            return match child.wait() {
                                Ok(status) => {
                                    let exitcode = status.code().unwrap_or(-1) as i64;
                                    #[cfg(unix)]
                                    let signal = {
                                        use std::os::unix::process::ExitStatusExt;
                                        status.signal().unwrap_or(0) as i64
                                    };
                                    #[cfg(not(unix))]
                                    let signal = 0i64;
                                    Ok(Self::make_proc_instance(
                                        exitcode,
                                        signal,
                                        pid,
                                        command_val,
                                        captured_err,
                                        captured_out,
                                    ))
                                }
                                Err(_) => Ok(Self::make_proc_instance(
                                    -1,
                                    0,
                                    pid,
                                    command_val,
                                    captured_err,
                                    captured_out,
                                )),
                            };
                        }
                    }
                }
                let mut proc = Self::make_proc_instance(
                    -1,
                    0,
                    0,
                    command_val,
                    opts.capture_err.then(String::new),
                    opts.capture_out.then(String::new),
                );
                Self::attach_proc_os_error(&mut proc, &os_error);
                Ok(proc)
            }
        }
    }

    /// Record the OS-level spawn error on a failed Proc so that
    /// `X::Proc::Unsuccessful.message` can report it (e.g. command-not-found
    /// produces "exit code: -1, ... OS error = No such file or directory").
    fn attach_proc_os_error(proc: &mut Value, os_error: &str) {
        if let Value::Instance { attributes, .. } = proc {
            attributes.insert("os-error".to_string(), Value::str(os_error.to_string()));
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
                Value::str(String::new()),
                None,
                None,
            ));
        }

        let opts = Self::extract_proc_options(args, 1);

        let command_val = Value::str(command_str.clone());

        let mut command = if cfg!(windows) {
            let mut cmd = Command::new("cmd");
            cmd.arg("/C").arg(&command_str);
            cmd
        } else {
            let mut cmd = Command::new("sh");
            cmd.arg("-c").arg(&command_str);
            cmd
        };

        if opts.capture_out {
            command.stdout(std::process::Stdio::piped());
        } else {
            command.stdout(std::process::Stdio::null());
        }
        if opts.capture_err {
            command.stderr(std::process::Stdio::piped());
        } else {
            command.stderr(std::process::Stdio::null());
        }

        // shell() defaults the child's working directory to the dynamic $*CWD
        // (set by `indir`), matching run().
        if let Some(cwd) = opts
            .cwd
            .clone()
            .or_else(|| self.get_dynamic_string("$*CWD"))
        {
            command.current_dir(cwd);
        }
        for (k, v) in opts.env {
            command.env(k, v);
        }

        match command.spawn() {
            Ok(mut child) => {
                let pid = child.id() as i64;
                let captured_out = if opts.capture_out {
                    child.stdout.take().map(|mut s| {
                        let mut buf = String::new();
                        use std::io::Read;
                        let _ = s.read_to_string(&mut buf);
                        buf
                    })
                } else {
                    None
                };
                let captured_err = if opts.capture_err {
                    child.stderr.take().map(|mut s| {
                        let mut buf = String::new();
                        use std::io::Read;
                        let _ = s.read_to_string(&mut buf);
                        buf
                    })
                } else {
                    None
                };
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
                        Ok(Self::make_proc_instance(
                            exitcode,
                            signal,
                            pid,
                            command_val,
                            captured_err,
                            captured_out,
                        ))
                    }
                    Err(_) => Ok(Self::make_proc_instance(
                        -1,
                        0,
                        pid,
                        command_val,
                        captured_err,
                        captured_out,
                    )),
                }
            }
            Err(_) => Ok(Self::make_proc_instance(-1, 0, 0, command_val, None, None)),
        }
    }
}
