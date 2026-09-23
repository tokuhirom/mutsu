use super::builtins_system::*;
use super::*;

impl Interpreter {
    #[cfg(all(unix, feature = "native"))]
    fn make_merged_output_pipe()
    -> Result<(std::process::Stdio, std::process::Stdio, std::fs::File), RuntimeError> {
        use std::os::fd::{FromRawFd, OwnedFd};

        let mut fds = [0; 2];
        // SAFETY: `fds` points to two writable file-descriptor slots, as
        // required by pipe(2). Ownership is transferred to the Rust types
        // immediately below on success.
        if unsafe { libc::pipe(fds.as_mut_ptr()) } != 0 {
            return Err(RuntimeError::new(format!(
                "Cannot create :merge pipe: {}",
                std::io::Error::last_os_error()
            )));
        }
        let reader = unsafe { std::fs::File::from_raw_fd(fds[0]) };
        let writer = unsafe { OwnedFd::from_raw_fd(fds[1]) };
        let stderr_writer = writer
            .try_clone()
            .map_err(|err| RuntimeError::new(format!("Cannot duplicate :merge pipe: {err}")))?;
        Ok((
            std::process::Stdio::from(writer),
            std::process::Stdio::from(stderr_writer),
            reader,
        ))
    }

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
            let arg = arg.unwrap_varref();
            match arg.view() {
                ValueView::Hash(_) | ValueView::Pair(_, _) => {}
                ValueView::Instance {
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
                ValueView::Array(elems, _) => {
                    for elem in elems.iter() {
                        positional.push(Value::to_string_value(elem));
                    }
                }
                // A Seq/Slip (e.g. from `run (|@cmd).grep(...)`) is an iterable and
                // flattens into the argument list just like an Array, matching Raku's
                // slurpy `*@args` behavior for `run`.
                ValueView::Seq(elems) | ValueView::HyperSeq(elems) | ValueView::RaceSeq(elems) => {
                    for elem in elems.iter() {
                        positional.push(Value::to_string_value(elem));
                    }
                }
                ValueView::Slip(elems) => {
                    for elem in elems.iter() {
                        positional.push(Value::to_string_value(elem));
                    }
                }
                _ => {
                    positional.push(arg.to_string_value());
                }
            }
        }

        let mut opts = Self::extract_proc_options(args, 0);
        // Rakudo's :merge captures the combined stdout/stderr stream in .out
        // for run() when no explicit :out(False) disables capture. Keep this
        // separate from capture_err: a merged Proc exposes the data through
        // .out and leaves .err undefined.
        if opts.merge && !opts.out_explicit {
            opts.capture_out = true;
        }

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
        // No `:env` override: explicitly apply mutsu's own `%*ENV` rather than
        // relying on `Command::spawn()`'s default OS-level inheritance, which
        // stops seeing a `%*ENV<k> = v`/`std::env::set_var` write once any OS
        // thread has ever been spawned in this process — see
        // `todo/deep/env-var-write-invisible-to-spawn-after-a-thread.md` (and
        // `native_proc_async.rs`'s `.start()`, which applies the same fix for
        // `Proc::Async`).
        let opts_env = if opts.env_explicit {
            opts.env.clone()
        } else {
            match self.env.get("%*ENV").map(Value::view) {
                Some(ValueView::Hash(map)) => map
                    .iter()
                    .map(|(k, v)| (k.clone(), v.to_string_value()))
                    .collect(),
                _ => HashMap::new(),
            }
        };

        let mut cmd = Command::new(program);
        Self::apply_run_args(&mut cmd, rest_args, opts.win_verbatim_args);

        // Handle :out with IO::Handle — redirect stdout to that file
        let mut stdout_file_for_merge: Option<std::fs::File> = None;
        let merge_to_capture = opts.merge && opts.capture_out && opts.out_handle_id.is_none();
        let mut merged_output_reader: Option<std::fs::File> = None;
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
        } else if merge_to_capture {
            #[cfg(all(unix, feature = "native"))]
            {
                let (stdout, stderr, reader) = Self::make_merged_output_pipe()?;
                merged_output_reader = Some(reader);
                cmd.stdout(stdout);
                cmd.stderr(stderr);
            }
            #[cfg(not(all(unix, feature = "native")))]
            {
                // The native Unix path above uses one OS pipe for both file
                // descriptors. Keep the capture behavior on targets without
                // that primitive; those targets do not provide the same
                // process support as the native build.
                cmd.stdout(std::process::Stdio::piped());
                cmd.stderr(std::process::Stdio::piped());
            }
        } else if opts.capture_out {
            cmd.stdout(std::process::Stdio::piped());
        } else if opts.out_explicit {
            cmd.stdout(std::process::Stdio::null());
        } else {
            cmd.stdout(std::process::Stdio::inherit());
        }
        if opts.merge && !merge_to_capture {
            if let Some(file) = stdout_file_for_merge {
                cmd.stderr(std::process::Stdio::from(file));
            } else {
                // :merge without :out(file) — stderr goes to same pipe as stdout
                cmd.stderr(std::process::Stdio::piped());
            }
        } else if merge_to_capture {
            // Both streams were configured above with the shared capture pipe.
        } else if opts.capture_err {
            cmd.stderr(std::process::Stdio::piped());
        } else if opts.err_explicit {
            cmd.stderr(std::process::Stdio::null());
        } else {
            cmd.stderr(std::process::Stdio::inherit());
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
                // `Command` retains its Stdio configuration after spawn. Drop
                // it before reading our merged pipe so the parent's duplicate
                // write descriptors do not keep the reader open after the
                // child exits.
                drop(cmd);

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
                            map.insert(
                                pid as u32,
                                std::sync::Arc::new(std::sync::Mutex::new(Some(stdin))),
                            );
                        }
                        let mut in_attrs = HashMap::new();
                        in_attrs.insert("proc-pid".to_string(), Value::int(pid));
                        Value::make_instance(Symbol::intern("IO::Pipe"), in_attrs)
                    } else {
                        Value::NIL
                    };
                    let mut attrs = HashMap::new();
                    attrs.insert("exitcode".to_string(), Value::int(-1));
                    attrs.insert("signal".to_string(), Value::int(0));
                    attrs.insert("pid".to_string(), Value::int(pid));
                    attrs.insert("command".to_string(), command_val);
                    attrs.insert("in".to_string(), in_pipe);
                    attrs.insert("live".to_string(), Value::TRUE);
                    if opts.bin {
                        attrs.insert("bin".to_string(), Value::TRUE);
                    }
                    let proc = Value::make_instance(Symbol::intern("Proc"), attrs);
                    if let Ok(mut map) = proc_by_pid_map().lock() {
                        map.insert(pid, proc.clone());
                    }
                    return Ok(proc);
                }

                let captured_out = if let Some(reader) = merged_output_reader.as_mut() {
                    let mut buf = String::new();
                    use std::io::Read;
                    let _ = reader.read_to_string(&mut buf);
                    Some(buf)
                } else if opts.capture_out {
                    child.stdout.take().map(|mut s| {
                        let mut buf = String::new();
                        use std::io::Read;
                        let _ = s.read_to_string(&mut buf);
                        buf
                    })
                } else {
                    None
                };
                let captured_err =
                    if opts.capture_err || (merge_to_capture && merged_output_reader.is_none()) {
                        child.stderr.take().map(|mut s| {
                            let mut buf = String::new();
                            use std::io::Read;
                            let _ = s.read_to_string(&mut buf);
                            buf
                        })
                    } else {
                        None
                    };
                let (captured_err, captured_out) =
                    if merge_to_capture && merged_output_reader.is_none() {
                        (
                            None,
                            captured_out
                                .map(|out| format!("{}{}", out, captured_err.unwrap_or_default())),
                        )
                    } else {
                        (captured_err, captured_out)
                    };
                match child.wait() {
                    Ok(status) => {
                        let (exitcode, signal) = super::builtins_system::exit_status_parts(&status);
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
                        } else if opts.out_explicit {
                            retry.stdout(std::process::Stdio::null());
                        } else {
                            retry.stdout(std::process::Stdio::inherit());
                        }
                        if opts.capture_err {
                            retry.stderr(std::process::Stdio::piped());
                        } else if opts.err_explicit {
                            retry.stderr(std::process::Stdio::null());
                        } else {
                            retry.stderr(std::process::Stdio::inherit());
                        }
                        if let Some(cwd) = opts_cwd {
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
                                    let (exitcode, signal) =
                                        super::builtins_system::exit_status_parts(&status);
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
        if let ValueView::Instance { attributes, .. } = proc.view() {
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
        } else if opts.out_explicit {
            command.stdout(std::process::Stdio::null());
        } else {
            command.stdout(std::process::Stdio::inherit());
        }
        if opts.capture_err {
            command.stderr(std::process::Stdio::piped());
        } else if opts.err_explicit {
            command.stderr(std::process::Stdio::null());
        } else {
            command.stderr(std::process::Stdio::inherit());
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
        // See the matching comment in `builtin_run` above: no `:env` override
        // means explicitly apply mutsu's own `%*ENV` rather than relying on
        // default OS-level inheritance.
        if opts.env_explicit {
            for (k, v) in opts.env {
                command.env(k, v);
            }
        } else if let Some(ValueView::Hash(map)) = self.env.get("%*ENV").map(Value::view) {
            for (k, v) in map.iter() {
                command.env(k, v.to_string_value());
            }
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
                        let (exitcode, signal) = super::builtins_system::exit_status_parts(&status);
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
