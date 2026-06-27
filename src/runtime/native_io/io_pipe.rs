use super::*;

impl Interpreter {
    pub(crate) fn native_io_pipe(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Check if this is a writable stdin pipe (from run :in)
        if let Some(Value::Int(pid)) = attributes.get("proc-pid") {
            let pid_u32 = *pid as u32;
            match method {
                "print" => {
                    // Write to the child's stdin
                    if let Ok(map) = super::native_methods::proc_stdin_map().lock()
                        && let Some(stdin_arc) = map.get(&pid_u32)
                        && let Ok(mut guard) = stdin_arc.lock()
                        && let Some(ref mut stdin) = *guard
                    {
                        use std::io::Write;
                        for arg in args {
                            let s = arg.to_string_value();
                            let _ = stdin.write_all(s.as_bytes());
                        }
                        let _ = stdin.flush();
                    }
                    return Ok(Value::Bool(true));
                }
                "say" => {
                    // Write to the child's stdin with a trailing newline
                    if let Ok(map) = super::native_methods::proc_stdin_map().lock()
                        && let Some(stdin_arc) = map.get(&pid_u32)
                        && let Ok(mut guard) = stdin_arc.lock()
                        && let Some(ref mut stdin) = *guard
                    {
                        use std::io::Write;
                        for arg in args {
                            let s = arg.to_string_value();
                            let _ = stdin.write_all(s.as_bytes());
                        }
                        let _ = stdin.write_all(b"\n");
                        let _ = stdin.flush();
                    }
                    return Ok(Value::Bool(true));
                }
                "put" => {
                    // Same as say for IO handles
                    if let Ok(map) = super::native_methods::proc_stdin_map().lock()
                        && let Some(stdin_arc) = map.get(&pid_u32)
                        && let Ok(mut guard) = stdin_arc.lock()
                        && let Some(ref mut stdin) = *guard
                    {
                        use std::io::Write;
                        for arg in args {
                            let s = arg.to_string_value();
                            let _ = stdin.write_all(s.as_bytes());
                        }
                        let _ = stdin.write_all(b"\n");
                        let _ = stdin.flush();
                    }
                    return Ok(Value::Bool(true));
                }
                "flush" => {
                    if let Ok(map) = super::native_methods::proc_stdin_map().lock()
                        && let Some(stdin_arc) = map.get(&pid_u32)
                        && let Ok(mut guard) = stdin_arc.lock()
                        && let Some(ref mut stdin) = *guard
                    {
                        use std::io::Write;
                        let _ = stdin.flush();
                    }
                    return Ok(Value::Bool(true));
                }
                "write" => {
                    // Write binary data (Buf/Blob/utf8) to stdin
                    if let Ok(map) = super::native_methods::proc_stdin_map().lock()
                        && let Some(stdin_arc) = map.get(&pid_u32)
                        && let Ok(mut guard) = stdin_arc.lock()
                        && let Some(ref mut stdin) = *guard
                    {
                        use std::io::Write;
                        for arg in args {
                            match arg {
                                Value::Instance {
                                    class_name,
                                    attributes,
                                    ..
                                } if crate::runtime::utils::is_buf_or_blob_class(
                                    &class_name.resolve(),
                                ) =>
                                {
                                    // Try "bytes" first (make_buf), then "data"
                                    let map = attributes.as_map();
                                    let bytes_arr = map.get("bytes").or_else(|| map.get("data"));
                                    if let Some(Value::Array(bytes, _)) = bytes_arr {
                                        let data: Vec<u8> =
                                            bytes.iter().map(|v| v.to_f64() as u8).collect();
                                        let _ = stdin.write_all(&data);
                                    }
                                }
                                _ => {
                                    let s = arg.to_string_value();
                                    let _ = stdin.write_all(s.as_bytes());
                                }
                            }
                        }
                    }
                    return Ok(Value::Bool(true));
                }
                "close" => {
                    // Close the child's stdin (drop it)
                    if let Ok(map) = super::native_methods::proc_stdin_map().lock()
                        && let Some(stdin_arc) = map.get(&pid_u32)
                        && let Ok(mut guard) = stdin_arc.lock()
                    {
                        *guard = None; // Drop the ChildStdin
                    }
                    // `.close` on a Proc pipe returns the owning Proc.
                    if let Ok(map) = super::builtins_system::proc_by_pid_map().lock()
                        && let Some(proc) = map.get(pid)
                    {
                        return Ok(proc.clone());
                    }
                    return Ok(Value::Bool(true));
                }
                _ => {}
            }
        }
        // Handle "live" IO::Pipe from a live Proc (has `live-pid`).
        if let Some(Value::Int(live_pid)) = attributes.get("live-pid") {
            let pipe_type = attributes
                .get("pipe-type")
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| "out".to_string());
            let finalize_and_get_content = || -> String {
                let pid = *live_pid;
                if let Ok(mut map) = super::builtins_system::live_proc_map().lock()
                    && let Some(mut state) = map.remove(&pid)
                {
                    // Do NOT close stdin here -- let the explicit .close handle it.
                    // Closing stdin prematurely breaks concurrent writes from `start` blocks.
                    let captured_out: Option<String> = if state.capture_out {
                        state.child.stdout.take().map(|mut s| {
                            let mut buf = String::new();
                            use std::io::Read;
                            let _ = s.read_to_string(&mut buf);
                            buf
                        })
                    } else {
                        None
                    };
                    let captured_err: Option<String> = if state.capture_err {
                        state.child.stderr.take().map(|mut s| {
                            let mut buf = String::new();
                            use std::io::Read;
                            let _ = s.read_to_string(&mut buf);
                            buf
                        })
                    } else {
                        None
                    };
                    let (exitcode, signal): (i64, i64) = match state.child.wait() {
                        Ok(status) => {
                            let ec = status.code().unwrap_or(-1) as i64;
                            #[cfg(unix)]
                            let sig = {
                                use std::os::unix::process::ExitStatusExt;
                                status.signal().unwrap_or(0) as i64
                            };
                            #[cfg(not(unix))]
                            let sig = 0i64;
                            (ec, sig)
                        }
                        Err(_) => (-1i64, 0i64),
                    };
                    if let Ok(mut fmap) = super::builtins_system::finalized_proc_map().lock() {
                        fmap.insert(
                            pid,
                            super::builtins_system::FinalizedProc {
                                exitcode,
                                signal,
                                captured_out,
                                captured_err,
                            },
                        );
                    }
                }
                if let Ok(fmap) = super::builtins_system::finalized_proc_map().lock()
                    && let Some(finalized) = fmap.get(&pid)
                {
                    match pipe_type.as_str() {
                        "out" => finalized.captured_out.clone().unwrap_or_default(),
                        "err" => finalized.captured_err.clone().unwrap_or_default(),
                        _ => String::new(),
                    }
                } else {
                    String::new()
                }
            };
            match method {
                "slurp" | "Str" | "gist" => {
                    let has_bin = Self::named_bool(args, "bin");
                    let pipe_bin = matches!(attributes.get("bin"), Some(Value::Bool(true)));
                    let content = finalize_and_get_content();
                    if has_bin || pipe_bin {
                        return Ok(Self::make_buf(content.into_bytes()));
                    }
                    return Ok(Value::str(content));
                }
                "lines" => {
                    let content = finalize_and_get_content();
                    let trimmed = content.strip_suffix('\n').unwrap_or(&content);
                    let lines: Vec<Value> = if trimmed.is_empty() {
                        Vec::new()
                    } else {
                        trimmed
                            .split('\n')
                            .map(|s| Value::str(s.trim_end_matches('\r').to_string()))
                            .collect()
                    };
                    return Ok(Value::array(lines));
                }
                "get" => {
                    let content = finalize_and_get_content();
                    let line = content.lines().next().unwrap_or("");
                    return Ok(Value::str(line.to_string()));
                }
                "close" => {
                    let _ = finalize_and_get_content();
                    // `.close` on a Proc pipe returns the owning Proc.
                    if let Ok(map) = super::builtins_system::proc_by_pid_map().lock()
                        && let Some(proc) = map.get(live_pid)
                    {
                        return Ok(proc.clone());
                    }
                    return Ok(Value::Bool(true));
                }
                "encoding" => return Ok(Value::str("utf8".to_string())),
                "IO" | "path" => {
                    return Ok(Value::Package(crate::symbol::Symbol::intern("IO::Path")));
                }
                _ => {}
            }
        }

        // Look up persistent per-pipe cursor state (if any). Repeated calls
        // on the same logical IO::Pipe share this so `.get` / `.lines` can
        // advance through buffered content line by line.
        let pipe_id = attributes.get("pipe-id").and_then(|v| {
            if let Value::Int(i) = v {
                Some(*i)
            } else {
                None
            }
        });
        if let Some(id) = pipe_id {
            match method {
                "get" => {
                    if let Ok(mut map) = super::builtins_system::io_pipe_state_map().lock()
                        && let Some(state) = map.get_mut(&id)
                    {
                        if state.closed {
                            return Err(RuntimeError::io_closed("get"));
                        }
                        if state.cursor >= state.content.len() {
                            return Ok(Value::Nil);
                        }
                        let rest = &state.content[state.cursor..];
                        let (line, advance) = if let Some(pos) = rest.find('\n') {
                            let line = rest[..pos].trim_end_matches('\r').to_string();
                            (line, pos + 1)
                        } else {
                            (rest.to_string(), rest.len())
                        };
                        state.cursor += advance;
                        return Ok(Value::str(line));
                    }
                    return Ok(Value::Nil);
                }
                "lines" => {
                    if let Ok(mut map) = super::builtins_system::io_pipe_state_map().lock()
                        && let Some(state) = map.get_mut(&id)
                    {
                        if state.closed {
                            return Err(RuntimeError::io_closed("lines"));
                        }
                        let rest = state.content[state.cursor..].to_string();
                        state.cursor = state.content.len();
                        let trimmed = rest.strip_suffix('\n').unwrap_or(&rest);
                        let lines: Vec<Value> = if trimmed.is_empty() {
                            Vec::new()
                        } else {
                            trimmed
                                .split('\n')
                                .map(|s| Value::str(s.trim_end_matches('\r').to_string()))
                                .collect()
                        };
                        return Ok(Value::array(lines));
                    }
                    return Ok(Value::array(vec![]));
                }
                "eof" => {
                    if let Ok(map) = super::builtins_system::io_pipe_state_map().lock()
                        && let Some(state) = map.get(&id)
                    {
                        return Ok(Value::Bool(state.cursor >= state.content.len()));
                    }
                    return Ok(Value::Bool(true));
                }
                _ => {}
            }
        }
        let content = attributes
            .get("content")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        match method {
            "slurp" | "Str" | "gist" => {
                let has_bin = Self::named_bool(args, "bin");
                let pipe_bin = matches!(attributes.get("bin"), Some(Value::Bool(true)));
                if has_bin || pipe_bin {
                    return Ok(Self::make_buf(content.into_bytes()));
                }
                Ok(Value::str(content))
            }
            "encoding" => Ok(Value::str("utf8".to_string())),
            "close" => {
                if let Some(id) = pipe_id
                    && let Ok(mut map) = super::builtins_system::io_pipe_state_map().lock()
                    && let Some(state) = map.get_mut(&id)
                {
                    state.closed = true;
                }
                // Return the parent Proc object so that `$pipe.close === $proc`
                if let Some(id) = pipe_id
                    && let Ok(map) = super::builtins_system::pipe_proc_map().lock()
                    && let Some(proc_val) = map.get(&id)
                {
                    return Ok(proc_val.clone());
                }
                Ok(Value::Bool(true))
            }
            "proc" => {
                // Return the parent Proc object
                if let Some(id) = pipe_id
                    && let Ok(map) = super::builtins_system::pipe_proc_map().lock()
                    && let Some(proc_val) = map.get(&id)
                {
                    return Ok(proc_val.clone());
                }
                Ok(Value::Nil)
            }
            "IO" | "path" => {
                // Returns the IO::Path type object (not an instance)
                Ok(Value::Package(crate::symbol::Symbol::intern("IO::Path")))
            }
            "split" => {
                // Basic split for IO::Pipe
                let separator = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let skip_empty = args
                    .iter()
                    .any(|a| matches!(a, Value::Pair(k, v) if k == "skip-empty" && v.truthy()));
                let parts: Vec<Value> = content
                    .split(&separator)
                    .filter(|s| !skip_empty || !s.is_empty())
                    .map(|s| Value::str(s.to_string()))
                    .collect();
                Ok(Value::array(parts))
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on IO::Pipe",
                method
            ))),
        }
    }
}
