use super::native_methods::*;
use super::*;
use crate::symbol::Symbol;
use std::io::{Read, Write};
use std::sync::mpsc;

impl Interpreter {
    pub(super) fn native_proc_async_mut(
        &mut self,
        mut attrs: HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        let proc_async_error = |class_name: &str, attrs: &[(&str, Value)]| {
            let mut ex_attrs = HashMap::new();
            for (k, v) in attrs {
                ex_attrs.insert((*k).to_string(), v.clone());
            }
            let message = class_name.to_string();
            ex_attrs.insert("message".to_string(), Value::str(message.clone()));
            let ex = Value::make_instance(Symbol::intern(class_name), ex_attrs);
            RuntimeError {
                exception: Some(Box::new(ex)),
                ..RuntimeError::new(message)
            }
        };
        match method {
            "start" => {
                use std::process::{Command, Stdio};

                if attrs.get("started").is_some_and(|v| v.truthy()) {
                    return Err(proc_async_error("X::Proc::Async::AlreadyStarted", &[]));
                }
                attrs.insert("started".to_string(), Value::Bool(true));

                // Extract command and args
                let mut cmd_arr = match attrs.get("cmd") {
                    Some(Value::Array(arr, ..)) => arr.to_vec(),
                    _ => Vec::new(),
                };
                if let Some(first) = cmd_arr.first().cloned() {
                    let expanded = match first {
                        Value::Array(items, ..) => Some(items.to_vec()),
                        Value::Seq(items) => Some(items.to_vec()),
                        Value::Slip(items) => Some(items.to_vec()),
                        _ => None,
                    };
                    if let Some(mut items) = expanded {
                        if cmd_arr.len() > 1 {
                            items.extend(cmd_arr.into_iter().skip(1));
                        }
                        cmd_arr = items;
                    }
                }
                let (program, cmd_args): (String, Vec<String>) = if cmd_arr.is_empty() {
                    return Err(RuntimeError::new("Proc::Async: no command specified"));
                } else {
                    let prog = cmd_arr[0].to_string_value();
                    let a: Vec<String> = cmd_arr[1..].iter().map(|v| v.to_string_value()).collect();
                    (prog, a)
                };

                // Get stdout/stderr supply IDs
                let stdout_supply_id = attrs.get("stdout").and_then(|v| {
                    if let Value::Instance { attributes, .. } = v
                        && let Some(Value::Int(id)) = attributes.get("supply_id")
                    {
                        return Some(*id as u64);
                    }
                    None
                });
                let stderr_supply_id = attrs.get("stderr").and_then(|v| {
                    if let Value::Instance { attributes, .. } = v
                        && let Some(Value::Int(id)) = attributes.get("supply_id")
                    {
                        return Some(*id as u64);
                    }
                    None
                });
                let merged_supply_id = attrs.get("supply").and_then(|v| {
                    if let Value::Instance { attributes, .. } = v
                        && let Some(Value::Int(id)) = attributes.get("supply_id")
                    {
                        return Some(*id as u64);
                    }
                    None
                });

                // Check if :w flag is set (stdin should be piped)
                let w_flag = attrs.get("w").map(|v| v.truthy()).unwrap_or(false);
                let bound_stdin = attrs.get("stdin_bind").cloned();
                let bound_stdout = attrs.get("stdout_bind").cloned();
                let bound_stderr = attrs.get("stderr_bind").cloned();
                let stdin_bytes = match bound_stdin.as_ref() {
                    Some(value) => self.proc_async_bound_handle_bytes(value)?,
                    None => None,
                };
                let stdin_supply_id = bound_stdin
                    .as_ref()
                    .and_then(Self::proc_async_supply_id_from_value);
                let mut bound_stdout_file =
                    self.proc_async_bound_output_file(bound_stdout.as_ref())?;
                let mut bound_stderr_file =
                    self.proc_async_bound_output_file(bound_stderr.as_ref())?;

                // Spawn child process synchronously so we get the PID immediately
                let mut cmd = Command::new(&program);
                cmd.args(&cmd_args)
                    .stdout(Stdio::piped())
                    .stderr(Stdio::piped());
                if w_flag || bound_stdin.is_some() {
                    cmd.stdin(Stdio::piped());
                }

                let child_result = cmd.spawn();

                // If spawn failed, break all promises with X::OS and return
                if let Err(e) = child_result {
                    let os_error_msg = e.to_string();
                    let mut ex_attrs = HashMap::new();
                    ex_attrs.insert("os-error".to_string(), Value::str(os_error_msg.clone()));
                    ex_attrs.insert(
                        "message".to_string(),
                        Value::str(format!("Failed to spawn '{}': {}", program, os_error_msg)),
                    );
                    let os_error = Value::make_instance(Symbol::intern("X::OS"), ex_attrs);
                    attrs.insert("spawn_error".to_string(), os_error.clone());

                    // Break ready promise if set
                    if let Some(Value::Promise(ready)) = attrs.get("ready_promise") {
                        ready.break_with(os_error.clone(), String::new(), String::new());
                    }

                    // Send Quit to stdout/stderr supply channels so react blocks die
                    if let Some(sid) = stdout_supply_id {
                        let (tx, rx) = mpsc::channel();
                        if let Ok(mut map) = supply_channel_map().lock() {
                            map.insert(sid, rx);
                        }
                        let _ = tx.send(SupplyEvent::Quit(os_error.clone()));
                    }
                    if let Some(sid) = stderr_supply_id {
                        let (tx, rx) = mpsc::channel();
                        if let Ok(mut map) = supply_channel_map().lock() {
                            map.insert(sid, rx);
                        }
                        let _ = tx.send(SupplyEvent::Quit(os_error.clone()));
                    }

                    // Return a broken promise
                    let promise = SharedPromise::new();
                    promise.break_with(os_error, String::new(), String::new());
                    return Ok((Value::Promise(promise), attrs));
                }

                let mut child = child_result.unwrap();

                let pid = child.id();
                attrs.insert("pid".to_string(), Value::Int(pid as i64));

                if let Some(Value::Instance {
                    attributes: stdout_attrs,
                    ..
                }) = attrs.get("stdout")
                    && let Some(Value::Promise(promise)) =
                        stdout_attrs.get("native_descriptor_promise")
                {
                    promise.try_keep(Value::Int(1)).ok();
                }
                if let Some(Value::Instance {
                    attributes: stderr_attrs,
                    ..
                }) = attrs.get("stderr")
                    && let Some(Value::Promise(promise)) =
                        stderr_attrs.get("native_descriptor_promise")
                {
                    promise.try_keep(Value::Int(2)).ok();
                }

                // Resolve ready promise if set
                if let Some(Value::Promise(ready)) = attrs.get("ready_promise") {
                    ready.try_keep(Value::Int(pid as i64)).ok();
                }

                // Store stdin in global registry if piped
                if let Some(stdin) = child.stdin.take() {
                    let stdin_arc = Arc::new(std::sync::Mutex::new(Some(stdin)));
                    if w_flag && let Ok(mut map) = proc_stdin_map().lock() {
                        map.insert(pid, stdin_arc.clone());
                    }
                    if let Some(bytes) = stdin_bytes {
                        let stdin_arc = stdin_arc.clone();
                        std::thread::spawn(move || {
                            if let Ok(mut guard) = stdin_arc.lock()
                                && let Some(ref mut stdin) = *guard
                            {
                                let _ = stdin.write_all(&bytes);
                                let _ = stdin.flush();
                            }
                            if let Ok(mut guard) = stdin_arc.lock() {
                                *guard = None;
                            }
                        });
                    } else if let Some(source_supply_id) = stdin_supply_id {
                        let stdin_arc = stdin_arc.clone();
                        std::thread::spawn(move || {
                            if let Some(rx) = take_supply_channel(source_supply_id) {
                                while let Ok(event) = rx.recv() {
                                    match event {
                                        SupplyEvent::Emit(value) => {
                                            if let Ok(mut guard) = stdin_arc.lock()
                                                && let Some(ref mut stdin) = *guard
                                            {
                                                let _ = stdin
                                                    .write_all(value.to_string_value().as_bytes());
                                                let _ = stdin.flush();
                                            }
                                        }
                                        SupplyEvent::Done | SupplyEvent::Quit(_) => break,
                                    }
                                }
                            } else {
                                loop {
                                    if let Some(collected) =
                                        get_supply_collected_output(source_supply_id)
                                    {
                                        if let Ok(mut guard) = stdin_arc.lock()
                                            && let Some(ref mut stdin) = *guard
                                        {
                                            let _ = stdin.write_all(collected.as_bytes());
                                            let _ = stdin.flush();
                                        }
                                        break;
                                    }
                                    std::thread::sleep(std::time::Duration::from_millis(10));
                                }
                            }
                            if let Ok(mut guard) = stdin_arc.lock() {
                                *guard = None;
                            }
                        });
                    }
                }

                // Create streaming channels for stdout/stderr
                // These will be consumed by the react event loop
                let stdout_channel = stdout_supply_id.map(|sid| {
                    let (tx, rx) = mpsc::channel();
                    if let Ok(mut map) = supply_channel_map().lock() {
                        map.insert(sid, rx);
                    }
                    tx
                });
                let stderr_channel = stderr_supply_id.map(|sid| {
                    let (tx, rx) = mpsc::channel();
                    if let Ok(mut map) = supply_channel_map().lock() {
                        map.insert(sid, rx);
                    }
                    tx
                });

                // Take stdout/stderr handles before moving child into thread
                let child_stdout = child.stdout.take();
                let child_stderr = child.stderr.take();

                let promise = SharedPromise::new();
                let ret = Value::Promise(promise.clone());
                let cmd_arr_clone = cmd_arr.clone();

                std::thread::spawn(move || {
                    // Spawn stdout reader thread — streams raw chunks through channel
                    let stdout_handle = child_stdout.map(|stdout| {
                        let tx = stdout_channel;
                        std::thread::spawn(move || {
                            use std::io::Read;
                            let mut stdout = stdout;
                            let mut collected = String::new();
                            let mut buf = [0u8; 4096];
                            loop {
                                match stdout.read(&mut buf) {
                                    Ok(0) => break,
                                    Ok(n) => {
                                        let chunk = String::from_utf8_lossy(&buf[..n]).into_owned();
                                        if let Some(ref tx) = tx {
                                            let _ = tx
                                                .send(SupplyEvent::Emit(Value::str(chunk.clone())));
                                        }
                                        collected.push_str(&chunk);
                                    }
                                    Err(_) => break,
                                }
                            }
                            if let Some(ref tx) = tx {
                                let _ = tx.send(SupplyEvent::Done);
                            }
                            collected
                        })
                    });

                    // Spawn stderr reader thread — streams raw chunks through channel
                    let stderr_handle = child_stderr.map(|stderr| {
                        let tx = stderr_channel;
                        std::thread::spawn(move || {
                            use std::io::Read;
                            let mut stderr = stderr;
                            let mut collected = String::new();
                            let mut buf = [0u8; 4096];
                            loop {
                                match stderr.read(&mut buf) {
                                    Ok(0) => break,
                                    Ok(n) => {
                                        let chunk = String::from_utf8_lossy(&buf[..n]).into_owned();
                                        if let Some(ref tx) = tx {
                                            let _ = tx
                                                .send(SupplyEvent::Emit(Value::str(chunk.clone())));
                                        }
                                        collected.push_str(&chunk);
                                    }
                                    Err(_) => break,
                                }
                            }
                            if let Some(ref tx) = tx {
                                let _ = tx.send(SupplyEvent::Done);
                            }
                            collected
                        })
                    });

                    // Wait for child to exit
                    let status = child.wait();
                    let exit_code = status
                        .as_ref()
                        .map(|s| s.code().unwrap_or(-1))
                        .unwrap_or(-1) as i64;
                    let signal = {
                        #[cfg(unix)]
                        {
                            use std::os::unix::process::ExitStatusExt;
                            status
                                .as_ref()
                                .map(|s| s.signal().unwrap_or(0))
                                .unwrap_or(0) as i64
                        }
                        #[cfg(not(unix))]
                        {
                            0i64
                        }
                    };

                    // Join reader threads and collect output
                    let collected_stdout = stdout_handle
                        .and_then(|h| h.join().ok())
                        .unwrap_or_default();
                    let collected_stderr = stderr_handle
                        .and_then(|h| h.join().ok())
                        .unwrap_or_default();
                    let collected_stdout = collected_stdout.replace("\r\n", "\n");

                    // Clean up stdin registry
                    if let Ok(mut map) = proc_stdin_map().lock() {
                        map.remove(&pid);
                    }
                    if let Some(file) = bound_stdout_file.as_mut() {
                        let _ = file.write_all(collected_stdout.as_bytes());
                        let _ = file.flush();
                    }
                    if let Some(file) = bound_stderr_file.as_mut() {
                        let _ = file.write_all(collected_stderr.as_bytes());
                        let _ = file.flush();
                    }
                    if let Some(sid) = stdout_supply_id {
                        set_supply_collected_output(sid, collected_stdout.clone());
                    }
                    if let Some(sid) = stderr_supply_id {
                        set_supply_collected_output(sid, collected_stderr.clone());
                    }
                    let collected_merged = format!("{}{}", collected_stdout, collected_stderr);
                    if let Some(sid) = merged_supply_id {
                        set_supply_collected_output(sid, collected_merged.clone());
                    }
                    let stdout_taps = stdout_supply_id.map(get_supply_taps).unwrap_or_default();
                    let stderr_taps = stderr_supply_id.map(get_supply_taps).unwrap_or_default();
                    let supply_taps = merged_supply_id.map(get_supply_taps).unwrap_or_default();

                    let mut proc_attrs = HashMap::new();
                    proc_attrs.insert("exitcode".to_string(), Value::Int(exit_code));
                    proc_attrs.insert("signal".to_string(), Value::Int(signal));
                    proc_attrs.insert(
                        "command".to_string(),
                        Value::Array(
                            std::sync::Arc::new(cmd_arr_clone),
                            crate::value::ArrayKind::List,
                        ),
                    );
                    proc_attrs.insert("pid".to_string(), Value::Int(pid as i64));
                    if let Some(sid) = stdout_supply_id {
                        proc_attrs.insert("stdout_supply_id".to_string(), Value::Int(sid as i64));
                    }
                    if let Some(sid) = stderr_supply_id {
                        proc_attrs.insert("stderr_supply_id".to_string(), Value::Int(sid as i64));
                    }
                    proc_attrs.insert("collected_stdout".to_string(), Value::str(collected_stdout));
                    proc_attrs.insert("collected_stderr".to_string(), Value::str(collected_stderr));
                    proc_attrs.insert("collected_merged".to_string(), Value::str(collected_merged));
                    proc_attrs.insert("stdout_taps".to_string(), Value::array(stdout_taps));
                    proc_attrs.insert("stderr_taps".to_string(), Value::array(stderr_taps));
                    if let Some(sid) = merged_supply_id {
                        proc_attrs.insert("supply_id".to_string(), Value::Int(sid as i64));
                    }
                    proc_attrs.insert("supply_taps".to_string(), Value::array(supply_taps));
                    let proc_val = Value::make_instance(Symbol::intern("Proc"), proc_attrs);

                    promise.keep(proc_val, String::new(), String::new());
                });

                Ok((ret, attrs))
            }
            "kill" => {
                let started = attrs.get("started").is_some_and(|v| v.truthy());
                let has_pid = attrs.contains_key("pid");
                if !started {
                    return Err(proc_async_error(
                        "X::Proc::Async::MustBeStarted",
                        &[("method", Value::str_from("kill"))],
                    ));
                }
                if !has_pid {
                    return Ok((Value::Nil, attrs));
                }
                #[cfg(feature = "native")]
                if let Some(Value::Int(pid)) = attrs.get("pid") {
                    let sig = args
                        .first()
                        .and_then(|v| match v {
                            Value::Int(s) => Some(*s as i32),
                            Value::Enum { value, .. } => Some(value.as_i64() as i32),
                            Value::Str(s) => match s.as_str() {
                                "HUP" | "SIGHUP" => Some(libc::SIGHUP),
                                "INT" | "SIGINT" => Some(libc::SIGINT),
                                "QUIT" | "SIGQUIT" => Some(libc::SIGQUIT),
                                "KILL" | "SIGKILL" => Some(libc::SIGKILL),
                                "TERM" | "SIGTERM" => Some(libc::SIGTERM),
                                "PIPE" | "SIGPIPE" => Some(libc::SIGPIPE),
                                _ => s.parse::<i32>().ok(),
                            },
                            _ => None,
                        })
                        .unwrap_or(libc::SIGHUP);
                    unsafe {
                        libc::kill(*pid as i32, sig);
                    }
                }
                Ok((Value::Nil, attrs))
            }
            "write" => {
                if !attrs.get("w").is_some_and(|v| v.truthy()) {
                    return Err(proc_async_error(
                        "X::Proc::Async::OpenForWriting",
                        &[("method", Value::str_from("write"))],
                    ));
                }
                let started = attrs.get("started").is_some_and(|v| v.truthy());
                let has_pid = attrs.contains_key("pid");
                let spawn_failed = attrs.contains_key("spawn_error");
                if !started || (!has_pid && !spawn_failed) {
                    return Err(proc_async_error(
                        "X::Proc::Async::MustBeStarted",
                        &[("method", Value::str_from("write"))],
                    ));
                }
                // If process failed to spawn, die with the spawn error
                if let Some(err) = attrs.get("spawn_error").cloned() {
                    let p = SharedPromise::new();
                    p.break_with(err, String::new(), String::new());
                    return Ok((Value::Promise(p), attrs));
                }

                // Write bytes (Buf) to the process's stdin
                let data = args.first().cloned().unwrap_or(Value::Nil);
                let bytes: Vec<u8> = match &data {
                    Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } if {
                        let cn = class_name.resolve();
                        cn == "Buf"
                            || cn == "Blob"
                            || cn == "utf8"
                            || cn == "utf16"
                            || cn.starts_with("buf")
                            || cn.starts_with("blob")
                            || cn.starts_with("Buf[")
                            || cn.starts_with("Blob[")
                    } =>
                    {
                        if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                            items
                                .iter()
                                .map(|v| match v {
                                    Value::Int(i) => *i as u8,
                                    _ => 0,
                                })
                                .collect()
                        } else {
                            Vec::new()
                        }
                    }
                    Value::Str(s) => s.as_bytes().to_vec(),
                    _ => Vec::new(),
                };

                if let Some(Value::Int(pid)) = attrs.get("pid") {
                    let pid = *pid as u32;
                    if let Ok(map) = proc_stdin_map().lock()
                        && let Some(stdin_arc) = map.get(&pid).cloned()
                    {
                        drop(map);
                        if let Ok(mut guard) = stdin_arc.lock()
                            && let Some(ref mut stdin) = *guard
                        {
                            use std::io::Write;
                            let _ = stdin.write_all(&bytes);
                            let _ = stdin.flush();
                        }
                    }
                }

                // Return a kept Promise
                let p = SharedPromise::new();
                p.keep(Value::Bool(true), String::new(), String::new());
                Ok((Value::Promise(p), attrs))
            }
            "close-stdin" => {
                let started = attrs.get("started").is_some_and(|v| v.truthy());
                let has_pid = attrs.contains_key("pid");
                if !started {
                    return Err(proc_async_error(
                        "X::Proc::Async::MustBeStarted",
                        &[("method", Value::str_from("close-stdin"))],
                    ));
                }
                if !has_pid {
                    return Ok((Value::Bool(true), attrs));
                }
                if let Some(Value::Int(pid)) = attrs.get("pid") {
                    let pid = *pid as u32;
                    if let Ok(map) = proc_stdin_map().lock()
                        && let Some(stdin_arc) = map.get(&pid).cloned()
                    {
                        drop(map);
                        if let Ok(mut guard) = stdin_arc.lock() {
                            *guard = None; // Drop the ChildStdin to close it
                        }
                    }
                }
                Ok((Value::Bool(true), attrs))
            }
            "bind-stdin" => {
                if attrs.get("w").is_some_and(|v| v.truthy()) {
                    return Err(proc_async_error(
                        "X::Proc::Async::BindOrUse",
                        &[("handle", Value::str_from("stdin"))],
                    ));
                }
                let bound = args.first().cloned().unwrap_or(Value::Nil);
                attrs.insert("stdin_bind".to_string(), bound);
                Ok((Value::Nil, attrs))
            }
            "bind-stdout" | "bind-stderr" => {
                let handle_name = if method == "bind-stdout" {
                    "stdout"
                } else {
                    "stderr"
                };
                if attrs.get("supply_selected").is_some_and(|v| v.truthy())
                    || attrs
                        .get(&format!("{}_selected", handle_name))
                        .is_some_and(|v| v.truthy())
                {
                    return Err(proc_async_error(
                        "X::Proc::Async::BindOrUse",
                        &[("handle", Value::str_from(handle_name))],
                    ));
                }
                let bound = args.first().cloned().unwrap_or(Value::Nil);
                attrs.insert(format!("{}_bind", handle_name), bound);
                Ok((Value::Nil, attrs))
            }
            "ready" => {
                // If spawn failed, return a broken promise with the error
                if let Some(err) = attrs.get("spawn_error").cloned() {
                    let promise = SharedPromise::new();
                    promise.break_with(err, String::new(), String::new());
                    return Ok((Value::Promise(promise), attrs));
                }
                // Returns a Promise that resolves with the PID when the process
                // has been started. If already started, resolves immediately.
                let promise = SharedPromise::new();
                if let Some(Value::Int(pid)) = attrs.get("pid") {
                    promise.keep(Value::Int(*pid), String::new(), String::new());
                }
                // Store the ready promise so start can resolve it
                attrs.insert("ready_promise".to_string(), Value::Promise(promise.clone()));
                Ok((Value::Promise(promise), attrs))
            }
            "stdout" | "stderr" => {
                if attrs
                    .get(&format!("{}_bind", method))
                    .is_some_and(|v| !matches!(v, Value::Nil))
                {
                    return Err(proc_async_error(
                        "X::Proc::Async::BindOrUse",
                        &[("handle", Value::str_from(method))],
                    ));
                }
                if attrs.get("supply_selected").is_some_and(|v| v.truthy()) {
                    return Err(proc_async_error("X::Proc::Async::SupplyOrStd", &[]));
                }
                let requested_bin = args.iter().any(
                    |arg| matches!(arg, Value::Pair(key, value) if key == "bin" && value.truthy()),
                );
                let mode_key = format!("{}_mode", method);
                if let Some(prev) = attrs.get(&mode_key).and_then(|v| match v {
                    Value::Str(s) => Some(s.as_str()),
                    _ => None,
                }) {
                    let requested = if requested_bin { "bin" } else { "text" };
                    if prev != requested {
                        return Err(proc_async_error(
                            "X::Proc::Async::CharsOrBytes",
                            &[("handle", Value::str_from(method))],
                        ));
                    }
                }
                if method == "stdout" {
                    attrs.insert("stdout_selected".to_string(), Value::Bool(true));
                } else {
                    attrs.insert("stderr_selected".to_string(), Value::Bool(true));
                }
                attrs.insert(
                    mode_key,
                    Value::str_from(if requested_bin { "bin" } else { "text" }),
                );
                if attrs.get("started").is_some_and(|v| v.truthy()) {
                    return Err(proc_async_error(
                        "X::Proc::Async::TapBeforeSpawn",
                        &[("handle", Value::str_from(method))],
                    ));
                }
                if args
                    .iter()
                    .any(|arg| !matches!(arg, Value::Pair(key, _) if key == "bin" || key == "enc"))
                {
                    return Err(proc_async_error(
                        "X::Proc::Async::CharsOrBytes",
                        &[("handle", Value::str_from(method))],
                    ));
                }
                let value = attrs.get(method).cloned().unwrap_or(Value::Nil);
                Ok((value, attrs))
            }
            "Supply" => {
                if attrs
                    .get("stdout_bind")
                    .is_some_and(|v| !matches!(v, Value::Nil))
                {
                    return Err(proc_async_error(
                        "X::Proc::Async::BindOrUse",
                        &[("handle", Value::str_from("stdout"))],
                    ));
                }
                if attrs
                    .get("stderr_bind")
                    .is_some_and(|v| !matches!(v, Value::Nil))
                {
                    return Err(proc_async_error(
                        "X::Proc::Async::BindOrUse",
                        &[("handle", Value::str_from("stderr"))],
                    ));
                }
                if attrs.get("stdout_selected").is_some_and(|v| v.truthy())
                    || attrs.get("stderr_selected").is_some_and(|v| v.truthy())
                {
                    return Err(proc_async_error("X::Proc::Async::SupplyOrStd", &[]));
                }
                attrs.insert("supply_selected".to_string(), Value::Bool(true));
                Ok((attrs.get("supply").cloned().unwrap_or(Value::Nil), attrs))
            }
            "print" | "say" => {
                if !attrs.get("w").is_some_and(|v| v.truthy()) {
                    return Err(proc_async_error(
                        "X::Proc::Async::OpenForWriting",
                        &[("method", Value::str_from(method))],
                    ));
                }
                let started = attrs.get("started").is_some_and(|v| v.truthy());
                let has_pid = attrs.contains_key("pid");
                let spawn_failed = attrs.contains_key("spawn_error");
                if !started || (!has_pid && !spawn_failed) {
                    return Err(proc_async_error(
                        "X::Proc::Async::MustBeStarted",
                        &[("method", Value::str_from(method))],
                    ));
                }
                if let Some(err) = attrs.get("spawn_error").cloned() {
                    let p = SharedPromise::new();
                    p.break_with(err, String::new(), String::new());
                    return Ok((Value::Promise(p), attrs));
                }
                // Write string to stdin of process
                let data = args.first().cloned().unwrap_or(Value::Nil);
                let mut s = data.to_string_value();
                if method == "say" {
                    s.push('\n');
                }
                if let Some(Value::Int(pid)) = attrs.get("pid") {
                    let pid = *pid as u32;
                    if let Ok(map) = proc_stdin_map().lock()
                        && let Some(stdin_arc) = map.get(&pid).cloned()
                    {
                        drop(map);
                        if let Ok(mut guard) = stdin_arc.lock()
                            && let Some(ref mut stdin) = *guard
                        {
                            use std::io::Write;
                            let _ = stdin.write_all(s.as_bytes());
                            let _ = stdin.flush();
                        }
                    }
                }
                let p = SharedPromise::new();
                p.keep(Value::Bool(true), String::new(), String::new());
                Ok((Value::Promise(p), attrs))
            }
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on Proc::Async",
                method
            ))),
        }
    }
}

impl Interpreter {
    fn proc_async_bound_handle_bytes(
        &mut self,
        value: &Value,
    ) -> Result<Option<Vec<u8>>, RuntimeError> {
        let Some(handle_id) = Self::handle_id_from_value(value) else {
            return Ok(None);
        };
        let Some(state) = self.handles.get_mut(&handle_id) else {
            return Err(RuntimeError::new("Invalid IO::Handle"));
        };
        let Some(file) = state.file.as_mut() else {
            return Ok(None);
        };
        let mut bytes = Vec::new();
        file.read_to_end(&mut bytes)
            .map_err(|err| RuntimeError::new(format!("Failed to read bound handle: {}", err)))?;
        Ok(Some(bytes))
    }

    fn proc_async_bound_output_file(
        &mut self,
        value: Option<&Value>,
    ) -> Result<Option<std::fs::File>, RuntimeError> {
        let Some(value) = value else {
            return Ok(None);
        };
        let Some(handle_id) = Self::handle_id_from_value(value) else {
            return Ok(None);
        };
        let Some(state) = self.handles.get_mut(&handle_id) else {
            return Err(RuntimeError::new("Invalid IO::Handle"));
        };
        let Some(file) = state.file.as_mut() else {
            return Ok(None);
        };
        file.try_clone()
            .map(Some)
            .map_err(|err| RuntimeError::new(format!("Failed to clone bound handle: {}", err)))
    }

    fn proc_async_supply_id_from_value(value: &Value) -> Option<u64> {
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = value
            && class_name == "Supply"
            && let Some(Value::Int(sid)) = attributes.get("supply_id")
            && *sid >= 0
        {
            return Some(*sid as u64);
        }
        None
    }
}
