use super::native_methods::*;
use super::*;
use crate::symbol::Symbol;
use std::sync::mpsc;

impl Interpreter {
    pub(super) fn native_proc_async_mut(
        &mut self,
        mut attrs: HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        match method {
            "start" => {
                use std::process::{Command, Stdio};

                attrs.insert("started".to_string(), Value::Bool(true));

                // Extract command and args
                let cmd_arr = match attrs.get("cmd") {
                    Some(Value::Array(arr, ..)) => arr.to_vec(),
                    _ => Vec::new(),
                };
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

                // Get taps for non-streaming (backward compat) replay
                let stdout_taps = stdout_supply_id.map(get_supply_taps).unwrap_or_default();
                let stderr_taps = stderr_supply_id.map(get_supply_taps).unwrap_or_default();

                // Check if :w flag is set (stdin should be piped)
                let w_flag = attrs.get("w").map(|v| v.truthy()).unwrap_or(false);

                // Spawn child process synchronously so we get the PID immediately
                let mut cmd = Command::new(&program);
                cmd.args(&cmd_args)
                    .stdout(Stdio::piped())
                    .stderr(Stdio::piped());
                if w_flag {
                    cmd.stdin(Stdio::piped());
                }

                let child_result = cmd.spawn();

                // If spawn failed, break all promises with X::OS and return
                if let Err(e) = child_result {
                    let os_error_msg = e.to_string();
                    let mut ex_attrs = HashMap::new();
                    ex_attrs.insert("os-error".to_string(), Value::Str(os_error_msg.clone()));
                    ex_attrs.insert(
                        "message".to_string(),
                        Value::Str(format!("Failed to spawn '{}': {}", program, os_error_msg)),
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

                // Resolve ready promise if set
                if let Some(Value::Promise(ready)) = attrs.get("ready_promise") {
                    ready.try_keep(Value::Int(pid as i64)).ok();
                }

                // Store stdin in global registry if piped
                if let Some(stdin) = child.stdin.take() {
                    let stdin_arc = Arc::new(std::sync::Mutex::new(Some(stdin)));
                    if let Ok(mut map) = proc_stdin_map().lock() {
                        map.insert(pid, stdin_arc);
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
                                                .send(SupplyEvent::Emit(Value::Str(chunk.clone())));
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
                                                .send(SupplyEvent::Emit(Value::Str(chunk.clone())));
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

                    // Clean up stdin registry
                    if let Ok(mut map) = proc_stdin_map().lock() {
                        map.remove(&pid);
                    }

                    let mut proc_attrs = HashMap::new();
                    proc_attrs.insert("exitcode".to_string(), Value::Int(exit_code));
                    proc_attrs.insert("signal".to_string(), Value::Int(signal));
                    proc_attrs.insert("command".to_string(), Value::real_array(cmd_arr_clone));
                    proc_attrs.insert("pid".to_string(), Value::Int(pid as i64));
                    proc_attrs.insert("collected_stdout".to_string(), Value::Str(collected_stdout));
                    proc_attrs.insert("collected_stderr".to_string(), Value::Str(collected_stderr));
                    proc_attrs.insert("stdout_taps".to_string(), Value::array(stdout_taps));
                    proc_attrs.insert("stderr_taps".to_string(), Value::array(stderr_taps));
                    let proc_val = Value::make_instance(Symbol::intern("Proc"), proc_attrs);

                    promise.keep(proc_val, String::new(), String::new());
                });

                Ok((ret, attrs))
            }
            "kill" => {
                #[cfg(feature = "native")]
                if let Some(Value::Int(pid)) = attrs.get("pid") {
                    let sig = args
                        .first()
                        .and_then(|v| match v {
                            Value::Int(s) => Some(*s as i32),
                            Value::Enum { value, .. } => Some(*value as i32),
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
                    } if class_name == "Buf" => {
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
            "print" | "say" => {
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
