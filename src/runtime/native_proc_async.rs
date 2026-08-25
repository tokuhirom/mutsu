use super::native_methods::*;
use super::*;
use crate::symbol::Symbol;
use crate::value::AttrMap;
use std::io::{Read, Write};

/// Create a Buf Value from raw bytes.
fn make_buf_value(bytes: &[u8]) -> Value {
    crate::value::value_buf::make_buf_from_u8(bytes)
}

/// Incrementally UTF-8-decode a Proc::Async output stream. Emits the decoded valid
/// prefix through the supply channel, retains an INCOMPLETE trailing sequence in
/// `pending` (so a multibyte character split across two reads is not mis-flagged),
/// and returns `true` when a genuinely malformed byte is hit — the caller then
/// quits the supply, matching Rakudo ("stdout/stderr Supply quit on encoding
/// error", roast S17-procasync/encoding.t).
///
/// `translate_crlf`/`held_cr` mirror the whole-run `\r\n` -> `\n` translation
/// the replayed (post-exit) path applies to `collected_stdout` (mutsu-specific,
/// stdout only — see the caller): a lone trailing `\r` is held back rather than
/// emitted, in case the very next read starts with `\n` (a `\r\n` split across
/// two `read()`s), and flushed as-is once the stream ends with no `\n` to pair.
fn feed_utf8_incremental(
    pending: &mut Vec<u8>,
    new: &[u8],
    tx: &Option<super::native_methods::supply_channel::SupplySender>,
    collected: &mut String,
    translate_crlf: bool,
    held_cr: &mut bool,
) -> bool {
    pending.extend_from_slice(new);
    match std::str::from_utf8(pending) {
        Ok(s) => {
            emit_decoded_chunk(s, tx, collected, translate_crlf, held_cr);
            pending.clear();
            false
        }
        Err(e) => {
            let valid = e.valid_up_to();
            if valid > 0 {
                let s = std::str::from_utf8(&pending[..valid]).unwrap_or("");
                emit_decoded_chunk(s, tx, collected, translate_crlf, held_cr);
            }
            match e.error_len() {
                // Incomplete trailing sequence: keep the tail for the next read.
                None => {
                    pending.drain(..valid);
                    false
                }
                // A genuinely invalid byte: signal the encoding error.
                Some(_) => true,
            }
        }
    }
}

/// Send one decoded chunk (see [`feed_utf8_incremental`]), applying the
/// held-back-`\r` CRLF translation when `translate_crlf` is set.
fn emit_decoded_chunk(
    s: &str,
    tx: &Option<super::native_methods::supply_channel::SupplySender>,
    collected: &mut String,
    translate_crlf: bool,
    held_cr: &mut bool,
) {
    if s.is_empty() && !*held_cr {
        return;
    }
    let mut text = String::with_capacity(s.len() + 1);
    if std::mem::take(held_cr) {
        text.push('\r');
    }
    text.push_str(s);
    if translate_crlf {
        if let Some(stripped) = text.strip_suffix('\r') {
            *held_cr = true;
            text.truncate(stripped.len());
        }
        if text.contains('\r') {
            text = text.replace("\r\n", "\n");
        }
    }
    if text.is_empty() {
        return;
    }
    if let Some(tx) = tx {
        let _ = tx.send(SupplyEvent::Emit(Value::str(text.clone())));
    }
    collected.push_str(&text);
}

/// Flush a [`feed_utf8_incremental`] run's held-back lone `\r` (see its doc
/// comment) once the stream has genuinely ended with no following `\n` to
/// pair it with.
fn flush_held_cr(
    held_cr: bool,
    tx: &Option<super::native_methods::supply_channel::SupplySender>,
    collected: &mut String,
) {
    if held_cr {
        if let Some(tx) = tx {
            let _ = tx.send(SupplyEvent::Emit(Value::str("\r".to_string())));
        }
        collected.push('\r');
    }
}

/// Build a thrown `X::Proc::Async::*` error.
///
/// The exception instance carries only its genuine Raku attributes (`method` /
/// `handle` / `use`); the human-readable text is produced by
/// [`crate::builtins::exception_message::format_exception_message`] — the very
/// table `.message` / `.Str` / `.gist` consult when an exception has no
/// `message` attribute. Deliberately NOT storing a `message` attribute is what
/// makes the thrown value and a user-constructed
/// `X::Proc::Async::MustBeStarted.new(:method<say>)` render identically; the
/// old code stored the bare class name there, which shadowed the formatter and
/// made every one of these exceptions stringify as its own type name.
pub(in crate::runtime) fn proc_async_error(
    class_name: &str,
    attrs: &[(&str, Value)],
) -> RuntimeError {
    let ex_attrs: AttrMap = attrs.iter().map(|(k, v)| (*k, v.clone())).collect();
    let message =
        crate::builtins::exception_message::format_exception_message(class_name, &ex_attrs)
            .unwrap_or_else(|| class_name.to_string());
    let ex = Value::make_instance(Symbol::intern(class_name), ex_attrs);
    RuntimeError {
        exception: Some(Box::new(ex)),
        ..RuntimeError::new(message)
    }
}

/// The exception value sent on a `SupplyEvent::Quit` when a Proc::Async output
/// stream contains malformed UTF-8.
pub(in crate::runtime) fn malformed_utf8_quit_value() -> Value {
    let mut attrs = HashMap::new();
    attrs.insert(
        "message".to_string(),
        Value::str("Malformed UTF-8 in process output".to_string()),
    );
    Value::make_instance(Symbol::intern("X::Str::Decode::Malformed"), attrs)
}

impl Interpreter {
    pub(super) fn native_proc_async_mut(
        &mut self,
        mut attrs: AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, AttrMap), RuntimeError> {
        match method {
            "start" => {
                use std::process::{Command, Stdio};

                if attrs.get("started").is_some_and(|v| v.truthy()) {
                    return Err(proc_async_error("X::Proc::Async::AlreadyStarted", &[]));
                }
                attrs.insert("started".to_string(), Value::TRUE);

                // Extract command and args
                let mut cmd_arr = match attrs.get("cmd").map(Value::view) {
                    Some(ValueView::Array(arr, ..)) => arr.to_vec(),
                    _ => Vec::new(),
                };
                if let Some(first) = cmd_arr.first().cloned() {
                    let expanded = match first.view() {
                        ValueView::Array(items, ..) => Some(items.to_vec()),
                        ValueView::Seq(items) => Some(items.to_vec()),
                        ValueView::Slip(items) => Some(items.to_vec()),
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
                    if let ValueView::Instance { attributes, .. } = v.view()
                        && let Some(ValueView::Int(id)) =
                            attributes.as_map().get("supply_id").map(Value::view)
                    {
                        return Some(id as u64);
                    }
                    None
                });
                let stderr_supply_id = attrs.get("stderr").and_then(|v| {
                    if let ValueView::Instance { attributes, .. } = v.view()
                        && let Some(ValueView::Int(id)) =
                            attributes.as_map().get("supply_id").map(Value::view)
                    {
                        return Some(id as u64);
                    }
                    None
                });
                let merged_supply_id = attrs.get("supply").and_then(|v| {
                    if let ValueView::Instance { attributes, .. } = v.view()
                        && let Some(ValueView::Int(id)) =
                            attributes.as_map().get("supply_id").map(Value::view)
                    {
                        return Some(id as u64);
                    }
                    None
                });

                // Check if stdout/stderr should deliver binary (Buf) data
                let stdout_bin = attrs
                    .get("stdout_mode")
                    .is_some_and(|v| matches!(v.view(), ValueView::Str(s) if s.as_str() == "bin"));
                let stderr_bin = attrs
                    .get("stderr_mode")
                    .is_some_and(|v| matches!(v.view(), ValueView::Str(s) if s.as_str() == "bin"));

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

                // A stream is captured only when the program actually claimed
                // it: `.stdout`/`.stderr` (which sets `<h>_selected`), the
                // merged `.Supply`, or `bind-stdout`/`bind-stderr`. Rakudo
                // decides exactly this way — at accessor time, not tap time
                // (a Supply fetched before `.start` and tapped after it still
                // receives the output), and an unclaimed stream simply
                // inherits the parent's real fd. mutsu used to pipe both
                // streams unconditionally, so an unclaimed stream's output was
                // read into a channel nobody ever drained and silently
                // vanished instead of appearing on the parent's stdout/stderr.
                // `get_supply_taps` is folded in as a belt-and-braces union,
                // because not every claim goes through a `<h>_selected` write:
                // the read-only `native_proc_async` accessor path has no
                // `&mut self`, and `whenever $proc { ... }` reaches the merged
                // Supply by coercion rather than through the `.Supply` method
                // arm that sets `supply_selected`. A tap on any of the three
                // supplies therefore counts as a claim on its own — losing a
                // claim here would silently drop the output.
                let merged_claimed = attrs.get("supply_selected").is_some_and(|v| v.truthy())
                    || merged_supply_id.is_some_and(|sid| !get_supply_taps(sid).is_empty());
                let claimed = |selected_key: &str, bound: &Option<Value>, sid: Option<u64>| {
                    merged_claimed
                        || attrs.get(selected_key).is_some_and(|v| v.truthy())
                        || bound.as_ref().is_some_and(|v| !v.is_nil())
                        || sid.is_some_and(|sid| !get_supply_taps(sid).is_empty())
                };
                let capture_stdout = claimed("stdout_selected", &bound_stdout, stdout_supply_id);
                let capture_stderr = claimed("stderr_selected", &bound_stderr, stderr_supply_id);

                // Spawn child process synchronously so we get the PID immediately
                let mut cmd = Command::new(&program);
                cmd.args(&cmd_args)
                    .stdout(if capture_stdout {
                        Stdio::piped()
                    } else {
                        Stdio::inherit()
                    })
                    .stderr(if capture_stderr {
                        Stdio::piped()
                    } else {
                        Stdio::inherit()
                    });
                if w_flag || bound_stdin.is_some() {
                    cmd.stdin(Stdio::piped());
                }

                // Honor `.start(:$cwd, :$ENV)`. zef's tar/git/curl shell-outs run
                // with `:cwd($archive.parent)` and a *relative* path
                // (`./foo.tar.gz`), so an ignored `:cwd` made every `zef` extract
                // (and any relative-path child) run in the wrong directory and
                // fail. `:ENV` replaces the child's whole environment with the
                // given hash (Raku semantics), matching Rakudo's `Proc::Async`.
                let mut env_override = false;
                for arg in &args {
                    if let ValueView::Pair(key, val) = arg.view() {
                        match key.as_str() {
                            "cwd" => {
                                let dir = val.to_string_value();
                                if !dir.is_empty() {
                                    cmd.current_dir(dir);
                                }
                            }
                            "ENV" => {
                                if let ValueView::Hash(map) = val.view() {
                                    cmd.env_clear();
                                    for (env_key, env_val) in map.iter() {
                                        cmd.env(env_key, env_val.to_string_value());
                                    }
                                    env_override = true;
                                }
                            }
                            _ => {}
                        }
                    }
                }
                // No `:ENV` override: explicitly apply mutsu's own `%*ENV`
                // rather than relying on `Command::spawn()`'s default (inherit
                // the OS process environment as `std::env` sees it right now).
                // `%*ENV<k> = v` does mirror into `std::env::set_var` (see
                // `vm_var_assign_element.rs`/`vm_var_assign_index_named.rs`),
                // but that mutation is not reliably visible to a `spawn()` on
                // this thread once ANY other OS thread has ever been spawned in
                // this process (confirmed with a plain `Supply.interval(...)
                // .tap()` before a later default-env `run()` on unmodified
                // main — a std::env::set_var + threads + fork hazard, not
                // specific to Proc::Async; see the ADR-worthy
                // `todo/deep/env-var-write-invisible-to-spawn-after-a-thread.md`
                // finding). Reading mutsu's own `%*ENV` value directly sidesteps
                // the hazard entirely and is what `%*ENV` is authoritatively
                // supposed to mean anyway.
                if !env_override
                    && let Some(env_hash) = self.env.get("%*ENV")
                    && let ValueView::Hash(map) = env_hash.view()
                {
                    cmd.env_clear();
                    for (env_key, env_val) in map.iter() {
                        cmd.env(env_key, env_val.to_string_value());
                    }
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
                    if let Some(ValueView::Promise(ready)) =
                        attrs.get("ready_promise").map(Value::view)
                    {
                        ready.break_with(os_error.clone(), String::new(), String::new());
                    }

                    // Send Quit to stdout/stderr supply channels so react blocks die
                    if let Some(sid) = stdout_supply_id {
                        let (tx, rx) =
                            super::native_methods::supply_channel::supply_event_channel();
                        if let Ok(mut map) = supply_channel_map().lock() {
                            map.insert(sid, rx);
                        }
                        let _ = tx.send(SupplyEvent::Quit(os_error.clone()));
                    }
                    if let Some(sid) = stderr_supply_id {
                        let (tx, rx) =
                            super::native_methods::supply_channel::supply_event_channel();
                        if let Ok(mut map) = supply_channel_map().lock() {
                            map.insert(sid, rx);
                        }
                        let _ = tx.send(SupplyEvent::Quit(os_error.clone()));
                    }

                    // Return a broken promise
                    let promise = SharedPromise::new();
                    promise.break_with(os_error, String::new(), String::new());
                    return Ok((Value::promise(promise), attrs));
                }

                let mut child = child_result.unwrap();

                let pid = child.id();
                attrs.insert("pid".to_string(), Value::int(pid as i64));

                if let Some(ValueView::Instance {
                    attributes: stdout_attrs,
                    ..
                }) = attrs.get("stdout").map(Value::view)
                    && let Some(ValueView::Promise(promise)) = stdout_attrs
                        .as_map()
                        .get("native_descriptor_promise")
                        .map(Value::view)
                {
                    promise.try_keep(Value::int(1)).ok();
                }
                if let Some(ValueView::Instance {
                    attributes: stderr_attrs,
                    ..
                }) = attrs.get("stderr").map(Value::view)
                    && let Some(ValueView::Promise(promise)) = stderr_attrs
                        .as_map()
                        .get("native_descriptor_promise")
                        .map(Value::view)
                {
                    promise.try_keep(Value::int(2)).ok();
                }

                // Resolve ready promise if set
                if let Some(ValueView::Promise(ready)) = attrs.get("ready_promise").map(Value::view)
                {
                    ready.try_keep(Value::int(pid as i64)).ok();
                }

                // Store stdin in global registry if piped
                if let Some(stdin) = child.stdin.take() {
                    let stdin_arc = std::sync::Arc::new(std::sync::Mutex::new(Some(stdin)));
                    if w_flag && let Ok(mut map) = proc_stdin_map().lock() {
                        map.insert(pid, stdin_arc.clone());
                    }
                    if let Some(bytes) = stdin_bytes {
                        let stdin_arc = stdin_arc.clone();
                        // Deliberately UNREGISTERED: this thread touches only
                        // plain bytes (`Vec<u8>` + `Arc<Mutex<ChildStdin>>`),
                        // never a `Gc` value, and the pipe write can block
                        // indefinitely — registering it would starve the GC's
                        // stop-the-world instead.
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
                        // Receives `Value`s (Gc nodes) from the supply channel:
                        // must be a registered GC mutator, with the blocking
                        // recv as a quiescent safe region. Runs no user VM
                        // code, so the default stack suffices.
                        crate::runtime::builtins_system::spawn_gc_helper_thread(
                            "proc-in",
                            move || {
                                if let Some(rx) = take_supply_channel(source_supply_id) {
                                    while let Ok(event) = crate::gc::block_quiescent(|| rx.recv()) {
                                        match event {
                                            SupplyEvent::Emit(value) => {
                                                if let Ok(mut guard) = stdin_arc.lock()
                                                    && let Some(ref mut stdin) = *guard
                                                {
                                                    let _ = stdin.write_all(
                                                        value.to_string_value().as_bytes(),
                                                    );
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
                                        crate::gc::block_quiescent(|| {
                                            std::thread::sleep(std::time::Duration::from_millis(10))
                                        });
                                    }
                                }
                                if let Ok(mut guard) = stdin_arc.lock() {
                                    *guard = None;
                                }
                            },
                        );
                    }
                }

                // Create streaming channels for stdout/stderr
                // These will be consumed by the react event loop. An unclaimed
                // (inherited) stream has no pipe and therefore no reader
                // thread, so it gets no channel either — otherwise a receiver
                // nobody can ever feed would be left parked in the global map.
                let stdout_channel = stdout_supply_id.filter(|_| capture_stdout).map(|sid| {
                    let (tx, rx) = super::native_methods::supply_channel::supply_event_channel();
                    if let Ok(mut map) = supply_channel_map().lock() {
                        map.insert(sid, rx);
                    }
                    tx
                });
                let stderr_channel = stderr_supply_id.filter(|_| capture_stderr).map(|sid| {
                    let (tx, rx) = super::native_methods::supply_channel::supply_event_channel();
                    if let Ok(mut map) = supply_channel_map().lock() {
                        map.insert(sid, rx);
                    }
                    tx
                });

                // A tap registered on `.stdout`/`.stderr` before `.start()` (the
                // normal order — `X::Proc::Async::TapBeforeSpawn` rejects a
                // `.stdout`/`.stderr` accessor call after start, though a Supply
                // fetched early and tapped late is not itself rejected, so this
                // is a best-effort pass, not the only delivery path) can drain
                // this stream's channel live as the reader thread below produces
                // chunks, instead of waiting for the whole run to finish and
                // replaying it as one chunk (see
                // todo/tickets/procasync-stdout-is-not-incremental.md). Take the
                // channel back out immediately — nothing else has run yet to
                // race for it — and hand it to the same live act-loop pump
                // ordinary channel-backed supplies (signals, sockets, …) use.
                // The handles are joined below, alongside the reader threads,
                // before the child's exit is observed by `await`/`.result` —
                // otherwise a caller could see the Promise settle before the
                // last chunk (or the tap's `done =>`) was actually delivered.
                //
                // Skipped for a `whenever $p.stdout { ... }` registered inside a
                // `react`/`supply` block (`!self.supply_emit_buffer.is_empty() ||
                // self.react_active > 0` — the same condition
                // `subtest.rs`'s whenever-registration path itself checks to
                // decide "am I in react mode"; `react_active` alone is not
                // enough because a `whenever`'s SOURCE expression, including
                // this `.start()` call when it is `whenever $p.start { ... }`,
                // runs during react's collection pass, before the drive loop
                // that bumps `react_active` even starts). A `whenever` body
                // there shares lexicals with sibling `whenever`s through the
                // react loop's own single-threaded dispatch, not through a
                // general cross-thread cell — running this tap's callback on
                // a genuinely separate OS thread (as the live pump below
                // does) can leave a write invisible to a sibling `whenever`
                // that reads it (e.g. `whenever $p.stdout { $out ~= $_ }` /
                // `whenever $p.start { … $out … }`; see
                // `t/proc-async.t`'s chained-stdin cases, and
                // `todo/tickets/procasync-stdout-is-not-incremental.md`'s
                // notes on this guard). Plain `.tap()` outside `react` — the
                // shape that ticket reports — is unaffected and still
                // streams live.
                //
                // Also skipped for a non-UTF-8 `:enc` (`get_supply_enc`, set
                // by the "tap" handler from the tap's own effective
                // encoding): the reader thread's live decode
                // (`feed_utf8_incremental`) only understands UTF-8. The
                // await/result-time replay already decodes the raw bytes
                // with the stream's real encoding (`decode_proc_stream`), so
                // falling back to it here is correct, just not incremental
                // for that (uncommon) case.
                let mut live_tap_handles = Vec::new();
                for sid in [stdout_supply_id, stderr_supply_id].into_iter().flatten() {
                    if !self.supply_emit_buffer.is_empty() || self.react_active > 0 {
                        continue;
                    }
                    if let Some(enc) = get_supply_enc(sid)
                        && !matches!(enc.to_ascii_lowercase().as_str(), "utf-8" | "utf8" | "")
                    {
                        continue;
                    }
                    let Some(cb) = get_supply_taps(sid).into_iter().next() else {
                        continue;
                    };
                    let Some(rx) = take_supply_channel(sid) else {
                        continue;
                    };
                    mark_supply_live_tapped(sid);
                    let quit_cb = get_supply_quit_taps(sid).into_iter().next();
                    let mut thread_interp = self.clone_for_thread();
                    let close_flag = rx.close_flag();
                    let close_id = register_act_loop_close(close_flag.clone());
                    live_tap_handles.push(crate::runtime::worker_pool::submit_joinable(
                        move || {
                            Self::run_supply_act_loop(
                                &mut thread_interp,
                                &rx,
                                &cb,
                                0.0,
                                None,
                                quit_cb,
                                Some((close_id, close_flag)),
                                false,
                                true,
                                None,
                            );
                        },
                    ));
                }

                // Take stdout/stderr handles before moving child into thread
                let child_stdout = child.stdout.take();
                let child_stderr = child.stderr.take();

                let promise = SharedPromise::new();
                let ret = Value::promise(promise.clone());
                let cmd_arr_clone = cmd_arr.clone();

                // Builds Proc `Value`s (Gc nodes) and resolves the promise:
                // registered GC mutator; its child-wait / joins are quiescent.
                // Runs no user VM code (`keep` dispatches waiters to a fresh
                // user thread), so the default stack suffices.
                crate::runtime::builtins_system::spawn_gc_helper_thread("proc-wait", move || {
                    // Spawn stdout reader thread — streams raw chunks through channel
                    let stdout_handle = child_stdout.map(|stdout| {
                        let tx = stdout_channel;
                        let bin_mode = stdout_bin;
                        let sid = stdout_supply_id;
                        // Emits Buf `Value`s (Gc nodes): registered mutator,
                        // pipe reads quiescent. No user VM code — default
                        // stack.
                        crate::runtime::builtins_system::spawn_gc_helper_thread(
                            "proc-out",
                            move || {
                                use std::io::Read;
                                let mut stdout = stdout;
                                let mut collected = String::new();
                                let mut raw: Vec<u8> = Vec::new();
                                let mut buf = [0u8; 4096];
                                let mut pending: Vec<u8> = Vec::new();
                                let mut held_cr = false;
                                let mut quit = false;
                                loop {
                                    match crate::gc::block_quiescent(|| stdout.read(&mut buf)) {
                                        Ok(0) => break,
                                        Ok(n) => {
                                            raw.extend_from_slice(&buf[..n]);
                                            if bin_mode {
                                                if let Some(ref tx) = tx {
                                                    let buf_val = make_buf_value(&buf[..n]);
                                                    let _ = tx.send(SupplyEvent::Emit(buf_val));
                                                }
                                            } else if feed_utf8_incremental(
                                                &mut pending,
                                                &buf[..n],
                                                &tx,
                                                &mut collected,
                                                true,
                                                &mut held_cr,
                                            ) {
                                                if let Some(ref tx) = tx {
                                                    let _ = tx.send(SupplyEvent::Quit(
                                                        malformed_utf8_quit_value(),
                                                    ));
                                                }
                                                quit = true;
                                                break;
                                            }
                                        }
                                        Err(_) => break,
                                    }
                                }
                                if !quit {
                                    flush_held_cr(held_cr, &tx, &mut collected);
                                    if let Some(ref tx) = tx {
                                        let _ = tx.send(SupplyEvent::Done);
                                    }
                                }
                                // Retain the raw bytes so the await-time replay can
                                // decode them with the stream's effective encoding
                                // (the channel/`collected` path above only handles the
                                // default UTF-8 case).
                                if let Some(sid) = sid {
                                    set_supply_collected_bytes(sid, raw);
                                }
                                collected
                            },
                        )
                    });

                    // Spawn stderr reader thread — streams raw chunks through channel
                    let stderr_handle = child_stderr.map(|stderr| {
                        let tx = stderr_channel;
                        let bin_mode = stderr_bin;
                        let sid = stderr_supply_id;
                        // Same as the stdout reader: registered + quiescent
                        // reads, no user VM code — default stack.
                        crate::runtime::builtins_system::spawn_gc_helper_thread(
                            "proc-err",
                            move || {
                                use std::io::Read;
                                let mut stderr = stderr;
                                let mut collected = String::new();
                                let mut raw: Vec<u8> = Vec::new();
                                let mut buf = [0u8; 4096];
                                let mut pending: Vec<u8> = Vec::new();
                                let mut held_cr = false;
                                let mut quit = false;
                                loop {
                                    match crate::gc::block_quiescent(|| stderr.read(&mut buf)) {
                                        Ok(0) => break,
                                        Ok(n) => {
                                            raw.extend_from_slice(&buf[..n]);
                                            if bin_mode {
                                                if let Some(ref tx) = tx {
                                                    let buf_val = make_buf_value(&buf[..n]);
                                                    let _ = tx.send(SupplyEvent::Emit(buf_val));
                                                }
                                            } else if feed_utf8_incremental(
                                                &mut pending,
                                                &buf[..n],
                                                &tx,
                                                &mut collected,
                                                false,
                                                &mut held_cr,
                                            ) {
                                                if let Some(ref tx) = tx {
                                                    let _ = tx.send(SupplyEvent::Quit(
                                                        malformed_utf8_quit_value(),
                                                    ));
                                                }
                                                quit = true;
                                                break;
                                            }
                                        }
                                        Err(_) => break,
                                    }
                                }
                                if !quit && let Some(ref tx) = tx {
                                    let _ = tx.send(SupplyEvent::Done);
                                }
                                if let Some(sid) = sid {
                                    set_supply_collected_bytes(sid, raw);
                                }
                                collected
                            },
                        )
                    });

                    // Wait for child to exit (quiescent for the GC's STW)
                    let status = crate::gc::block_quiescent(|| child.wait());
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
                        .and_then(|h| crate::gc::block_quiescent(|| h.join()).ok())
                        .unwrap_or_default();
                    let collected_stderr = stderr_handle
                        .and_then(|h| crate::gc::block_quiescent(|| h.join()).ok())
                        .unwrap_or_default();
                    let collected_stdout = collected_stdout.replace("\r\n", "\n");

                    // Join any live tap consumers spawned above: the reader
                    // threads only guarantee the raw bytes were *read*, not that
                    // a live-tapped stream's last chunk (and `done =>`) was
                    // actually delivered to its callback yet.
                    for handle in live_tap_handles {
                        let _ = crate::gc::block_quiescent(|| handle.join());
                    }

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
                    proc_attrs.insert("exitcode".to_string(), Value::int(exit_code));
                    proc_attrs.insert("signal".to_string(), Value::int(signal));
                    proc_attrs.insert(
                        "command".to_string(),
                        Value::array_with_kind(
                            crate::gc::Gc::new(crate::value::ArrayData::new(cmd_arr_clone)),
                            crate::value::ArrayKind::List,
                        ),
                    );
                    proc_attrs.insert("pid".to_string(), Value::int(pid as i64));
                    if let Some(sid) = stdout_supply_id {
                        proc_attrs.insert("stdout_supply_id".to_string(), Value::int(sid as i64));
                    }
                    if let Some(sid) = stderr_supply_id {
                        proc_attrs.insert("stderr_supply_id".to_string(), Value::int(sid as i64));
                    }
                    proc_attrs.insert("collected_stdout".to_string(), Value::str(collected_stdout));
                    proc_attrs.insert("collected_stderr".to_string(), Value::str(collected_stderr));
                    proc_attrs.insert("collected_merged".to_string(), Value::str(collected_merged));
                    proc_attrs.insert("stdout_taps".to_string(), Value::array(stdout_taps));
                    proc_attrs.insert("stderr_taps".to_string(), Value::array(stderr_taps));
                    if let Some(sid) = merged_supply_id {
                        proc_attrs.insert("supply_id".to_string(), Value::int(sid as i64));
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
                    return Ok((Value::NIL, attrs));
                }
                #[cfg(feature = "native")]
                if let Some(ValueView::Int(pid)) = attrs.get("pid").map(Value::view) {
                    let sig = args
                        .first()
                        .and_then(|v| match v.view() {
                            ValueView::Int(s) => Some(s as i32),
                            ValueView::Enum { value, .. } => Some(value.as_i64() as i32),
                            ValueView::Str(s) => match s.as_str() {
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
                        libc::kill(pid as i32, sig);
                    }
                }
                Ok((Value::NIL, attrs))
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
                    return Ok((Value::promise(p), attrs));
                }

                // Write bytes (Buf) to the process's stdin
                let data = args.first().cloned().unwrap_or(Value::NIL);
                let bytes: Vec<u8> = match data.view() {
                    ValueView::Instance {
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
                        crate::value::value_buf::buf_bytes_or_empty(&attributes)
                    }
                    ValueView::Str(s) => {
                        let enc = attrs
                            .get("enc")
                            .map(Value::to_string_value)
                            .unwrap_or_else(|| "utf-8".to_string());
                        self.encode_with_encoding(s.as_str(), &enc)
                            .unwrap_or_else(|_| s.as_bytes().to_vec())
                    }
                    _ => Vec::new(),
                };

                if let Some(ValueView::Int(pid)) = attrs.get("pid").map(Value::view) {
                    let pid = pid as u32;
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
                p.keep(Value::TRUE, String::new(), String::new());
                Ok((Value::promise(p), attrs))
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
                    return Ok((Value::TRUE, attrs));
                }
                if let Some(ValueView::Int(pid)) = attrs.get("pid").map(Value::view) {
                    let pid = pid as u32;
                    if let Ok(map) = proc_stdin_map().lock()
                        && let Some(stdin_arc) = map.get(&pid).cloned()
                    {
                        drop(map);
                        if let Ok(mut guard) = stdin_arc.lock() {
                            *guard = None; // Drop the ChildStdin to close it
                        }
                    }
                }
                Ok((Value::TRUE, attrs))
            }
            "bind-stdin" => {
                if attrs.get("w").is_some_and(|v| v.truthy()) {
                    return Err(proc_async_error(
                        "X::Proc::Async::BindOrUse",
                        &[
                            ("handle", Value::str_from("stdin")),
                            ("use", Value::str_from("use :w")),
                        ],
                    ));
                }
                let bound = args.first().cloned().unwrap_or(Value::NIL);
                attrs.insert("stdin_bind".to_string(), bound);
                Ok((Value::NIL, attrs))
            }
            "bind-stdout" | "bind-stderr" => {
                let handle_name = if method == "bind-stdout" {
                    "stdout"
                } else {
                    "stderr"
                };
                // Rakudo distinguishes the two ways the stream was already
                // claimed: the merged `.Supply` reads as "the output Supply",
                // the per-stream accessor as "the <handle> Supply".
                if attrs.get("supply_selected").is_some_and(|v| v.truthy()) {
                    return Err(proc_async_error(
                        "X::Proc::Async::BindOrUse",
                        &[
                            ("handle", Value::str_from(handle_name)),
                            ("use", Value::str_from("get the output Supply")),
                        ],
                    ));
                }
                if attrs
                    .get(format!("{}_selected", handle_name))
                    .is_some_and(|v| v.truthy())
                {
                    return Err(proc_async_error(
                        "X::Proc::Async::BindOrUse",
                        &[
                            ("handle", Value::str_from(handle_name)),
                            ("use", Value::str(format!("get the {} Supply", handle_name))),
                        ],
                    ));
                }
                let bound = args.first().cloned().unwrap_or(Value::NIL);
                attrs.insert(format!("{}_bind", handle_name), bound);
                Ok((Value::NIL, attrs))
            }
            "ready" => {
                // If spawn failed, return a broken promise with the error
                if let Some(err) = attrs.get("spawn_error").cloned() {
                    let promise = SharedPromise::new();
                    promise.break_with(err, String::new(), String::new());
                    return Ok((Value::promise(promise), attrs));
                }
                // Returns a Promise that resolves with the PID when the process
                // has been started. If already started, resolves immediately.
                let promise = SharedPromise::new();
                if let Some(ValueView::Int(pid)) = attrs.get("pid").map(Value::view) {
                    promise.keep(Value::int(pid), String::new(), String::new());
                }
                // Store the ready promise so start can resolve it
                attrs.insert("ready_promise".to_string(), Value::promise(promise.clone()));
                Ok((Value::promise(promise), attrs))
            }
            "stdout" | "stderr" => {
                if attrs
                    .get(format!("{}_bind", method))
                    .is_some_and(|v| !v.is_nil())
                {
                    return Err(proc_async_error(
                        "X::Proc::Async::BindOrUse",
                        &[
                            ("handle", Value::str_from(method)),
                            ("use", Value::str(format!("get the {} Supply", method))),
                        ],
                    ));
                }
                if attrs.get("supply_selected").is_some_and(|v| v.truthy()) {
                    return Err(proc_async_error("X::Proc::Async::SupplyOrStd", &[]));
                }
                let requested_bin = args.iter().any(
                    |arg| matches!(arg.view(), ValueView::Pair(key, value) if key == "bin" && value.truthy()),
                );
                let mode_key = format!("{}_mode", method);
                if let Some(prev) = attrs.get(&mode_key).and_then(|v| match v.view() {
                    ValueView::Str(s) => Some(s.to_string()),
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
                    attrs.insert("stdout_selected".to_string(), Value::TRUE);
                } else {
                    attrs.insert("stderr_selected".to_string(), Value::TRUE);
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
                    .any(|arg| !matches!(arg.view(), ValueView::Pair(key, _) if key == "bin" || key == "enc"))
                {
                    return Err(proc_async_error(
                        "X::Proc::Async::CharsOrBytes",
                        &[("handle", Value::str_from(method))],
                    ));
                }
                // A per-tap `:enc` (e.g. `$proc.stdout(:enc('latin-1'))`) overrides
                // the constructor `:enc` for this stream's decode. Return a Supply
                // whose `enc` attribute carries the override so the tap records it.
                let value = attrs.get(method).cloned().unwrap_or(Value::NIL);
                let value = if let Some(enc_pair) = args.iter().find_map(|arg| match arg.view() {
                    ValueView::Pair(key, v) if key == "enc" => Some(v.to_string_value()),
                    _ => None,
                }) {
                    if let ValueView::Instance {
                        class_name,
                        attributes,
                        ..
                    } = value.view()
                    {
                        let mut new_attrs = attributes.as_map().clone();
                        new_attrs.insert("enc".to_string(), Value::str(enc_pair));
                        Value::make_instance(class_name, new_attrs)
                    } else {
                        value
                    }
                } else {
                    value
                };
                Ok((value, attrs))
            }
            "Supply" => {
                if attrs.get("stdout_bind").is_some_and(|v| !v.is_nil()) {
                    return Err(proc_async_error(
                        "X::Proc::Async::BindOrUse",
                        &[
                            ("handle", Value::str_from("stdout")),
                            ("use", Value::str_from("get the output Supply")),
                        ],
                    ));
                }
                if attrs.get("stderr_bind").is_some_and(|v| !v.is_nil()) {
                    return Err(proc_async_error(
                        "X::Proc::Async::BindOrUse",
                        &[
                            ("handle", Value::str_from("stderr")),
                            ("use", Value::str_from("get the output Supply")),
                        ],
                    ));
                }
                if attrs.get("stdout_selected").is_some_and(|v| v.truthy())
                    || attrs.get("stderr_selected").is_some_and(|v| v.truthy())
                {
                    return Err(proc_async_error("X::Proc::Async::SupplyOrStd", &[]));
                }
                attrs.insert("supply_selected".to_string(), Value::TRUE);
                Ok((attrs.get("supply").cloned().unwrap_or(Value::NIL), attrs))
            }
            "print" | "put" | "say" => {
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
                    return Ok((Value::promise(p), attrs));
                }
                // Write string to stdin of process, encoded with the constructor
                // `:enc` (`say`/`put` add a trailing newline).
                let data = args.first().cloned().unwrap_or(Value::NIL);
                let mut s = data.to_string_value();
                if method == "say" || method == "put" {
                    s.push('\n');
                }
                let enc = attrs
                    .get("enc")
                    .map(Value::to_string_value)
                    .unwrap_or_else(|| "utf-8".to_string());
                let bytes = self
                    .encode_with_encoding(&s, &enc)
                    .unwrap_or_else(|_| s.as_bytes().to_vec());
                if let Some(ValueView::Int(pid)) = attrs.get("pid").map(Value::view) {
                    let pid = pid as u32;
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
                let p = SharedPromise::new();
                p.keep(Value::TRUE, String::new(), String::new());
                Ok((Value::promise(p), attrs))
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
        let mut table = self.io_handles_mut();
        let Some(state) = table.map.get_mut(&handle_id) else {
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
        let mut table = self.io_handles_mut();
        let Some(state) = table.map.get_mut(&handle_id) else {
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
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = value.view()
            && class_name == "Supply"
            && let Some(ValueView::Int(sid)) = attributes.as_map().get("supply_id").map(Value::view)
            && sid >= 0
        {
            return Some(sid as u64);
        }
        None
    }
}
