use super::*;

impl Interpreter {
    /// Interpreter-native dispatch for the pure-handle methods of an `IO::Handle`
    /// (`close`/`tell`/`eof`/`seek`/`opened`/`t` plus the PR-D Tier-1
    /// setters/getters `chomp`/`nl-out`/`out-buffer`/`encoding` and
    /// `native-descriptor`) — the ones that touch only the handle's own state,
    /// with no `emit_output` / env / encoding-helper dependency.
    /// Operates on the Interpreter's own [`io_handles`](Interpreter::io_handles_mut) handle and the
    /// shared `IoHandleState` methods (the single authoritative impl the
    /// interpreter's `*_handle_value` wrappers also use), so behavior is identical
    /// to the interpreter's native fork.
    ///
    /// Returns `None` (fall through to the interpreter) for any other receiver,
    /// any other method, unexpected arity, or junction arguments (which need
    /// interpreter autothreading). Restricted to the exact `"IO::Handle"` class:
    /// `IO::Socket::INET` (socket-semantic close) and `IO::Pipe` (process-reaping
    /// close) are intentionally excluded.
    pub(crate) fn try_native_io_handle_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let id = match target {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "IO::Handle" => match attributes.as_map().get("handle") {
                Some(Value::Int(i)) if *i >= 0 => *i as usize,
                _ => return None,
            },
            _ => return None,
        };

        // Pure-handle setters/getters whose argument touches only handle state
        // (PLAN.md ③ native IO PR-D Tier-1). Junction arguments fall through to
        // the interpreter for autothreading.
        if args.iter().any(|a| matches!(a, Value::Junction { .. })) {
            return None;
        }

        // `.flush` is target-agnostic and pure (flush pending out-buffer + the OS
        // file buffer); shared `flush_for_method` (PR-D Tier-2b). A handle id not
        // in the table falls through — the interpreter shapes that into an
        // `X::IO::Flush` Failure (a value, not an error), which this Err-on-absent
        // path below cannot reproduce.
        if method == "flush" && args.is_empty() {
            let mut table = self.io_handles_mut();
            return table
                .map
                .get_mut(&id)
                .map(|state| state.flush_for_method().map(|_| Value::Bool(true)));
        }

        // `.encoding` returns Nil for binary mode and a Str otherwise, so its
        // result shaping is method-specific (mirrors the interpreter's
        // `native_io` arm exactly).
        enum EncodingOp {
            /// Getter: shape the current encoding into Nil("bin") / Str.
            Get,
            /// Setter to binary mode (`:bin`, `"bin"`, or `Nil` arg) -> Nil.
            SetBin,
            /// Setter to a named encoding -> Str(encoding).
            Set(String),
        }

        enum Op {
            Tell,
            Eof,
            Opened,
            Tty,
            Close,
            Seek(i64, i32),
            Chomp(Option<bool>),
            NlOut(Option<String>),
            OutBuffer(Option<Option<usize>>),
            Encoding(EncodingOp),
            NativeDescriptor,
        }
        let op = match method {
            "tell" if args.is_empty() => Op::Tell,
            "eof" if args.is_empty() => Op::Eof,
            "opened" if args.is_empty() => Op::Opened,
            "t" if args.is_empty() => Op::Tty,
            "close" if args.is_empty() => Op::Close,
            "native-descriptor" if args.is_empty() => Op::NativeDescriptor,
            "chomp" => Op::Chomp(args.first().map(|a| a.truthy())),
            "nl-out" => Op::NlOut(args.first().map(|a| a.to_string_value())),
            "out-buffer" => Op::OutBuffer(
                args.first()
                    .map(crate::runtime::Interpreter::parse_out_buffer_size),
            ),
            "encoding" => Op::Encoding(match args.first() {
                None => EncodingOp::Get,
                Some(Value::Nil) => EncodingOp::SetBin,
                Some(arg) => {
                    let enc = arg.to_string_value();
                    if enc == "bin" {
                        EncodingOp::SetBin
                    } else {
                        EncodingOp::Set(enc)
                    }
                }
            }),
            "seek" => {
                // seek($offset, $whence = SeekFromBeginning). Mirror the
                // interpreter's `native_io_handle` arg handling exactly: a
                // non-Int offset coerces to 0 (`unwrap_or(0)`), and the whence
                // string maps to 0/1/2 (anything else -> 0). Defer to the
                // interpreter when an arg is a junction (needs autothreading) or
                // the arity is unexpected.
                if args.len() > 2 || args.iter().any(|a| matches!(a, Value::Junction { .. })) {
                    return None;
                }
                let pos = match args.first() {
                    Some(Value::Int(i)) => *i,
                    _ => 0,
                };
                let mode = match args.get(1) {
                    Some(v) => match v.to_string_value().as_str() {
                        "SeekFromCurrent" => 1,
                        "SeekFromEnd" => 2,
                        _ => 0,
                    },
                    None => 0,
                };
                Op::Seek(pos, mode)
            }
            _ => return None,
        };

        let mut table = self.io_handles_mut();
        let Some(state) = table.map.get_mut(&id) else {
            return Some(Err(RuntimeError::new("Invalid IO::Handle")));
        };
        let result = match op {
            Op::Tell => state.tell().map(Value::Int),
            Op::Eof => state.eof().map(Value::Bool),
            Op::Opened => Ok(Value::Bool(state.is_opened())),
            Op::Tty => Ok(Value::Bool(state.is_tty())),
            Op::Close => state.close().map(Value::Bool),
            Op::Seek(pos, mode) => state.seek(pos, mode).map(Value::Int),
            Op::Chomp(set) => Ok(Value::Bool(state.chomp_setting(set))),
            Op::NlOut(set) => Ok(Value::str(state.nl_out_setting(set))),
            Op::OutBuffer(set) => state.out_buffer_setting(set).map(|n| Value::Int(n as i64)),
            Op::NativeDescriptor => state.native_descriptor().map(Value::Int),
            Op::Encoding(enc_op) => Ok(match enc_op {
                EncodingOp::Get => {
                    let cur = state.encoding_setting(None);
                    if cur == "bin" {
                        Value::Nil
                    } else {
                        Value::str(cur)
                    }
                }
                EncodingOp::SetBin => {
                    state.encoding_setting(Some("bin".to_string()));
                    Value::Nil
                }
                EncodingOp::Set(enc) => {
                    state.encoding_setting(Some(enc.clone()));
                    Value::str(enc)
                }
            }),
        };
        Some(result)
    }

    /// Interpreter-native text output for a `File`+UTF8 `IO::Handle` receiver
    /// (`print`/`put`/`say`/`printf`/`print-nl`): build the payload exactly as
    /// the interpreter's `native_io` handlers do — `render_str_value` for
    /// print/put, `render_gist_value` for say (byte-identical; the same helpers
    /// the Interpreter's own `say`/`print` ops use), the pure `sprintf` helpers for
    /// printf — and write it through the Interpreter's `io_handles` handle via the shared
    /// `IoHandleState::native_text_write` (PLAN.md ③ native IO PR-D Tier-2a/2b).
    ///
    /// Returns `None` (fall through to the interpreter) for any non-`IO::Handle`
    /// receiver, any other method, a junction argument (autothreading), or a
    /// handle whose target/encoding is not File+UTF8 — Stdout/Stderr need
    /// `emit_output`/`stderr_output` and a non-UTF8 File needs
    /// `encode_with_encoding`, neither Interpreter-reachable yet (③ 後段/④). The
    /// target/encoding gate is read *before* the payload is built so a
    /// fall-through never double-runs the argument stringification.
    pub(crate) fn try_native_io_handle_output(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let id = match target {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "IO::Handle" => match attributes.as_map().get("handle") {
                Some(Value::Int(i)) if *i >= 0 => *i as usize,
                _ => return None,
            },
            _ => return None,
        };

        enum Kind {
            Print,
            Put,
            Say,
            Printf,
            PrintNl,
        }
        let kind = match method {
            "print" => Kind::Print,
            "put" => Kind::Put,
            "say" => Kind::Say,
            "printf" => Kind::Printf,
            "print-nl" => Kind::PrintNl,
            _ => return None,
        };
        // Junction args autothread in the interpreter; fall through for those
        // (printf's first-arg junction also threads there).
        if args.iter().any(|a| matches!(a, Value::Junction { .. })) {
            return None;
        }

        // Resolve the target *before* building the payload, so falling through
        // (Socket / non-UTF8 File / Stdin) never runs the arg stringification
        // twice. Stdout/Stderr emit via the Interpreter's shared output sink (③後段 PR-C);
        // File writes its handle state (Tier-2a).
        enum Tgt {
            File,
            Stdout,
            Stderr,
        }
        let tgt = {
            let table = self.io_handles_mut();
            let state = table.map.get(&id)?;
            if state.can_native_text_write() {
                Tgt::File
            } else if state.is_stdout_target() {
                Tgt::Stdout
            } else if state.is_stderr_target() {
                Tgt::Stderr
            } else {
                return None;
            }
        };

        // Build the argument content. `print-nl` has no args — its payload is the
        // handle's `nl_out`, which `prepare_text_payload` appends via `newline`
        // (content = "", newline = true). Its closed-handle error uses "write",
        // matching the interpreter's `print-nl` (which calls `write_to_handle_value`).
        let (content, newline, trying): (String, bool, &str) = match kind {
            Kind::PrintNl => (String::new(), true, "write"),
            // printf: validate the directives then format, exactly as the
            // interpreter's `printf` arm (pure `sprintf` helpers, no handle state).
            Kind::Printf => {
                let fmt = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let rest = if args.is_empty() { &[][..] } else { &args[1..] };
                if let Err(e) =
                    crate::runtime::sprintf::validate_sprintf_directives(&fmt, rest.len())
                {
                    return Some(Err(e));
                }
                (
                    crate::runtime::sprintf::format_sprintf_args(&fmt, rest),
                    false,
                    method,
                )
            }
            Kind::Say => {
                let mut c = String::new();
                for arg in args {
                    c.push_str(&loan_env!(self, render_gist_value(arg)));
                }
                (c, true, method)
            }
            // print / put use `render_str_value`.
            Kind::Print => {
                let mut c = String::new();
                for arg in args {
                    c.push_str(&loan_env!(self, render_str_value(arg)));
                }
                (c, false, method)
            }
            Kind::Put => {
                let mut c = String::new();
                for arg in args {
                    c.push_str(&loan_env!(self, render_str_value(arg)));
                }
                (c, true, method)
            }
        };

        // File: prepare + buffered file write in one confined guard.
        if matches!(tgt, Tgt::File) {
            let mut table = self.io_handles_mut();
            let Some(state) = table.map.get_mut(&id) else {
                return Some(Err(RuntimeError::new("Invalid IO::Handle")));
            };
            return Some(
                state
                    .native_text_write(&content, newline, trying)
                    .map(|_| Value::Bool(true)),
            );
        }

        // Stdout/Stderr: prepare the payload on the receiver handle (closed check
        // + nl_out + bytes_written), then emit via the shared output sink.
        let payload = {
            let mut table = self.io_handles_mut();
            let Some(state) = table.map.get_mut(&id) else {
                return Some(Err(RuntimeError::new("Invalid IO::Handle")));
            };
            match state.prepare_text_payload(&content, newline, trying) {
                Ok(p) => p,
                Err(e) => return Some(Err(e)),
            }
        };
        if matches!(tgt, Tgt::Stdout) {
            self.vm_emit_stdout(&payload);
        } else {
            self.vm_emit_stderr(&payload);
        }
        Some(Ok(Value::Bool(true)))
    }

    /// Interpreter-native raw byte output for a `File` `IO::Handle` receiver
    /// (`write` / `spurt`): build the bytes exactly as the interpreter's
    /// `native_io` handlers do and write them straight to the file via the
    /// shared `IoHandleState::native_write_bytes_file` (raw, `:out-buffer`- and
    /// encoding-bypassing — same semantics as `write_bytes_to_handle_value`).
    /// PLAN.md ③ native IO PR-D Tier-2c.
    ///
    /// `write` concatenates each arg's bytes — buffer types (Buf/Blob/utf8/
    /// utf16) via `supply_chunk_to_bytes` (utf16-aware), non-buffers via
    /// `render_str_value` (their UTF-8 bytes). `spurt` writes its single
    /// argument: a Buf's raw bytes, or a Str's UTF-8 bytes.
    ///
    /// Returns `None` (fall through) for any non-`IO::Handle` receiver, any other
    /// method, a junction argument, a non-File target (Stdout/Stderr need
    /// `emit_output`), or a `spurt` of a Str on a non-UTF-8 handle (needs
    /// `encode_with_encoding`). The File gate is read before the bytes are built.
    pub(crate) fn try_native_io_handle_byte_output(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let id = match target {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "IO::Handle" => match attributes.as_map().get("handle") {
                Some(Value::Int(i)) if *i >= 0 => *i as usize,
                _ => return None,
            },
            _ => return None,
        };
        if !matches!(method, "write" | "spurt") {
            return None;
        }
        // Junction args autothread in the interpreter; fall through for those.
        if args.iter().any(|a| matches!(a, Value::Junction { .. })) {
            return None;
        }

        // File-target gate, read before building the bytes. For `spurt` of a Str
        // the handle encoding must also be UTF-8 (a non-UTF-8 encoding re-enters
        // `encode_with_encoding`); `write` and a Buf `spurt` ignore encoding.
        let spurt_str_needs_utf8 =
            method == "spurt" && !args.first().map(Self::is_buf_value).unwrap_or(false);
        {
            let table = self.io_handles_mut();
            let state = table.map.get(&id)?;
            if !state.is_file_target() {
                return None;
            }
            if spurt_str_needs_utf8 && !state.can_native_text_write() {
                return None;
            }
        }

        let bytes: Vec<u8> = if method == "spurt" {
            let content_value = args
                .first()
                .cloned()
                .unwrap_or_else(|| Value::Str(String::new().into()));
            if Self::is_buf_value(&content_value) {
                Self::extract_buf_bytes(&content_value)
            } else {
                // Gate guaranteed UTF-8, so the Str's bytes are its UTF-8 bytes.
                content_value.to_string_value().into_bytes()
            }
        } else {
            // write: concatenate each arg — buffer types via supply_chunk_to_bytes
            // (utf16-aware), non-buffers via render_str_value (UTF-8 bytes).
            let mut out = Vec::new();
            for arg in args {
                if Self::is_buf_value(arg) {
                    out.extend(self.supply_chunk_to_bytes(arg, "utf-8"));
                } else {
                    out.extend(loan_env!(self, render_str_value(arg)).into_bytes());
                }
            }
            out
        };

        let mut table = self.io_handles_mut();
        let Some(state) = table.map.get_mut(&id) else {
            return Some(Err(RuntimeError::new("Invalid IO::Handle")));
        };
        Some(
            state
                .native_write_bytes_file(&bytes)
                .map(|_| Value::Bool(true)),
        )
    }

    /// Interpreter-native reads from a `File`+UTF8 `IO::Handle` receiver (③後段 PR-D read
    /// side): `get` (next line → `Str`/`Nil`), `slurp` (rest of file → `Str`),
    /// `read` (up to N bytes → `Buf`). Each delegates to the shared
    /// `IoHandleState` reader and shapes the result exactly as the interpreter's
    /// handler.
    ///
    /// Returns `None` (fall through) for any non-`IO::Handle` receiver, any other
    /// method, or a handle the native path can't serve — Stdin / ArgFiles (need
    /// `@*ARGS`), a non-UTF-8 / `:bin` slurp (needs `Buf` / `decode_with_encoding`),
    /// junction args — keeping the interpreter's richer read.
    pub(crate) fn try_native_io_handle_read(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let id = match target {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "IO::Handle" => match attributes.as_map().get("handle") {
                Some(Value::Int(i)) if *i >= 0 => *i as usize,
                _ => return None,
            },
            _ => return None,
        };
        if args.iter().any(|a| matches!(a, Value::Junction { .. })) {
            return None;
        }
        match method {
            "get" => {
                if !args.is_empty() {
                    return None;
                }
                let mut table = self.io_handles_mut();
                let state = table.map.get_mut(&id)?;
                // File + UTF-8/binary only; everything else falls through.
                if !state.can_native_text_write() {
                    return None;
                }
                Some(
                    state
                        .read_line_native()
                        .map(|line| line.map(Value::str).unwrap_or(Value::Nil)),
                )
            }
            "slurp" => {
                // `:bin` falls through (returns a Buf via the interpreter).
                let has_bin = args
                    .iter()
                    .any(|a| matches!(a, Value::Pair(k, v) if k == "bin" && v.truthy()));
                let mut table = self.io_handles_mut();
                let state = table.map.get_mut(&id)?;
                if !state.can_native_slurp_string(has_bin) {
                    return None;
                }
                Some(state.slurp_string_native().map(Value::str))
            }
            "read" => {
                // `.read` returns a Buf. Parse the byte count exactly as the
                // interpreter (positive Int → that many bytes; else read to EOF).
                let count = match args.first() {
                    Some(Value::Int(i)) if *i > 0 => *i as usize,
                    None => 0,
                    // Any other first arg (0/negative Int, non-Int) → the
                    // interpreter coerces to 0 (read all); match that.
                    Some(Value::Int(_)) => 0,
                    _ => return None,
                };
                let mut table = self.io_handles_mut();
                let state = table.map.get_mut(&id)?;
                if !state.is_file_target() {
                    return None;
                }
                Some(
                    state
                        .read_bytes_native(count)
                        .map(crate::runtime::Interpreter::make_buf),
                )
            }
            "getc" => {
                if !args.is_empty() {
                    return None;
                }
                let mut table = self.io_handles_mut();
                let state = table.map.get_mut(&id)?;
                // File + UTF-8/binary only (utf16/non-UTF8 need the interpreter).
                if !state.can_native_text_write() {
                    return None;
                }
                // One character, possibly multi-byte; empty -> Nil (as `getc`).
                Some(state.read_chars_native(Some(1)).map(|s| {
                    if s.is_empty() {
                        Value::Nil
                    } else {
                        Value::str(s)
                    }
                }))
            }
            "readchars" => {
                let mut table = self.io_handles_mut();
                let state = table.map.get_mut(&id)?;
                if !state.can_native_text_write() {
                    return None;
                }
                // count: a non-negative integer (parsed as the interpreter does);
                // an invalid arg is the same error; no arg reads to EOF.
                let count = match args.first() {
                    None => None,
                    Some(arg) => match crate::runtime::Interpreter::parse_out_buffer_size(arg) {
                        Some(n) => Some(n),
                        None => {
                            return Some(Err(RuntimeError::new(
                                "readchars count must be a non-negative integer",
                            )));
                        }
                    },
                };
                Some(state.read_chars_native(count).map(Value::str))
            }
            _ => None,
        }
    }
}
