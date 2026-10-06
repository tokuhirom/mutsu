use super::*;

/// Whether any user `WRITE` or `READ` method has ever been declared, anywhere
/// in this process.
///
/// [`Interpreter::try_user_io_handle_method`] sits in the method-dispatch probe
/// chain *without a method-name gate*, so it runs on every method call on every
/// instance — and it was the only probe in that chain that paid for the
/// privilege, resolving the receiver's class `Symbol` to an owned `String` and
/// walking its MRO looking for `IO::Handle` before it could decline. That cost
/// a heap allocation, a string hash and an MRO walk on, for example, every
/// `$o.m()` on a plain user class declared three lines above.
///
/// The probe's own final condition is `!has_write && !has_read`, and
/// `has_user_method` reads the reverse index that `reindex_user_method_name`
/// maintains. So a clear latch here is a proof that the probe would decline:
/// there is no class it could answer for. Same shape and same soundness
/// argument as `crate::value::ANY_DESTROY_DECLARED`.
///
/// Monotonic and never cleared; an over-set only makes the (correct) probe run.
static IO_HANDLE_USER_METHOD_SEEN: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

/// Record that a user `WRITE` or `READ` method now exists (see
/// [`IO_HANDLE_USER_METHOD_SEEN`]). Idempotent; never cleared.
pub(crate) fn note_io_handle_user_method_declared() {
    IO_HANDLE_USER_METHOD_SEEN.store(true, std::sync::atomic::Ordering::Release);
}

/// Whether any user `WRITE`/`READ` method has been registered.
#[inline]
pub(crate) fn io_handle_user_method_declared() -> bool {
    IO_HANDLE_USER_METHOD_SEEN.load(std::sync::atomic::Ordering::Acquire)
}

impl Interpreter {
    /// Build a `Buf[uint8]` value from raw bytes (for a user `WRITE(Blob)` arg).
    fn make_uint8_buf(bytes: Vec<u8>) -> Value {
        crate::builtins::buf_write_num::make_buf_value("Buf", bytes)
    }

    /// Call a user-subclassed `IO::Handle`'s `WRITE(Blob:D)` with `bytes`.
    fn call_user_io_write(
        &mut self,
        target: &Value,
        bytes: Vec<u8>,
    ) -> Result<Value, RuntimeError> {
        let buf = Self::make_uint8_buf(bytes);
        self.call_method_with_values(target.clone(), "WRITE", vec![buf])
    }

    /// Call a user-subclassed `IO::Handle`'s `EOF` predicate.
    /// The handle instance's id, which keys its pushback buffer
    /// (`Interpreter::user_io_read_buffers`).
    fn user_io_handle_id(target: &Value) -> Option<u64> {
        match target.view() {
            ValueView::Instance { id, .. } => Some(id),
            _ => None,
        }
    }

    /// `EOF` for the buffered reader: the user handle is only at end of input
    /// once its own `EOF` says so AND the pushback buffer is drained. A `READ`
    /// that over-returned typically reports `EOF` immediately (it handed
    /// everything over in one call), so consulting the user method alone would
    /// throw away the bytes it just gave us.
    fn call_user_io_eof(&mut self, target: &Value) -> Result<bool, RuntimeError> {
        if Self::user_io_handle_id(target)
            .and_then(|id| self.io.user_io_read_buffers.get(&id))
            .is_some_and(|buf| !buf.is_empty())
        {
            return Ok(false);
        }
        Ok(self
            .call_method_with_values(target.clone(), "EOF", vec![])?
            .truthy())
    }

    /// Call a user-subclassed `IO::Handle`'s `READ(n)`, returning at most `n`
    /// bytes.
    ///
    /// `IO::Handle.read($n)` keeps whatever `READ` hands back beyond `$n` and
    /// serves the next read from it. `Type/IO/Handle.rakudoc`'s second worked
    /// example depends on it: its `READ` ignores the byte count and returns the
    /// whole buffer every time, and rakudo still prints `one` then `two`.
    /// Without the pushback the first `.get` swallowed both lines, and
    /// `read_user_io_char` — which asks for one byte at a time — could not work
    /// against such a handle at all.
    fn call_user_io_read(&mut self, target: &Value, n: usize) -> Result<Vec<u8>, RuntimeError> {
        let Some(id) = Self::user_io_handle_id(target) else {
            let r =
                self.call_method_with_values(target.clone(), "READ", vec![Value::int(n as i64)])?;
            return Ok(Self::extract_buf_bytes(&r));
        };
        while self
            .io
            .user_io_read_buffers
            .get(&id)
            .is_none_or(|buf| buf.len() < n)
        {
            let buffered = self.io.user_io_read_buffers.get(&id).map_or(0, Vec::len);
            let want = n - buffered;
            let r = self.call_method_with_values(
                target.clone(),
                "READ",
                vec![Value::int(want as i64)],
            )?;
            let chunk = Self::extract_buf_bytes(&r);
            if chunk.is_empty() {
                break;
            }
            self.io
                .user_io_read_buffers
                .entry(id)
                .or_default()
                .extend(chunk);
        }
        let buf = self.io.user_io_read_buffers.entry(id).or_default();
        let take = n.min(buf.len());
        Ok(buf.drain(..take).collect())
    }

    /// Drain the rest of a user handle: read chunks via `READ` until `EOF`.
    fn read_all_user_io(&mut self, target: &Value) -> Result<Vec<u8>, RuntimeError> {
        let mut out = Vec::new();
        loop {
            if self.call_user_io_eof(target)? {
                break;
            }
            let chunk = self.call_user_io_read(target, 65536)?;
            if chunk.is_empty() {
                break;
            }
            out.extend(chunk);
        }
        Ok(out)
    }

    /// The line separators for a user handle: the instance's `nl-in` attribute
    /// (a Str or list of Str set via `$fh.nl-in = ...`), defaulting to `["\n"]`.
    fn user_io_line_separators(target: &Value) -> Vec<Vec<u8>> {
        let raw = match target.view() {
            ValueView::Instance { attributes, .. } => attributes.as_map().get("nl-in").cloned(),
            _ => None,
        };
        let seps: Vec<Vec<u8>> = match raw.as_ref() {
            Some(other) => match other.view() {
                ValueView::Array(items, ..) => items
                    .iter()
                    .map(|v| v.to_string_value().into_bytes())
                    .collect(),
                ValueView::Str(s) => vec![s.as_bytes().to_vec()],
                _ => vec![other.to_string_value().into_bytes()],
            },
            None => Vec::new(),
        };
        if seps.is_empty() {
            vec![b"\n".to_vec()]
        } else {
            seps
        }
    }

    /// Read one line from a user handle by consuming bytes via `READ(1)` until a
    /// separator suffix is seen (then chomped) or `EOF`. Returns `None` at EOF.
    fn read_user_io_line(
        &mut self,
        target: &Value,
        seps: &[Vec<u8>],
    ) -> Result<Option<Vec<u8>>, RuntimeError> {
        if self.call_user_io_eof(target)? {
            return Ok(None);
        }
        let mut line: Vec<u8> = Vec::new();
        loop {
            if self.call_user_io_eof(target)? {
                break;
            }
            let b = self.call_user_io_read(target, 1)?;
            if b.is_empty() {
                break;
            }
            line.extend(&b);
            if let Some(sep) = seps.iter().find(|s| !s.is_empty() && line.ends_with(s)) {
                line.truncate(line.len() - sep.len());
                break;
            }
        }
        Ok(Some(line))
    }

    /// Read one character (a complete UTF-8 scalar) from a user handle. Returns
    /// `None` at EOF. Reads the leading byte, then the continuation bytes implied
    /// by its UTF-8 length.
    fn read_user_io_char(&mut self, target: &Value) -> Result<Option<String>, RuntimeError> {
        if self.call_user_io_eof(target)? {
            return Ok(None);
        }
        let lead = self.call_user_io_read(target, 1)?;
        let Some(&b0) = lead.first() else {
            return Ok(None);
        };
        let extra = if b0 < 0x80 {
            0
        } else if b0 >> 5 == 0b110 {
            1
        } else if b0 >> 4 == 0b1110 {
            2
        } else if b0 >> 3 == 0b11110 {
            3
        } else {
            0
        };
        let mut bytes = vec![b0];
        for _ in 0..extra {
            let nb = self.call_user_io_read(target, 1)?;
            if nb.is_empty() {
                break;
            }
            bytes.extend(nb);
        }
        crate::builtins::decode_utf8_code_point(&bytes).map(Some)
    }

    /// Dispatch high-level `IO::Handle` methods on a USER SUBCLASS that overrides
    /// `WRITE`/`READ`/`EOF` (roast S32-io/io-handle.t `.WRITE` / `.EOF/.WRITE`
    /// subtests). Such a handle has no underlying OS file: the high-level output
    /// methods (`print`/`put`/`say`/`printf`/`print-nl`/`write`/`spurt`) are
    /// implemented by encoding to bytes and calling the user `WRITE`. State that
    /// the native path keeps in the handle table (encoding, `nl-out`) is not
    /// available, so `encoding` defaults to UTF-8 and `nl-out` to "\n".
    ///
    /// Returns `None` (fall through) for the exact `IO::Handle` class (the native
    /// file path owns it), a receiver whose class does not inherit `IO::Handle`,
    /// a class that overrides neither `WRITE` nor `READ`, a junction argument, or
    /// a method this user path does not implement.
    pub(crate) fn try_user_io_handle_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        // No user `WRITE`/`READ` exists anywhere, so the `!has_write &&
        // !has_read` bail below is already decided — answer it for one relaxed
        // load instead of an owned `String` and an MRO walk. See
        // `IO_HANDLE_USER_METHOD_SEEN`.
        if !io_handle_user_method_declared() {
            return None;
        }
        let class_name = match target.view() {
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            _ => return None,
        };
        if class_name == "IO::Handle" {
            return None;
        }
        if !self
            .class_mro(&class_name)
            .iter()
            .any(|c| c == "IO::Handle")
        {
            return None;
        }
        let has_write = self.has_user_method(&class_name, "WRITE");
        let has_read = self.has_user_method(&class_name, "READ");
        if !has_write && !has_read {
            return None;
        }
        if args
            .iter()
            .any(|a| matches!(a.view(), ValueView::Junction { .. }))
        {
            return None;
        }

        // `encoding` is accepted but not stored (defaults to UTF-8 for the user
        // byte path); the TWEAK `self.encoding: 'utf8'` in the spec relies only on
        // it not throwing. A `Nil` arg (binary) returns Nil like the native path.
        if method == "encoding" {
            return match args.first() {
                Some(arg) if arg.is_nil() => Some(Ok(Value::NIL)),
                Some(arg) => {
                    let enc = arg.to_string_value();
                    Some(Ok(if enc == "bin" {
                        Value::NIL
                    } else {
                        Value::str(enc)
                    }))
                }
                None => Some(Ok(Value::str("utf-8".to_string()))),
            };
        }

        // A handle that overrides BOTH `WRITE` and `READ` must still reach the
        // read block below: this used to `return None` from the write match's
        // catch-all, so `.read`/`.eof`/`.getc` on such a handle fell through to
        // the native `IO::Handle` arm and died with "Expected IO::Handle" --
        // which is exactly the shape `Type/IO/Handle.rakudoc`'s second worked
        // example uses. `break 'write` falls through instead.
        #[allow(clippy::never_loop)]
        'write: {
            if has_write {
                // `nl-out` for the newline-appending methods; default "\n".
                let nl_out = match target.view() {
                    ValueView::Instance { attributes, .. } => attributes
                        .as_map()
                        .get("nl-out")
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|| "\n".to_string()),
                    _ => "\n".to_string(),
                };
                // Raw-byte methods first: their argument is (or may be) a Blob.
                match method {
                    "write" => {
                        let mut out = Vec::new();
                        for arg in args {
                            if Self::is_buf_value(arg) {
                                out.extend(self.supply_chunk_to_bytes(arg, "utf-8"));
                            } else {
                                out.extend(loan_env!(self, render_str_value(arg)).into_bytes());
                            }
                        }
                        return Some(self.call_user_io_write(target, out).map(|_| Value::TRUE));
                    }
                    "spurt" => {
                        let cv = args
                            .first()
                            .cloned()
                            .unwrap_or_else(|| Value::str(String::new()));
                        let bytes = if Self::is_buf_value(&cv) {
                            Self::extract_buf_bytes(&cv)
                        } else {
                            cv.to_string_value().into_bytes()
                        };
                        return Some(self.call_user_io_write(target, bytes).map(|_| Value::TRUE));
                    }
                    _ => {}
                }
                // Text methods: build the string content then append `nl-out`.
                let (content, newline) = match method {
                    "print" => {
                        let mut c = String::new();
                        for arg in args {
                            c.push_str(&loan_env!(self, render_str_value(arg)));
                        }
                        (c, false)
                    }
                    "put" => {
                        let mut c = String::new();
                        for arg in args {
                            c.push_str(&loan_env!(self, render_str_value(arg)));
                        }
                        (c, true)
                    }
                    "say" => {
                        let mut c = String::new();
                        for arg in args {
                            match loan_env!(self, render_gist_value(arg)) {
                                Ok(g) => c.push_str(&g),
                                Err(e) => return Some(Err(e)),
                            }
                        }
                        (c, true)
                    }
                    "printf" => {
                        // printf requires a format argument; a bare `$handle.printf`
                        // matches no candidate (roast .../multi-no-match.t).
                        if args.is_empty() {
                            return Some(Err(
                                crate::runtime::methods_signature_errors::make_multi_no_match_error(
                                    "printf",
                                ),
                            ));
                        }
                        let fmt = args
                            .first()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let rest = &args[1..];
                        if let Err(e) =
                            crate::runtime::sprintf::validate_sprintf_directives(&fmt, rest.len())
                        {
                            return Some(Err(e));
                        }
                        (
                            crate::runtime::sprintf::format_sprintf_args(&fmt, rest),
                            false,
                        )
                    }
                    "print-nl" => (String::new(), true),
                    _ => break 'write,
                };
                let mut text = content;
                if newline {
                    text.push_str(&nl_out);
                }
                return Some(
                    self.call_user_io_write(target, text.into_bytes())
                        .map(|_| Value::TRUE),
                );
            }
        }

        if has_read {
            match method {
                "eof" => return Some(self.call_user_io_eof(target).map(Value::truth)),
                "slurp" => {
                    let bytes = match self.read_all_user_io(target) {
                        Ok(b) => b,
                        Err(e) => return Some(Err(e)),
                    };
                    return Some(crate::builtins::decode_utf8_handle_text(&bytes).map(Value::str));
                }
                "get" => {
                    let seps = Self::user_io_line_separators(target);
                    return Some(match self.read_user_io_line(target, &seps) {
                        Ok(Some(line)) => {
                            crate::builtins::decode_utf8_handle_text(&line).map(Value::str)
                        }
                        Ok(None) => Ok(Value::NIL),
                        Err(e) => Err(e),
                    });
                }
                "getc" => {
                    return Some(match self.read_user_io_char(target) {
                        Ok(Some(c)) => Ok(Value::str(crate::builtins::nfc(c))),
                        Ok(None) => Ok(Value::NIL),
                        Err(e) => Err(e),
                    });
                }
                "read" => {
                    let n = match args.first().map(Value::view) {
                        Some(ValueView::Int(i)) if i >= 0 => i as usize,
                        Some(ValueView::Num(f)) if f >= 0.0 => f as usize,
                        _ => 0,
                    };
                    return Some(self.call_user_io_read(target, n).map(Self::make_uint8_buf));
                }
                "readchars" => {
                    let n = match args.first().map(Value::view) {
                        Some(ValueView::Int(i)) if i >= 0 => i as usize,
                        Some(ValueView::Num(f)) if f >= 0.0 => f as usize,
                        _ => 0,
                    };
                    let mut s = String::new();
                    for _ in 0..n {
                        match self.read_user_io_char(target) {
                            Ok(Some(c)) => s.push_str(&c),
                            Ok(None) => break,
                            Err(e) => return Some(Err(e)),
                        }
                    }
                    return Some(Ok(Value::str(crate::builtins::nfc(s))));
                }
                "lines" => {
                    let seps = Self::user_io_line_separators(target);
                    let mut out = Vec::new();
                    loop {
                        match self.read_user_io_line(target, &seps) {
                            Ok(Some(line)) => match crate::builtins::decode_utf8_handle_text(&line)
                            {
                                Ok(text) => out.push(Value::str(text)),
                                Err(e) => return Some(Err(e)),
                            },
                            Ok(None) => break,
                            Err(e) => return Some(Err(e)),
                        }
                    }
                    return Some(Ok(Value::seq(out)));
                }
                // words/split/comb operate on the fully-read, decoded text by
                // delegating to the corresponding Str method (which already
                // implements the regex/whitespace semantics).
                "words" | "split" | "comb" => {
                    let bytes = match self.read_all_user_io(target) {
                        Ok(b) => b,
                        Err(e) => return Some(Err(e)),
                    };
                    let text = match crate::builtins::decode_utf8_handle_text(&bytes) {
                        Ok(text) => Value::str(text),
                        Err(e) => return Some(Err(e)),
                    };
                    return Some(self.call_method_with_values(text, method, args.to_vec()));
                }
                _ => {}
            }
        }

        None
    }
}
