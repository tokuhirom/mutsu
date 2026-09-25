use super::*;
use crate::value::AttrMap;

impl Interpreter {
    /// File *content reads* on an `IO::Path` (`slurp`/`lines`/`words`): resolve
    /// the path against the VM-owned cwd, then either read the entire file and
    /// decode it (`slurp`) or open a private read handle whose deferred Seq
    /// reads records on demand (`lines`/`words`, see
    /// [`Self::io_path_lines_or_words`]). The VM dispatches them natively
    /// (ledger §D) via the single impl `native_io_path` also delegates to.
    /// `comb` and `open`/`spurt` return `None` and stay in `native_io_path`.
    pub(crate) fn try_io_path_content_read(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(method, "slurp" | "lines" | "words") {
            return None;
        }
        Some(self.io_path_content_read(attributes, method, args))
    }

    /// The fallible body of [`Self::try_io_path_content_read`] (the gate returns
    /// `Option` so it cannot use `?`).
    fn io_path_content_read(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let path_buf = self.resolve_io_path_buf(attributes, &p);
        match method {
            "slurp" => {
                let (_, _, _, bin, _, _, _, _, enc, _, _) = self.parse_io_flags_values(args);
                if bin {
                    let bytes = fs::read(&path_buf).map_err(|err| {
                        RuntimeError::new(format!("Failed to slurp '{}': {}", p, err))
                    })?;
                    let byte_vals: Vec<Value> = bytes
                        .into_iter()
                        .map(|b| Value::int(i64::from(b)))
                        .collect();
                    return Ok(crate::value::value_buf::make_buf(
                        Symbol::intern("Buf[uint8]"),
                        byte_vals,
                    ));
                }
                // A non-utf-8 encoding reads raw bytes and decodes; utf-8 reads
                // the string directly (stripping a leading BOM).
                let needs_non_utf8 = enc.as_ref().is_some_and(|e| {
                    let lower = e.to_lowercase();
                    lower != "utf-8" && lower != "utf8"
                });
                if needs_non_utf8 {
                    let bytes = fs::read(&path_buf).map_err(|err| {
                        RuntimeError::new(format!("Failed to slurp '{}': {}", p, err))
                    })?;
                    let decoded = self.decode_with_encoding(&bytes, enc.as_ref().unwrap())?;
                    Ok(Value::str(super::utils::translate_nl_in(decoded)))
                } else {
                    let content = fs::read_to_string(&path_buf).map_err(|err| {
                        RuntimeError::new(format!("Failed to slurp '{}': {}", p, err))
                    })?;
                    Ok(Value::str(super::utils::decode_text_content(content)))
                }
            }
            "lines" | "words" => {
                self.io_path_lines_or_words(&path_buf, &p, method == "words", args)
            }
            _ => unreachable!("io_path_content_read called with non-content method"),
        }
    }

    /// `IO::Path.lines` / `.words`: open a private read handle and return its
    /// deferred line / word Seq (`SeqSource::IoLines`), as Rakudo's
    /// `self.open(:$chomp, :$enc, :$nl-in).lines(:close)` does. `.head(n)`,
    /// `.first` and `[i]` then read only the prefix they need (ADR-0119,
    /// #9257). The handle closes when the read reaches EOF, or when a
    /// consuming `.head` / `.first` is done with it (`take_seq_prefix`); a Seq
    /// that is abandoned half-read keeps it open, as in Rakudo. Reads go
    /// through a buffer (`SeqFileReader`), since nothing else can see the
    /// handle's file offset. With a `$limit` the first `$limit` records are
    /// read eagerly and the handle is closed.
    // Cost: O(1) (an open(2)) without a limit; O(bytes up to the limit-th
    // record) with one.
    fn io_path_lines_or_words(
        &mut self,
        path_buf: &std::path::Path,
        display: &str,
        words: bool,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let (_, _, _, _, chomp, nl_in, _, _, enc, _, _) = self.parse_io_flags_values(args);
        let handle = self
            .open_file_handle(
                path_buf,
                true,
                false,
                false,
                false,
                chomp,
                nl_in,
                None,
                None,
                enc,
                false,
                false,
                Some(std::path::Path::new(display)),
            )
            .map_err(|err| {
                let reason = err.message.rsplit(": ").next().unwrap_or("").to_string();
                RuntimeError::new(format!("Failed to read '{}': {}", display, reason))
            })?;
        self.with_handle_mut(&handle, |state| {
            state.close_on_exhaust = true;
            state.seq_reader = state
                .file
                .take()
                .map(crate::runtime::handle_seq_reader::SeqFileReader::new);
            Ok(())
        })?;
        let Some(limit) = args.iter().find_map(numeric_limit_arg) else {
            return Ok(Value::lazy_io_lines(handle, false, words));
        };
        let mut parts = Vec::new();
        while parts.len() < limit {
            let next = if words {
                self.read_word_from_handle_value(&handle)?
            } else {
                self.read_line_from_handle_value(&handle)?
            };
            match next {
                Some(s) => parts.push(Value::str(s)),
                None => break,
            }
        }
        self.close_handle_value(&handle)?;
        Ok(Value::seq(parts))
    }

    /// Open a file handle for an `IO::Path` (`open`): allocate an `io_handles`
    /// entry and return the `IO::Handle`. This is the one IO::Path FS method that
    /// mutates VM-owned `io_handles` state (`&mut self`) — but the VM *owns* that
    /// table (a shared `Arc<RwLock>`), so it dispatches `open` natively (ledger §D
    /// ③) via the single shared `open_file_handle` the interpreter also uses, with
    /// the same `:r`/`:w`/`:a`/`:rw`/`:bin`/`:enc`/`:create`/`:exclusive` flag
    /// handling and the same Failure-on-error shaping. Path resolution
    /// (`resolve_io_path_buf`) and flag parsing (`parse_io_flags_values`) are
    /// `&self` reads returning owned values, so there is no borrow conflict with
    /// the subsequent `&mut self` `open_file_handle`. Returns `None` for any other
    /// method.
    pub(crate) fn try_io_path_open(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if method != "open" {
            return None;
        }
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let path_buf = self.resolve_io_path_buf(attributes, &p);
        let (
            read,
            write,
            append,
            bin,
            line_chomp,
            line_separators,
            out_buffer_capacity,
            nl_out,
            enc,
            create,
            exclusive,
        ) = self.parse_io_flags_values(args);
        Some(
            match self.open_file_handle(
                &path_buf,
                read,
                write,
                append,
                bin,
                line_chomp,
                line_separators,
                out_buffer_capacity,
                nl_out,
                enc,
                create,
                exclusive,
                Some(std::path::Path::new(&p)),
            ) {
                Ok(handle) => Ok(handle),
                // Like the `open` sub, `IO::Path.open` returns a Failure (wrapping
                // the exception) on error rather than throwing.
                Err(err) => {
                    let class_name = err
                        .exception
                        .as_deref()
                        .and_then(|ex| match ex.view() {
                            ValueView::Instance { class_name, .. } => Some(class_name.to_string()),
                            _ => None,
                        })
                        .unwrap_or_else(|| "X::AdHoc".to_string());
                    Ok(io_exception_failure(&class_name, err.message.into_owned()))
                }
            },
        )
    }

    /// `IO::Path.comb`: read the whole file, then comb the content. The matcher
    /// dispatch (`dispatch_comb_with_args`) is `&mut self` because a regex/closure
    /// matcher runs the match engine — but it reads no `io_handles`, so the VM
    /// dispatches `comb` natively (ledger §D): the single impl `native_io_path`
    /// also delegates to. **Also fixes a pre-existing bug**: the no-matcher form
    /// (`$path.IO.comb` with no positional) used to return an empty Seq here (the
    /// old arm mapped `dispatch_comb_with_args`'s `None` to empty); it now splits
    /// the content into graphemes, matching `Str.comb` and Rakudo. Returns `None`
    /// for any other method.
    pub(crate) fn try_io_path_comb(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if method != "comb" {
            return None;
        }
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let path_buf = self.resolve_io_path_buf(attributes, &p);
        let content = match fs::read_to_string(&path_buf) {
            Ok(c) => super::utils::decode_text_content(c),
            Err(err) => {
                return Some(Err(RuntimeError::new(format!(
                    "Failed to read '{}': {}",
                    p, err
                ))));
            }
        };
        // Filter out :close (irrelevant for IO::Path) before delegating.
        let comb_args: Vec<Value> = args
            .iter()
            .filter(|a| !matches!(a.view(), ValueView::Pair(k, _) if k == "close"))
            .cloned()
            .collect();
        // No matcher combs into graphemes inside `dispatch_comb_with_args`
        // (same as `Str.comb` with no args / Rakudo).
        self.dispatch_comb_with_args(Value::str(content), &comb_args)
    }
}
