use super::*;
use unicode_segmentation::UnicodeSegmentation;

impl Interpreter {
    /// Filesystem *whole-file content reads* on an `IO::Path`
    /// (`slurp`/`lines`/`words`): resolve the path against the VM-owned cwd, read
    /// the entire file (`fs::read[_to_string]`), then split / decode the bytes.
    /// These allocate **no `io_handles`** and emit nothing; flag parsing
    /// (`parse_io_flags_values`) and encoding lookup (`decode_with_encoding`, which
    /// reads the VM-owned encoding registry) are `&self` reads, so the VM
    /// dispatches them natively (ledger §D) via the single impl `native_io_path`
    /// also delegates to. `comb` (its regex/closure dispatch needs `&mut self`)
    /// and `open`/`spurt` (io_handles / FS writes) return `None` and stay in
    /// `native_io_path`. Behavior-invariant (same read + split/decode logic).
    pub(crate) fn try_io_path_content_read(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(method, "slurp" | "lines" | "words") {
            return None;
        }
        Some(self.io_path_content_read(attributes, method, args))
    }

    /// The fallible body of [`try_io_path_content_read`] (the gate returns
    /// `Option` so it cannot use `?`). Resolves the path then reads + splits /
    /// decodes the whole file. Behavior-invariant with the arms `native_io_path`
    /// previously held.
    fn io_path_content_read(
        &self,
        attributes: &HashMap<String, Value>,
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
                        .map(|b| Value::Int(i64::from(b)))
                        .collect();
                    let mut attrs = HashMap::new();
                    attrs.insert("bytes".to_string(), Value::array(byte_vals));
                    return Ok(Value::make_instance(Symbol::intern("Buf[uint8]"), attrs));
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
                    Ok(Value::str(decoded))
                } else {
                    let content = fs::read_to_string(&path_buf).map_err(|err| {
                        RuntimeError::new(format!("Failed to slurp '{}': {}", p, err))
                    })?;
                    Ok(Value::str(super::utils::strip_utf8_bom(content)))
                }
            }
            "lines" => {
                let content = fs::read_to_string(&path_buf)
                    .map_err(|err| RuntimeError::new(format!("Failed to read '{}': {}", p, err)))?;
                let content = super::utils::strip_utf8_bom(content);
                let (_, _, _, _, chomp, nl_in, _, _, _, _, _) = self.parse_io_flags_values(args);
                let mut parts = Self::split_content_by_separators(&content, &nl_in, chomp);
                if let Some(n) = args.iter().find_map(numeric_limit_arg) {
                    parts.truncate(n);
                }
                Ok(Value::Seq(std::sync::Arc::new(parts)))
            }
            "words" => {
                let content = fs::read_to_string(&path_buf)
                    .map_err(|err| RuntimeError::new(format!("Failed to read '{}': {}", p, err)))?;
                let content = super::utils::strip_utf8_bom(content);
                let mut parts: Vec<Value> = content
                    .split_whitespace()
                    .map(|token| Value::str(token.to_string()))
                    .collect();
                if let Some(n) = args.iter().find_map(numeric_limit_arg) {
                    parts.truncate(n);
                }
                Ok(Value::Seq(std::sync::Arc::new(parts)))
            }
            _ => unreachable!("io_path_content_read called with non-content method"),
        }
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
        attributes: &HashMap<String, Value>,
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
            ) {
                Ok(handle) => Ok(handle),
                // Like the `open` sub, `IO::Path.open` returns a Failure (wrapping
                // the exception) on error rather than throwing.
                Err(err) => {
                    let class_name = err
                        .exception
                        .as_deref()
                        .and_then(|ex| match ex {
                            Value::Instance { class_name, .. } => Some(class_name.to_string()),
                            _ => None,
                        })
                        .unwrap_or_else(|| "X::AdHoc".to_string());
                    Ok(io_exception_failure(&class_name, err.message))
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
        attributes: &HashMap<String, Value>,
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
            Ok(c) => super::utils::strip_utf8_bom(c),
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
            .filter(|a| !matches!(a, Value::Pair(k, _) if k == "close"))
            .cloned()
            .collect();
        Some(
            match self.dispatch_comb_with_args(Value::str(content.clone()), &comb_args) {
                Some(res) => res,
                // No matcher: comb into graphemes (same as `Str.comb` with no
                // args / Rakudo). The old arm wrongly returned an empty Seq.
                None => {
                    let parts: Vec<Value> = content
                        .graphemes(true)
                        .map(|g| Value::str(g.to_string()))
                        .collect();
                    Ok(Value::Seq(std::sync::Arc::new(parts)))
                }
            },
        )
    }
}
