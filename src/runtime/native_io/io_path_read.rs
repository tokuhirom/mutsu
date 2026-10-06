use super::*;
use crate::value::AttrMap;

impl Interpreter {
    /// `IO::Path.slurp`: resolve the path against the cwd, then read the entire
    /// file and decode it (a `Buf[uint8]` with `:bin`; text decoded with
    /// `:enc`, utf-8 by default).
    // Cost: O(b), b = the file's size in bytes.
    pub(crate) fn io_path_slurp(
        &mut self,
        attributes: &AttrMap,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let path_buf = self.resolve_io_path_buf(attributes, &p);
        let (_, _, _, bin, _, _, _, _, enc, _, _) = self.parse_io_flags_values(args);
        self.slurp_file(&path_buf, bin, enc.as_deref())
    }

    /// `IO::Path.lines`: the deferred Seq of the file's lines, see
    /// [`Self::io_path_lines_or_words`].
    // Cost: O(1) (an open(2)) without a limit; O(bytes up to the limit-th
    // record) with one.
    pub(crate) fn io_path_lines(
        &mut self,
        attributes: &AttrMap,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        self.io_path_records(attributes, false, args)
    }

    /// `IO::Path.words`: the deferred Seq of the file's words, see
    /// [`Self::io_path_lines_or_words`].
    // Cost: O(1) (an open(2)) without a limit; O(bytes up to the limit-th
    // record) with one.
    pub(crate) fn io_path_words(
        &mut self,
        attributes: &AttrMap,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        self.io_path_records(attributes, true, args)
    }

    fn io_path_records(
        &mut self,
        attributes: &AttrMap,
        words: bool,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let path_buf = self.resolve_io_path_buf(attributes, &p);
        self.io_path_lines_or_words(&path_buf, &p, words, args)
    }

    /// Read the whole file at `path_buf` (already resolved against the cwd):
    /// a `Buf[uint8]` with `bin`, otherwise text decoded with `enc` (utf-8 by
    /// default, stripping a leading BOM). The one implementation behind the
    /// `slurp` sub and `IO::Path.slurp`; a failure is Rakudo's open error
    /// (`fs_errors::read_whole_failed`).
    // Cost: O(b), b = the file's size in bytes.
    pub(crate) fn slurp_file(
        &self,
        path_buf: &std::path::Path,
        bin: bool,
        enc: Option<&str>,
    ) -> Result<Value, RuntimeError> {
        let fail = |err: std::io::Error| super::fs_errors::read_whole_failed(path_buf, &err);
        if bin {
            let bytes = fs::read(path_buf).map_err(fail)?;
            let byte_vals: Vec<Value> = bytes
                .into_iter()
                .map(|b| Value::int(i64::from(b)))
                .collect();
            return Ok(crate::value::value_buf::make_buf(
                Symbol::intern("Buf[uint8]"),
                byte_vals,
            ));
        }
        // A non-utf-8 encoding reads raw bytes and decodes; utf-8 reads the
        // string directly (stripping a leading BOM).
        let non_utf8 = enc.filter(|e| {
            let lower = e.to_lowercase();
            lower != "utf-8" && lower != "utf8"
        });
        if let Some(enc) = non_utf8 {
            let bytes = fs::read(path_buf).map_err(fail)?;
            let decoded = self.decode_with_encoding(&bytes, enc)?;
            Ok(Value::str(super::utils::translate_nl_in(decoded)))
        } else {
            let content = fs::read_to_string(path_buf).map_err(fail)?;
            Ok(Value::str(super::utils::decode_text_content(content)))
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
        let utf8 = enc
            .as_deref()
            .is_none_or(|e| matches!(e.to_lowercase().as_str(), "utf-8" | "utf8"));
        let handle = self.open_file_handle(
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
        )?;
        self.with_handle_mut(&handle, |state| {
            state.close_on_exhaust = true;
            state.seq_reader = state
                .file
                .take()
                .map(|file| crate::runtime::handle_seq_reader::SeqFileReader::new(file, utf8));
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

    /// `IO::Path.open`: allocate an `io_handles` entry and return the
    /// `IO::Handle`, with the `:r`/`:w`/`:a`/`:rw`/`:bin`/`:enc`/`:create`/
    /// `:exclusive` flag handling of the `open` sub (the single
    /// `open_file_handle`). Path resolution (`resolve_io_path_buf`) and flag
    /// parsing (`parse_io_flags_values`) are `&self` reads returning owned
    /// values, so there is no borrow conflict with the subsequent `&mut self`
    /// `open_file_handle`. Like the `open` sub, it returns a `Failure` (wrapping
    /// the exception) on error rather than throwing.
    // Cost: O(p) plus one open(2), p = chars of the path.
    pub(crate) fn io_path_open(
        &mut self,
        attributes: &AttrMap,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
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
        Ok(
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
                Ok(handle) => handle,
                Err(err) => super::fs_errors::open_error_failure(err),
            },
        )
    }

    /// `IO::Path.comb`: read the whole file, then comb the content. The matcher
    /// dispatch (`dispatch_comb_with_args`) is `&mut self` because a regex/closure
    /// matcher runs the match engine, but it reads no `io_handles`. The
    /// no-matcher form splits the content into graphemes, as `Str.comb` and
    /// Rakudo do.
    // Cost: O(b), b = the file's size in bytes, plus the matcher's own cost.
    pub(crate) fn io_path_comb(
        &mut self,
        attributes: &AttrMap,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let path_buf = self.resolve_io_path_buf(attributes, &p);
        let content = match fs::read_to_string(&path_buf) {
            Ok(c) => super::utils::decode_text_content(c),
            Err(err) => {
                return Err(RuntimeError::new(format!(
                    "Failed to read '{}': {}",
                    p, err
                )));
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
            .unwrap_or_else(|| Ok(Value::seq(Vec::new())))
    }
}
