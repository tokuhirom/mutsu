use super::*;
use crate::value::AttrMap;


impl Interpreter {
    // Cost: TODO
    pub(crate) fn io_handle_get(
        &mut self,
        target_val: &Value,
        _args: &[Value],
    ) -> Result<Value, RuntimeError> {
        Ok(self
        .read_line_from_handle_value(target_val)?
        .map(Value::str)
        .unwrap_or(Value::NIL))
    }

    // Cost: TODO
    pub(crate) fn io_handle_getc(
        &mut self,
        target_val: &Value,
        _args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let encoding =
            self.with_handle_mut(target_val, |state| Ok(state.encoding.clone()))?;
        let needs_decode = !encoding.is_empty()
            && encoding != "utf-8"
            && encoding != "utf8"
            && encoding != "bin";
        if needs_decode {
            // For single-byte encodings, read 1 byte and decode
            let bytes = self.read_bytes_from_handle_value(target_val, 1)?;
            if bytes.is_empty() {
                Ok(Value::NIL)
            } else {
                let decoded = self.decode_with_encoding(&bytes, &encoding)?;
                Ok(Value::str(decoded))
            }
        } else {
            // UTF-8: read one grapheme cluster (base + extending
            // codepoints), matching `.chars`/`.comb`. Seekable files use
            // the grapheme path; other targets fall back to a single
            // codepoint.
            match self.read_grapheme_from_handle_value(target_val, 1)? {
                Some(s) if s.is_empty() => Ok(Value::NIL), // EOF
                Some(s) => Ok(Value::str(s)),
                None => {
                    let s = self.read_chars_from_handle_value(target_val, Some(1))?;
                    if s.is_empty() {
                        Ok(Value::NIL)
                    } else {
                        Ok(Value::str(s))
                    }
                }
            }
        }
    }

    // Cost: TODO
    pub(crate) fn io_handle_readchars(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let count = if let Some(arg) = args.first() {
            match Self::parse_out_buffer_size(arg) {
                Some(n) => Some(n),
                None => {
                    return Err(RuntimeError::new(
                        "readchars count must be a non-negative integer",
                    ));
                }
            }
        } else {
            None
        };
        // A bounded readchars reads that many GRAPHEMES (matching
        // `.getc`/NFG); an unbounded read (whole file) is the same string
        // either way. Non-file / non-utf8 handles fall back to codepoints.
        if let Some(n) = count
            && let Some(s) = self.read_grapheme_from_handle_value(target_val, n)?
        {
            return Ok(Value::str(s));
        }
        Ok(Value::str(
            self.read_chars_from_handle_value(target_val, count)?,
        ))
    }

    // Cost: TODO
    pub(crate) fn io_handle_lines(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut limit: Option<usize> = None;
        let mut close_after = false;
        for arg in args {
            match arg.view() {
                ValueView::Pair(k, v) if k == "close" => {
                    close_after = v.truthy();
                }
                // Any numeric (incl. allomorphs) is a row limit; named
                // args and Whatever/+Inf mean "no limit".
                _ => {
                    if let Some(n) = numeric_limit_arg(arg) {
                        limit = Some(n);
                    }
                }
            }
        }
        if limit.is_some() {
            // Bounded: read eagerly, return Seq
            let mut lines = Vec::new();
            while let Some(line) = self.read_line_from_handle_value(target_val)? {
                lines.push(Value::str(line));
                if let Some(n) = limit
                    && lines.len() >= n
                {
                    break;
                }
            }
            if close_after {
                self.close_handle_value(target_val)?;
            }
            Ok(Value::seq(lines))
        } else {
            // No limit: return a lazy IO lines iterator so that
            // consumers (e.g. for-loop) can read on demand and
            // $fh.tell reflects the current position.
            Ok(Value::lazy_io_lines(target_val.clone(), false, false))
        }
    }

    // Cost: TODO
    pub(crate) fn io_handle_words(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut limit: Option<usize> = None;
        let mut close_after = false;
        for arg in args {
            match arg.view() {
                ValueView::Pair(k, v) if k == "close" => {
                    close_after = v.truthy();
                }
                // Any numeric (incl. allomorphs) is a word limit; named
                // args and Whatever/+Inf mean "no limit".
                _ => {
                    if let Some(n) = numeric_limit_arg(arg) {
                        limit = Some(n);
                    }
                }
            }
        }
        if limit.is_none() {
            // No limit: return a lazy word iterator so a partial consumer
            // (e.g. `$fh.words[1,2]`) leaves the handle open, while a full
            // consumer triggers close-on-exhaust when `:close` was given.
            if close_after {
                self.with_handle_mut(target_val, |state| {
                    state.close_on_exhaust = true;
                    Ok(())
                })?;
            }
            return Ok(Value::lazy_io_lines(target_val.clone(), false, true));
        }
        let mut words = Vec::new();
        'outer: while let Some(word) = self.read_word_from_handle_value(target_val)? {
            words.push(Value::str(word));
            if let Some(n) = limit
                && words.len() >= n
            {
                break 'outer;
            }
        }
        if close_after {
            self.close_handle_value(target_val)?;
        }
        Ok(Value::seq(words))
    }

    // Cost: TODO
    pub(crate) fn io_handle_read(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // .read() always returns a Buf (Buf[uint8]) in Raku
        let count = args
            .first()
            .and_then(|v| match v.view() {
                ValueView::Int(i) if i > 0 => Some(i as usize),
                _ => None,
            })
            .unwrap_or(0);
        if count > 0 {
            let bytes = self.read_bytes_from_handle_value(target_val, count)?;
            return Ok(Self::make_buf(bytes));
        }
        let mut all_bytes = Vec::new();
        loop {
            let chunk = self.read_bytes_from_handle_value(target_val, 8192)?;
            if chunk.is_empty() {
                break;
            }
            all_bytes.extend(chunk);
        }
        Ok(Self::make_buf(all_bytes))
    }

    // `slurp-rest` is the deprecated Rakudo spelling of "slurp the rest
    // of the handle from the current position" — same behavior as
    // `.slurp` here, which also reads from the current position.
    // Cost: TODO
    pub(crate) fn io_handle_slurp(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let has_bin_arg = Self::named_bool(args, "bin");
        let (is_bin, handle_encoding) = self.with_handle_mut(target_val, |state| {
            let bin = has_bin_arg || state.bin || state.encoding == "bin";
            let enc = state.encoding.clone();
            Ok((bin, enc))
        })?;
        let mut all_bytes = Vec::new();
        loop {
            let chunk = self.read_bytes_from_handle_value(target_val, 8192)?;
            if chunk.is_empty() {
                break;
            }
            all_bytes.extend(chunk);
        }
        if is_bin {
            return Ok(Self::make_buf(all_bytes));
        }
        // Decode using the handle's encoding if it's not UTF-8
        let needs_decode = !handle_encoding.is_empty()
            && handle_encoding != "utf-8"
            && handle_encoding != "utf8"
            && handle_encoding != "bin";
        // Every text-mode decode normalizes CRLF to LF, whatever the
        // encoding (`translate_nl_in`).
        let text = if needs_decode {
            self.decode_with_encoding(&all_bytes, &handle_encoding)?
        } else {
            // The one text decoder, matching `IO::Path.slurp` and Rakudo: NFC
            // normalized, and a malformed byte throws rather than silently
            // becoming U+FFFD. Non-utf8 encodings (incl. the lenient
            // `utf8-c8`) took the `decode_with_encoding` branch above.
            crate::builtins::decode_utf8_handle_text(&all_bytes)?
        };
        Ok(Value::str(crate::runtime::utils::translate_nl_in(text)))
    }

    // Cost: TODO
    pub(crate) fn io_handle_split(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Slurp the handle, optionally close it, then delegate to the
        // generic Str.split implementation.
        let close = args.iter().any(
            |a| matches!(a.view(), ValueView::Pair(k, v) if k == "close" && v.truthy()),
        );
        // Filter out :close from args before delegating to split.
        let split_args: Vec<Value> = args
            .iter()
            .filter(|a| !matches!(a.view(), ValueView::Pair(k, _) if k == "close"))
            .cloned()
            .collect();
        let mut all_bytes = Vec::new();
        loop {
            let chunk = self.read_bytes_from_handle_value(target_val, 8192)?;
            if chunk.is_empty() {
                break;
            }
            all_bytes.extend(chunk);
        }
        let text = crate::runtime::utils::translate_nl_in(
            crate::builtins::decode_utf8_handle_text(&all_bytes)?,
        );
        if close {
            let _ = self.close_handle_value(target_val)?;
        }
        self.handle_split_method(Value::str(text), split_args)
    }

    // Cost: TODO
    pub(crate) fn io_handle_comb(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Slurp the handle, optionally close it, then delegate to the
        // generic Str.comb implementation.
        let close = args.iter().any(
            |a| matches!(a.view(), ValueView::Pair(k, v) if k == "close" && v.truthy()),
        );
        // Filter out :close from args before delegating to comb.
        let comb_args: Vec<Value> = args
            .iter()
            .filter(|a| !matches!(a.view(), ValueView::Pair(k, _) if k == "close"))
            .cloned()
            .collect();
        let mut all_bytes = Vec::new();
        loop {
            let chunk = self.read_bytes_from_handle_value(target_val, 8192)?;
            if chunk.is_empty() {
                break;
            }
            all_bytes.extend(chunk);
        }
        let text = crate::runtime::utils::translate_nl_in(
            crate::builtins::decode_utf8_handle_text(&all_bytes)?,
        );
        if close {
            let _ = self.close_handle_value(target_val)?;
        }
        self.dispatch_comb_with_args(Value::str(text), &comb_args)
            .unwrap_or_else(|| Ok(Value::seq(Vec::new())))
    }

    // Cost: TODO
    pub(crate) fn io_handle_supply(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let target = Self::io_handle_attrs(target_val);
        self.handle_supply(&target, args)
    }


    fn handle_supply(&mut self, target: &AttrMap, args: &[Value]) -> Result<Value, RuntimeError> {
        // Extract :size named parameter (default 65536)
        let size = args
            .iter()
            .find_map(|a| {
                if let ValueView::Pair(name, val) = a.view()
                    && name == "size"
                {
                    match val.view() {
                        ValueView::Int(i) => Some(i as usize),
                        _ => None,
                    }
                } else {
                    None
                }
            })
            .unwrap_or(65536);

        let is_bin = target.get("bin").is_some_and(|v| v.truthy());

        let target_val = Value::make_instance(Symbol::intern("IO::Handle"), target.clone());

        let mut values = Vec::new();
        if is_bin {
            // Binary mode: read bytes in chunks, produce Buf instances
            loop {
                let bytes = self.read_bytes_from_handle_value(&target_val, size)?;
                if bytes.is_empty() {
                    break;
                }
                for chunk in bytes.chunks(size) {
                    values.push(crate::value::value_buf::make_buf_from_bytes(
                        Symbol::intern("Buf[uint8]"),
                        chunk,
                    ));
                }
                if bytes.len() < size {
                    break;
                }
            }
        } else {
            // Text mode: read all content as string, split into chunks of `size` chars
            let path = target.get("path").map(|v| v.to_string_value());
            let content = if let Some(ref p) = path {
                fs::read_to_string(p)
                    .map_err(|err| RuntimeError::new(format!("Failed to read '{}': {}", p, err)))?
            } else {
                // Read from handle
                let mut all = String::new();
                while let Some(line) = self.read_line_from_handle_value(&target_val)? {
                    all.push_str(&line);
                    all.push('\n');
                }
                all
            };
            let chars: Vec<char> = content.chars().collect();
            for chunk in chars.chunks(size) {
                let s: String = chunk.iter().collect();
                values.push(Value::str(s));
            }
        }

        let mut supply_attrs = HashMap::new();
        supply_attrs.insert("live".to_string(), Value::FALSE);
        supply_attrs.insert("values".to_string(), Value::array(values));
        Ok(Value::make_instance(Symbol::intern("Supply"), supply_attrs))
    }
}
