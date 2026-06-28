use super::*;

impl Interpreter {
    /// Mutable dispatch for `IO::Handle` methods that mutate the receiver in
    /// place. Currently only `.open`: in Raku `$fh.open(...)` opens the handle
    /// *and returns self*, so a later `$fh.print`/`$fh.print-nl` operates on the
    /// now-opened handle. mutsu's `.open` builds a fresh opened-handle instance
    /// (the handle id lives in the handle table); to match Raku, the receiver's
    /// attributes must be replaced with the opened handle's so the caller's
    /// binding (`$fh`, or the `with` topic `$_`) reflects the open. The returned
    /// `updated` map is written back to the receiver by the caller
    /// (`write_back_sharing`). Any other method falls back to the immutable path
    /// via the sentinel "No native mutable method" error.
    pub(crate) fn native_io_handle_mut(
        &mut self,
        attributes: HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        if method == "open" {
            let result = self.native_io_handle(&attributes, "open", args)?;
            // A successful open returns an `IO::Handle` instance carrying the new
            // handle id; an error returns a `Failure`. Only mutate the receiver on
            // success — on failure the handle stays unopened (as in Raku).
            if let Value::Instance {
                class_name,
                attributes: new_attrs,
                ..
            } = &result
                && class_name == "IO::Handle"
            {
                let updated = new_attrs.as_map().clone();
                return Ok((result, updated));
            }
            return Ok((result, attributes));
        }
        Err(RuntimeError::new(format!(
            "No native mutable method '{}' on 'IO::Handle'",
            method
        )))
    }

    pub(crate) fn native_io_handle(
        &mut self,
        target: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let target_val = Value::make_instance(Symbol::intern("IO::Handle"), target.clone());
        match method {
            "DESTROY" => {
                // Standard handles ($*IN, $*OUT, $*ERR) must not be closed by DESTROY
                let is_std = if let Some(id) = Self::handle_id_from_value(&target_val) {
                    self.io_handles().map.get(&id).is_some_and(|s| {
                        matches!(
                            s.target,
                            IoHandleTarget::Stdin | IoHandleTarget::Stdout | IoHandleTarget::Stderr
                        )
                    })
                } else {
                    false
                };
                if !is_std {
                    let _ = self.close_handle_value(&target_val)?;
                }
                Ok(Value::Bool(true))
            }
            "path" | "IO" => {
                // For standard handles ($*IN, $*OUT, $*ERR), return IO::Special
                if let Some(id) = Self::handle_id_from_value(&target_val)
                    && let Some(state) = self.io_handles().map.get(&id)
                {
                    let special_name = match state.target {
                        IoHandleTarget::Stdout => Some("STDOUT"),
                        IoHandleTarget::Stderr => Some("STDERR"),
                        IoHandleTarget::Stdin => Some("STDIN"),
                        _ => None,
                    };
                    if let Some(name) = special_name {
                        return Ok(Self::make_io_special_instance(name));
                    }
                }
                if let Some(path_val) = target.get("path") {
                    let io_path = match path_val {
                        Value::Instance { class_name, .. } if class_name == "IO::Path" => {
                            path_val.clone()
                        }
                        _ => self.make_io_path_instance(&path_val.to_string_value()),
                    };
                    return Ok(io_path);
                }
                Ok(Value::Nil)
            }
            "Str" | "gist" => {
                if let Some(path_val) = target.get("path") {
                    let path = match path_val {
                        Value::Instance {
                            class_name,
                            attributes,
                            ..
                        } if class_name == "IO::Path" => attributes
                            .as_map()
                            .get("path")
                            .map(|v| v.to_string_value())
                            .unwrap_or_default(),
                        _ => path_val.to_string_value(),
                    };
                    return Ok(Value::str(path));
                }
                Ok(Value::str_from("IO::Handle()"))
            }
            "open" => {
                // IO::Handle.new(:path(...)).open(:w, :nl-out(...))
                // Merge instance attributes with open args (args override instance attrs)
                let path_str = target
                    .get("path")
                    .map(|v| match v {
                        Value::Instance {
                            class_name,
                            attributes,
                            ..
                        } if class_name == "IO::Path" => attributes
                            .as_map()
                            .get("path")
                            .map(|p| p.to_string_value())
                            .unwrap_or_default(),
                        _ => v.to_string_value(),
                    })
                    .unwrap_or_default();
                let path_buf = self.resolve_path(&path_str);

                // Build merged args: instance attributes as defaults, open args override
                let mut merged_args = Vec::new();
                // Collect which keys the open() args explicitly specify
                let mut explicit_keys: std::collections::HashSet<String> =
                    std::collections::HashSet::new();
                for arg in &args {
                    if let Value::Pair(name, _) = arg {
                        explicit_keys.insert(name.clone());
                    }
                }
                // Add instance attributes as pairs (only if not overridden by open args)
                for (key, value) in target.iter() {
                    if key == "handle" || key == "path" || key == "mode" {
                        continue;
                    }
                    if !explicit_keys.contains(key) {
                        merged_args.push(Value::Pair(key.clone(), Box::new(value.clone())));
                    }
                }
                merged_args.extend(args.iter().cloned());

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
                ) = self.parse_io_flags_values(&merged_args);
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
                    // Like the `open` sub, `IO::Handle.open` returns a Failure
                    // (wrapping the exception) on error rather than throwing.
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
                }
            }
            "nl-out" => {
                let set = args.first().map(|a| a.to_string_value());
                let nl =
                    self.with_handle_mut(&target_val, |state| Ok(state.nl_out_setting(set)))?;
                Ok(Value::str(nl))
            }
            "nl-in" => {
                if let Some(arg) = args.first() {
                    let _ = self.with_handle_mut_opt(&target_val, |state| {
                        match arg {
                            Value::Array(items, ..) => {
                                let seps: Vec<Vec<u8>> = items
                                    .iter()
                                    .map(|v: &Value| v.to_string_value().into_bytes())
                                    .collect();
                                state.line_separators = seps;
                            }
                            _ => {
                                let s = arg.to_string_value();
                                state.line_separators = vec![s.clone().into_bytes()];
                            }
                        }
                        Ok(())
                    })?;
                    return Ok(arg.clone());
                }
                let got = self.with_handle_mut_opt(&target_val, |state| {
                    if state.line_separators.len() == 1 {
                        Ok(Value::str(
                            String::from_utf8_lossy(&state.line_separators[0]).to_string(),
                        ))
                    } else {
                        let items: Vec<Value> = state
                            .line_separators
                            .iter()
                            .map(|s| Value::str(String::from_utf8_lossy(s).to_string()))
                            .collect();
                        Ok(Value::real_array(items))
                    }
                })?;
                match got {
                    Some(v) => Ok(v),
                    None => {
                        // Default nl-in for unopened handles
                        let items: Vec<Value> = self
                            .default_line_separators()
                            .iter()
                            .map(|s| Value::str(String::from_utf8_lossy(s).to_string()))
                            .collect();
                        if items.len() == 1 {
                            Ok(items.into_iter().next().unwrap())
                        } else {
                            Ok(Value::real_array(items))
                        }
                    }
                }
            }
            "chomp" => {
                let set = args.first().map(|a| a.truthy());
                let chomp =
                    self.with_handle_mut(&target_val, |state| Ok(state.chomp_setting(set)))?;
                Ok(Value::Bool(chomp))
            }
            "print-nl" => {
                let nl = self.with_handle_mut(&target_val, |state| Ok(state.nl_out.clone()))?;
                self.write_to_handle_value(&target_val, &nl, false)?;
                Ok(Value::Bool(true))
            }
            "close" => Ok(Value::Bool(self.close_handle_value(&target_val)?)),
            "get" => Ok(self
                .read_line_from_handle_value(&target_val)?
                .map(Value::str)
                .unwrap_or(Value::Nil)),
            "getc" => {
                let encoding =
                    self.with_handle_mut(&target_val, |state| Ok(state.encoding.clone()))?;
                let needs_decode = !encoding.is_empty()
                    && encoding != "utf-8"
                    && encoding != "utf8"
                    && encoding != "bin";
                if needs_decode {
                    // For single-byte encodings, read 1 byte and decode
                    let bytes = self.read_bytes_from_handle_value(&target_val, 1)?;
                    if bytes.is_empty() {
                        Ok(Value::Nil)
                    } else {
                        let decoded = self.decode_with_encoding(&bytes, &encoding)?;
                        Ok(Value::str(decoded))
                    }
                } else {
                    // UTF-8: read one character, possibly multi-byte.
                    let s = self.read_chars_from_handle_value(&target_val, Some(1))?;
                    if s.is_empty() {
                        Ok(Value::Nil)
                    } else {
                        Ok(Value::str(s))
                    }
                }
            }
            "readchars" => {
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
                Ok(Value::str(
                    self.read_chars_from_handle_value(&target_val, count)?,
                ))
            }
            "lines" => {
                let mut limit: Option<usize> = None;
                let mut close_after = false;
                for arg in &args {
                    match arg {
                        Value::Pair(k, v) if k == "close" => {
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
                    while let Some(line) = self.read_line_from_handle_value(&target_val)? {
                        lines.push(Value::str(line));
                        if let Some(n) = limit
                            && lines.len() >= n
                        {
                            break;
                        }
                    }
                    if close_after {
                        self.close_handle_value(&target_val)?;
                    }
                    Ok(Value::Seq(std::sync::Arc::new(lines)))
                } else {
                    // No limit: return a lazy IO lines iterator so that
                    // consumers (e.g. for-loop) can read on demand and
                    // $fh.tell reflects the current position.
                    Ok(Value::LazyIoLines {
                        handle: Box::new(target_val.clone()),
                        kv: false,
                        words: false,
                    })
                }
            }
            "words" => {
                let mut limit: Option<usize> = None;
                let mut close_after = false;
                for arg in &args {
                    match arg {
                        Value::Pair(k, v) if k == "close" => {
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
                        self.with_handle_mut(&target_val, |state| {
                            state.close_on_word_exhaust = true;
                            Ok(())
                        })?;
                    }
                    return Ok(Value::LazyIoLines {
                        handle: Box::new(target_val.clone()),
                        kv: false,
                        words: true,
                    });
                }
                let mut words = Vec::new();
                'outer: while let Some(word) = self.read_word_from_handle_value(&target_val)? {
                    words.push(Value::str(word));
                    if let Some(n) = limit
                        && words.len() >= n
                    {
                        break 'outer;
                    }
                }
                if close_after {
                    self.close_handle_value(&target_val)?;
                }
                Ok(Value::Seq(std::sync::Arc::new(words)))
            }
            "read" => {
                // .read() always returns a Buf (Buf[uint8]) in Raku
                let count = args
                    .first()
                    .and_then(|v| match v {
                        Value::Int(i) if *i > 0 => Some(*i as usize),
                        _ => None,
                    })
                    .unwrap_or(0);
                if count > 0 {
                    let bytes = self.read_bytes_from_handle_value(&target_val, count)?;
                    return Ok(Self::make_buf(bytes));
                }
                let mut all_bytes = Vec::new();
                loop {
                    let chunk = self.read_bytes_from_handle_value(&target_val, 8192)?;
                    if chunk.is_empty() {
                        break;
                    }
                    all_bytes.extend(chunk);
                }
                Ok(Self::make_buf(all_bytes))
            }
            "write" => {
                let mut bytes = Vec::new();
                for arg in &args {
                    match arg {
                        Value::Instance { class_name, .. }
                            if {
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
                            bytes.extend(self.supply_chunk_to_bytes(arg, "utf-8"));
                        }
                        _ => bytes.extend(self.render_str_value(arg).into_bytes()),
                    }
                }
                // Write raw bytes directly to avoid UTF-8 lossy conversion
                // which corrupts non-UTF-8 binary data (e.g., ISO-8859-1 encoded bytes)
                self.write_bytes_to_handle_value(&target_val, &bytes)?;
                Ok(Value::Bool(true))
            }
            "print" => {
                let mut content = String::new();
                for arg in &args {
                    content.push_str(&self.render_str_value(arg));
                }
                self.write_to_handle_value_trying(&target_val, &content, false, "print")?;
                Ok(Value::Bool(true))
            }
            "printf" => {
                // If the first arg is a Junction, thread through it
                if let Some(Value::Junction { kind: _, values }) = args.first() {
                    let mut content = String::new();
                    for v in values.iter() {
                        content.push_str(&self.render_str_value(v));
                    }
                    self.write_to_handle_value_trying(&target_val, &content, false, "printf")?;
                    Ok(Value::Bool(true))
                } else {
                    let fmt = args
                        .first()
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let rest = &args[1..];
                    super::sprintf::validate_sprintf_directives(&fmt, rest.len())?;
                    let content = super::sprintf::format_sprintf_args(&fmt, rest);
                    self.write_to_handle_value_trying(&target_val, &content, false, "printf")?;
                    Ok(Value::Bool(true))
                }
            }
            "say" => {
                let mut content = String::new();
                for arg in &args {
                    content.push_str(&self.render_gist_value(arg));
                }
                self.write_to_handle_value_trying(&target_val, &content, true, "say")?;
                Ok(Value::Bool(true))
            }
            "put" => {
                let mut content = String::new();
                for arg in &args {
                    content.push_str(&self.render_str_value(arg));
                }
                self.write_to_handle_value_trying(&target_val, &content, true, "put")?;
                Ok(Value::Bool(true))
            }
            "spurt" => {
                // IO::Handle.spurt($data) -- write data to the handle
                let content_value = args
                    .first()
                    .cloned()
                    .unwrap_or(Value::Str(String::new().into()));
                let is_buf = crate::runtime::Interpreter::is_buf_value(&content_value);
                if is_buf {
                    let bytes = crate::runtime::Interpreter::extract_buf_bytes(&content_value);
                    self.write_bytes_to_handle_value(&target_val, &bytes)?;
                } else {
                    let content = content_value.to_string_value();
                    // Determine encoding from the handle's state
                    let enc = self
                        .with_handle_mut_opt(&target_val, |state| Ok(state.encoding.clone()))?
                        .unwrap_or_else(|| "utf-8".to_string());
                    let bytes = if enc == "utf-8" || enc == "utf8" {
                        content.into_bytes()
                    } else {
                        self.encode_with_encoding(&content, &enc)?
                    };
                    self.write_bytes_to_handle_value(&target_val, &bytes)?;
                }
                Ok(Value::Bool(true))
            }
            "flush" => {
                let flushed =
                    self.with_handle_mut_opt(&target_val, |state| state.flush_for_method())?;
                if flushed.is_some() {
                    Ok(Value::Bool(true))
                } else {
                    let mut ex_attrs = HashMap::new();
                    ex_attrs.insert(
                        "message".to_string(),
                        Value::str_from("Failed to flush handle"),
                    );
                    let ex = Value::make_instance(Symbol::intern("X::IO::Flush"), ex_attrs);
                    let mut failure_attrs = HashMap::new();
                    failure_attrs.insert("exception".to_string(), ex);
                    Ok(Value::make_instance(
                        Symbol::intern("Failure"),
                        failure_attrs,
                    ))
                }
            }
            "out-buffer" => {
                let set = args.first().map(Self::parse_out_buffer_size);
                let size =
                    self.with_handle_mut(&target_val, |state| state.out_buffer_setting(set))?;
                Ok(Value::Int(size as i64))
            }
            "seek" => {
                let pos = args
                    .first()
                    .and_then(|v| match v {
                        Value::Int(i) => Some(*i),
                        _ => None,
                    })
                    .unwrap_or(0);
                // Second argument: seek mode
                let mode_str = args
                    .get(1)
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "SeekFromBeginning".to_string());
                let seek_mode = match mode_str.as_str() {
                    "SeekFromBeginning" => 0,
                    "SeekFromCurrent" => 1,
                    "SeekFromEnd" => 2,
                    _ => 0,
                };
                let offset = self.seek_handle_value(&target_val, pos, seek_mode)?;
                Ok(Value::Int(offset))
            }
            "tell" => {
                let position = self.tell_handle_value(&target_val)?;
                Ok(Value::Int(position))
            }
            "eof" => {
                let at_end = self.handle_eof_value(&target_val)?;
                Ok(Value::Bool(at_end))
            }
            "t" => {
                let is_tty = self.with_handle_mut(&target_val, |state| Ok(state.is_tty()))?;
                Ok(Value::Bool(is_tty))
            }
            "encoding" => {
                if let Some(arg) = args.first() {
                    // Nil means switch to binary mode (no encoding)
                    if matches!(arg, Value::Nil) {
                        self.set_handle_encoding(&target_val, Some("bin".to_string()))?;
                        return Ok(Value::Nil);
                    }
                    let encoding = arg.to_string_value();
                    if encoding == "bin" {
                        self.set_handle_encoding(&target_val, Some("bin".to_string()))?;
                        return Ok(Value::Nil);
                    }
                    self.set_handle_encoding(&target_val, Some(encoding.clone()))?;
                    return Ok(Value::str(encoding));
                }
                let current = self.set_handle_encoding(&target_val, None)?;
                if current == "bin" {
                    return Ok(Value::Nil);
                }
                Ok(Value::str(current))
            }
            "opened" => {
                let opened = self.with_handle_mut(&target_val, |state| Ok(state.is_opened()))?;
                Ok(Value::Bool(opened))
            }
            "slurp" => {
                let has_bin_arg = Self::named_bool(&args, "bin");
                let (is_bin, handle_encoding) = self.with_handle_mut(&target_val, |state| {
                    let bin = has_bin_arg || state.bin || state.encoding == "bin";
                    let enc = state.encoding.clone();
                    Ok((bin, enc))
                })?;
                let mut all_bytes = Vec::new();
                loop {
                    let chunk = self.read_bytes_from_handle_value(&target_val, 8192)?;
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
                if needs_decode {
                    let decoded = self.decode_with_encoding(&all_bytes, &handle_encoding)?;
                    Ok(Value::str(decoded))
                } else {
                    Ok(Value::str(String::from_utf8_lossy(&all_bytes).to_string()))
                }
            }
            "split" => {
                // Slurp the handle, optionally close it, then delegate to the
                // generic Str.split implementation.
                let close = args
                    .iter()
                    .any(|a| matches!(a, Value::Pair(k, v) if k == "close" && v.truthy()));
                // Filter out :close from args before delegating to split.
                let split_args: Vec<Value> = args
                    .iter()
                    .filter(|a| !matches!(a, Value::Pair(k, _) if k == "close"))
                    .cloned()
                    .collect();
                let mut all_bytes = Vec::new();
                loop {
                    let chunk = self.read_bytes_from_handle_value(&target_val, 8192)?;
                    if chunk.is_empty() {
                        break;
                    }
                    all_bytes.extend(chunk);
                }
                let text = String::from_utf8_lossy(&all_bytes).to_string();
                if close {
                    let _ = self.close_handle_value(&target_val)?;
                }
                self.handle_split_method(Value::str(text), split_args)
            }
            "comb" => {
                // Slurp the handle, optionally close it, then delegate to the
                // generic Str.comb implementation.
                let close = args
                    .iter()
                    .any(|a| matches!(a, Value::Pair(k, v) if k == "close" && v.truthy()));
                // Filter out :close from args before delegating to comb.
                let comb_args: Vec<Value> = args
                    .iter()
                    .filter(|a| !matches!(a, Value::Pair(k, _) if k == "close"))
                    .cloned()
                    .collect();
                let mut all_bytes = Vec::new();
                loop {
                    let chunk = self.read_bytes_from_handle_value(&target_val, 8192)?;
                    if chunk.is_empty() {
                        break;
                    }
                    all_bytes.extend(chunk);
                }
                let text = String::from_utf8_lossy(&all_bytes).to_string();
                if close {
                    let _ = self.close_handle_value(&target_val)?;
                }
                match self.dispatch_comb_with_args(Value::str(text), &comb_args) {
                    Some(res) => res,
                    None => Ok(Value::Seq(std::sync::Arc::new(Vec::new()))),
                }
            }
            "Supply" => self.handle_supply(target, &args),
            "native-descriptor" => {
                let fd = self.with_handle_mut(&target_val, |state| state.native_descriptor())?;
                Ok(Value::Int(fd))
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on IO::Handle",
                method
            ))),
        }
    }

    fn handle_supply(
        &mut self,
        target: &HashMap<String, Value>,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Extract :size named parameter (default 65536)
        let size = args
            .iter()
            .find_map(|a| {
                if let Value::Pair(name, val) = a
                    && name == "size"
                {
                    match val.as_ref() {
                        Value::Int(i) => Some(*i as usize),
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
                    let byte_vals: Vec<Value> =
                        chunk.iter().map(|b| Value::Int(*b as i64)).collect();
                    let mut buf_attrs = HashMap::new();
                    buf_attrs.insert("bytes".to_string(), Value::array(byte_vals));
                    values.push(Value::make_instance(
                        Symbol::intern("Buf[uint8]"),
                        buf_attrs,
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
        supply_attrs.insert("live".to_string(), Value::Bool(false));
        supply_attrs.insert("values".to_string(), Value::array(values));
        Ok(Value::make_instance(Symbol::intern("Supply"), supply_attrs))
    }
}
