use super::*;
use crate::symbol::Symbol;

/// Check for NUL bytes in a path and return X::IO::Null error if found.
pub(super) fn check_null_in_path(path: &str) -> Result<(), RuntimeError> {
    if path.contains('\0') {
        Err(RuntimeError::new(
            "X::IO::Null: Found null byte in pathname",
        ))
    } else {
        Ok(())
    }
}

pub(super) fn io_exception_error(class_name: &str, message: String) -> RuntimeError {
    let mut err = RuntimeError::new(message);
    err.exception = Some(Box::new(Value::make_instance(
        Symbol::intern(class_name),
        HashMap::new(),
    )));
    err
}

/// Create a Failure value wrapping an IO exception.
pub(super) fn io_exception_failure(class_name: &str, message: String) -> Value {
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(message));
    let ex = Value::make_instance(Symbol::intern(class_name), attrs);
    let mut failure_attrs = HashMap::new();
    failure_attrs.insert("exception".to_string(), ex);
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}

#[cfg(unix)]
pub(super) fn has_required_mode_bits(path: &Path, read: bool, write: bool, execute: bool) -> bool {
    use std::os::unix::fs::PermissionsExt;
    let mode = match fs::metadata(path) {
        Ok(meta) => meta.permissions().mode() & 0o777,
        Err(_) => return false,
    };
    (!read || (mode & 0o444) != 0)
        && (!write || (mode & 0o222) != 0)
        && (!execute || (mode & 0o111) != 0)
}

#[cfg(not(unix))]
pub(super) fn has_required_mode_bits(
    _path: &Path,
    _read: bool,
    _write: bool,
    _execute: bool,
) -> bool {
    true
}

pub(super) fn parse_io_requirements(args: &[Value]) -> (bool, bool, bool, bool) {
    let mut require_dir = true;
    let mut require_read = false;
    let mut require_write = false;
    let mut require_exec = false;
    for arg in args {
        if let Value::Pair(key, val) = arg {
            match key.as_str() {
                "d" => require_dir = val.truthy(),
                "r" => require_read = val.truthy(),
                "w" => require_write = val.truthy(),
                "x" => require_exec = val.truthy(),
                _ => {}
            }
        }
    }
    (require_dir, require_read, require_write, require_exec)
}

impl Interpreter {
    pub(super) fn dir_test_matches(
        &mut self,
        test: &Value,
        entry_name: &str,
        dir_path: &Path,
    ) -> bool {
        if let Value::Bool(b) = test {
            return *b;
        }

        let saved_cwd = self.env.get("$*CWD").cloned();
        let saved_cwd_star = self.env.get("*CWD").cloned();
        let saved_topic = self.env.get("_").cloned();
        let saved_dollar_topic = self.env.get("$_").cloned();

        let cwd_val = self.make_io_path_instance(&Self::stringify_path(dir_path));
        self.env.insert("$*CWD".to_string(), cwd_val.clone());
        self.env.insert("*CWD".to_string(), cwd_val);
        self.env
            .insert("_".to_string(), Value::str(entry_name.to_string()));
        self.env
            .insert("$_".to_string(), Value::str(entry_name.to_string()));

        let matched = match test {
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } => self
                .call_sub_value(test.clone(), vec![Value::str(entry_name.to_string())], true)
                .map(|v| v.truthy())
                .unwrap_or(false),
            _ => self.smart_match(&Value::str(entry_name.to_string()), test),
        };

        if let Some(v) = saved_cwd {
            self.env.insert("$*CWD".to_string(), v);
        } else {
            self.env.remove("$*CWD");
        }
        if let Some(v) = saved_cwd_star {
            self.env.insert("*CWD".to_string(), v);
        } else {
            self.env.remove("*CWD");
        }
        if let Some(v) = saved_topic {
            self.env.insert("_".to_string(), v);
        } else {
            self.env.remove("_");
        }
        if let Some(v) = saved_dollar_topic {
            self.env.insert("$_".to_string(), v);
        } else {
            self.env.remove("$_");
        }

        matched
    }

    /// VM-native dispatch for the file/FS builtin *functions* (`slurp`/`open`/
    /// `unlink`/…). These read or mutate the filesystem and the VM-owned `io_handles`
    /// store; the `builtin_*` impls already own that state, but the only path reaching
    /// them was the generic `call_function` name-match fallback (§D state ownership ③
    /// — IO native methods were already drained; this drains the function forms).
    /// Dispatched after all user-sub resolution (so a user `sub slurp` still wins),
    /// mirroring the `call_function` IO arms 1:1 — same args, same `self`, byte-identical.
    ///
    /// Deliberately excludes `indir` (runs a callback block), `chdir`/`tmpdir`/
    /// `homedir` (process-cwd/env side state), and the output routines
    /// (`print`/`say`/`note`/`warn`/`sink`) — those keep their existing dispatch.
    pub(crate) fn try_native_io_function(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let r = match name {
            "slurp" => self.builtin_slurp(args),
            "spurt" => self.builtin_spurt(args),
            "unlink" => self.builtin_unlink(args),
            "open" => self.builtin_open(args),
            "close" => self.builtin_close(args),
            "dir" => self.builtin_dir(args),
            "copy" => self.builtin_copy(args),
            "rename" | "move" => self.builtin_rename(name, args),
            "chmod" => self.builtin_chmod(args),
            "mkdir" => self.builtin_mkdir(args),
            "rmdir" => self.builtin_rmdir(args),
            "link" => self.builtin_link(args),
            "symlink" => self.builtin_symlink(args),
            _ => return None,
        };
        Some(r)
    }

    pub(super) fn builtin_slurp(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Check if first arg is a named pair (not a positional path)
        let first_is_pair = args.first().is_none_or(|v| matches!(v, Value::Pair(_, _)));
        // If no positional path argument, slurp from $*ARGFILES
        if first_is_pair {
            let argfiles = self.env.get("$*ARGFILES").cloned().unwrap_or(Value::Nil);
            return self.call_method_with_values(argfiles, "slurp", args.to_vec());
        }
        // If first arg is an IO::Handle, delegate to .slurp() method on it
        if let Some(handle @ Value::Instance { class_name, .. }) = args.first()
            && class_name == "IO::Handle"
        {
            let has_close = args[1..].iter().any(
                |arg| matches!(arg, Value::Pair(name, value) if name == "close" && value.truthy()),
            );
            // Filter out :close from args passed to the slurp method
            let remaining: Vec<Value> = args[1..]
                .iter()
                .filter(|arg| !matches!(arg, Value::Pair(name, _) if name == "close"))
                .cloned()
                .collect();
            let result = self.call_method_with_values(handle.clone(), "slurp", remaining)?;
            if has_close {
                self.call_method_with_values(handle.clone(), "close", vec![])?;
            }
            return Ok(result);
        }
        let path = args.first().unwrap().to_string_value();
        check_null_in_path(&path)?;
        let bin = args
            .iter()
            .skip(1)
            .any(|arg| matches!(arg, Value::Pair(name, value) if name == "bin" && value.truthy()));
        let enc = args.iter().skip(1).find_map(|arg| {
            if let Value::Pair(name, value) = arg
                && name == "enc"
            {
                return Some(value.to_string_value());
            }
            None
        });
        if bin {
            let bytes = fs::read(&path)
                .map_err(|err| RuntimeError::new(format!("Failed to slurp '{}': {}", path, err)))?;
            let byte_vals: Vec<Value> = bytes
                .into_iter()
                .map(|b| Value::Int(i64::from(b)))
                .collect();
            let mut attrs = HashMap::new();
            attrs.insert("bytes".to_string(), Value::array(byte_vals));
            return Ok(Value::make_instance(Symbol::intern("Buf[uint8]"), attrs));
        }
        // If a non-UTF-8 encoding is specified, read raw bytes and decode
        let needs_non_utf8 = enc.as_ref().is_some_and(|e| {
            let lower = e.to_lowercase();
            lower != "utf-8" && lower != "utf8"
        });
        if needs_non_utf8 {
            let bytes = fs::read(&path)
                .map_err(|err| RuntimeError::new(format!("Failed to slurp '{}': {}", path, err)))?;
            let decoded = self.decode_with_encoding(&bytes, enc.as_ref().unwrap())?;
            Ok(Value::str(decoded))
        } else {
            let content = fs::read_to_string(&path)
                .map_err(|err| RuntimeError::new(format!("Failed to slurp '{}': {}", path, err)))?;
            let content = super::utils::strip_utf8_bom(content);
            Ok(Value::str(content))
        }
    }

    pub(super) fn builtin_spurt(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // If the first argument is an IO::Handle, delegate to IO::Handle.spurt
        if let Some(Value::Instance {
            class_name,
            attributes,
            ..
        }) = args.first()
            && class_name == "IO::Handle"
        {
            let content_value = args
                .get(1)
                .cloned()
                .unwrap_or(Value::Str(String::new().into()));
            let method_args = std::iter::once(content_value)
                .chain(args.iter().skip(2).cloned())
                .collect();
            return self.native_io_handle(&(attributes).as_map(), "spurt", method_args);
        }
        let path = args
            .first()
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("spurt requires a path argument"))?;
        check_null_in_path(&path)?;
        let content_value = args
            .get(1)
            .ok_or_else(|| RuntimeError::new("spurt requires a content argument"))?;
        let mut append = false;
        let mut createonly = false;
        let mut enc: Option<String> = None;
        for arg in args.iter().skip(2) {
            if let Value::Pair(key, val) = arg {
                match key.as_str() {
                    "append" => append = val.truthy(),
                    "createonly" => createonly = val.truthy(),
                    "enc" => enc = Some(val.to_string_value()),
                    _ => {}
                }
            }
        }
        let resolved = self.resolve_path(&path);
        if createonly && resolved.exists() {
            return Ok(io_exception_failure(
                "X::IO::Spurt",
                format!("Failed to spurt '{}': file already exists", path),
            ));
        }
        let is_buf = crate::runtime::Interpreter::is_buf_value(content_value);
        let write_result = if is_buf {
            let bytes = crate::runtime::Interpreter::extract_buf_bytes(content_value);
            if append {
                use std::io::Write;
                fs::OpenOptions::new()
                    .append(true)
                    .create(true)
                    .open(&resolved)
                    .and_then(|mut file| file.write_all(&bytes))
            } else {
                fs::write(&resolved, &bytes)
            }
        } else {
            let content = content_value.to_string_value();
            let bytes = if let Some(ref enc_name) = enc {
                match self.encode_with_encoding(&content, enc_name) {
                    Ok(mut b) => {
                        // For utf16 (auto-endian), prepend BOM like Raku does
                        let enc_lower = enc_name.to_lowercase();
                        if enc_lower == "utf-16" || enc_lower == "utf16" {
                            let bom: &[u8] = if cfg!(target_endian = "little") {
                                &[0xFF, 0xFE]
                            } else {
                                &[0xFE, 0xFF]
                            };
                            let mut with_bom = Vec::with_capacity(bom.len() + b.len());
                            with_bom.extend_from_slice(bom);
                            with_bom.append(&mut b);
                            with_bom
                        } else {
                            b
                        }
                    }
                    Err(e) => {
                        return Ok(io_exception_failure("X::IO::Spurt", e.message));
                    }
                }
            } else {
                content.into_bytes()
            };
            if append {
                use std::io::Write;
                fs::OpenOptions::new()
                    .append(true)
                    .create(true)
                    .open(&resolved)
                    .and_then(|mut file| file.write_all(&bytes))
            } else {
                fs::write(&resolved, &bytes)
            }
        };
        match write_result {
            Ok(()) => Ok(Value::Bool(true)),
            Err(err) => Ok(io_exception_failure(
                "X::IO::Spurt",
                format!("Failed to spurt '{}': {}", path, err),
            )),
        }
    }

    pub(super) fn builtin_unlink(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Raku's unlink() returns a list of filenames that were requested to be
        // removed (always truthy). Errors other than NotFound still throw.
        let mut names = Vec::new();
        for arg in args {
            let path = arg.to_string_value();
            let resolved = self.resolve_path(&path);
            match fs::remove_file(&resolved) {
                Ok(()) => {}
                Err(err) if err.kind() == std::io::ErrorKind::NotFound => {}
                Err(err) => {
                    return Err(RuntimeError::new(format!(
                        "Failed to unlink '{}': {}",
                        path, err
                    )));
                }
            }
            names.push(Value::Str(path.into()));
        }
        Ok(Value::array(names))
    }

    pub(super) fn builtin_open(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // `open` takes an `IO()`-coercible path. When handed an IO::Handle
        // (e.g. `open(IO::Handle.new(:path($p)))`), coerce it to its `.path`
        // so the underlying file is opened, matching rakudo.
        let path = match args.first() {
            Some(Value::Instance {
                class_name,
                attributes,
                ..
            }) if class_name.resolve() == "IO::Handle" => attributes
                .as_map()
                .get("path")
                .map(|p| p.to_string_value())
                .unwrap_or_default(),
            Some(v) => v.to_string_value(),
            None => return Err(RuntimeError::new("open requires a path argument")),
        };
        check_null_in_path(&path)?;
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
        ) = self.parse_io_flags_values(&args[1..]);
        let path_buf = self.resolve_path(&path);
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
            // Raku returns a Failure (wrapping the exception) when open() fails,
            // rather than dying immediately. Sinking/using the Failure later
            // throws the exception. Preserve any specific exception type the
            // error already carries; otherwise default to X::AdHoc.
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

    pub(super) fn builtin_close(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let handle = args
            .first()
            .ok_or_else(|| RuntimeError::new("close requires a handle"))?;
        Ok(Value::Bool(self.close_handle_value(handle)?))
    }
}
