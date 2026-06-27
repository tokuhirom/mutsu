use super::*;

impl Interpreter {
    /// Single-path filesystem *mutations* on an `IO::Path`
    /// (`spurt`/`mkdir`/`rmdir`/`unlink`/`chmod`): resolve the path against the
    /// VM-owned cwd, then perform a one-shot syscall (`fs::write`/`create_dir_all`/
    /// `remove_dir`/`remove_file`/`set_permissions`). They allocate **no
    /// `io_handles`** — `spurt` opens, writes, and immediately drops its file
    /// handle. Encoding lookup (`encode_with_encoding`, reading the VM-owned
    /// encoding registry) is a `&self` read, so the VM dispatches these natively
    /// (ledger §D): the single impl `native_io_path` also delegates to. Two-path
    /// ops (`copy`/`rename`/`move`/`symlink`/`link`, which resolve a destination)
    /// and handle-opening `open` return `None` and stay in `native_io_path`.
    pub(crate) fn try_io_path_fs_mutate(
        &self,
        attributes: &HashMap<String, Value>,
        class_name: &str,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(method, "spurt" | "mkdir" | "rmdir" | "unlink" | "chmod") {
            return None;
        }
        Some(self.io_path_fs_mutate(attributes, class_name, method, args))
    }

    /// The fallible body of [`try_io_path_fs_mutate`] (the gate returns `Option`
    /// so it cannot use `?`). Behavior-invariant with the arms `native_io_path`
    /// previously held.
    fn io_path_fs_mutate(
        &self,
        attributes: &HashMap<String, Value>,
        class_name: &str,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let path_buf = self.resolve_io_path_buf(attributes, &p);
        match method {
            "spurt" => {
                let content_value = args
                    .first()
                    .cloned()
                    .unwrap_or(Value::Str(String::new().into()));
                let mut append = false;
                let mut createonly = false;
                let mut enc: Option<String> = None;
                for arg in args.iter().skip(1) {
                    if let Value::Pair(key, val) = arg {
                        match key.as_str() {
                            "append" => append = val.truthy(),
                            "createonly" => createonly = val.truthy(),
                            "enc" => enc = Some(val.to_string_value()),
                            _ => {}
                        }
                    }
                }
                if createonly && path_buf.exists() {
                    return Ok(io_exception_failure(
                        "X::IO::Spurt",
                        format!("Failed to spurt '{}': file already exists", p),
                    ));
                }
                let is_buf = crate::runtime::Interpreter::is_buf_value(&content_value);
                let write_result = if is_buf {
                    let bytes = crate::runtime::Interpreter::extract_buf_bytes(&content_value);
                    if append {
                        use std::io::Write;
                        fs::OpenOptions::new()
                            .append(true)
                            .create(true)
                            .open(&path_buf)
                            .and_then(|mut file| file.write_all(&bytes))
                    } else {
                        fs::write(&path_buf, &bytes)
                    }
                } else {
                    let content = content_value.to_string_value();
                    let bytes = if let Some(ref enc_name) = enc {
                        match self.encode_with_encoding(&content, enc_name) {
                            Ok(mut b) => {
                                // For utf16 (auto-endian), prepend BOM
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
                            .open(&path_buf)
                            .and_then(|mut file| file.write_all(&bytes))
                    } else {
                        fs::write(&path_buf, &bytes)
                    }
                };
                match write_result {
                    Ok(()) => Ok(Value::Bool(true)),
                    Err(err) => Ok(io_exception_failure(
                        "X::IO::Spurt",
                        format!("Failed to spurt '{}': {}", p, err),
                    )),
                }
            }
            "mkdir" => match fs::create_dir_all(&path_buf) {
                Ok(()) => Ok(Value::make_instance(
                    Symbol::intern(class_name),
                    attributes.clone(),
                )),
                Err(err) => {
                    let msg = format!(
                        "Failed to create directory '{}' with mode '0o777': Failed to mkdir: {}",
                        p, err
                    );
                    let mut ex_attrs = HashMap::new();
                    ex_attrs.insert("message".to_string(), Value::str_from(&msg));
                    ex_attrs.insert("path".to_string(), Value::str_from(&p));
                    let ex = Value::make_instance(Symbol::intern("X::IO::Mkdir"), ex_attrs);
                    let mut failure_attrs = HashMap::new();
                    failure_attrs.insert("exception".to_string(), ex);
                    Ok(Value::make_instance(
                        Symbol::intern("Failure"),
                        failure_attrs,
                    ))
                }
            },
            "rmdir" => match fs::remove_dir(&path_buf) {
                Ok(()) => Ok(Value::Bool(true)),
                Err(err) => {
                    let msg = format!("Failed to remove the directory '{}': {}", p, err);
                    let mut ex_attrs = HashMap::new();
                    ex_attrs.insert("message".to_string(), Value::str_from(&msg));
                    ex_attrs.insert("path".to_string(), Value::str_from(&p));
                    let ex = Value::make_instance(Symbol::intern("X::IO::Rmdir"), ex_attrs);
                    let mut failure_attrs = HashMap::new();
                    failure_attrs.insert("exception".to_string(), ex);
                    Ok(Value::make_instance(
                        Symbol::intern("Failure"),
                        failure_attrs,
                    ))
                }
            },
            "unlink" => match fs::remove_file(&path_buf) {
                Ok(()) => Ok(Value::Bool(true)),
                Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(Value::Bool(false)),
                Err(err) => Err(RuntimeError::new(format!(
                    "Failed to unlink '{}': {}",
                    p, err
                ))),
            },
            "chmod" => {
                let mode_value = args
                    .first()
                    .cloned()
                    .ok_or_else(|| RuntimeError::new("chmod requires mode"))?;
                let mode_int = match mode_value {
                    Value::Int(i) => i as u32,
                    Value::Str(s) => u32::from_str_radix(&s, 8).unwrap_or(0),
                    other => {
                        return Err(RuntimeError::new(format!(
                            "Invalid mode: {}",
                            other.to_string_value()
                        )));
                    }
                };
                #[cfg(unix)]
                {
                    let perms = PermissionsExt::from_mode(mode_int);
                    fs::set_permissions(&path_buf, perms).map_err(|err| {
                        RuntimeError::new(format!("Failed to chmod '{}': {}", p, err))
                    })?;
                }
                #[cfg(not(unix))]
                {
                    let _ = mode_int;
                    return Err(RuntimeError::new("chmod not supported on this platform"));
                }
                Ok(Value::Bool(true))
            }
            _ => unreachable!("io_path_fs_mutate called with non-mutation method"),
        }
    }

    /// Two-path filesystem operations on an `IO::Path`
    /// (`copy`/`rename`/`move`/`symlink`/`link`): resolve both the receiver path
    /// and the destination/link path against the VM-owned cwd, then perform a
    /// one-shot syscall (`fs::copy`/`fs::rename`/`unix_fs::symlink`/
    /// `fs::hard_link`). They allocate **no `io_handles`** and only read the
    /// VM-owned cwd (`resolve_path`, `&self`), so the VM dispatches them natively
    /// (ledger §D): the single impl `native_io_path` also delegates to. Returns
    /// `None` for any other method.
    pub(crate) fn try_io_path_two_path_op(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(method, "copy" | "rename" | "move" | "symlink" | "link") {
            return None;
        }
        Some(self.io_path_two_path_op(attributes, method, args))
    }

    /// The fallible body of [`try_io_path_two_path_op`] (the gate returns `Option`
    /// so it cannot use `?`). Behavior-invariant with the arms `native_io_path`
    /// previously held.
    fn io_path_two_path_op(
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
            "copy" => {
                let dest = args
                    .first()
                    .map(|v| v.to_string_value())
                    .ok_or_else(|| RuntimeError::new("copy requires destination"))?;
                let createonly = Self::named_bool(args, "createonly");
                let dest_buf = self.resolve_path(&dest);
                // Check if source and destination are the same file
                if path_buf == dest_buf
                    || (path_buf.exists()
                        && dest_buf.exists()
                        && fs::canonicalize(&path_buf).ok() == fs::canonicalize(&dest_buf).ok())
                {
                    return Ok(io_exception_failure(
                        "X::IO::Copy",
                        format!(
                            "Failed to copy '{}': source and destination are the same file",
                            p
                        ),
                    ));
                }
                if createonly && dest_buf.exists() {
                    return Ok(io_exception_failure(
                        "X::IO::Copy",
                        format!("Failed to copy '{}': destination already exists", p),
                    ));
                }
                fs::copy(&path_buf, &dest_buf)
                    .map_err(|err| RuntimeError::new(format!("Failed to copy '{}': {}", p, err)))?;
                Ok(Value::Bool(true))
            }
            "rename" | "move" => {
                let ex_type = if method == "rename" {
                    "X::IO::Rename"
                } else {
                    "X::IO::Move"
                };
                let verb = if method == "rename" { "rename" } else { "move" };
                let dest = args
                    .first()
                    .map(|v| v.to_string_value())
                    .ok_or_else(|| RuntimeError::new("rename/move requires destination"))?;
                let createonly = Self::named_bool(args, "createonly");
                let dest_buf = self.resolve_path(&dest);
                // Check if source and destination are the same file
                if path_buf == dest_buf
                    || (path_buf.exists()
                        && dest_buf.exists()
                        && fs::canonicalize(&path_buf).ok() == fs::canonicalize(&dest_buf).ok())
                {
                    let ex = Value::make_instance(Symbol::intern(ex_type), HashMap::new());
                    let mut failure_attrs = HashMap::new();
                    failure_attrs.insert("exception".to_string(), ex);
                    failure_attrs.insert("handled".to_string(), Value::Bool(false));
                    failure_attrs.insert(
                        "message".to_string(),
                        Value::Str(
                            format!(
                                "Failed to {} '{}': source and destination are the same file",
                                verb, p
                            )
                            .into(),
                        ),
                    );
                    return Ok(Value::make_instance(
                        Symbol::intern("Failure"),
                        failure_attrs,
                    ));
                }
                if createonly && dest_buf.exists() {
                    return Err(io_exception(
                        ex_type,
                        format!("Failed to {} '{}': destination already exists", verb, p),
                    ));
                }
                fs::rename(&path_buf, &dest_buf).map_err(|err| {
                    io_exception(ex_type, format!("Failed to {} '{}': {}", verb, p, err))
                })?;
                Ok(Value::Bool(true))
            }
            "symlink" => {
                // IO::Path.symlink($name, :$absolute = True)
                // Creates a symlink named $name pointing to self (the target).
                let link_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .ok_or_else(|| RuntimeError::new("symlink requires a link name"))?;
                // :absolute defaults to True; :!absolute uses the original path string.
                let absolute = Self::named_value(args, "absolute")
                    .map(|v| v.truthy())
                    .unwrap_or(true);
                let link_buf = self.resolve_path(&link_name);
                let target_for_symlink = if absolute {
                    path_buf.clone()
                } else {
                    std::path::PathBuf::from(&p)
                };
                #[cfg(unix)]
                {
                    match unix_fs::symlink(&target_for_symlink, &link_buf) {
                        Ok(()) => Ok(Value::Bool(true)),
                        Err(err) => Ok(Self::make_symlink_failure(&p, &link_name, &err)),
                    }
                }
                #[cfg(windows)]
                {
                    let metadata = fs::metadata(&target_for_symlink);
                    let result = if metadata.map(|meta| meta.is_dir()).unwrap_or(false) {
                        windows_fs::symlink_dir(&target_for_symlink, &link_buf)
                    } else {
                        windows_fs::symlink_file(&target_for_symlink, &link_buf)
                    };
                    match result {
                        Ok(()) => Ok(Value::Bool(true)),
                        Err(err) => Ok(Self::make_symlink_failure(&p, &link_name, &err)),
                    }
                }
                #[cfg(not(any(unix, windows)))]
                {
                    Err(RuntimeError::new("symlink not supported on this platform"))
                }
            }
            "link" => {
                // IO::Path.link($name): creates a new hard link named $name
                // pointing to self (the target). Fails with X::IO::Link.
                let link_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .ok_or_else(|| RuntimeError::new("link requires a link name"))?;
                let link_buf = self.resolve_path(&link_name);
                match fs::hard_link(&path_buf, &link_buf) {
                    Ok(()) => Ok(Value::Bool(true)),
                    Err(err) => Ok(Self::make_link_failure(&p, &link_name, &err)),
                }
            }
            _ => unreachable!("io_path_two_path_op called with non-two-path method"),
        }
    }
}
