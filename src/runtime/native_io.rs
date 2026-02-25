use super::*;

impl Interpreter {
    pub(super) fn native_io_path(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let instance_cwd = attributes.get("cwd").map(|v| v.to_string_value());
        let path_buf = if Path::new(&p).is_absolute() {
            self.resolve_path(&p)
        } else if let Some(cwd) = &instance_cwd {
            self.apply_chroot(PathBuf::from(cwd).join(Path::new(&p)))
        } else {
            self.resolve_path(&p)
        };
        let cwd_path = self.get_cwd_path();
        let original = Path::new(&p);
        match method {
            "Str" | "gist" => Ok(Value::Str(p.clone())),
            "raku" | "perl" => {
                let escape = |s: &str| {
                    s.replace('\\', "\\\\")
                        .replace('"', "\\\"")
                        .replace('\n', "\\n")
                        .replace('\t', "\\t")
                        .replace('\r', "\\r")
                        .replace('\0', "\\0")
                };
                let cwd = instance_cwd.unwrap_or_else(|| Self::stringify_path(&cwd_path));
                Ok(Value::Str(format!(
                    "IO::Path.new(\"{}\", :CWD(\"{}\"))",
                    escape(&p),
                    escape(&cwd)
                )))
            }
            "IO" => Ok(Value::make_instance(
                "IO::Path".to_string(),
                attributes.clone(),
            )),
            "basename" => {
                let bname = if p == "." || p == ".." {
                    p.clone()
                } else if p.ends_with("/.") || p.ends_with("\\.") {
                    ".".to_string()
                } else if p.ends_with("/..") || p.ends_with("\\..") {
                    "..".to_string()
                } else {
                    original
                        .file_name()
                        .map(|s| s.to_string_lossy().to_string())
                        .unwrap_or_default()
                };
                Ok(Value::Str(bname))
            }
            "dirname" => {
                let dname = original
                    .parent()
                    .map(Self::stringify_path)
                    .unwrap_or_else(|| ".".to_string());
                Ok(Value::Str(dname))
            }
            "parent" => {
                let mut levels = 1i64;
                if let Some(Value::Int(i)) = args.first() {
                    levels = (*i).max(1);
                }
                let mut path = p.clone();
                for _ in 0..levels {
                    if path == "." {
                        // "." → ".."
                        path = "..".to_string();
                    } else if path == ".." || path.ends_with("/..") || path.ends_with("\\..") {
                        // ".." chain → append "/.."
                        path = format!("{}/..", path);
                    } else if path == "/" {
                        // Filesystem root has no parent in Raku; stays at "/"
                        break;
                    } else if let Some(par) = Path::new(&path).parent() {
                        let s = par.to_string_lossy().to_string();
                        if s.is_empty() {
                            // bare filename like "foo" → parent is "."
                            path = ".".to_string();
                        } else {
                            path = s;
                        }
                    } else {
                        path = ".".to_string();
                    }
                }
                Ok(self.make_io_path_instance(&path))
            }
            "sibling" => {
                let sibling_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let parent = original.parent().unwrap_or_else(|| Path::new("."));
                let sibling_path = Self::stringify_path(&parent.join(&sibling_name));
                Ok(self.make_io_path_instance(&sibling_path))
            }
            "child" | "add" => {
                let child_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                if child_name.contains('\0') || p.contains('\0') {
                    return Err(RuntimeError::new(
                        "X::IO::Null: Found null byte in pathname",
                    ));
                }
                let joined = Self::stringify_path(&original.join(&child_name));
                Ok(self.make_io_path_instance(&joined))
            }
            "extension" => {
                let ext = original
                    .extension()
                    .map(|s| s.to_string_lossy().to_string())
                    .unwrap_or_default();
                Ok(Value::Str(ext))
            }
            "absolute" => {
                let absolute = Self::stringify_path(&path_buf);
                Ok(Value::Str(absolute))
            }
            "relative" => {
                let rel_base = instance_cwd
                    .as_ref()
                    .map(PathBuf::from)
                    .unwrap_or_else(|| cwd_path.clone());
                let rel = path_buf
                    .strip_prefix(&rel_base)
                    .map(Self::stringify_path)
                    .unwrap_or_else(|_| Self::stringify_path(&path_buf));
                Ok(Value::Str(rel))
            }
            "starts-with" => {
                let prefix = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Ok(Value::Bool(p.starts_with(&prefix)))
            }
            "resolve" => {
                // Try to canonicalize; if the path doesn't exist, return as-is
                let resolved = match fs::canonicalize(&path_buf) {
                    Ok(canonical) => Self::stringify_path(&canonical),
                    Err(_) => p.clone(),
                };
                Ok(self.make_io_path_instance(&resolved))
            }
            "volume" => {
                let volume = path_buf
                    .components()
                    .next()
                    .map(|comp| comp.as_os_str().to_string_lossy().to_string())
                    .unwrap_or_default();
                Ok(Value::Str(volume))
            }
            "is-absolute" => Ok(Value::Bool(original.is_absolute())),
            "is-relative" => Ok(Value::Bool(!original.is_absolute())),
            "e" => Ok(Value::Bool(path_buf.exists())),
            "f" => Ok(Value::Bool(path_buf.is_file())),
            "d" => Ok(Value::Bool(path_buf.is_dir())),
            "l" => {
                let linked = fs::symlink_metadata(&path_buf)
                    .map(|meta| meta.file_type().is_symlink())
                    .unwrap_or(false);
                Ok(Value::Bool(linked))
            }
            "r" => Ok(Value::Bool(fs::metadata(&path_buf).is_ok())),
            "w" => {
                let writable = fs::metadata(&path_buf)
                    .map(|meta| !meta.permissions().readonly())
                    .unwrap_or(false);
                Ok(Value::Bool(writable))
            }
            "x" => {
                let executable = fs::metadata(&path_buf)
                    .map(|meta| Self::metadata_is_executable(&meta))
                    .unwrap_or(false);
                Ok(Value::Bool(executable))
            }
            "mode" => {
                let metadata = fs::metadata(&path_buf)
                    .map_err(|err| RuntimeError::new(format!("Failed to stat '{}': {}", p, err)))?;
                #[cfg(unix)]
                {
                    let mode = metadata.permissions().mode() & 0o777;
                    Ok(Value::Str(format!("{:04o}", mode)))
                }
                #[cfg(not(unix))]
                {
                    let mode = if metadata.permissions().readonly() {
                        "0444"
                    } else {
                        "0666"
                    };
                    Ok(Value::Str(mode.to_string()))
                }
            }
            "s" => {
                let size = fs::metadata(&path_buf)
                    .map(|meta| meta.len())
                    .map_err(|err| RuntimeError::new(format!("Failed to stat '{}': {}", p, err)))?;
                Ok(Value::Int(size as i64))
            }
            "z" => {
                let zero = fs::metadata(&path_buf)
                    .map(|meta| meta.len() == 0)
                    .unwrap_or(false);
                Ok(Value::Bool(zero))
            }
            "modified" => {
                let ts = fs::metadata(&path_buf)
                    .and_then(|meta| meta.modified())
                    .map(Self::system_time_to_int)
                    .map_err(|err| {
                        RuntimeError::new(format!("Failed to get modified time '{}': {}", p, err))
                    })?;
                Ok(Value::Int(ts))
            }
            "accessed" => {
                let ts = fs::metadata(&path_buf)
                    .and_then(|meta| meta.accessed())
                    .map(Self::system_time_to_int)
                    .map_err(|err| {
                        RuntimeError::new(format!("Failed to get accessed time '{}': {}", p, err))
                    })?;
                Ok(Value::Int(ts))
            }
            "changed" => {
                let ts = fs::metadata(&path_buf)
                    .and_then(|meta| meta.modified())
                    .map(Self::system_time_to_int)
                    .map_err(|err| {
                        RuntimeError::new(format!("Failed to get changed time '{}': {}", p, err))
                    })?;
                Ok(Value::Int(ts))
            }
            "lines" => {
                let content = fs::read_to_string(&path_buf)
                    .map_err(|err| RuntimeError::new(format!("Failed to read '{}': {}", p, err)))?;
                let parts = content
                    .lines()
                    .map(|line| Value::Str(line.to_string()))
                    .collect();
                Ok(Value::array(parts))
            }
            "words" => {
                let content = fs::read_to_string(&path_buf)
                    .map_err(|err| RuntimeError::new(format!("Failed to read '{}': {}", p, err)))?;
                let parts = content
                    .split_whitespace()
                    .map(|token| Value::Str(token.to_string()))
                    .collect();
                Ok(Value::array(parts))
            }
            "slurp" => {
                let content = fs::read_to_string(&path_buf).map_err(|err| {
                    RuntimeError::new(format!("Failed to slurp '{}': {}", p, err))
                })?;
                Ok(Value::Str(content))
            }
            "open" => {
                let (read, write, append, bin, line_separators) = self.parse_io_flags_values(&args);
                self.open_file_handle(&path_buf, read, write, append, bin, line_separators)
            }
            "copy" => {
                let dest = args
                    .first()
                    .map(|v| v.to_string_value())
                    .ok_or_else(|| RuntimeError::new("copy requires destination"))?;
                let dest_buf = self.resolve_path(&dest);
                fs::copy(&path_buf, &dest_buf)
                    .map_err(|err| RuntimeError::new(format!("Failed to copy '{}': {}", p, err)))?;
                Ok(Value::Bool(true))
            }
            "rename" | "move" => {
                let dest = args
                    .first()
                    .map(|v| v.to_string_value())
                    .ok_or_else(|| RuntimeError::new("rename/move requires destination"))?;
                let dest_buf = self.resolve_path(&dest);
                fs::rename(&path_buf, &dest_buf).map_err(|err| {
                    RuntimeError::new(format!("Failed to rename '{}': {}", p, err))
                })?;
                Ok(Value::Bool(true))
            }
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
            "mkdir" => {
                fs::create_dir_all(&path_buf).map_err(|err| {
                    RuntimeError::new(format!("Failed to mkdir '{}': {}", p, err))
                })?;
                Ok(Value::make_instance(
                    "IO::Path".to_string(),
                    attributes.clone(),
                ))
            }
            "rmdir" => match fs::remove_dir(&path_buf) {
                Ok(()) => Ok(Value::Bool(true)),
                Err(_) => Ok(Value::Bool(false)),
            },
            "dir" => {
                let mut entries = Vec::new();
                let requested = PathBuf::from(&p);
                let requested_is_absolute = requested.is_absolute();
                let make_entry = |out_path: PathBuf| {
                    let mut attrs = HashMap::new();
                    attrs.insert(
                        "path".to_string(),
                        Value::Str(Self::stringify_path(&out_path)),
                    );
                    if let Some(cwd) = &instance_cwd
                        && !out_path.is_absolute()
                    {
                        attrs.insert("cwd".to_string(), Value::Str(cwd.clone()));
                    }
                    Value::make_instance("IO::Path".to_string(), attrs)
                };
                for entry in fs::read_dir(&path_buf).map_err(|err| {
                    RuntimeError::new(format!("Failed to read dir '{}': {}", p, err))
                })? {
                    let entry = entry.map_err(|err| {
                        RuntimeError::new(format!("Failed to read dir entry '{}': {}", p, err))
                    })?;
                    let file_name = entry.file_name();
                    let out_path = if requested_is_absolute {
                        path_buf.join(&file_name)
                    } else if p == "." {
                        PathBuf::from(&file_name)
                    } else {
                        requested.join(&file_name)
                    };
                    entries.push(make_entry(out_path));
                }
                Ok(Value::array(entries))
            }
            "spurt" => {
                let content = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                fs::write(&path_buf, &content).map_err(|err| {
                    RuntimeError::new(format!("Failed to spurt '{}': {}", p, err))
                })?;
                Ok(Value::Bool(true))
            }
            "unlink" => match fs::remove_file(&path_buf) {
                Ok(()) => Ok(Value::Bool(true)),
                Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(Value::Bool(false)),
                Err(err) => Err(RuntimeError::new(format!(
                    "Failed to unlink '{}': {}",
                    p, err
                ))),
            },
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on IO::Path",
                method
            ))),
        }
    }

    pub(super) fn native_io_handle(
        &mut self,
        target: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let target_val = Value::make_instance("IO::Handle".to_string(), target.clone());
        match method {
            "close" => Ok(Value::Bool(self.close_handle_value(&target_val)?)),
            "get" => Ok(self
                .read_line_from_handle_value(&target_val)?
                .map(Value::Str)
                .unwrap_or(Value::Nil)),
            "getc" => {
                let bytes = self.read_bytes_from_handle_value(&target_val, 1)?;
                Ok(Value::Str(String::from_utf8_lossy(&bytes).to_string()))
            }
            "lines" => {
                let mut lines = Vec::new();
                while let Some(line) = self.read_line_from_handle_value(&target_val)? {
                    lines.push(Value::Str(line));
                }
                Ok(Value::array(lines))
            }
            "words" => {
                let mut words = Vec::new();
                while let Some(line) = self.read_line_from_handle_value(&target_val)? {
                    for token in line.split_whitespace() {
                        words.push(Value::Str(token.to_string()));
                    }
                }
                Ok(Value::array(words))
            }
            "read" => {
                let count = args
                    .first()
                    .and_then(|v| match v {
                        Value::Int(i) if *i > 0 => Some(*i as usize),
                        _ => None,
                    })
                    .unwrap_or(0);
                if count > 0 {
                    let bytes = self.read_bytes_from_handle_value(&target_val, count)?;
                    return Ok(Value::Str(String::from_utf8_lossy(&bytes).to_string()));
                }
                let path = {
                    let state = self.handle_state_mut(&target_val)?;
                    state.path.clone()
                };
                if let Some(path) = path {
                    let content = fs::read_to_string(&path).map_err(|err| {
                        RuntimeError::new(format!("Failed to read '{}': {}", path, err))
                    })?;
                    return Ok(Value::Str(content));
                }
                Ok(Value::Str(String::new()))
            }
            "write" | "print" => {
                let content = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                self.write_to_handle_value(&target_val, &content, false)?;
                Ok(Value::Bool(true))
            }
            "say" | "put" => {
                let content = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                self.write_to_handle_value(&target_val, &content, true)?;
                Ok(Value::Bool(true))
            }
            "flush" => {
                if let Ok(state) = self.handle_state_mut(&target_val)
                    && let Some(file) = state.file.as_mut()
                {
                    file.flush().map_err(|err| {
                        RuntimeError::new(format!("Failed to flush handle: {}", err))
                    })?;
                }
                Ok(Value::Bool(true))
            }
            "seek" => {
                let pos = args
                    .first()
                    .and_then(|v| match v {
                        Value::Int(i) => Some(*i),
                        _ => None,
                    })
                    .unwrap_or(0);
                let offset = self.seek_handle_value(&target_val, pos)?;
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
            "encoding" => {
                if let Some(arg) = args.first() {
                    let encoding = arg.to_string_value();
                    let prev = self.set_handle_encoding(&target_val, Some(encoding.clone()))?;
                    return Ok(Value::Str(prev));
                }
                let current = self.set_handle_encoding(&target_val, None)?;
                Ok(Value::Str(current))
            }
            "opened" => {
                let state = self.handle_state_mut(&target_val)?;
                Ok(Value::Bool(!state.closed))
            }
            "slurp" => {
                let path = {
                    let state = self.handle_state_mut(&target_val)?;
                    state.path.clone()
                };
                if let Some(path) = path {
                    let content = fs::read_to_string(&path).map_err(|err| {
                        RuntimeError::new(format!("Failed to slurp '{}': {}", path, err))
                    })?;
                    return Ok(Value::Str(content));
                }
                Ok(Value::Str(String::new()))
            }
            "Supply" => self.handle_supply(target, &args),
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

        let target_val = Value::make_instance("IO::Handle".to_string(), target.clone());

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
                    values.push(Value::make_instance("Buf".to_string(), buf_attrs));
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
                values.push(Value::Str(s));
            }
        }

        let mut supply_attrs = HashMap::new();
        supply_attrs.insert("live".to_string(), Value::Bool(false));
        supply_attrs.insert("values".to_string(), Value::array(values));
        Ok(Value::make_instance("Supply".to_string(), supply_attrs))
    }

    pub(super) fn native_io_pipe(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        let content = attributes
            .get("content")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        match method {
            "slurp" | "Str" | "gist" => Ok(Value::Str(content)),
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on IO::Pipe",
                method
            ))),
        }
    }
}
