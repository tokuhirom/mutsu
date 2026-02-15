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
        let path_buf = self.resolve_path(&p);
        let cwd_path = self.get_cwd_path();
        let original = Path::new(&p);
        match method {
            "Str" | "gist" => Ok(Value::Str(p.clone())),
            "IO" => Ok(Value::make_instance(
                "IO::Path".to_string(),
                attributes.clone(),
            )),
            "basename" => {
                let bname = original
                    .file_name()
                    .map(|s| s.to_string_lossy().to_string())
                    .unwrap_or_default();
                Ok(Value::Str(bname))
            }
            "parent" => {
                let mut levels = 1i64;
                if let Some(Value::Int(i)) = args.first() {
                    levels = (*i).max(1);
                }
                let mut path = p.clone();
                for _ in 0..levels {
                    if let Some(par) = Path::new(&path).parent() {
                        let s = par.to_string_lossy().to_string();
                        if s.is_empty() {
                            path = ".".to_string();
                            break;
                        }
                        path = s;
                    } else {
                        path = ".".to_string();
                        break;
                    }
                }
                Ok(self.make_io_path_instance(&path))
            }
            "child" | "add" => {
                let child_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
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
                Ok(self.make_io_path_instance(&absolute))
            }
            "relative" => {
                let rel = path_buf
                    .strip_prefix(&cwd_path)
                    .map(Self::stringify_path)
                    .unwrap_or_else(|_| Self::stringify_path(&path_buf));
                Ok(Value::Str(rel))
            }
            "resolve" => {
                let canonical = fs::canonicalize(&path_buf).map_err(|err| {
                    RuntimeError::new(format!("Failed to resolve '{}': {}", p, err))
                })?;
                let resolved = Self::stringify_path(&canonical);
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
                Ok(Value::Array(parts))
            }
            "words" => {
                let content = fs::read_to_string(&path_buf)
                    .map_err(|err| RuntimeError::new(format!("Failed to read '{}': {}", p, err)))?;
                let parts = content
                    .split_whitespace()
                    .map(|token| Value::Str(token.to_string()))
                    .collect();
                Ok(Value::Array(parts))
            }
            "slurp" => {
                let content = fs::read_to_string(&path_buf).map_err(|err| {
                    RuntimeError::new(format!("Failed to slurp '{}': {}", p, err))
                })?;
                Ok(Value::Str(content))
            }
            "open" => {
                let (read, write, append) = Self::parse_io_flags_values(&args);
                self.open_file_handle(&path_buf, read, write, append)
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
                Ok(Value::Bool(true))
            }
            "rmdir" => {
                fs::remove_dir(&path_buf).map_err(|err| {
                    RuntimeError::new(format!("Failed to rmdir '{}': {}", p, err))
                })?;
                Ok(Value::Bool(true))
            }
            "dir" => {
                let mut entries = Vec::new();
                for entry in fs::read_dir(&path_buf).map_err(|err| {
                    RuntimeError::new(format!("Failed to read dir '{}': {}", p, err))
                })? {
                    let entry = entry.map_err(|err| {
                        RuntimeError::new(format!("Failed to read dir entry '{}': {}", p, err))
                    })?;
                    entries.push(Value::Str(entry.path().to_string_lossy().to_string()));
                }
                Ok(Value::Array(entries))
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
            "unlink" => {
                fs::remove_file(&path_buf).map_err(|err| {
                    RuntimeError::new(format!("Failed to unlink '{}': {}", p, err))
                })?;
                Ok(Value::Bool(true))
            }
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
            "get" => {
                let line = self.read_line_from_handle_value(&target_val)?;
                if line.is_empty() {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::Str(line))
                }
            }
            "getc" => {
                let bytes = self.read_bytes_from_handle_value(&target_val, 1)?;
                Ok(Value::Str(String::from_utf8_lossy(&bytes).to_string()))
            }
            "lines" => {
                let mut lines = Vec::new();
                loop {
                    let line = self.read_line_from_handle_value(&target_val)?;
                    if line.is_empty() {
                        break;
                    }
                    lines.push(Value::Str(line));
                }
                Ok(Value::Array(lines))
            }
            "words" => {
                let mut words = Vec::new();
                loop {
                    let line = self.read_line_from_handle_value(&target_val)?;
                    if line.is_empty() {
                        break;
                    }
                    for token in line.split_whitespace() {
                        words.push(Value::Str(token.to_string()));
                    }
                }
                Ok(Value::Array(words))
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
            "say" => {
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
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on IO::Handle",
                method
            ))),
        }
    }
}
