use super::*;

impl Interpreter {
    pub(super) fn builtin_slurp(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let path = args
            .first()
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("slurp requires a path argument"))?;
        let content = fs::read_to_string(&path)
            .map_err(|err| RuntimeError::new(format!("Failed to slurp '{}': {}", path, err)))?;
        Ok(Value::Str(content))
    }

    pub(super) fn builtin_spurt(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let path = args
            .first()
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("spurt requires a path argument"))?;
        let content = args
            .get(1)
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("spurt requires a content argument"))?;
        fs::write(&path, &content)
            .map_err(|err| RuntimeError::new(format!("Failed to spurt '{}': {}", path, err)))?;
        Ok(Value::Bool(true))
    }

    pub(super) fn builtin_unlink(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let path = args
            .first()
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("unlink requires a path argument"))?;
        match fs::remove_file(&path) {
            Ok(()) => Ok(Value::Bool(true)),
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(Value::Bool(false)),
            Err(err) => Err(RuntimeError::new(format!(
                "Failed to unlink '{}': {}",
                path, err
            ))),
        }
    }

    pub(super) fn builtin_open(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let path = args
            .first()
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("open requires a path argument"))?;
        let (read, write, append, bin) = Self::parse_io_flags_values(&args[1..]);
        let path_buf = self.resolve_path(&path);
        self.open_file_handle(&path_buf, read, write, append, bin)
    }

    pub(super) fn builtin_close(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let handle = args
            .first()
            .ok_or_else(|| RuntimeError::new("close requires a handle"))?;
        Ok(Value::Bool(self.close_handle_value(handle)?))
    }

    pub(super) fn builtin_dir(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let requested = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| {
                self.get_dynamic_string("$*CWD")
                    .unwrap_or_else(|| ".".to_string())
            });
        let path_buf = self.resolve_path(&requested);
        let mut entries = Vec::new();
        for entry in fs::read_dir(&path_buf).map_err(|err| {
            RuntimeError::new(format!("Failed to read dir '{}': {}", requested, err))
        })? {
            let entry = entry.map_err(|err| {
                RuntimeError::new(format!("Failed to read dir entry '{}': {}", requested, err))
            })?;
            entries.push(Value::Str(entry.path().to_string_lossy().to_string()));
        }
        Ok(Value::Array(entries))
    }

    pub(super) fn builtin_copy(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let source = args
            .first()
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("copy requires a source path"))?;
        let dest = args
            .get(1)
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("copy requires a destination path"))?;
        let src_buf = self.resolve_path(&source);
        let dest_buf = self.resolve_path(&dest);
        fs::copy(&src_buf, &dest_buf)
            .map_err(|err| RuntimeError::new(format!("Failed to copy '{}': {}", source, err)))?;
        Ok(Value::Bool(true))
    }

    pub(super) fn builtin_rename(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let source = args
            .first()
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("rename requires a source path"))?;
        let dest = args
            .get(1)
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("rename requires a destination path"))?;
        let src_buf = self.resolve_path(&source);
        let dest_buf = self.resolve_path(&dest);
        fs::rename(&src_buf, &dest_buf)
            .map_err(|err| RuntimeError::new(format!("Failed to rename '{}': {}", source, err)))?;
        Ok(Value::Bool(true))
    }

    pub(super) fn builtin_chmod(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let path = args
            .first()
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("chmod requires a path"))?;
        let mode_value = args
            .get(1)
            .cloned()
            .ok_or_else(|| RuntimeError::new("chmod requires a mode"))?;
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
        let path_buf = self.resolve_path(&path);
        #[cfg(unix)]
        {
            let perms = PermissionsExt::from_mode(mode_int);
            fs::set_permissions(&path_buf, perms)
                .map_err(|err| RuntimeError::new(format!("Failed to chmod '{}': {}", path, err)))?;
        }
        #[cfg(not(unix))]
        {
            return Err(RuntimeError::new("chmod not supported on this platform"));
        }
        Ok(Value::Bool(true))
    }

    pub(super) fn builtin_mkdir(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let path = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| {
                self.get_dynamic_string("$*CWD")
                    .unwrap_or_else(|| ".".to_string())
            });
        let path_buf = self.resolve_path(&path);
        fs::create_dir_all(&path_buf)
            .map_err(|err| RuntimeError::new(format!("Failed to mkdir '{}': {}", path, err)))?;
        Ok(Value::Bool(true))
    }

    pub(super) fn builtin_rmdir(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let path = args
            .first()
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("rmdir requires a path"))?;
        let path_buf = self.resolve_path(&path);
        fs::remove_dir(&path_buf)
            .map_err(|err| RuntimeError::new(format!("Failed to rmdir '{}': {}", path, err)))?;
        Ok(Value::Bool(true))
    }

    pub(super) fn builtin_chdir(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let path = args
            .first()
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("chdir requires a path"))?;
        let path_buf = self.resolve_path(&path);
        if !path_buf.is_dir() {
            return Err(RuntimeError::new(format!(
                "chdir path is not a directory: {}",
                path
            )));
        }
        let cwd_val = self.make_io_path_instance(&Self::stringify_path(&path_buf));
        self.env.insert("$*CWD".to_string(), cwd_val.clone());
        self.env.insert("*CWD".to_string(), cwd_val);
        Ok(Value::Bool(true))
    }

    pub(super) fn builtin_indir(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let path = args
            .first()
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("indir requires a path"))?;
        let path_buf = self.resolve_path(&path);
        if !path_buf.is_dir() {
            return Err(RuntimeError::new(format!(
                "indir path is not a directory: {}",
                path
            )));
        }
        let saved = self.env.get("$*CWD").cloned();
        let cwd_val = self.make_io_path_instance(&Self::stringify_path(&path_buf));
        self.env.insert("$*CWD".to_string(), cwd_val.clone());
        self.env.insert("*CWD".to_string(), cwd_val);
        let result = if let Some(body) = args.get(1) {
            if matches!(body, Value::Sub { .. }) {
                self.call_sub_value(body.clone(), vec![], false)
            } else {
                Ok(body.clone())
            }
        } else {
            Ok(Value::Nil)
        };
        if let Some(prev) = saved {
            self.env.insert("$*CWD".to_string(), prev.clone());
            self.env.insert("*CWD".to_string(), prev);
        } else {
            self.env.remove("$*CWD");
            self.env.remove("*CWD");
        }
        result
    }

    pub(super) fn builtin_tmpdir(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(path_value) = args.first() {
            let path = path_value.to_string_value();
            let path_buf = self.resolve_path(&path);
            if !path_buf.is_dir() {
                return Err(RuntimeError::new("tmpdir path must be a directory"));
            }
            let repr = Self::stringify_path(&path_buf);
            self.env
                .insert("$*TMPDIR".to_string(), Value::Str(repr.clone()));
            return Ok(Value::Str(repr));
        }
        Ok(Value::Str(
            self.get_dynamic_string("$*TMPDIR").unwrap_or_default(),
        ))
    }

    pub(super) fn builtin_homedir(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(path_value) = args.first() {
            let path = path_value.to_string_value();
            let path_buf = self.resolve_path(&path);
            if !path_buf.is_dir() {
                return Err(RuntimeError::new("homedir path must be a directory"));
            }
            let repr = Self::stringify_path(&path_buf);
            self.env
                .insert("$*HOME".to_string(), Value::Str(repr.clone()));
            return Ok(Value::Str(repr));
        }
        Ok(Value::Str(
            self.get_dynamic_string("$*HOME").unwrap_or_default(),
        ))
    }

    pub(super) fn builtin_link(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let target = args
            .first()
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("link requires a target"))?;
        let link = args
            .get(1)
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("link requires a link name"))?;
        let target_buf = self.resolve_path(&target);
        let link_buf = self.resolve_path(&link);
        fs::hard_link(&target_buf, &link_buf).map_err(|err| {
            RuntimeError::new(format!("Failed to create link '{}': {}", target, err))
        })?;
        Ok(Value::Bool(true))
    }

    pub(super) fn builtin_symlink(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let target = args
            .first()
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("symlink requires a target"))?;
        let link = args
            .get(1)
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("symlink requires a link name"))?;
        let target_buf = self.resolve_path(&target);
        let link_buf = self.resolve_path(&link);
        #[cfg(unix)]
        {
            unix_fs::symlink(&target_buf, &link_buf).map_err(|err| {
                RuntimeError::new(format!("Failed to symlink '{}': {}", target, err))
            })?;
        }
        #[cfg(windows)]
        {
            let metadata = fs::metadata(&target_buf);
            if metadata.map(|meta| meta.is_dir()).unwrap_or(false) {
                windows_fs::symlink_dir(&target_buf, &link_buf).map_err(|err| {
                    RuntimeError::new(format!("Failed to symlink '{}': {}", target, err))
                })?;
            } else {
                windows_fs::symlink_file(&target_buf, &link_buf).map_err(|err| {
                    RuntimeError::new(format!("Failed to symlink '{}': {}", target, err))
                })?;
            }
        }
        Ok(Value::Bool(true))
    }

    pub(super) fn builtin_print(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut content = String::new();
        if args.is_empty() && name == "note" {
            content.push_str("Noted");
        } else {
            for arg in args {
                content.push_str(&arg.to_string_value());
            }
        }
        let (handle, newline) = match name {
            "print" => ("$*OUT", false),
            "say" => ("$*OUT", true),
            _ => ("$*ERR", true),
        };
        self.write_to_named_handle(handle, &content, newline)?;
        Ok(Value::Nil)
    }

    pub(super) fn builtin_prompt(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let msg = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        self.write_to_named_handle("$*OUT", &msg, false)?;
        if let Some(handle) = self.default_input_handle() {
            let line = self.read_line_from_handle_value(&handle)?;
            return Ok(Value::Str(line));
        }
        Ok(Value::Str(String::new()))
    }

    pub(super) fn builtin_get(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let handle = args
            .first()
            .cloned()
            .or_else(|| self.default_input_handle());
        if let Some(handle) = handle {
            let line = self.read_line_from_handle_value(&handle)?;
            return Ok(Value::Str(line));
        }
        Ok(Value::Str(String::new()))
    }

    pub(super) fn builtin_lines(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let handle = args
            .first()
            .cloned()
            .or_else(|| self.default_input_handle());
        if let Some(handle) = handle {
            let mut lines = Vec::new();
            loop {
                let line = self.read_line_from_handle_value(&handle)?;
                if line.is_empty() {
                    break;
                }
                lines.push(Value::Str(line));
            }
            return Ok(Value::Array(lines));
        }
        Ok(Value::Array(Vec::new()))
    }

    pub(super) fn builtin_words(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let handle = if args.is_empty() {
            self.default_input_handle()
        } else if args.first().and_then(Self::handle_id_from_value).is_some() {
            args.first().cloned()
        } else {
            None
        };
        if let Some(handle) = handle {
            let mut words = Vec::new();
            loop {
                let line = self.read_line_from_handle_value(&handle)?;
                if line.is_empty() {
                    break;
                }
                for token in line.split_whitespace() {
                    words.push(Value::Str(token.to_string()));
                }
            }
            return Ok(Value::Array(words));
        }
        Ok(Value::Array(Vec::new()))
    }
}
