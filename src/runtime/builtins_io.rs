use super::*;

/// Check for NUL bytes in a path and return X::IO::Null error if found.
fn check_null_in_path(path: &str) -> Result<(), RuntimeError> {
    if path.contains('\0') {
        Err(RuntimeError::new(
            "X::IO::Null: Found null byte in pathname",
        ))
    } else {
        Ok(())
    }
}

impl Interpreter {
    fn dir_test_matches(&mut self, test: &Value, entry_name: &str, dir_path: &Path) -> bool {
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
            .insert("_".to_string(), Value::Str(entry_name.to_string()));
        self.env
            .insert("$_".to_string(), Value::Str(entry_name.to_string()));

        let matched = match test {
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } => self
                .call_sub_value(test.clone(), vec![Value::Str(entry_name.to_string())], true)
                .map(|v| v.truthy())
                .unwrap_or(false),
            _ => self.smart_match(&Value::Str(entry_name.to_string()), test),
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

    pub(super) fn builtin_slurp(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let path = args
            .first()
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("slurp requires a path argument"))?;
        check_null_in_path(&path)?;
        let content = fs::read_to_string(&path)
            .map_err(|err| RuntimeError::new(format!("Failed to slurp '{}': {}", path, err)))?;
        Ok(Value::Str(content))
    }

    pub(super) fn builtin_spurt(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let path = args
            .first()
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("spurt requires a path argument"))?;
        check_null_in_path(&path)?;
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
        check_null_in_path(&path)?;
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

    pub(super) fn builtin_dir(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut requested_opt: Option<String> = None;
        let mut requested_cwd_opt: Option<String> = None;
        let mut test_opt: Option<Value> = None;
        for arg in args {
            if let Value::Pair(key, val) = arg
                && key == "test"
            {
                test_opt = Some((**val).clone());
                continue;
            }
            if requested_opt.is_none() {
                if let Value::Instance {
                    class_name,
                    attributes,
                    ..
                } = arg
                    && class_name == "IO::Path"
                {
                    requested_opt = Some(
                        attributes
                            .get("path")
                            .map(|v| v.to_string_value())
                            .unwrap_or_default(),
                    );
                    requested_cwd_opt = attributes.get("cwd").map(|v| v.to_string_value());
                } else {
                    requested_opt = Some(arg.to_string_value());
                }
            }
        }
        let requested = requested_opt.unwrap_or_else(|| {
            self.get_dynamic_string("$*CWD")
                .unwrap_or_else(|| ".".to_string())
        });
        let path_buf = if Path::new(&requested).is_absolute() {
            self.resolve_path(&requested)
        } else if let Some(cwd) = &requested_cwd_opt {
            self.apply_chroot(PathBuf::from(cwd).join(Path::new(&requested)))
        } else {
            self.resolve_path(&requested)
        };
        let requested_path = PathBuf::from(&requested);
        let requested_is_absolute = requested_path.is_absolute();
        let mut entries = Vec::new();
        let mut push_entry = |basename: &str| {
            if let Some(test) = &test_opt
                && !self.dir_test_matches(test, basename, &path_buf)
            {
                return;
            }
            let out_path = if requested_is_absolute {
                path_buf.join(basename)
            } else if requested == "." {
                PathBuf::from(basename)
            } else {
                requested_path.join(basename)
            };
            let mut attrs = HashMap::new();
            attrs.insert(
                "path".to_string(),
                Value::Str(Self::stringify_path(&out_path)),
            );
            if let Some(cwd) = &requested_cwd_opt
                && !out_path.is_absolute()
            {
                attrs.insert("cwd".to_string(), Value::Str(cwd.clone()));
            }
            entries.push(Value::make_instance("IO::Path".to_string(), attrs));
        };

        if test_opt.is_some() {
            push_entry(".");
            push_entry("..");
        }
        for entry in fs::read_dir(&path_buf).map_err(|err| {
            RuntimeError::new(format!("Failed to read dir '{}': {}", requested, err))
        })? {
            let entry = entry.map_err(|err| {
                RuntimeError::new(format!("Failed to read dir entry '{}': {}", requested, err))
            })?;
            let basename = entry.file_name().to_string_lossy().to_string();
            push_entry(&basename);
        }
        Ok(Value::array(entries))
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
        check_null_in_path(&path)?;
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
            if matches!(body, Value::Sub(_)) {
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
            let val = self.make_io_path_instance(&repr);
            self.env.insert("$*TMPDIR".to_string(), val.clone());
            return Ok(val);
        }
        Ok(self
            .env
            .get("$*TMPDIR")
            .cloned()
            .unwrap_or_else(|| Value::Str(String::new())))
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
            "say" | "put" => ("$*OUT", true),
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
        if let Some(first) = args.first()
            && Self::handle_id_from_value(first).is_none()
        {
            let text = first.to_string_value();
            let mut limit: Option<usize> = None;
            let mut chomp = true;
            for arg in &args[1..] {
                match arg {
                    Value::Pair(key, value) if key == "chomp" => {
                        chomp = value.truthy();
                    }
                    Value::Int(i) => {
                        limit = Some((*i).max(0) as usize);
                    }
                    Value::BigInt(bi) => {
                        use num_traits::ToPrimitive;
                        limit = Some(bi.to_usize().unwrap_or(usize::MAX));
                    }
                    Value::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                        limit = None;
                    }
                    Value::Num(f) if *f >= 0.0 => {
                        limit = Some(*f as usize);
                    }
                    Value::Rat(n, d) if *d == 0 && *n > 0 => {
                        limit = None;
                    }
                    _ => {}
                }
            }
            let mut lines = crate::builtins::split_lines_with_chomp(&text, chomp);
            if let Some(n) = limit {
                lines.truncate(n);
            }
            let values = lines.into_iter().map(Value::Str).collect();
            return Ok(Value::array(values));
        }

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
            return Ok(Value::array(lines));
        }
        Ok(Value::array(Vec::new()))
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
            return Ok(Value::array(words));
        }
        Ok(Value::array(Vec::new()))
    }
}
