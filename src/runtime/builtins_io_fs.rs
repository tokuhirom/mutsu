//! Filesystem builtins: `dir`, `copy`, `rename`, `chmod`, `mkdir`, `rmdir`.
use super::builtins_io::{check_null_in_path, io_exception_error, io_exception_failure};
use super::*;

impl Interpreter {
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
                            .as_map()
                            .get("path")
                            .map(|v| v.to_string_value())
                            .unwrap_or_default(),
                    );
                    requested_cwd_opt = attributes.as_map().get("cwd").map(|v| v.to_string_value());
                } else {
                    requested_opt = Some(arg.to_string_value());
                }
            }
        }
        let dir_from_cwd = requested_opt.is_none();
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
            // When dir() is called with no explicit path (using $*CWD) or with ".",
            // return just basenames. Otherwise prefix with the requested path.
            let out_path = if dir_from_cwd || requested == "." {
                PathBuf::from(basename)
            } else if requested_is_absolute {
                path_buf.join(basename)
            } else {
                requested_path.join(basename)
            };
            let mut attrs = HashMap::new();
            attrs.insert(
                "path".to_string(),
                Value::str(Self::stringify_path(&out_path)),
            );
            if !out_path.is_absolute() {
                if let Some(cwd) = &requested_cwd_opt {
                    attrs.insert("cwd".to_string(), Value::str(cwd.clone()));
                } else if dir_from_cwd && requested_is_absolute {
                    attrs.insert(
                        "cwd".to_string(),
                        Value::str(Self::stringify_path(&path_buf)),
                    );
                }
            }
            entries.push(Value::make_instance(Symbol::intern("IO::Path"), attrs));
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
        let mut positional = Vec::new();
        let mut createonly = false;
        for arg in args {
            match arg {
                Value::Pair(key, value) if key == "createonly" => {
                    createonly = value.truthy();
                }
                _ => positional.push(arg),
            }
        }
        let source = positional
            .first()
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("copy requires a source path"))?;
        let dest = positional
            .get(1)
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("copy requires a destination path"))?;
        check_null_in_path(&source)?;
        check_null_in_path(&dest)?;
        let src_buf = self.resolve_path(&source);
        let dest_buf = self.resolve_path(&dest);
        if createonly && dest_buf.exists() {
            return Ok(io_exception_failure(
                "X::IO::Copy",
                format!("Failed to copy '{}': destination already exists", source),
            ));
        }
        if src_buf.is_dir() {
            return Ok(io_exception_failure(
                "X::IO::Copy",
                format!("Failed to copy '{}': source is a directory", source),
            ));
        }
        if src_buf.exists() && dest_buf.exists() {
            let same_file = fs::canonicalize(&src_buf).ok() == fs::canonicalize(&dest_buf).ok();
            if same_file {
                return Ok(io_exception_failure(
                    "X::IO::Copy",
                    format!(
                        "Failed to copy '{}': source and destination are the same file",
                        source
                    ),
                ));
            }
        }
        fs::copy(&src_buf, &dest_buf).map_err(|err| {
            io_exception_error(
                "X::IO::Copy",
                format!("Failed to copy '{}': {}", source, err),
            )
        })?;
        Ok(Value::Bool(true))
    }

    pub(super) fn builtin_rename(&self, name: &str, args: &[Value]) -> Result<Value, RuntimeError> {
        let ex_type = if name == "rename" {
            "X::IO::Rename"
        } else {
            "X::IO::Move"
        };
        let mut positional = Vec::new();
        let mut createonly = false;
        for arg in args {
            match arg {
                Value::Pair(key, value) if key == "createonly" => {
                    createonly = value.truthy();
                }
                _ => positional.push(arg),
            }
        }
        let source = positional
            .first()
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("rename requires a source path"))?;
        let dest = positional
            .get(1)
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("rename requires a destination path"))?;
        let src_buf = self.resolve_path(&source);
        let dest_buf = self.resolve_path(&dest);
        // Check if source and destination are the same file
        if src_buf == dest_buf
            || (src_buf.exists()
                && dest_buf.exists()
                && fs::canonicalize(&src_buf).ok() == fs::canonicalize(&dest_buf).ok())
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
                        name, source
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
            return Err(io_exception_error(
                ex_type,
                format!(
                    "Failed to {} '{}': destination already exists",
                    name, source
                ),
            ));
        }
        fs::rename(&src_buf, &dest_buf).map_err(|err| {
            io_exception_error(ex_type, format!("Failed to {} '{}': {}", name, source, err))
        })?;
        Ok(Value::Bool(true))
    }

    pub(super) fn builtin_chmod(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mode_value = args
            .first()
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
        let mut changed = Vec::new();
        for path_value in args.iter().skip(1) {
            let path = path_value.to_string_value();
            let path_buf = self.resolve_path(&path);
            #[cfg(unix)]
            {
                let perms = PermissionsExt::from_mode(mode_int);
                fs::set_permissions(&path_buf, perms).map_err(|err| {
                    RuntimeError::new(format!("Failed to chmod '{}': {}", path, err))
                })?;
            }
            #[cfg(not(unix))]
            {
                return Err(RuntimeError::new("chmod not supported on this platform"));
            }
            changed.push(path_value.clone());
        }
        Ok(Value::Array(
            crate::value::Value::array_arc(changed),
            ArrayKind::List,
        ))
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
        match fs::remove_dir(&path_buf) {
            Ok(()) => Ok(Value::Bool(true)),
            Err(_) => Ok(Value::Bool(false)),
        }
    }
}
