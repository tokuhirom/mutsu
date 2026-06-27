//! Directory navigation (`chdir`/`indir`/`tmpdir`/`homedir`) and link (`link`/`symlink`) builtins.
use super::builtins_io::{
    check_null_in_path, has_required_mode_bits, io_exception_failure, parse_io_requirements,
};
use super::*;

impl Interpreter {
    pub(super) fn builtin_chdir(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let arg = args
            .first()
            .ok_or_else(|| RuntimeError::new("chdir requires a path"))?;
        let (require_dir, require_read, require_write, require_exec) = parse_io_requirements(args);
        let effective_arg = if let Value::Capture { positional, named } = arg {
            if named.is_empty() && positional.len() == 1 {
                positional[0].clone()
            } else {
                arg.clone()
            }
        } else {
            arg.clone()
        };
        let mut requested = effective_arg.to_string_value();
        let mut requested_cwd_opt: Option<String> = None;
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &effective_arg
            && (class_name == "IO::Path"
                || self
                    .class_mro(&class_name.resolve())
                    .iter()
                    .any(|n| n == "IO::Path"))
        {
            requested = attributes
                .as_map()
                .get("path")
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            requested_cwd_opt = attributes.as_map().get("cwd").map(|v| v.to_string_value());
        }
        check_null_in_path(&requested)?;
        let path_buf = if Path::new(&requested).is_absolute() {
            self.resolve_path(&requested)
        } else if let Some(cwd) = &requested_cwd_opt {
            self.apply_chroot(PathBuf::from(cwd).join(Path::new(&requested)))
        } else {
            self.resolve_path(&requested)
        };
        let absolute_target = if path_buf.is_absolute() {
            path_buf
        } else {
            self.resolve_path(&Self::stringify_path(&path_buf))
        };
        if !absolute_target.exists() {
            return Ok(io_exception_failure(
                "X::IO::Chdir",
                format!(
                    "Failed to chdir to '{}': no such file or directory",
                    requested
                ),
            ));
        }
        if require_dir && !absolute_target.is_dir() {
            return Ok(io_exception_failure(
                "X::IO::Chdir",
                format!("Failed to chdir to '{}': not a directory", requested),
            ));
        }
        if !has_required_mode_bits(&absolute_target, require_read, require_write, require_exec) {
            return Ok(io_exception_failure(
                "X::IO::Chdir",
                format!("Failed to chdir to '{}': permission denied", requested),
            ));
        }
        let canonical = fs::canonicalize(&absolute_target).unwrap_or(absolute_target);
        // Raku's chdir primarily updates $*CWD. We attempt the OS-level
        // chdir for directories but do not treat failure as fatal — the
        // real validation comes from the :d/:r/:w/:x test adverbs above.
        if canonical.is_dir() {
            let _ = std::env::set_current_dir(&canonical);
        }
        let cwd_val = self.make_io_path_instance(&Self::stringify_path(&canonical));
        self.env.insert("$*CWD".to_string(), cwd_val.clone());
        self.env.insert("*CWD".to_string(), cwd_val.clone());
        Ok(cwd_val)
    }

    pub(super) fn builtin_indir(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let (require_dir, require_read, require_write, require_exec) = parse_io_requirements(args);
        let mut path_arg: Option<Value> = None;
        let mut body_arg: Option<Value> = None;
        for arg in args {
            if matches!(arg, Value::Pair(_, _)) {
                continue;
            }
            if path_arg.is_none() {
                path_arg = Some(arg.clone());
            } else {
                body_arg = Some(arg.clone());
            }
        }
        let path_arg = path_arg.ok_or_else(|| RuntimeError::new("indir requires a path"))?;
        let mut requested = path_arg.to_string_value();
        let mut requested_cwd_opt: Option<String> = None;
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &path_arg
            && class_name == "IO::Path"
        {
            requested = attributes
                .as_map()
                .get("path")
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            requested_cwd_opt = attributes.as_map().get("cwd").map(|v| v.to_string_value());
        }
        check_null_in_path(&requested)?;
        let path_buf = if Path::new(&requested).is_absolute() {
            self.resolve_path(&requested)
        } else if let Some(cwd) = &requested_cwd_opt {
            self.apply_chroot(PathBuf::from(cwd).join(Path::new(&requested)))
        } else {
            self.resolve_path(&requested)
        };
        let absolute_target = if path_buf.is_absolute() {
            path_buf
        } else {
            self.resolve_path(&Self::stringify_path(&path_buf))
        };
        if !absolute_target.exists() {
            return Ok(io_exception_failure(
                "X::IO::Chdir",
                format!(
                    "Failed to chdir to '{}': no such file or directory",
                    requested
                ),
            ));
        }
        if require_dir && !absolute_target.is_dir() {
            return Ok(io_exception_failure(
                "X::IO::Chdir",
                format!("Failed to chdir to '{}': not a directory", requested),
            ));
        }
        if !has_required_mode_bits(&absolute_target, require_read, require_write, require_exec) {
            return Ok(io_exception_failure(
                "X::IO::Chdir",
                format!("Failed to chdir to '{}': permission denied", requested),
            ));
        }
        let saved = self.env.get("$*CWD").cloned();
        let canonical = fs::canonicalize(&absolute_target).unwrap_or(absolute_target);
        let cwd_val = self.make_io_path_instance(&Self::stringify_path(&canonical));
        self.env.insert("$*CWD".to_string(), cwd_val.clone());
        self.env.insert("*CWD".to_string(), cwd_val);
        let result = if let Some(body) = body_arg {
            if matches!(
                body,
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
            ) {
                self.call_sub_value(body.clone(), vec![], true)
            } else {
                Ok(body)
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
            .unwrap_or_else(|| Value::str(String::new())))
    }

    pub(super) fn builtin_homedir(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(path_value) = args.first() {
            let path = path_value.to_string_value();
            let path_buf = self.resolve_path(&path);
            if !path_buf.is_dir() {
                return Err(RuntimeError::new("homedir path must be a directory"));
            }
            let repr = Self::stringify_path(&path_buf);
            let home_val = self.make_io_path_instance(&repr);
            self.env.insert("$*HOME".to_string(), home_val);
            return Ok(self.make_io_path_instance(&repr));
        }
        Ok(Value::str(
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
        // Creates a new hard link `$link` pointing at `$target`. On failure
        // (target missing, link already exists, ...) Raku returns a Failure
        // carrying X::IO::Link rather than throwing immediately.
        match fs::hard_link(&target_buf, &link_buf) {
            Ok(()) => Ok(Value::Bool(true)),
            Err(err) => Ok(Self::make_link_failure(&target, &link, &err)),
        }
    }

    pub(super) fn make_link_failure(target: &str, link: &str, err: &std::io::Error) -> Value {
        use crate::symbol::Symbol;
        let msg = format!(
            "Failed to create hard link '{}' for target '{}': {}",
            link, target, err
        );
        let target_io = Value::make_instance_without_destroy(Symbol::intern("IO::Path"), {
            let mut a = std::collections::HashMap::new();
            a.insert("path".to_string(), Value::str_from(target));
            a
        });
        let name_io = Value::make_instance_without_destroy(Symbol::intern("IO::Path"), {
            let mut a = std::collections::HashMap::new();
            a.insert("path".to_string(), Value::str_from(link));
            a
        });
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg));
        attrs.insert("target".to_string(), target_io);
        attrs.insert("name".to_string(), name_io);
        let ex = Value::make_instance(Symbol::intern("X::IO::Link"), attrs);
        let mut failure_attrs = std::collections::HashMap::new();
        failure_attrs.insert("exception".to_string(), ex);
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
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
        // The target path is passed to the OS as-is (relative stays relative).
        // The link path is resolved to handle CWD.
        let target_buf = std::path::PathBuf::from(&target);
        let link_buf = self.resolve_path(&link);
        #[cfg(unix)]
        {
            match unix_fs::symlink(&target_buf, &link_buf) {
                Ok(()) => Ok(Value::Bool(true)),
                Err(err) => Ok(Self::make_symlink_failure(&target, &link, &err)),
            }
        }
        #[cfg(windows)]
        {
            let metadata = fs::metadata(&target_buf);
            let result = if metadata.map(|meta| meta.is_dir()).unwrap_or(false) {
                windows_fs::symlink_dir(&target_buf, &link_buf)
            } else {
                windows_fs::symlink_file(&target_buf, &link_buf)
            };
            match result {
                Ok(()) => Ok(Value::Bool(true)),
                Err(err) => Ok(Self::make_symlink_failure(&target, &link, &err)),
            }
        }
        #[cfg(not(any(unix, windows)))]
        {
            Err(RuntimeError::new("symlink not supported on this platform"))
        }
    }

    pub(super) fn make_symlink_failure(target: &str, link: &str, err: &std::io::Error) -> Value {
        use crate::symbol::Symbol;
        let msg = format!(
            "Failed to create symlink '{}' for target '{}': {}",
            link, target, err
        );
        let target_io = Value::make_instance_without_destroy(Symbol::intern("IO::Path"), {
            let mut a = std::collections::HashMap::new();
            a.insert("path".to_string(), Value::str_from(target));
            a
        });
        let name_io = Value::make_instance_without_destroy(Symbol::intern("IO::Path"), {
            let mut a = std::collections::HashMap::new();
            a.insert("path".to_string(), Value::str_from(link));
            a
        });
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg));
        attrs.insert("target".to_string(), target_io);
        attrs.insert("name".to_string(), name_io);
        let ex = Value::make_instance(Symbol::intern("X::IO::Symlink"), attrs);
        let mut failure_attrs = std::collections::HashMap::new();
        failure_attrs.insert("exception".to_string(), ex);
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }
}
