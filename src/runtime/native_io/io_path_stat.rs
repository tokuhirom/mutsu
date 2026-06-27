use super::*;

impl Interpreter {
    /// `.absolute` / `.relative` on an `IO::Path`: like [`try_io_path_lexical`],
    /// these derive a string from the path, but additionally depend on the
    /// **cwd** (`$*CWD` / the instance `cwd` attribute / the process cwd) — read
    /// through `&self` (`resolve_path`/`get_cwd_path`/`apply_chroot`), which are
    /// purely lexical (no filesystem access). The VM owns env/cwd, so this is a
    /// native dispatch; the single shared impl that `native_io_path` delegates to.
    /// Returns `None` for any other method (`.resolve` canonicalizes against the
    /// real filesystem and stays in `native_io_path`).
    pub(crate) fn try_io_path_cwd_method(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(method, "absolute" | "relative") {
            return None;
        }
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
        Some(match method {
            "absolute" => {
                if Self::is_win32_spec(attributes) {
                    let base = args
                        .first()
                        .map(|v| v.to_string_value())
                        .or_else(|| instance_cwd.clone())
                        .unwrap_or_else(|| Self::stringify_path(&cwd_path));
                    let abs = if Self::io_path_is_absolute_win32(&p) {
                        p.clone()
                    } else {
                        let sep = '\\';
                        if base.ends_with('\\') || base.ends_with('/') {
                            format!("{}{}", base, p)
                        } else {
                            format!("{}{}{}", base, sep, p)
                        }
                    };
                    let cleaned = Self::canonpath_win32(&abs, false);
                    Ok(Value::str(cleaned))
                } else if Self::is_cygwin_spec(attributes) {
                    let base = args
                        .first()
                        .map(|v| v.to_string_value())
                        .or_else(|| instance_cwd.clone())
                        .unwrap_or_else(|| Self::stringify_path(&cwd_path));
                    let pn = p.replace('\\', "/");
                    let abs = if Self::io_path_is_absolute_win32(&pn) {
                        pn
                    } else {
                        let bn = base.replace('\\', "/");
                        if bn.ends_with('/') {
                            format!("{}{}", bn, pn)
                        } else {
                            format!("{}/{}", bn, pn)
                        }
                    };
                    Ok(Value::str(Self::canonpath_cygwin(&abs, false)))
                } else {
                    let base = args.first().map(|v| v.to_string_value());
                    if let Some(base) = base {
                        if original.is_absolute() {
                            Ok(Value::str(p.clone()))
                        } else {
                            let joined = PathBuf::from(&base).join(&p);
                            Ok(Value::str(Self::stringify_path(&joined)))
                        }
                    } else {
                        let absolute = Self::stringify_path(&path_buf);
                        Ok(Value::str(absolute))
                    }
                }
            }
            "relative" => {
                if Self::is_win32_spec(attributes) {
                    let base = args
                        .first()
                        .map(|v| v.to_string_value())
                        .or_else(|| instance_cwd.clone())
                        .unwrap_or_else(|| Self::stringify_path(&cwd_path));
                    let norm_p = p.replace('/', "\\");
                    let norm_base = base.replace('/', "\\");
                    let rel = norm_p
                        .strip_prefix(&norm_base)
                        .and_then(|r| r.strip_prefix('\\'))
                        .unwrap_or(&norm_p);
                    Ok(Value::str(rel.to_string()))
                } else if Self::is_cygwin_spec(attributes) {
                    let base = args
                        .first()
                        .map(|v| v.to_string_value())
                        .or_else(|| instance_cwd.clone())
                        .unwrap_or_else(|| Self::stringify_path(&cwd_path));
                    let norm_p = p.replace('\\', "/");
                    let norm_base = base.replace('\\', "/");
                    let rel = norm_p
                        .strip_prefix(&norm_base)
                        .and_then(|r| r.strip_prefix('/'))
                        .unwrap_or(&norm_p);
                    Ok(Value::str(rel.to_string()))
                } else {
                    let rel_base_arg = args.first().map(|v| v.to_string_value());
                    let rel_base = rel_base_arg
                        .as_ref()
                        .map(PathBuf::from)
                        .or_else(|| instance_cwd.as_ref().map(PathBuf::from))
                        .unwrap_or_else(|| cwd_path.clone());
                    let rel = path_buf
                        .strip_prefix(&rel_base)
                        .map(Self::stringify_path)
                        .unwrap_or_else(|_| Self::stringify_path(&path_buf));
                    Ok(Value::str(rel))
                }
            }
            _ => unreachable!(),
        })
    }

    /// Filesystem `stat`-only predicates / accessors on an `IO::Path`
    /// (`e`/`f`/`d`/`l`/`r`/`w`/`x`/`rw`/`rwx`/`z` file tests and the
    /// `mode`/`s`/`created`/`modified`/`accessed`/`changed` stat readers). They
    /// resolve the receiver's path against the cwd (`&self`, VM-owned env) and
    /// then read the filesystem via `stat` only — no `io_handles` allocation, no
    /// `emit_output`, no encoding/content read. So the VM can dispatch them
    /// natively (ledger §D): the single shared impl that `native_io_path`
    /// delegates to. Returns `None` for any other method (content reads
    /// `slurp`/`lines`/handle-opening `open`/`spurt`, which need flags/encoding/
    /// `io_handles` and stay in `native_io_path`).
    pub(crate) fn try_io_path_fs_stat(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(
            method,
            "e" | "f"
                | "d"
                | "l"
                | "r"
                | "w"
                | "x"
                | "rw"
                | "rwx"
                | "z"
                | "mode"
                | "s"
                | "created"
                | "modified"
                | "accessed"
                | "changed"
        ) {
            return None;
        }
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let path_buf = self.resolve_io_path_buf(attributes, &p);
        Some(Self::io_path_stat_result(&path_buf, &p, method))
    }

    /// Resolve an `IO::Path`'s `path` attribute to an absolute filesystem
    /// `PathBuf` against the VM-owned cwd (`$*CWD` / the instance `cwd` attribute /
    /// the process cwd), applying any chroot. Purely lexical (no filesystem
    /// access) — shared by the `&self` native IO::Path methods
    /// (`try_io_path_fs_stat` / `try_io_path_content_read`) and `native_io_path`.
    pub(crate) fn resolve_io_path_buf(
        &self,
        attributes: &HashMap<String, Value>,
        p: &str,
    ) -> PathBuf {
        let instance_cwd = attributes.get("cwd").map(|v| v.to_string_value());
        if Path::new(p).is_absolute() {
            self.resolve_path(p)
        } else if let Some(cwd) = &instance_cwd {
            self.apply_chroot(PathBuf::from(cwd).join(Path::new(p)))
        } else {
            self.resolve_path(p)
        }
    }

    /// Pure `stat`-based result for the [`try_io_path_fs_stat`] methods given an
    /// already-resolved `path_buf` (and the original `p` for error/Failure
    /// messages). Factored out so both the VM-native path and `native_io_path`
    /// run the exact same filesystem queries and Failure shaping.
    fn io_path_stat_result(path_buf: &Path, p: &str, method: &str) -> Result<Value, RuntimeError> {
        match method {
            "e" => Ok(Value::Bool(path_buf.exists())),
            "f" => match fs::metadata(path_buf) {
                Ok(meta) => Ok(Value::Bool(meta.is_file())),
                Err(_) => Ok(io_path_missing_failure(p, method)),
            },
            "d" => match fs::metadata(path_buf) {
                Ok(meta) => Ok(Value::Bool(meta.is_dir())),
                Err(_) => Ok(io_path_missing_failure(p, method)),
            },
            "l" => match fs::symlink_metadata(path_buf) {
                Ok(meta) => Ok(Value::Bool(meta.file_type().is_symlink())),
                Err(_) => Ok(io_path_missing_failure(p, method)),
            },
            "r" => match fs::metadata(path_buf) {
                Ok(_) => Ok(Value::Bool(path_is_readable(path_buf))),
                Err(_) => Ok(io_path_missing_failure(p, method)),
            },
            "w" => match fs::metadata(path_buf) {
                Ok(_) => Ok(Value::Bool(path_is_writable(path_buf))),
                Err(_) => Ok(io_path_missing_failure(p, method)),
            },
            "x" => match fs::metadata(path_buf) {
                Ok(_) => Ok(Value::Bool(path_is_executable(path_buf))),
                Err(_) => Ok(io_path_missing_failure(p, method)),
            },
            "rw" => match fs::metadata(path_buf) {
                Ok(_) => Ok(Value::Bool(
                    path_is_readable(path_buf) && path_is_writable(path_buf),
                )),
                Err(_) => Ok(io_path_missing_failure(p, method)),
            },
            "rwx" => match fs::metadata(path_buf) {
                Ok(_) => Ok(Value::Bool(
                    path_is_readable(path_buf)
                        && path_is_writable(path_buf)
                        && path_is_executable(path_buf),
                )),
                Err(_) => Ok(io_path_missing_failure(p, method)),
            },
            "z" => match fs::metadata(path_buf) {
                Ok(meta) => Ok(Value::Bool(meta.len() == 0)),
                Err(_) => {
                    let message = format!("Failed to find '{}' while trying to do '.z'", p);
                    let mut attrs = HashMap::new();
                    attrs.insert("message".to_string(), Value::str(message));
                    attrs.insert("path".to_string(), Value::str(p.to_string()));
                    attrs.insert("trying".to_string(), Value::str_from("z"));
                    let ex = Value::make_instance(Symbol::intern("X::IO::DoesNotExist"), attrs);
                    let mut failure_attrs = HashMap::new();
                    failure_attrs.insert("exception".to_string(), ex);
                    Ok(Value::make_instance(
                        Symbol::intern("Failure"),
                        failure_attrs,
                    ))
                }
            },
            "mode" => {
                let metadata = fs::metadata(path_buf)
                    .map_err(|err| RuntimeError::new(format!("Failed to stat '{}': {}", p, err)))?;
                #[cfg(unix)]
                {
                    let mode = metadata.permissions().mode() & 0o777;
                    Ok(Value::str(format!("{:04o}", mode)))
                }
                #[cfg(not(unix))]
                {
                    let mode = if metadata.permissions().readonly() {
                        "0444"
                    } else {
                        "0666"
                    };
                    Ok(Value::str(mode.to_string()))
                }
            }
            "s" => {
                let size = io_path_metadata(path_buf, p, method)?.len();
                Ok(Value::Int(size as i64))
            }
            "created" => {
                let ts = fs::metadata(path_buf)
                    .and_then(|meta| meta.created())
                    .map(Self::system_time_to_int)
                    .map_err(|err| {
                        RuntimeError::new(format!("Failed to get created time '{}': {}", p, err))
                    })?;
                Ok(Value::Int(ts))
            }
            "modified" => {
                let ts = fs::metadata(path_buf)
                    .and_then(|meta| meta.modified())
                    .map(Self::system_time_to_int)
                    .map_err(|err| {
                        RuntimeError::new(format!("Failed to get modified time '{}': {}", p, err))
                    })?;
                Ok(Value::Int(ts))
            }
            "accessed" => {
                let ts = fs::metadata(path_buf)
                    .and_then(|meta| meta.accessed())
                    .map(Self::system_time_to_int)
                    .map_err(|err| {
                        RuntimeError::new(format!("Failed to get accessed time '{}': {}", p, err))
                    })?;
                Ok(Value::Int(ts))
            }
            "changed" => {
                #[cfg(unix)]
                {
                    use std::os::unix::fs::MetadataExt;
                    let meta = fs::metadata(path_buf).map_err(|err| {
                        RuntimeError::new(format!("Failed to get changed time '{}': {}", p, err))
                    })?;
                    Ok(Value::Int(meta.ctime()))
                }
                #[cfg(not(unix))]
                {
                    // On non-Unix platforms, fall back to modified time
                    let ts = fs::metadata(path_buf)
                        .and_then(|meta| meta.modified())
                        .map(Self::system_time_to_int)
                        .map_err(|err| {
                            RuntimeError::new(format!(
                                "Failed to get changed time '{}': {}",
                                p, err
                            ))
                        })?;
                    Ok(Value::Int(ts))
                }
            }
            _ => unreachable!("io_path_stat_result called with non-stat method"),
        }
    }
}
