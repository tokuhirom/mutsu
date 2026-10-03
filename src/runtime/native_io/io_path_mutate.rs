use super::*;
use crate::value::AttrMap;

impl Interpreter {
    /// Single-path filesystem *mutations* on an `IO::Path`
    /// (`spurt`/`mkdir`/`rmdir`/`unlink`/`chmod`/`chown`): resolve the path against the
    /// VM-owned cwd, then perform a one-shot syscall (`fs::write`/`create_dir_all`/
    /// `remove_dir`/`remove_file`/`set_permissions`). They allocate **no
    /// `io_handles`** — `spurt` opens, writes, and immediately drops its file
    /// handle. Encoding lookup (`encode_with_encoding`, reading the VM-owned
    /// encoding registry) is a `&self` read, so the VM dispatches these natively
    /// (ledger §D): the single impl `native_io_path` also delegates to. Two-path
    /// ops (`copy`/`rename`/`move`/`symlink`/`link`, which resolve a destination)
    /// and handle-opening `open` return `None` and stay in `native_io_path`.
    /// Cost: O(p + a + c) plus one filesystem mutation, p = path length,
    /// a = argument count, c = content bytes written by `spurt` (zero otherwise).
    pub(crate) fn try_io_path_fs_mutate(
        &self,
        attributes: &AttrMap,
        class_name: &str,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(
            method,
            "spurt" | "mkdir" | "rmdir" | "unlink" | "chmod" | "chown"
        ) {
            return None;
        }
        Some(self.io_path_fs_mutate(attributes, class_name, method, args))
    }

    /// The fallible body of [`Self::try_io_path_fs_mutate`] (the gate returns `Option`
    /// so it cannot use `?`). Behavior-invariant with the arms `native_io_path`
    /// previously held.
    fn io_path_fs_mutate(
        &self,
        attributes: &AttrMap,
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
                let content_value = args.first().cloned().unwrap_or(Value::str(String::new()));
                let mut append = false;
                let mut createonly = false;
                let mut enc: Option<String> = None;
                for arg in args.iter().skip(1) {
                    if let ValueView::Pair(key, val) = arg.view() {
                        match key.as_str() {
                            "append" => append = val.truthy(),
                            "createonly" => createonly = val.truthy(),
                            "enc" => enc = Some(val.to_string_value()),
                            _ => {}
                        }
                    }
                }
                Ok(self.spurt_file(
                    &path_buf,
                    &content_value,
                    append,
                    createonly,
                    enc.as_deref(),
                ))
            }
            "mkdir" => match self.mkdir_op(&path_buf) {
                Ok(()) => Ok(Value::make_instance(
                    Symbol::intern(class_name),
                    attributes.clone(),
                )),
                Err(exception) => Ok(super::fs_errors::failure_of(exception)),
            },
            "rmdir" => Ok(self.rmdir_op(&path_buf)),
            // Per raku, `.unlink` returns True on success and when the file did
            // not exist (it is already gone, as the `unlink` sub also reports),
            // and fails softly (a Failure carrying X::IO::Unlink) for
            // any other error (e.g. the path is a directory) so `without`/`try`
            // can handle it rather than the method throwing.
            "unlink" => match fs::remove_file(&path_buf) {
                Ok(()) => Ok(Value::TRUE),
                Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(Value::TRUE),
                Err(err) => {
                    // Rakudo reports libuv's wording ("illegal operation on a
                    // directory" for a directory target).
                    let reason = super::fs_errors::libuv_text(&err);
                    let msg = format!(
                        "Failed to remove the file '{}': Failed to delete file: {}",
                        Self::stringify_path(&path_buf),
                        reason
                    );
                    let mut ex_attrs = HashMap::new();
                    ex_attrs.insert("message".to_string(), Value::str_from(&msg));
                    ex_attrs.insert("path".to_string(), Value::str_from(&p));
                    let ex = Value::make_instance(Symbol::intern("X::IO::Unlink"), ex_attrs);
                    let mut failure_attrs = HashMap::new();
                    failure_attrs.insert("exception".to_string(), ex);
                    Ok(Value::make_instance(
                        Symbol::intern("Failure"),
                        failure_attrs,
                    ))
                }
            },
            "chmod" => {
                // Permission bits are a unix concept; everything below the gate
                // compiles only where it can run (wasm32 is not unix).
                #[cfg(not(unix))]
                {
                    let _ = args;
                    Err(RuntimeError::new("chmod not supported on this platform"))
                }
                #[cfg(unix)]
                {
                    let mode_value = args
                        .first()
                        .cloned()
                        .ok_or_else(|| RuntimeError::new("chmod requires mode"))?;
                    if let Some(err) = self.failure_to_runtime_error_if_unhandled(&mode_value) {
                        return Err(err);
                    }
                    let mode_int = match mode_value.view() {
                        ValueView::Int(i) => i as u32,
                        // An allomorph (e.g. IntStr from `:chmod<0o777>`) carries its
                        // already-evaluated integer in the inner value; coerce through it.
                        ValueView::Mixin(..) | ValueView::BigInt(_) => {
                            crate::runtime::to_int(&mode_value) as u32
                        }
                        ValueView::Str(s) => u32::from_str_radix(&s, 8).unwrap_or(0),
                        _ => {
                            return Err(RuntimeError::new(format!(
                                "Invalid mode: {}",
                                mode_value.to_string_value()
                            )));
                        }
                    };
                    Ok(self.chmod_op(&path_buf, mode_int))
                }
            }
            "chown" => {
                #[cfg(not(unix))]
                {
                    let _ = args;
                    Err(RuntimeError::new("chown not supported on this platform"))
                }
                #[cfg(unix)]
                {
                    use std::ffi::CString;
                    use std::os::unix::ffi::OsStrExt;

                    let mut uid = !0 as libc::uid_t;
                    let mut gid = !0 as libc::gid_t;
                    for arg in args {
                        let ValueView::Pair(name, value) = arg.view() else {
                            return Err(RuntimeError::new("chown accepts only named uid and gid"));
                        };
                        if let Some(err) = self.failure_to_runtime_error_if_unhandled(value) {
                            return Err(err);
                        }
                        let ValueView::Int(number) = value.view() else {
                            return Err(RuntimeError::new(format!(
                                "chown {} must be an Int",
                                name
                            )));
                        };
                        match name.as_str() {
                            "uid" => uid = number as libc::uid_t,
                            "gid" => gid = number as libc::gid_t,
                            _ => {
                                return Err(RuntimeError::new(format!(
                                    "Unknown chown argument: {}",
                                    name
                                )));
                            }
                        }
                    }
                    let cpath = CString::new(path_buf.as_os_str().as_bytes())
                        .map_err(|_| RuntimeError::new("chown path contains a NUL byte"))?;
                    if unsafe { libc::chown(cpath.as_ptr(), uid, gid) } == 0 {
                        Ok(Value::TRUE)
                    } else {
                        Ok(io_exception_failure(
                            "X::IO::Chown",
                            format!(
                                "Failed to change owner of '{}': {}",
                                p,
                                std::io::Error::last_os_error()
                            ),
                        ))
                    }
                }
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
        attributes: &AttrMap,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(method, "copy" | "rename" | "move" | "symlink" | "link") {
            return None;
        }
        Some(self.io_path_two_path_op(attributes, method, args))
    }

    /// The fallible body of [`Self::try_io_path_two_path_op`] (the gate returns `Option`
    /// so it cannot use `?`). Behavior-invariant with the arms `native_io_path`
    /// previously held.
    fn io_path_two_path_op(
        &self,
        attributes: &AttrMap,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // ADR-0070: the arms below read the destination positionally, so an
        // undeclared named argument must not stand in for it
        // (`"/tmp".IO.link(:zzz)` linked to a file named "zzz\tTrue").
        let stripped = crate::builtins::strip_undeclared_nameds(method, args);
        let args: &[Value] = stripped.as_deref().unwrap_or(args);
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let path_buf = self.resolve_io_path_buf(attributes, &p);
        match method {
            "copy" | "rename" | "move" => {
                let dest = args
                    .first()
                    .map(|v| v.to_string_value())
                    .ok_or_else(|| RuntimeError::new(format!("{} requires destination", method)))?;
                let createonly = Self::named_bool(args, "createonly");
                let dest_buf = self.resolve_path(&dest);
                Ok(if method == "copy" {
                    self.copy_file_op(&path_buf, &dest_buf, createonly)
                } else {
                    self.rename_file_op(method, &path_buf, &dest_buf, createonly)
                })
            }
            "symlink" => {
                // Platforms with no symlink syscall refuse before touching the
                // args, so the rest of the arm compiles only where it can run.
                #[cfg(not(any(unix, windows)))]
                {
                    let _ = args;
                    Err(RuntimeError::new("symlink not supported on this platform"))
                }
                #[cfg(any(unix, windows))]
                {
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
                            Ok(()) => Ok(Value::TRUE),
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
                            Ok(()) => Ok(Value::TRUE),
                            Err(err) => Ok(Self::make_symlink_failure(&p, &link_name, &err)),
                        }
                    }
                }
            }
            "link" => {
                // IO::Path.link($name): creates a new hard link named $name
                // pointing to self (the target). Fails with X::IO::Link.
                let link_name = args.first().map(|v| v.to_string_value()).ok_or_else(|| {
                    RuntimeError::new("Too few positionals passed; expected 2 arguments but got 1")
                })?;
                let link_buf = self.resolve_path(&link_name);
                match fs::hard_link(&path_buf, &link_buf) {
                    Ok(()) => Ok(Value::TRUE),
                    Err(err) => Ok(Self::make_link_failure(&p, &link_name, &err)),
                }
            }
            _ => unreachable!("io_path_two_path_op called with non-two-path method"),
        }
    }
}
