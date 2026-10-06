use super::*;
use crate::value::AttrMap;

impl Interpreter {
    /// The path of an `IO::Path` receiver (its `path` attribute) and that path
    /// resolved against the cwd, which a mutation acts on.
    fn io_path_resolved(&self, attributes: &AttrMap) -> (String, PathBuf) {
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let path_buf = self.resolve_io_path_buf(attributes, &p);
        (p, path_buf)
    }

    /// `IO::Path.spurt($content, :append, :createonly, :enc)`: write the content
    /// in one go. No `io_handles` entry: it opens, writes and drops its file.
    /// The encoding lookup (`encode_with_encoding`) reads the VM-owned
    /// encoding registry.
    // Cost: O(p + c), p = path length, c = content bytes written.
    pub(crate) fn io_path_spurt(&self, attributes: &AttrMap, args: &[Value]) -> Value {
        let (_, path_buf) = self.io_path_resolved(attributes);
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
        self.spurt_file(&path_buf, &content_value, append, createonly, enc.as_deref())
    }

    /// `IO::Path.mkdir($mode)`: make the directory (and its missing parents)
    /// and answer `receiver`, or a `Failure` carrying the `X::IO::Mkdir`.
    // Cost: O(p) plus one mkdir(2) per missing parent, p = path length.
    pub(crate) fn io_path_mkdir(
        &self,
        attributes: &AttrMap,
        args: &[Value],
        receiver: Value,
    ) -> Value {
        let (_, path_buf) = self.io_path_resolved(attributes);
        match self.mkdir_op(&path_buf, super::fs_ops::mkdir_mode(args.first())) {
            Ok(()) => receiver,
            Err(exception) => super::fs_errors::failure_of(exception),
        }
    }

    /// `IO::Path.rmdir`.
    // Cost: O(p) plus one rmdir(2), p = path length.
    pub(crate) fn io_path_rmdir(&self, attributes: &AttrMap) -> Value {
        let (_, path_buf) = self.io_path_resolved(attributes);
        self.rmdir_op(&path_buf)
    }

    /// `IO::Path.unlink`. Per raku, it returns True on success and when the
    /// file did not exist (it is already gone, as the `unlink` sub also
    /// reports), and fails softly (a Failure carrying X::IO::Unlink) for any
    /// other error (e.g. the path is a directory) so `without`/`try` can handle
    /// it rather than the method throwing.
    // Cost: O(p) plus one unlink(2), p = path length.
    pub(crate) fn io_path_unlink(&self, attributes: &AttrMap) -> Value {
        let (p, path_buf) = self.io_path_resolved(attributes);
        match super::fs_syscalls::unlink_file(&path_buf) {
            Ok(()) => Value::TRUE,
            Err(reason) => {
                // Rakudo reports libuv's wording ("illegal operation on a
                // directory" for a directory target).
                let msg = format!(
                    "Failed to remove the file '{}': {}",
                    Self::stringify_path(&path_buf),
                    reason
                );
                let mut ex_attrs = HashMap::new();
                ex_attrs.insert("message".to_string(), Value::str_from(&msg));
                ex_attrs.insert("path".to_string(), Value::str_from(&p));
                let ex = Value::make_instance(Symbol::intern("X::IO::Unlink"), ex_attrs);
                let mut failure_attrs = HashMap::new();
                failure_attrs.insert("exception".to_string(), ex);
                Value::make_instance(Symbol::intern("Failure"), failure_attrs)
            }
        }
    }

    /// `IO::Path.chmod($mode)`.
    // Cost: O(p) plus one chmod(2), p = path length.
    pub(crate) fn io_path_chmod(
        &self,
        attributes: &AttrMap,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Permission bits are a unix concept; everything below the gate
        // compiles only where it can run (wasm32 is not unix).
        #[cfg(not(unix))]
        {
            let _ = (attributes, args);
            Err(RuntimeError::new("chmod not supported on this platform"))
        }
        #[cfg(unix)]
        {
            let (_, path_buf) = self.io_path_resolved(attributes);
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

    /// `IO::Path.chown(:uid, :gid)`.
    // Cost: O(p) plus one chown(2), p = path length.
    pub(crate) fn io_path_chown(
        &self,
        attributes: &AttrMap,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        #[cfg(not(unix))]
        {
            let _ = (attributes, args);
            Err(RuntimeError::new("chown not supported on this platform"))
        }
        #[cfg(unix)]
        {
            use std::ffi::CString;
            use std::os::unix::ffi::OsStrExt;

            let (p, path_buf) = self.io_path_resolved(attributes);
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
            // SAFETY: `cpath` is a NUL-terminated C string that outlives the call.
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

    /// `IO::Path.copy($dest, :createonly)`, `rename` and `move` (named by
    /// `method`, which the failure messages use): resolve the receiver's path
    /// and the destination against the cwd, then one syscall. No `io_handles`.
    // Cost: O(p + d) plus one syscall (a copy reads and writes the file), p, d
    // = path lengths.
    pub(crate) fn io_path_copy_or_move(
        &self,
        attributes: &AttrMap,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let (_, path_buf) = self.io_path_resolved(attributes);
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

    /// `IO::Path.symlink($name, :absolute)`: create a symlink named `$name`
    /// pointing to the receiver (the target). `:absolute` defaults to True;
    /// `:!absolute` uses the original path string.
    // Cost: O(p + n) plus one symlink(2), p, n = path lengths.
    pub(crate) fn io_path_symlink(
        &self,
        attributes: &AttrMap,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Platforms with no symlink syscall refuse before touching the
        // args, so the rest compiles only where it can run.
        #[cfg(not(any(unix, windows)))]
        {
            let _ = (attributes, args);
            Err(RuntimeError::new("symlink not supported on this platform"))
        }
        #[cfg(any(unix, windows))]
        {
            let (p, path_buf) = self.io_path_resolved(attributes);
            let link_name = args
                .first()
                .map(|v| v.to_string_value())
                .ok_or_else(|| RuntimeError::new("symlink requires a link name"))?;
            let absolute = Self::named_value(args, "absolute")
                .map(|v| v.truthy())
                .unwrap_or(true);
            let target_for_symlink = if absolute {
                path_buf.clone()
            } else {
                std::path::PathBuf::from(&p)
            };
            Ok(self.symlink_op(&path_buf, &target_for_symlink, &link_name))
        }
    }

    /// `IO::Path.link($name)`: create a hard link named `$name` pointing to
    /// the receiver (the target). Fails with `X::IO::Link`.
    // Cost: O(p + n) plus one link(2), p, n = path lengths.
    pub(crate) fn io_path_link(
        &self,
        attributes: &AttrMap,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let (_, path_buf) = self.io_path_resolved(attributes);
        let link_name = args.first().map(|v| v.to_string_value()).ok_or_else(|| {
            RuntimeError::new("Too few positionals passed; expected 2 arguments but got 1")
        })?;
        Ok(self.hard_link_op(&path_buf, &link_name))
    }
}
