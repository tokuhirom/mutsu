//! The file-system operations that both a routine and an `IO::Path` method
//! expose (`spurt`, `copy`, `rename`/`move`, `rmdir`, `mkdir`, `chmod`): one
//! implementation each, taking paths already resolved against the cwd, so the
//! sub and the method cannot drift apart (#9878). Errors are reported in
//! Rakudo's wording through [`super::fs_errors`].

use super::fs_errors::{failure_of, libuv_text, path_exception, two_path_exception};
use super::*;

impl Interpreter {
    /// Write `content` (a `Str`, or a `Blob` written as-is) to the file at
    /// `path_buf` (already resolved against the cwd): truncating, appending
    /// with `append`, or refusing an existing file with `createonly` (an
    /// `O_EXCL` open, so the check and the create are one step). The one
    /// implementation behind the `spurt` sub and `IO::Path.spurt`. Returns
    /// `True`, or a `Failure` — Rakudo's open error
    /// (`fs_errors::open_failed_failure`) when the file cannot be opened.
    // Cost: O(c), c = the content length in bytes.
    pub(crate) fn spurt_file(
        &self,
        path_buf: &Path,
        content: &Value,
        append: bool,
        createonly: bool,
        enc: Option<&str>,
    ) -> Value {
        use std::io::Write;
        let bytes = if crate::runtime::Interpreter::is_buf_value(content) {
            crate::runtime::Interpreter::extract_buf_bytes(content)
        } else {
            let text = content.to_string_value();
            match enc {
                Some(enc_name) => match self.encode_with_encoding(&text, enc_name) {
                    Ok(mut encoded) => {
                        // utf16 (auto-endian) gets a BOM, as in Raku.
                        let enc_lower = enc_name.to_lowercase();
                        if enc_lower == "utf-16" || enc_lower == "utf16" {
                            let bom: &[u8] = if cfg!(target_endian = "little") {
                                &[0xFF, 0xFE]
                            } else {
                                &[0xFE, 0xFF]
                            };
                            let mut with_bom = Vec::with_capacity(bom.len() + encoded.len());
                            with_bom.extend_from_slice(bom);
                            with_bom.append(&mut encoded);
                            with_bom
                        } else {
                            encoded
                        }
                    }
                    Err(e) => {
                        return io_exception_failure("X::IO::Spurt", e.message.into_owned());
                    }
                },
                None => text.into_bytes(),
            }
        };
        let mut options = fs::OpenOptions::new();
        if append {
            options.append(true).create(true);
        } else if createonly {
            options.write(true).create_new(true);
        } else {
            options.write(true).create(true).truncate(true);
        }
        let mut file = match options.open(path_buf) {
            Ok(file) => file,
            Err(err) => return super::fs_errors::open_failed_failure(path_buf, &err),
        };
        match file.write_all(&bytes) {
            Ok(()) => Value::TRUE,
            Err(err) => io_exception_failure(
                "X::IO::Spurt",
                format!(
                    "Failed to write to file {}: {}",
                    path_buf.to_string_lossy(),
                    super::fs_errors::strerror_text(&err)
                ),
            ),
        }
    }

    /// `copy` / `IO::Path.copy`: copy the file at `from` to `to` (both already
    /// resolved against the cwd). The one implementation behind the sub and
    /// the method. Returns `True`, or a `Failure` carrying `X::IO::Copy` in
    /// Rakudo's wording (`fs_errors::two_path_exception`).
    // Cost: O(b), b = the source file's size in bytes.
    pub(crate) fn copy_file_op(&self, from: &Path, to: &Path, createonly: bool) -> Value {
        match Self::copy_file_reason(from, to, createonly) {
            Ok(()) => Value::TRUE,
            Err(reason) => failure_of(two_path_exception("X::IO::Copy", "copy", from, to, &reason)),
        }
    }

    /// The copy itself, answering the `os-error` text on failure (also the
    /// fallback step of `move`). Rakudo refuses a copy onto the source itself
    /// by comparing the absolute paths; this also compares the canonical
    /// paths, since copying a file onto itself through another spelling would
    /// truncate it.
    // Cost: O(b), b = the source file's size in bytes.
    fn copy_file_reason(from: &Path, to: &Path, createonly: bool) -> Result<(), String> {
        if createonly && to.exists() {
            return Err(":createonly specified and destination exists".to_string());
        }
        if from == to
            || (from.exists()
                && to.exists()
                && fs::canonicalize(from).ok() == fs::canonicalize(to).ok())
        {
            return Err("source and target are the same".to_string());
        }
        // libuv refuses a directory on either side with EISDIR; `fs::copy`
        // reports a directory source as a generic "not a regular file".
        if from.is_dir() || to.is_dir() {
            return Err("Failed to copy file: illegal operation on a directory".to_string());
        }
        fs::copy(from, to)
            .map(|_| ())
            .map_err(|err| format!("Failed to copy file: {}", libuv_text(&err)))
    }

    /// `rename` / `move` and their `IO::Path` methods: move the file at `from`
    /// to `to` (both already resolved against the cwd). `rename` is one
    /// `rename(2)`; `move` falls back to copy-then-unlink when that fails
    /// (another file system), so its failure reports the copy's reason, and it
    /// refuses a move onto the source itself, as Rakudo does. Returns `True`,
    /// or a `Failure` carrying `X::IO::Rename` / `X::IO::Move`.
    // Cost: O(1) for a same-filesystem rename; O(b) for `move`'s copy
    // fallback, b = the source file's size in bytes.
    pub(crate) fn rename_file_op(
        &self,
        verb: &str,
        from: &Path,
        to: &Path,
        createonly: bool,
    ) -> Value {
        let class_name = if verb == "move" {
            "X::IO::Move"
        } else {
            "X::IO::Rename"
        };
        let fail =
            |reason: &str| failure_of(two_path_exception(class_name, verb, from, to, reason));
        if createonly && to.exists() {
            return fail(":createonly specified and destination exists");
        }
        if verb == "move" && from == to {
            return fail("source and target are the same");
        }
        match fs::rename(from, to) {
            Ok(()) => Value::TRUE,
            Err(err) if verb != "move" => {
                fail(&format!("Failed to rename file: {}", libuv_text(&err)))
            }
            Err(_) => match Self::copy_file_reason(from, to, createonly) {
                Ok(()) => match fs::remove_file(from) {
                    Ok(()) => Value::TRUE,
                    Err(err) => fail(&format!("Failed to delete file: {}", libuv_text(&err))),
                },
                Err(reason) => fail(&reason),
            },
        }
    }

    /// `IO::Path.rmdir`: remove the empty directory at `path` (already
    /// resolved against the cwd). Returns `True`, or a `Failure` carrying
    /// `X::IO::Rmdir`.
    // Cost: O(1) plus one `rmdir(2)`.
    pub(crate) fn rmdir_op(&self, path: &Path) -> Value {
        match fs::remove_dir(path) {
            Ok(()) => Value::TRUE,
            Err(err) => {
                let os_error = format!("Failed to rmdir: {}", libuv_text(&err));
                let message = format!(
                    "Failed to remove the directory '{}': {}",
                    path.to_string_lossy(),
                    os_error
                );
                failure_of(path_exception(
                    "X::IO::Rmdir",
                    message,
                    path,
                    &os_error,
                    &[],
                ))
            }
        }
    }

    /// `mkdir` / `IO::Path.mkdir`: create the directory at `path` (already
    /// resolved against the cwd) and any missing parents. On failure, the
    /// `X::IO::Mkdir` exception, for the caller to throw or wrap.
    // Cost: O(d), d = the number of missing path components.
    pub(crate) fn mkdir_op(&self, path: &Path) -> Result<(), Value> {
        fs::create_dir_all(path).map_err(|err| {
            let os_error = format!("Failed to mkdir: {}", libuv_text(&err));
            let message = format!(
                "Failed to create directory '{}' with mode '0o777': {}",
                path.to_string_lossy(),
                os_error
            );
            path_exception(
                "X::IO::Mkdir",
                message,
                path,
                &os_error,
                &[("mode", Value::int(0o777))],
            )
        })
    }

    /// `IO::Path.chmod`: set the permission bits of `path` (already resolved
    /// against the cwd) to `mode`. Returns `True`, or a `Failure` carrying
    /// `X::IO::Chmod`.
    // Cost: O(1) plus one `chmod(2)`.
    #[cfg(unix)]
    pub(crate) fn chmod_op(&self, path: &Path, mode: u32) -> Value {
        match fs::set_permissions(path, PermissionsExt::from_mode(mode)) {
            Ok(()) => Value::TRUE,
            Err(err) => {
                let os_error = format!("Failed to set permissions on path: {}", libuv_text(&err));
                let message = format!(
                    "Failed to set the mode of '{}' to '0o{:o}': {}",
                    path.to_string_lossy(),
                    mode,
                    os_error
                );
                failure_of(path_exception(
                    "X::IO::Chmod",
                    message,
                    path,
                    &os_error,
                    &[("mode", Value::int(i64::from(mode)))],
                ))
            }
        }
    }
}
