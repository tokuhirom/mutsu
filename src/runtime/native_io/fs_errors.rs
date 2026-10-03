//! Rakudo-compatible file-system error reporting (#9878).
//!
//! Rakudo's file errors come from two layers, and each spells them its own way:
//!
//! - **Opening a file** (`open`, `slurp`, `spurt`, `IO::Path.lines`/`.words`,
//!   `EVALFILE`, `Grammar.parsefile`) fails inside MoarVM's `open_fh`, which
//!   dies with an `X::AdHoc` reading `Failed to open file <absolute path>:
//!   <strerror text>` — the C library's capitalised wording, without Rust's
//!   ` (os error N)` suffix. Slurping a directory instead says `Tried to open
//!   directory <absolute path>`, while `IO::Handle.open` checks first and fails
//!   with an `X::IO::Directory` that names the path as written.
//! - **Every other syscall** (copy, rename, rmdir, chmod, ...) goes through
//!   libuv, so the operation's `X::IO::*` exception carries an `os-error` of the
//!   form `Failed to <op>: <uv_strerror text>` — lower-case, in libuv's own
//!   wording (`EISDIR` is "illegal operation on a directory", `EEXIST` is "file
//!   already exists").
//!
//! Every message names the path as `IO::Path.absolute` gives it: joined to the
//! cwd but *not* normalised (`../x` stays `/cwd/../x`), which is exactly the
//! `PathBuf` that `resolve_path` / `resolve_io_path_buf` return. Callers pass
//! that resolved path, never the string the user wrote.
//!
//! This module is the one place those spellings live; each file routine only
//! decides which of them applies.

use super::*;
use std::io::ErrorKind;

/// The C library's description of an I/O error (`strerror`), without Rust's
/// ` (os error N)` suffix: "No such file or directory", "Is a directory".
/// Cost: O(m), m = the message length.
pub(crate) fn strerror_text(err: &std::io::Error) -> String {
    let full = err.to_string();
    match full.find(" (os error ") {
        Some(end) => full[..end].to_string(),
        None => full,
    }
}

/// An I/O error as libuv's `uv_strerror` spells it — the reason text MoarVM
/// puts in every libuv-backed `X::IO::*` exception. The common file-system
/// errors use libuv's own wording, which differs from the C library's for
/// several of them; anything else falls back to the C text, lower-cased.
/// Cost: O(m), m = the message length.
pub(crate) fn libuv_text(err: &std::io::Error) -> String {
    let known = match err.kind() {
        ErrorKind::NotFound => Some("no such file or directory"),
        ErrorKind::PermissionDenied => Some("permission denied"),
        ErrorKind::AlreadyExists => Some("file already exists"),
        ErrorKind::IsADirectory => Some("illegal operation on a directory"),
        ErrorKind::NotADirectory => Some("not a directory"),
        ErrorKind::DirectoryNotEmpty => Some("directory not empty"),
        ErrorKind::ReadOnlyFilesystem => Some("read-only file system"),
        ErrorKind::CrossesDevices => Some("cross-device link not permitted"),
        ErrorKind::InvalidFilename => Some("name too long"),
        ErrorKind::StorageFull => Some("no space left on device"),
        ErrorKind::ResourceBusy => Some("resource busy or locked"),
        ErrorKind::ExecutableFileBusy => Some("text file is busy"),
        ErrorKind::FileTooLarge => Some("file too large"),
        ErrorKind::TooManyLinks => Some("too many links"),
        _ => None,
    };
    if let Some(text) = known {
        return text.to_string();
    }
    let mut text = strerror_text(err);
    // OS error texts are ASCII, so lower-casing the first byte is exact.
    if let Some(first) = text.get_mut(0..1) {
        first.make_ascii_lowercase();
    }
    text
}

/// `Failed to open file <path>: <strerror>` — MoarVM's `open_fh` message.
/// Cost: O(p + m), p = path length, m = the OS message length.
pub(crate) fn open_failed_message(path: &Path, err: &std::io::Error) -> String {
    format!(
        "Failed to open file {}: {}",
        path.to_string_lossy(),
        strerror_text(err)
    )
}

/// Opening `path` failed: the `X::AdHoc` Rakudo throws.
/// Cost: O(p + m), as [`open_failed_message`].
pub(crate) fn open_failed(path: &Path, err: &std::io::Error) -> RuntimeError {
    RuntimeError::new(open_failed_message(path, err))
}

/// Opening `path` failed, for the routines that return a `Failure` instead of
/// throwing (`spurt`).
/// Cost: O(p + m), as [`open_failed_message`].
pub(crate) fn open_failed_failure(path: &Path, err: &std::io::Error) -> Value {
    RuntimeError::adhoc_failure(&open_failed_message(path, err))
}

/// Reading the whole of `path` (`slurp`, `EVALFILE`, `Grammar.parsefile`)
/// failed. A directory gets MoarVM's `Tried to open directory <path>` —
/// opening one read-only succeeds on POSIX and only the read fails, so the
/// check is on the path, not on the error kind alone.
/// Cost: O(p + m) plus one `stat`, p = path length, m = the OS message length.
pub(crate) fn read_whole_failed(path: &Path, err: &std::io::Error) -> RuntimeError {
    if err.kind() == ErrorKind::IsADirectory || path.is_dir() {
        RuntimeError::new(format!(
            "Tried to open directory {}",
            path.to_string_lossy()
        ))
    } else {
        open_failed(path, err)
    }
}

/// The `X::IO::Directory` that `IO::Handle.open` fails with for a directory,
/// naming the path as the user wrote it (Rakudo checks `.d` before opening).
/// Cost: O(p), p = path length.
pub(crate) fn directory_open_error(path: &str) -> RuntimeError {
    let message = format!(
        "'{}' is a directory, cannot do '.open' on a directory",
        path
    );
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    attrs.insert("path".to_string(), Value::str(path.to_string()));
    attrs.insert("trying".to_string(), Value::str("open".to_string()));
    let mut err = RuntimeError::new(message);
    err.exception = Some(Box::new(Value::make_instance(
        Symbol::intern("X::IO::Directory"),
        attrs,
    )));
    err
}

/// The `X::IO::Copy` / `X::IO::Rename` / `X::IO::Move` exception: `Failed to
/// <verb> '<from>' to '<to>': <os-error>`, with the `from`, `to` and
/// `os-error` attributes Rakudo's class declares.
/// Cost: O(f + t + e), the lengths of the two paths and the reason.
pub(crate) fn two_path_exception(
    class_name: &str,
    verb: &str,
    from: &Path,
    to: &Path,
    os_error: &str,
) -> Value {
    let from = from.to_string_lossy();
    let to = to.to_string_lossy();
    let message = format!("Failed to {} '{}' to '{}': {}", verb, from, to, os_error);
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(message));
    attrs.insert("from".to_string(), Value::str(from.into_owned()));
    attrs.insert("to".to_string(), Value::str(to.into_owned()));
    attrs.insert("os-error".to_string(), Value::str(os_error.to_string()));
    Value::make_instance(Symbol::intern(class_name), attrs)
}

/// A one-path `X::IO::*` exception (`X::IO::Rmdir`, `X::IO::Chmod`, ...):
/// `message` is the full text, `path` the absolute path and `os-error` the
/// libuv reason; `extra` adds class-specific attributes (`mode`).
/// Cost: O(p + e + x), the path and reason lengths and the extra attributes.
pub(crate) fn path_exception(
    class_name: &str,
    message: String,
    path: &Path,
    os_error: &str,
    extra: &[(&str, Value)],
) -> Value {
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(message));
    attrs.insert(
        "path".to_string(),
        Value::str(path.to_string_lossy().into_owned()),
    );
    attrs.insert("os-error".to_string(), Value::str(os_error.to_string()));
    for (name, value) in extra {
        attrs.insert((*name).to_string(), value.clone());
    }
    Value::make_instance(Symbol::intern(class_name), attrs)
}

/// Wrap an exception instance in an unhandled `Failure`.
/// Cost: O(1).
pub(crate) fn failure_of(exception: Value) -> Value {
    let mut failure_attrs = HashMap::new();
    failure_attrs.insert("exception".to_string(), exception);
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}

/// The `Failure` an `open` routine returns for a failed open: the error's own
/// typed exception (`X::IO::Directory`, ...) when it carries one, otherwise an
/// `X::AdHoc` with its message — every attribute is kept, not only the class.
/// Cost: O(m), m = the message length.
pub(crate) fn open_error_failure(err: RuntimeError) -> Value {
    match err.exception {
        Some(exception) => failure_of(*exception),
        None => RuntimeError::adhoc_failure(&err.message),
    }
}

/// Throw an exception instance: a `RuntimeError` carrying it and its message.
/// Cost: O(m), m = the message length.
pub(crate) fn error_of(exception: Value) -> RuntimeError {
    let message = match exception.view() {
        ValueView::Instance { attributes, .. } => attributes
            .as_map()
            .get("message")
            .map(Value::to_string_value)
            .unwrap_or_default(),
        _ => String::new(),
    };
    let mut err = RuntimeError::new(message);
    err.exception = Some(Box::new(exception));
    err
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strerror_drops_the_rust_suffix() {
        let err = std::io::Error::from(ErrorKind::NotFound);
        assert!(!strerror_text(&err).contains("os error"));
        #[cfg(unix)]
        {
            let err = std::io::Error::from_raw_os_error(2);
            assert_eq!(strerror_text(&err), "No such file or directory");
        }
    }

    #[cfg(unix)]
    #[test]
    fn libuv_wording_differs_from_the_c_library() {
        let eisdir = std::io::Error::from_raw_os_error(21);
        assert_eq!(libuv_text(&eisdir), "illegal operation on a directory");
        let eexist = std::io::Error::from_raw_os_error(17);
        assert_eq!(libuv_text(&eexist), "file already exists");
        let einval = std::io::Error::from_raw_os_error(22);
        assert_eq!(libuv_text(&einval), "invalid argument");
    }
}
