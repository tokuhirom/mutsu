//! The bare file-system syscalls, each reporting failure as MoarVM does.
//!
//! MoarVM runs these through libuv and dies with `Failed to <op>: <libuv
//! text>` (`fs_errors::libuv_text`). That string is both what the `nqp::` op
//! raises (`nqp::rmdir`, `nqp::chmod`, ...) and the `os-error` Rakudo's
//! `X::IO::*` exception wraps for the Raku routine and `IO::Path` method built
//! on the same op. So each syscall lives here once, answering that text, and
//! the `nqp::` op and the Raku-level operation (`fs_ops`) both call it.
//!
//! Every path is already resolved against the cwd by the caller.

use super::fs_errors::libuv_text;
use std::fs;
use std::path::Path;

/// `unlink(2)`: remove a file. A path that does not exist is not an error,
/// as in MoarVM; a directory is ("illegal operation on a directory").
// Cost: O(1) plus one `unlink(2)`.
pub(crate) fn unlink_file(path: &Path) -> Result<(), String> {
    match fs::remove_file(path) {
        Ok(()) => Ok(()),
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(err) => Err(format!("Failed to delete file: {}", libuv_text(&err))),
    }
}

/// `rmdir(2)`: remove an empty directory.
// Cost: O(1) plus one `rmdir(2)`.
pub(crate) fn remove_dir(path: &Path) -> Result<(), String> {
    fs::remove_dir(path).map_err(|err| format!("Failed to rmdir: {}", libuv_text(&err)))
}

/// Create the directory at `path` with permission bits `mode` (before the
/// umask), along with any missing parents, as MoarVM's `mkdir` does. An
/// existing directory is not an error.
// Cost: O(d), d = the number of missing path components.
pub(crate) fn make_dir_all(path: &Path, mode: u32) -> Result<(), String> {
    let mut builder = fs::DirBuilder::new();
    builder.recursive(true);
    #[cfg(unix)]
    {
        use std::os::unix::fs::DirBuilderExt;
        builder.mode(mode);
    }
    #[cfg(not(unix))]
    let _ = mode;
    builder
        .create(path)
        .map_err(|err| format!("Failed to mkdir: {}", libuv_text(&err)))
}

/// `chmod(2)`: set the permission bits of `path`.
// Cost: O(1) plus one `chmod(2)`.
#[cfg(unix)]
pub(crate) fn set_mode(path: &Path, mode: u32) -> Result<(), String> {
    use std::os::unix::fs::PermissionsExt;
    fs::set_permissions(path, fs::Permissions::from_mode(mode))
        .map_err(|err| format!("Failed to set permissions on path: {}", libuv_text(&err)))
}

/// `chown(2)`: set the owner and group of `path`; a negative id leaves that
/// one unchanged.
// Cost: O(1) plus one `chown(2)`.
#[cfg(unix)]
pub(crate) fn set_owner(path: &Path, uid: i64, gid: i64) -> Result<(), String> {
    let id = |n: i64| u32::try_from(n).ok();
    std::os::unix::fs::chown(path, id(uid), id(gid))
        .map_err(|err| format!("Failed to set owner/group on path: {}", libuv_text(&err)))
}

/// `rename(2)`: move `from` to `to` within one file system.
// Cost: O(1) plus one `rename(2)`.
pub(crate) fn rename_path(from: &Path, to: &Path) -> Result<(), String> {
    fs::rename(from, to).map_err(|err| format!("Failed to rename file: {}", libuv_text(&err)))
}

/// `link(2)`: make `link` a new hard link to `target`.
// Cost: O(1) plus one `link(2)`.
pub(crate) fn hard_link(target: &Path, link: &Path) -> Result<(), String> {
    fs::hard_link(target, link).map_err(|err| format!("Failed to link file: {}", libuv_text(&err)))
}

/// `symlink(2)`: make `link` a symbolic link whose contents are `target`.
// Cost: O(1) plus one `symlink(2)`.
#[cfg(unix)]
pub(crate) fn symlink(target: &Path, link: &Path) -> Result<(), String> {
    std::os::unix::fs::symlink(target, link)
        .map_err(|err| format!("Failed to symlink file: {}", libuv_text(&err)))
}

/// `chdir(2)`: change the PROCESS working directory (not `$*CWD`).
// Cost: O(1) plus one `chdir(2)`.
pub(crate) fn change_dir(path: &Path) -> Result<(), String> {
    std::env::set_current_dir(path).map_err(|err| format!("chdir failed: {}", libuv_text(&err)))
}
