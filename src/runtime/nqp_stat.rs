//! `nqp::stat` / `nqp::lstat`: one field of a path's filesystem status,
//! selected by MoarVM's `STAT_*` constant (`compiler::nqp_forms`).
//!
//! MoarVM's numbering: the portable fields count up from 0, the platform
//! (`struct stat`) fields count down from -1. `File::Stat` reads every one of
//! them through `nqp::const::STAT_*`.

use std::fs;
use std::path::Path;

use crate::value::RuntimeError;

/// The value of the `STAT_*` field `code` for `path`. `lstat` reports a
/// symlink itself rather than its target (`STAT_ISLNK` always does).
/// `STAT_EXISTS` never fails; every other field of a path that cannot be
/// stat'ed is an error, as in MoarVM. A field this platform cannot answer
/// (and `STAT_BACKUPTIME`, which no platform MoarVM supports has) is -1.
// Cost: O(p) + one syscall, p = length of `path`.
pub(crate) fn nqp_stat_field(
    path: &Path,
    shown: &str,
    code: i64,
    lstat: bool,
) -> Result<i64, RuntimeError> {
    if code == 0 {
        // STAT_EXISTS
        let exists = if lstat {
            fs::symlink_metadata(path).is_ok()
        } else {
            path.exists()
        };
        return Ok(i64::from(exists));
    }
    let meta = if lstat || code == 12 {
        fs::symlink_metadata(path)
    } else {
        fs::metadata(path)
    };
    let meta = meta.map_err(|_| RuntimeError::new(format!("Failed to stat file: {shown}")))?;
    let secs = |t: std::io::Result<std::time::SystemTime>| {
        t.ok()
            .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
            .map_or(-1, |d| d.as_secs() as i64)
    };
    Ok(match code {
        1 => meta.len() as i64,                         // STAT_FILESIZE
        2 => i64::from(meta.is_dir()),                  // STAT_ISDIR
        3 => i64::from(meta.is_file()),                 // STAT_ISREG
        4 => i64::from(is_device(&meta)),               // STAT_ISDEV
        5 => secs(meta.created()),                      // STAT_CREATETIME
        6 => secs(meta.accessed()),                     // STAT_ACCESSTIME
        7 => secs(meta.modified()),                     // STAT_MODIFYTIME
        12 => i64::from(meta.file_type().is_symlink()), // STAT_ISLNK
        _ => platform_field(&meta, code),
    })
}

/// The time `STAT_*` field `code` (5 created, 6 accessed, 7 modified,
/// 8 changed) of `path`, in fractional POSIX seconds: `nqp::stat_time` /
/// `nqp::lstat_time`, and the `IO::Path.created`/`.accessed`/`.modified`/
/// `.changed` readers Rakudo builds on them. A field the platform cannot
/// answer (and any other code) is -1.
// Cost: O(p) + one syscall, p = length of `path`.
pub(crate) fn stat_time(path: &Path, code: i64, lstat: bool) -> std::io::Result<f64> {
    let meta = if lstat {
        fs::symlink_metadata(path)?
    } else {
        fs::metadata(path)?
    };
    let secs = |t: std::io::Result<std::time::SystemTime>| {
        t.ok()
            .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
            .map_or(-1.0, |d| d.as_secs_f64())
    };
    Ok(match code {
        5 => secs(meta.created()),
        6 => secs(meta.accessed()),
        7 => secs(meta.modified()),
        8 => change_time(&meta),
        _ => -1.0,
    })
}

#[cfg(unix)]
fn change_time(meta: &fs::Metadata) -> f64 {
    use std::os::unix::fs::MetadataExt;
    meta.ctime() as f64 + meta.ctime_nsec() as f64 / 1e9
}

/// No inode change time off Unix: the modification time stands in for it.
#[cfg(not(unix))]
fn change_time(meta: &fs::Metadata) -> f64 {
    meta.modified()
        .ok()
        .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
        .map_or(-1.0, |d| d.as_secs_f64())
}

#[cfg(unix)]
fn is_device(meta: &fs::Metadata) -> bool {
    use std::os::unix::fs::FileTypeExt;
    let ft = meta.file_type();
    ft.is_char_device() || ft.is_block_device()
}

#[cfg(not(unix))]
fn is_device(_meta: &fs::Metadata) -> bool {
    false
}

/// `STAT_CHANGETIME`, `STAT_UID`, `STAT_GID` and the `STAT_PLATFORM_*`
/// fields, all read straight out of `struct stat`.
#[cfg(unix)]
fn platform_field(meta: &fs::Metadata, code: i64) -> i64 {
    use std::os::unix::fs::MetadataExt;
    match code {
        8 => meta.ctime(),            // STAT_CHANGETIME
        10 => i64::from(meta.uid()),  // STAT_UID
        11 => i64::from(meta.gid()),  // STAT_GID
        -1 => meta.dev() as i64,      // STAT_PLATFORM_DEV
        -2 => meta.ino() as i64,      // STAT_PLATFORM_INODE
        -3 => i64::from(meta.mode()), // STAT_PLATFORM_MODE
        -4 => meta.nlink() as i64,    // STAT_PLATFORM_NLINKS
        -5 => meta.rdev() as i64,     // STAT_PLATFORM_DEVTYPE
        -6 => meta.blksize() as i64,  // STAT_PLATFORM_BLOCKSIZE
        -7 => meta.blocks() as i64,   // STAT_PLATFORM_BLOCKS
        _ => -1,
    }
}

#[cfg(not(unix))]
fn platform_field(meta: &fs::Metadata, code: i64) -> i64 {
    match code {
        8 => meta
            .modified()
            .ok()
            .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
            .map_or(-1, |d| d.as_secs() as i64),
        _ => -1,
    }
}
