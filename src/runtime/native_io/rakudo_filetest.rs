//! `Rakudo::Internals.FILETEST-*`: the eighteen stat-based file tests rakudo's
//! `IO::Path` methods are built on (`FILETEST-E($abspath)` is what `.e` calls).
//!
//! They share their filesystem queries with the `IO::Path` readers
//! ([`io_file_test`], `nqp_stat::stat_time`), so
//! the method and the internal helper cannot disagree. They differ from the
//! methods in their answers: an `Int` 1/0 for a test, and a thrown error, not a
//! `Failure`, when a test that needs a `stat` meets a missing path.

use super::helpers::io_file_test;
use super::*;

/// The answer of `Rakudo::Internals.FILETEST-<name>($path)`, `name` being the
/// suffix after `FILETEST-` (`E`, `D`, `ES`, `MODIFIED`, ...).
///
/// `E`, `R`, `W`, `X`, `RW`, `RWX`, `ES` and `LE` answer 0 for a missing path;
/// every other test throws `Failed to stat file: no such file or directory`.
// Cost: O(p) plus at most three filesystem queries, p = path length.
pub(crate) fn rakudo_filetest(name: &str, path: &Path) -> Result<Value, RuntimeError> {
    let flag = |answer: bool| Ok(Value::int(i64::from(answer)));
    let missing = || {
        RuntimeError::new("Failed to stat file: no such file or directory".to_string())
    };
    match name {
        "E" | "R" | "W" | "X" | "RW" | "RWX" => {
            flag(io_file_test(path, &name.to_ascii_lowercase()).unwrap_or(false))
        }
        "LE" => flag(fs::symlink_metadata(path).is_ok()),
        "D" | "F" | "L" | "Z" => io_file_test(path, &name.to_ascii_lowercase())
            .ok_or_else(missing)
            .and_then(flag),
        "S" => fs::metadata(path)
            .map(|m| Value::int(m.len() as i64))
            .map_err(|_| missing()),
        "ES" => Ok(Value::int(fs::metadata(path).map_or(0, |m| m.len() as i64))),
        "MODE" => fs::metadata(path)
            .map(|m| Value::int(file_mode(&m)))
            .map_err(|_| missing()),
        "CREATED" | "ACCESSED" | "MODIFIED" | "CHANGED" => {
            let code = match name {
                "CREATED" => 5,
                "ACCESSED" => 6,
                "MODIFIED" => 7,
                _ => 8,
            };
            crate::runtime::nqp_stat::stat_time(path, code, false)
                .map(Value::num)
                .map_err(|_| missing())
        }
        _ => Err(RuntimeError::new(format!(
            "No such method 'FILETEST-{name}' for invocant of type 'Rakudo::Internals'"
        ))),
    }
}

/// The permission bits of `meta` (`stat`'s `st_mode & 0o7777`).
#[cfg(unix)]
fn file_mode(meta: &fs::Metadata) -> i64 {
    use std::os::unix::fs::PermissionsExt;
    i64::from(meta.permissions().mode() & 0o7777)
}

#[cfg(not(unix))]
fn file_mode(meta: &fs::Metadata) -> i64 {
    if meta.permissions().readonly() { 0o444 } else { 0o666 }
}
