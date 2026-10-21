//! The `MVM_COVERAGE_LOG` line-hit log (#11413).
//!
//! MoarVM, when `MVM_COVERAGE_LOG` names a file, appends a `HIT  <file>  <line>`
//! record for every statement line it runs; tools such as `Code::Coverage` run
//! `$*EXECUTABLE` with the variable set and read the file back. mutsu answers
//! the same protocol, but only when the variable is set at process start: the
//! compiler then emits one [`crate::opcode::OpCode::CoverageHit`] per statement
//! line, so a run without it pays nothing.
//!
//! The file is named by the user's own environment, so it is opened the way any
//! user-named output is (create, append); a symbolic link is refused rather than
//! followed. A `(file, line)` pair is written once: the readers squish the set.

use std::collections::HashSet;
use std::io::Write;
use std::sync::{Mutex, OnceLock};

struct CoverageLog {
    file: std::fs::File,
    written: HashSet<String>,
}

static LOG: OnceLock<Option<Mutex<CoverageLog>>> = OnceLock::new();

fn log() -> Option<&'static Mutex<CoverageLog>> {
    LOG.get_or_init(|| {
        let path = std::env::var_os("MVM_COVERAGE_LOG").filter(|p| !p.is_empty())?;
        let path = std::path::Path::new(&path);
        if std::fs::symlink_metadata(path).is_ok_and(|m| m.file_type().is_symlink()) {
            return None;
        }
        let file = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
            .ok()?;
        Some(Mutex::new(CoverageLog {
            file,
            written: HashSet::new(),
        }))
    })
    .as_ref()
}

/// Whether this process records line hits (the variable was set and its file
/// opened). Asked by the compiler, once per statement line it compiles.
// Cost: O(1) after the first call, which opens the log file.
pub(crate) fn enabled() -> bool {
    log().is_some()
}

/// Record the hit `record` (`HIT  <file>  <line>\n`) unless it is already in
/// the log.
// Cost: O(r), r = length of the record (a hash and, the first time, one write).
pub(crate) fn hit(record: &str) {
    let Some(log) = log() else {
        return;
    };
    let Ok(mut log) = log.lock() else {
        return;
    };
    if log.written.contains(record) {
        return;
    }
    // A write failure (disk full, closed pipe) must not fail the program under
    // measurement: the log is a side channel.
    let _ = log.file.write_all(record.as_bytes());
    log.written.insert(record.to_string());
}

/// The log record for line `line` of `file`.
// Cost: O(f), f = length of the file name.
pub(crate) fn record(file: &str, line: i64) -> String {
    format!("HIT  {file}  {line}\n")
}
