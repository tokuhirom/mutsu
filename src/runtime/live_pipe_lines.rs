//! Line-at-a-time reads of a live `run(..., :out)` pipe (`$proc.out.get`)
//! while the child still runs.
//!
//! A child that answers a line and then waits on its stdin (Red's lock-holder
//! test helper: `say "locked"; $*IN.get`) must hand that line to the parent
//! before it exits; reading the pipe to its end first deadlocks both. The
//! bytes read past the line stay buffered here and are prepended when the
//! pipe is later slurped (`finalize`).

use super::builtins_system::live_proc_map;
use std::collections::HashMap;
use std::io::Read;
use std::sync::{Mutex, OnceLock};

/// Read-ahead bytes per live pipe, keyed by `(pid, is_stderr)`.
type PendingMap = Mutex<HashMap<(i64, bool), Vec<u8>>>;

/// Bytes read from a live pipe beyond the lines `get` handed out, keyed by
/// `(pid, is_stderr)`.
fn pending_map() -> &'static PendingMap {
    static MAP: OnceLock<PendingMap> = OnceLock::new();
    MAP.get_or_init(|| Mutex::new(HashMap::new()))
}

/// The bytes of the live pipe `(pid, is_stderr)` that a `get` read ahead and
/// did not hand out; they precede whatever the pipe still holds.
// Cost: O(1).
pub(crate) fn take_pending(pid: i64, is_stderr: bool) -> Vec<u8> {
    pending_map()
        .lock()
        .ok()
        .and_then(|mut map| map.remove(&(pid, is_stderr)))
        .unwrap_or_default()
}

/// Split the first line off `buf`: the text before its first `\n` (a text
/// pipe also drops the `\r` of a CR-LF), or all of `buf` at end of input.
fn split_line(buf: &mut Vec<u8>, at_eof: bool, bin: bool) -> Option<String> {
    let end = match buf.iter().position(|b| *b == b'\n') {
        Some(newline) => newline,
        None if at_eof && !buf.is_empty() => buf.len(),
        None => return None,
    };
    let rest = buf.split_off((end + 1).min(buf.len()));
    let mut line = std::mem::replace(buf, rest);
    if line.last() == Some(&b'\n') {
        line.pop();
    }
    if !bin && line.last() == Some(&b'\r') {
        line.pop();
    }
    Some(String::from_utf8_lossy(&line).into_owned())
}

/// The next line of the live pipe `(pid, is_stderr)`: `None` when the
/// process is no longer live (the caller reads its finalized output), or
/// `Some(None)` at the end of the pipe.
// Cost: O(l), l = bytes read up to the line's end (one 4 KiB read at a time),
// plus the wait for the child to write them.
pub(crate) fn read_line(pid: i64, is_stderr: bool) -> Option<Option<String>> {
    let mut pending = take_pending(pid, is_stderr);
    // The pipe end leaves the shared state while it is read, so no lock is
    // held across the blocking read.
    let (mut out, mut err, bin) = {
        let mut map = live_proc_map().lock().ok()?;
        let state = map.get_mut(&pid)?;
        let captured = if is_stderr {
            state.capture_err
        } else {
            state.capture_out
        };
        if !captured {
            return None;
        }
        let bin = state.bin;
        if is_stderr {
            (None, state.child.stderr.take(), bin)
        } else {
            (state.child.stdout.take(), None, bin)
        }
    };
    let mut at_eof = false;
    let line = loop {
        if let Some(line) = split_line(&mut pending, at_eof, bin) {
            break Some(line);
        }
        if at_eof {
            break None;
        }
        let mut chunk = [0u8; 4096];
        let read = match (out.as_mut(), err.as_mut()) {
            (Some(stdout), _) => crate::runtime::exit_aware_read::ExitAware(stdout).read(&mut chunk),
            (_, Some(stderr)) => crate::runtime::exit_aware_read::ExitAware(stderr).read(&mut chunk),
            _ => Ok(0),
        };
        match read {
            Ok(0) | Err(_) => at_eof = true,
            Ok(n) => pending.extend_from_slice(&chunk[..n]),
        }
    };
    if let Ok(mut map) = live_proc_map().lock()
        && let Some(state) = map.get_mut(&pid)
    {
        if out.is_some() {
            state.child.stdout = out.take();
        }
        if err.is_some() {
            state.child.stderr = err.take();
        }
    }
    if !pending.is_empty()
        && let Ok(mut map) = pending_map().lock()
    {
        map.insert((pid, is_stderr), pending);
    }
    Some(line)
}
