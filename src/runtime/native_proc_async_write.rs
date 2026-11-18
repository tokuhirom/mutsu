//! What `Proc::Async.write` / `.print` / `.say` / `.put` answer.
//!
//! The returned Promise is kept once the bytes are in the child's stdin pipe
//! and broken when they could not be written, as Rakudo does: a child that
//! closed its stdin early (`head -1`, `grep -q`) breaks it with `broken pipe`,
//! and a child that has already been reaped with `Cannot write to process
//! that has already terminated`. A producer feeding a child chunk by chunk
//! (Rakuish's `in-chunks`) learns from that broken Promise that it can stop;
//! a Promise kept regardless had it generate its whole stream into nothing.
use super::native_methods::proc_stdin_map;
use crate::value::{SharedPromise, Value};

/// Write `bytes` to the stdin of the child `pid` and return the Promise the
/// write method answers.
// Cost: O(n), n = bytes.len(); one registry lookup, O(1).
pub(super) fn write_child_stdin(pid: u32, bytes: &[u8]) -> Value {
    let promise = SharedPromise::new();
    let stdin_arc = proc_stdin_map()
        .lock()
        .ok()
        .and_then(|map| map.get(&pid).cloned());
    let outcome = match stdin_arc {
        // The `proc-wait` thread drops the entry once the child is reaped.
        None => Err("Cannot write to process that has already terminated".to_string()),
        Some(stdin_arc) => match stdin_arc.lock() {
            Ok(mut guard) => match *guard {
                Some(ref mut stdin) => {
                    use std::io::Write;
                    stdin
                        .write_all(bytes)
                        .and_then(|()| stdin.flush())
                        .map_err(|e| write_error_message(&e))
                }
                // `close-stdin` already ran; the bytes have nowhere to go.
                // TODO: Rakudo breaks this Promise too; its message has not
                // been measured, so the write is still reported as kept.
                None => Ok(()),
            },
            Err(_) => Ok(()),
        },
    };
    match outcome {
        Ok(()) => promise.keep(Value::TRUE, String::new(), String::new()),
        Err(message) => promise.break_with(Value::str(message), String::new(), String::new()),
    }
    Value::promise(promise)
}

/// The message Rakudo (libuv) breaks a failed stdin write with: the lowercase
/// `uv_strerror` text, `broken pipe` for a reader that went away.
fn write_error_message(e: &std::io::Error) -> String {
    if e.kind() == std::io::ErrorKind::BrokenPipe {
        "broken pipe".to_string()
    } else {
        e.to_string()
    }
}
