//! The file-handle and filesystem `nqp::` ops (#11501): `print`/`say`, the
//! `*fh` handle ops, and the path ops (`mkdir`, `unlink`, `chmod`, ...).
//!
//! Each op is the primitive under a Raku-level operation mutsu already has,
//! and calls the same routine: the handle ops go through the shared handle
//! table (`with_handle_mut`, the `IO::Handle` method helpers), and the path
//! ops through `native_io::fs_syscalls`, which `IO::Path`'s methods use too.
//! An op that fails dies with MoarVM's message (`Failed to rmdir: <reason>`),
//! where the Raku level wraps the same text in an `X::IO::*` Failure.
//!
//! As in MoarVM, a path op answers its path operand, except `link` and
//! `symlink`, and the handle ops `flushfh`/`seekfh`, which answer null.
//! Relative paths resolve against the interpreter's cwd, as `nqp::open` and
//! `nqp::stat` do.

use crate::runtime::native_io::fs_syscalls;
use crate::runtime::{Interpreter, IoHandleTarget, RuntimeError};
use crate::value::Value;

fn sarg(args: &[Value], i: usize) -> String {
    args.get(i).map(Value::to_string_value).unwrap_or_default()
}

fn iarg(args: &[Value], i: usize) -> i64 {
    args.get(i).map(crate::runtime::to_int).unwrap_or(0)
}

fn operand(args: &[Value], i: usize) -> Value {
    args.get(i).cloned().unwrap_or(Value::NIL)
}

/// How many operands each op of this table and of `nqp_ops_sys` takes.
/// MoarVM's ops have a fixed operand count, and Rakudo rejects a call with
/// another count at compile time; a missing operand must never be read as a
/// default, since an empty path resolves to the cwd (`nqp::chmod()` would
/// chmod it to 0).
// Cost: O(1).
pub(super) fn operand_count(op: &str) -> Option<usize> {
    Some(match op {
        "getpid" | "getppid" | "execname" | "cpucores" | "freemem" | "totalmem" | "uname"
        | "getsignals" | "getenvhash" | "backendconfig" | "cwd" => 0,
        "exit" | "sleep" | "decodelocaltime" | "print" | "say" | "flushfh" | "tellfh" | "eoffh"
        | "filenofh" | "getport" | "chdir" | "rmdir" | "unlink" | "fileexecutable"
        | "filewritable" => 1,
        "writefh" | "mkdir" | "rename" | "copy" | "link" | "symlink" | "chmod" | "stat_time"
        | "lstat_time" => 2,
        "seekfh" | "chown" => 3,
        _ => return None,
    })
}

/// Rakudo's compile-time error for an op called with the wrong operand
/// count; `None` when the count is right (or `op` is not one of these).
// Cost: O(1).
pub(super) fn operand_count_error(op: &str, args: &[Value]) -> Option<RuntimeError> {
    let want = operand_count(op)?;
    (args.len() != want).then(|| {
        RuntimeError::new(format!(
            "Arg count {} doesn't equal required operand count {want} for op '{op}'",
            args.len()
        ))
    })
}

/// A path op's result: its path operand on success, else MoarVM's error.
fn path_result(args: &[Value], r: Result<(), String>) -> Result<Value, RuntimeError> {
    r.map(|()| operand(args, 0)).map_err(RuntimeError::new)
}

impl Interpreter {
    /// Try a file-handle / filesystem `nqp::` op. An op this table does not
    /// know goes on to the stream-decoding table.
    pub(crate) fn call_nqp_op_fs(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if let Some(err) = operand_count_error(op, args) {
            return Some(Err(err));
        }
        Some(match op {
            // -- standard output --
            // nqp::print($s) / nqp::say($s): write to the PROCESS stdout
            // (not `$*OUT`), `say` adding a newline; both answer `$s`.
            // Cost: O(n), n = chars of $s.
            "print" | "say" => {
                let mut text = sarg(args, 0);
                if op == "say" {
                    text.push('\n');
                }
                let out = self.std_handle(IoHandleTarget::Stdout);
                self.write_bytes_to_handle_value(&out, text.as_bytes())
                    .map(|()| operand(args, 0))
            }

            // -- handle ops --
            // nqp::writefh($fh, $buf): write the bytes of a Buf/Blob; answers
            // the buffer.
            // Cost: O(b), b = bytes written.
            "writefh" => {
                let buf = operand(args, 1);
                if !Self::is_buf_value(&buf) {
                    return Some(Err(RuntimeError::new(
                        "write_fhb requires a native array to read from",
                    )));
                }
                let bytes = Self::extract_buf_bytes(&buf);
                self.write_bytes_to_handle_value(&operand(args, 0), &bytes)
                    .map(|()| buf)
            }
            // nqp::flushfh($fh): push buffered output to the OS; null.
            // Cost: O(b), b = bytes pending in the handle's buffer.
            "flushfh" => self
                .with_handle_mut(&operand(args, 0), |state| state.flush_for_method())
                .map(|()| Value::NIL),
            // nqp::seekfh($fh, $offset, $whence): whence 0 from the start,
            // 1 from the current position, 2 from the end; null.
            // Cost: O(1) plus one `lseek(2)`.
            "seekfh" => self
                .seek_handle_value(&operand(args, 0), iarg(args, 1), iarg(args, 2) as i32)
                .map(|_| Value::NIL),
            // nqp::tellfh($fh): the byte position.
            // Cost: O(1).
            "tellfh" => self.tell_handle_value(&operand(args, 0)).map(Value::int),
            // nqp::eoffh($fh): 1 once a read has reached the end, else 0.
            // Cost: O(1) (a read handle may peek one buffer).
            "eoffh" => self
                .handle_eof_value(&operand(args, 0))
                .map(|eof| Value::int(i64::from(eof))),
            // nqp::filenofh($fh): the OS file descriptor; -1 once closed (or
            // for a handle with none).
            // Cost: O(1).
            "filenofh" => self.with_handle_mut(&operand(args, 0), |state| {
                Ok(Value::int(if state.closed {
                    -1
                } else {
                    state.native_descriptor().unwrap_or(-1)
                }))
            }),
            // nqp::getport($sock): the local TCP port of a socket handle.
            // Cost: O(1) plus one `getsockname(2)`.
            "getport" => self.with_handle_mut(&operand(args, 0), |state| {
                state
                    .local_port()
                    .map(|p| Value::int(i64::from(p)))
                    .ok_or_else(|| RuntimeError::new("Cannot getport for this kind of handle"))
            }),

            // -- path ops --
            // nqp::cwd(): the PROCESS working directory.
            // Cost: O(p), p = length of the path.
            "cwd" => std::env::current_dir()
                .map(|p| Value::str(p.to_string_lossy().into_owned()))
                .map_err(|e| RuntimeError::new(format!("Failed to determine cwd: {e}"))),
            // nqp::chdir($path): change the PROCESS working directory; `$*CWD`
            // is a Raku-level variable the op does not touch (as in Rakudo).
            // Cost: O(p) + one syscall, p = length of $path.
            "chdir" => {
                let path = self.resolve_path(&sarg(args, 0));
                path_result(args, fs_syscalls::change_dir(&path))
            }
            // nqp::mkdir($path, $mode): create the directory and any missing
            // parents; an existing directory is not an error.
            // Cost: O(d) + syscalls, d = missing path components.
            "mkdir" => {
                let path = self.resolve_path(&sarg(args, 0));
                let mode = args
                    .get(1)
                    .map_or(0o777, |m| crate::runtime::to_int(m) as u32);
                path_result(args, fs_syscalls::make_dir_all(&path, mode))
            }
            // Cost: O(p) + one syscall, p = length of $path.
            "rmdir" => {
                let path = self.resolve_path(&sarg(args, 0));
                path_result(args, fs_syscalls::remove_dir(&path))
            }
            // nqp::unlink($path): a missing file is not an error.
            // Cost: O(p) + one syscall, p = length of $path.
            "unlink" => {
                let path = self.resolve_path(&sarg(args, 0));
                path_result(args, fs_syscalls::unlink_file(&path))
            }
            // Cost: O(p) + one syscall, p = length of the paths.
            "rename" => {
                let from = self.resolve_path(&sarg(args, 0));
                let to = self.resolve_path(&sarg(args, 1));
                path_result(args, fs_syscalls::rename_path(&from, &to))
            }
            // nqp::copy($from, $to): the copy `IO::Path.copy` makes.
            // Cost: O(b), b = the source file's size in bytes.
            "copy" => {
                let from = self.resolve_path(&sarg(args, 0));
                let to = self.resolve_path(&sarg(args, 1));
                path_result(args, Self::copy_file_reason(&from, &to, false))
            }
            // nqp::link($target, $link) / nqp::symlink($target, $link): null.
            // Cost: O(p) + one syscall, p = length of the paths.
            "link" => {
                let target = self.resolve_path(&sarg(args, 0));
                let link = self.resolve_path(&sarg(args, 1));
                fs_syscalls::hard_link(&target, &link)
                    .map(|()| Value::NIL)
                    .map_err(RuntimeError::new)
            }
            // The link stores `$target` as written, as `symlink(2)` does.
            // Cost: O(p) + one syscall, p = length of the paths.
            #[cfg(unix)]
            "symlink" => {
                let target = std::path::PathBuf::from(sarg(args, 0));
                let link = self.resolve_path(&sarg(args, 1));
                fs_syscalls::symlink(&target, &link)
                    .map(|()| Value::NIL)
                    .map_err(RuntimeError::new)
            }
            // Cost: O(p) + one syscall, p = length of $path.
            #[cfg(unix)]
            "chmod" => {
                let path = self.resolve_path(&sarg(args, 0));
                path_result(args, fs_syscalls::set_mode(&path, iarg(args, 1) as u32))
            }
            // nqp::chown($path, $uid, $gid): -1 leaves an id unchanged.
            // Cost: O(p) + one syscall, p = length of $path.
            #[cfg(unix)]
            "chown" => {
                let path = self.resolve_path(&sarg(args, 0));
                path_result(
                    args,
                    fs_syscalls::set_owner(&path, iarg(args, 1), iarg(args, 2)),
                )
            }
            // nqp::fileexecutable($path) / nqp::filewritable($path): 0/1, by
            // `access(2)` (the test `IO::Path.x`/`.w` make); 0 for a missing
            // path.
            // Cost: O(p) + one syscall, p = length of $path.
            "fileexecutable" | "filewritable" => {
                let path = self.resolve_path(&sarg(args, 0));
                let ok = if op == "fileexecutable" {
                    crate::runtime::native_io::path_is_executable(&path)
                } else {
                    crate::runtime::native_io::path_is_writable(&path)
                };
                Ok(Value::int(i64::from(ok)))
            }
            // nqp::stat_time($path, $code) / nqp::lstat_time: a `STAT_*TIME`
            // field in fractional seconds (`IO::Path.modified` and kin).
            // Cost: O(p) + one syscall, p = length of $path.
            "stat_time" | "lstat_time" => {
                let path = self.resolve_path(&sarg(args, 0));
                crate::runtime::nqp_stat::stat_time(&path, iarg(args, 1), op == "lstat_time")
                    .map(Value::num)
                    .map_err(|e| {
                        RuntimeError::new(format!(
                            "Failed to stat file: {}",
                            crate::runtime::native_io::fs_errors::libuv_text(&e)
                        ))
                    })
            }
            _ => return self.call_nqp_op_decoder(op, args),
        })
    }
}
