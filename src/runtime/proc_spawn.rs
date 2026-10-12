//! The process half shared by `Proc::Async.start` (`native_proc_async`) and
//! `nqp::spawnprocasync` (`nqp_ops_proc`, ADR-11502 §2.4.3): building and
//! spawning the child, and the read loop of one of its output pipes. What
//! each caller does with a chunk (decode it into a Supply, or push a `Buf`
//! onto a scheduler queue) stays with the caller.

use std::io::Read;
use std::process::{Child, Command, Stdio};

/// What a child is spawned with.
pub(crate) struct ProcSpawn {
    pub(crate) program: String,
    pub(crate) args: Vec<String>,
    /// The child's working directory; `None` (or empty) keeps the parent's.
    pub(crate) cwd: Option<String>,
    /// The child's whole environment; `None` inherits the OS environment.
    pub(crate) env: Option<Vec<(String, String)>>,
    pub(crate) stdin: ProcStdin,
    /// Whether stdout / stderr are piped to the parent (else inherited).
    pub(crate) pipe_stdout: bool,
    pub(crate) pipe_stderr: bool,
}

/// What the child's stdin is.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum ProcStdin {
    /// The parent's stdin.
    Inherit,
    /// A pipe the parent writes.
    Piped,
    /// Closed: the child reads end of file at once.
    Null,
}

impl ProcSpawn {
    /// Spawn the child.
    // Cost: O(a + e), a = arguments, e = environment entries (plus the
    // fork/exec).
    pub(crate) fn spawn(&self) -> std::io::Result<Child> {
        let pipe = |piped: bool| {
            if piped {
                Stdio::piped()
            } else {
                Stdio::inherit()
            }
        };
        let mut cmd = Command::new(&self.program);
        cmd.args(&self.args)
            .stdout(pipe(self.pipe_stdout))
            .stderr(pipe(self.pipe_stderr));
        match self.stdin {
            ProcStdin::Inherit => {}
            ProcStdin::Piped => {
                cmd.stdin(Stdio::piped());
            }
            ProcStdin::Null => {
                cmd.stdin(Stdio::null());
            }
        }
        if let Some(dir) = self.cwd.as_deref().filter(|dir| !dir.is_empty()) {
            cmd.current_dir(dir);
        }
        if let Some(env) = &self.env {
            cmd.env_clear();
            cmd.envs(env.iter().map(|(k, v)| (k, v)));
        }
        cmd.spawn()
    }
}

/// Read `reader` to its end, handing each chunk to `on_chunk`; the blocking
/// read is GC-quiescent. Stops early when `on_chunk` answers `false`, or when
/// a read times out (a socket with a read timeout) and `stop` answers `true`.
/// Answers the read error that ended the stream, if any. Shared by the
/// process pipes and the `nqp::asyncreadbytes` socket reader.
// Cost: O(n), n = bytes read.
pub(crate) fn pump_pipe<R: Read>(
    mut reader: R,
    stop: impl Fn() -> bool,
    mut on_chunk: impl FnMut(&[u8]) -> bool,
) -> Option<std::io::Error> {
    let mut buf = [0u8; 4096];
    loop {
        match crate::gc::block_quiescent(|| reader.read(&mut buf)) {
            Ok(0) => return None,
            Ok(n) => {
                if !on_chunk(&buf[..n]) {
                    return None;
                }
            }
            Err(e) if e.kind() == std::io::ErrorKind::Interrupted => {}
            Err(e)
                if matches!(
                    e.kind(),
                    std::io::ErrorKind::WouldBlock | std::io::ErrorKind::TimedOut
                ) =>
            {
                if stop() {
                    return None;
                }
            }
            Err(e) => return Some(e),
        }
    }
}
