//! Blocking reads on the main program's thread that give up when a worker's
//! `exit` is waiting to be adopted (ADR-11617, #13100).
//!
//! A `read(2)` on an empty pipe or terminal cannot be woken, so on the adopter
//! thread a read first waits for its fd to become readable with `poll(2)`,
//! re-checking `exit_handover::requested` every `exit_handover::POLL`. Once a
//! handover is pending the read fails with an error [`read_error`] turns into
//! `RuntimeError::exit_handover_pending`; the interpreter is halted by then and
//! unwinds to its END phasers. Every other thread reads as before.
//!
//! Standard input is read through std's process-wide buffer, and its fd is
//! only waited on while that buffer is empty: a line already buffered must be
//! returned even though the fd has nothing more to read.

use crate::value::RuntimeError;
use std::io::{self, BufRead, Read};
use std::sync::atomic::{AtomicBool, Ordering};

/// The payload of the `io::Error` an interrupted read returns.
#[derive(Debug)]
struct ExitPending;

impl std::fmt::Display for ExitPending {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("a worker thread's exit is being adopted")
    }
}

impl std::error::Error for ExitPending {}

/// Whether `err` is the error a read returns when it gave up on a pending
/// worker `exit`.
// Cost: O(1).
pub(crate) fn is_exit_pending(err: &io::Error) -> bool {
    err.get_ref().is_some_and(|inner| inner.is::<ExitPending>())
}

/// The `RuntimeError` for a failed read: the exit-handover unwind for an
/// interrupted one, otherwise `"<what>: <err>"`.
// Cost: O(1).
pub(crate) fn read_error(err: io::Error, what: &str) -> RuntimeError {
    if is_exit_pending(&err) {
        RuntimeError::exit_handover_pending()
    } else {
        RuntimeError::new(format!("{what}: {err}"))
    }
}

/// On the adopter thread, wait until `fd` is readable (data, end of file, or
/// an error the read itself will report), failing once a worker's `exit` is
/// pending. Returns at once on every other thread.
// Cost: O(1) plus the wait; the adopter wakes once per `exit_handover::POLL`.
#[cfg(all(unix, feature = "native"))]
fn wait_readable(fd: i32) -> io::Result<()> {
    if !crate::exit_handover::is_adopter_thread() {
        return Ok(());
    }
    let timeout_ms = crate::exit_handover::POLL.as_millis() as libc::c_int;
    loop {
        if crate::exit_handover::requested() {
            return Err(io::Error::other(ExitPending));
        }
        let mut pfd = libc::pollfd {
            fd,
            events: libc::POLLIN,
            revents: 0,
        };
        // SAFETY: `pfd` is one valid, initialized `pollfd` that outlives the
        // call, and the count passed is 1.
        let ready = unsafe { libc::poll(&mut pfd, 1, timeout_ms) };
        if ready > 0 {
            return Ok(());
        }
        if ready < 0 && io::Error::last_os_error().kind() != io::ErrorKind::Interrupted {
            // `poll` itself failed: fall back to the plain blocking read.
            return Ok(());
        }
    }
}

#[cfg(not(all(unix, feature = "native")))]
fn wait_readable(_fd: i32) -> io::Result<()> {
    Ok(())
}

/// A readable OS handle whose fd [`ExitAware`] can wait on.
pub(crate) trait PollFd {
    /// The fd to `poll`, or `None` where there is none to wait on.
    fn poll_fd(&self) -> Option<i32>;
}

macro_rules! poll_fd_via_as_raw_fd {
    ($($ty:ty),*) => {$(
        impl PollFd for $ty {
            #[cfg(unix)]
            fn poll_fd(&self) -> Option<i32> {
                Some(std::os::fd::AsRawFd::as_raw_fd(self))
            }
            #[cfg(not(unix))]
            fn poll_fd(&self) -> Option<i32> {
                None
            }
        }
    )*};
}

poll_fd_via_as_raw_fd!(
    std::process::ChildStdout,
    std::process::ChildStderr,
    std::fs::File
);

impl<T: PollFd> PollFd for &mut T {
    fn poll_fd(&self) -> Option<i32> {
        (**self).poll_fd()
    }
}

/// An unbuffered reader (a child's output pipe) whose reads give up on a
/// pending worker `exit` on the adopter thread.
pub(crate) struct ExitAware<R>(pub(crate) R);

impl<R: Read + PollFd> Read for ExitAware<R> {
    // Cost: O(n) for the bytes read, plus one `poll` on the adopter thread.
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        if let Some(fd) = self.0.poll_fd() {
            wait_readable(fd)?;
        }
        self.0.read(buf)
    }
}

/// Read a child's output pipe to its end, as `read_to_string` does: a read
/// error other than a pending worker `exit`, or output that is not UTF-8,
/// yields the empty string.
// Cost: O(b), b = bytes read.
pub(crate) fn slurp_pipe<R: Read + PollFd>(reader: R) -> Result<String, RuntimeError> {
    let mut bytes = Vec::new();
    match ExitAware(reader).read_to_end(&mut bytes) {
        Err(err) if is_exit_pending(&err) => Err(RuntimeError::exit_handover_pending()),
        Ok(_) => Ok(String::from_utf8(bytes).unwrap_or_default()),
        Err(_) => Ok(String::new()),
    }
}

/// Whether std's stdin buffer is known to be empty, so the next read goes to
/// fd 0. Written only while the stdin lock is held, by [`StdinReader`]; every
/// runtime read of standard input goes through it.
static STDIN_BUFFER_EMPTY: AtomicBool = AtomicBool::new(true);

/// The process's standard input, locked, whose reads give up on a pending
/// worker `exit` on the adopter thread.
pub(crate) struct StdinReader {
    lock: io::StdinLock<'static>,
    /// Bytes left in the slice the last `fill_buf` returned.
    available: usize,
}

impl StdinReader {
    // Cost: O(1) plus the wait for the stdin lock.
    pub(crate) fn lock() -> Self {
        Self {
            lock: io::stdin().lock(),
            available: 0,
        }
    }
}

impl BufRead for StdinReader {
    // Cost: O(1) when bytes are buffered; otherwise one `read(2)` of up to the
    // buffer size, plus one `poll` per `exit_handover::POLL` on the adopter.
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        if STDIN_BUFFER_EMPTY.load(Ordering::Relaxed) {
            wait_readable(0)?;
        }
        let buf = self.lock.fill_buf()?;
        self.available = buf.len();
        STDIN_BUFFER_EMPTY.store(buf.is_empty(), Ordering::Relaxed);
        Ok(buf)
    }

    // Cost: O(1).
    fn consume(&mut self, amount: usize) {
        self.lock.consume(amount);
        self.available = self.available.saturating_sub(amount);
        if self.available == 0 {
            STDIN_BUFFER_EMPTY.store(true, Ordering::Relaxed);
        }
    }
}

impl Read for StdinReader {
    // Cost: O(n) for the bytes copied out of the buffer.
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let available = self.fill_buf()?;
        let n = available.len().min(buf.len());
        buf[..n].copy_from_slice(&available[..n]);
        self.consume(n);
        Ok(n)
    }
}
