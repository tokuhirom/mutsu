//! The pseudo-terminal behind `Proc::Async.new(:pty(:rows, :cols))` (#17751).
//!
//! The child gets the slave side as its stdin, stdout and stderr, a session of
//! its own and the slave as its controlling terminal, so `$*OUT.t` is true in
//! it. The parent keeps the master: what the child writes is read from it (the
//! `.stdout` Supply; stderr is merged there, as rakudo documents), and what
//! the parent writes goes to it. Every fd made here is close-on-exec, so the
//! master never leaks into an unrelated child (`docs/security.md`); the slave
//! reaches the child only through the stdio redirection, which sets it on
//! fds 0-2 there.

use std::fs::File;
use std::io::Read;
use std::process::{Child, Command};

/// The size of a pseudo-terminal, in character cells.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct PtySize {
    pub(crate) rows: u16,
    pub(crate) cols: u16,
}

/// The reading side of a pty master. A read after the last slave holder has
/// gone answers `EIO` on Linux; that is the end of the stream, not an error.
pub(crate) struct PtyReader(pub(crate) File);

impl Read for PtyReader {
    // Cost: one read(2).
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match self.0.read(buf) {
            #[cfg(all(unix, feature = "native"))]
            Err(e) if e.raw_os_error() == Some(libc::EIO) => Ok(0),
            other => other,
        }
    }
}

/// Spawn `cmd` behind a new pseudo-terminal of `size`; answers the child and
/// the master side.
// Cost: O(1) syscalls plus the fork/exec.
#[cfg(all(unix, feature = "native"))]
pub(crate) fn spawn_with_pty(mut cmd: Command, size: PtySize) -> std::io::Result<(Child, File)> {
    use std::os::fd::FromRawFd;
    use std::os::unix::process::CommandExt;
    let master_fd = open_master()?;
    // SAFETY: `master_fd` is a fresh fd nothing else owns; the `File` takes
    // ownership and closes it on every path below.
    let master = unsafe { File::from_raw_fd(master_fd) };
    // SAFETY: `grantpt`/`unlockpt` act on the fd only.
    if unsafe { libc::grantpt(master_fd) } != 0 || unsafe { libc::unlockpt(master_fd) } != 0 {
        return Err(std::io::Error::last_os_error());
    }
    let name = slave_name(master_fd)?;
    // SAFETY: `slave_name` answers a NUL-terminated path.
    let slave_fd = unsafe {
        libc::open(
            name.as_ptr(),
            libc::O_RDWR | libc::O_NOCTTY | libc::O_CLOEXEC,
        )
    };
    if slave_fd < 0 {
        return Err(std::io::Error::last_os_error());
    }
    // SAFETY: `slave_fd` is a fresh fd nothing else owns.
    let slave = unsafe { File::from_raw_fd(slave_fd) };
    resize(&master, size)?;
    use std::process::Stdio;
    cmd.stdin(Stdio::from(slave.try_clone()?))
        .stdout(Stdio::from(slave.try_clone()?))
        .stderr(Stdio::from(slave));
    // SAFETY: the hook runs in the forked child before exec and calls only
    // async-signal-safe functions (`setsid`, `ioctl`) on its own fds: it
    // starts a session and makes the slave, already on fd 0, the session's
    // controlling terminal.
    unsafe {
        cmd.pre_exec(|| {
            if libc::setsid() < 0 {
                return Err(std::io::Error::last_os_error());
            }
            if libc::ioctl(0, libc::TIOCSCTTY as _, 0) < 0 {
                return Err(std::io::Error::last_os_error());
            }
            Ok(())
        });
    }
    // The command (and with it the parent's copies of the slave) is dropped
    // at return, so the master reads end of stream once the child and its
    // own children have closed theirs.
    let child = cmd.spawn()?;
    Ok((child, master))
}

/// A new pty master, close-on-exec.
// Cost: one or two syscalls.
#[cfg(all(unix, feature = "native"))]
fn open_master() -> std::io::Result<libc::c_int> {
    // macOS's `posix_openpt` takes only `O_RDWR | O_NOCTTY`, so close-on-exec
    // is set right after; Linux opens it close-on-exec at once.
    #[cfg(target_os = "macos")]
    let flags = libc::O_RDWR | libc::O_NOCTTY;
    #[cfg(not(target_os = "macos"))]
    let flags = libc::O_RDWR | libc::O_NOCTTY | libc::O_CLOEXEC;
    // SAFETY: `posix_openpt` takes only flags and returns a new fd or -1; it
    // touches no memory of ours.
    let fd = unsafe { libc::posix_openpt(flags) };
    if fd < 0 {
        return Err(std::io::Error::last_os_error());
    }
    #[cfg(target_os = "macos")]
    // SAFETY: `fd` is the fd just opened; `F_SETFD` only sets its flag.
    if unsafe { libc::fcntl(fd, libc::F_SETFD, libc::FD_CLOEXEC) } < 0 {
        let err = std::io::Error::last_os_error();
        // SAFETY: closing the fd this function owns.
        unsafe { libc::close(fd) };
        return Err(err);
    }
    Ok(fd)
}

/// The path of the slave side of the pty whose master is `master_fd`, as a
/// NUL-terminated buffer.
// Cost: one syscall.
#[cfg(all(unix, feature = "native", any(target_os = "linux", target_os = "android")))]
fn slave_name(master_fd: libc::c_int) -> std::io::Result<[libc::c_char; 128]> {
    let mut name = [0 as libc::c_char; 128];
    // SAFETY: `name` is a writable buffer of the length passed; `ptsname_r`
    // writes a NUL-terminated path into it or fails.
    if unsafe { libc::ptsname_r(master_fd, name.as_mut_ptr(), name.len()) } != 0 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(name)
}

// Cost: one syscall.
#[cfg(all(unix, feature = "native", target_os = "macos"))]
fn slave_name(master_fd: libc::c_int) -> std::io::Result<[libc::c_char; 128]> {
    let mut name = [0 as libc::c_char; 128];
    // SAFETY: `TIOCPTYGNAME` writes a NUL-terminated path of at most 128
    // bytes into the buffer, which is that long (the reentrant form of
    // `ptsname` on macOS).
    if unsafe { libc::ioctl(master_fd, libc::TIOCPTYGNAME as _, name.as_mut_ptr()) } < 0 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(name)
}

// Cost: O(1).
#[cfg(all(
    unix,
    feature = "native",
    not(any(target_os = "linux", target_os = "android", target_os = "macos"))
))]
fn slave_name(_master_fd: libc::c_int) -> std::io::Result<[libc::c_char; 128]> {
    Err(std::io::Error::new(
        std::io::ErrorKind::Unsupported,
        "a pseudo-terminal is not supported on this platform",
    ))
}

/// A pseudo-terminal is a Unix facility; elsewhere `:pty` cannot start.
// Cost: O(1).
#[cfg(not(all(unix, feature = "native")))]
pub(crate) fn spawn_with_pty(_cmd: Command, _size: PtySize) -> std::io::Result<(Child, File)> {
    Err(std::io::Error::new(
        std::io::ErrorKind::Unsupported,
        "a pseudo-terminal is not supported on this platform",
    ))
}

/// Set the window size of the pty whose master is `master` (`TIOCSWINSZ`;
/// the child's foreground group gets `SIGWINCH`).
// Cost: one ioctl(2).
#[cfg(all(unix, feature = "native"))]
pub(crate) fn resize(master: &File, size: PtySize) -> std::io::Result<()> {
    use std::os::fd::AsRawFd;
    let ws = libc::winsize {
        ws_row: size.rows,
        ws_col: size.cols,
        ws_xpixel: 0,
        ws_ypixel: 0,
    };
    // SAFETY: `ws` is a valid `winsize` that outlives the call, and the
    // ioctl only reads it.
    if unsafe { libc::ioctl(master.as_raw_fd(), libc::TIOCSWINSZ, &ws) } < 0 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(())
}

// Cost: O(1).
#[cfg(not(all(unix, feature = "native")))]
pub(crate) fn resize(_master: &File, _size: PtySize) -> std::io::Result<()> {
    Ok(())
}

/// The attribute keys a `Proc::Async` keeps its `:pty` size under.
pub(crate) const PTY_ROWS: &str = "pty_rows";
pub(crate) const PTY_COLS: &str = "pty_cols";

/// The pty size a `Proc::Async` was created with, `None` without `:pty`.
// Cost: O(1).
pub(crate) fn pty_size_of(attrs: &crate::value::AttrMap) -> Option<PtySize> {
    use crate::value::ValueView;
    let cell = |key: &str| match attrs.get(key).map(crate::value::Value::view) {
        Some(ValueView::Int(n)) => Some(n.clamp(0, i64::from(u16::MAX)) as u16),
        _ => None,
    };
    Some(PtySize {
        rows: cell(PTY_ROWS)?,
        cols: cell(PTY_COLS)?,
    })
}

impl crate::runtime::Interpreter {
    /// `Proc::Async.resize-pty(:rows, :cols)`: set the size of the pty a
    /// started child runs behind (the child sees `SIGWINCH`). A process
    /// without a pty, or not started yet, answers `Nil` and keeps its size
    /// for a later start, as rakudo's does.
    // Cost: O(a) for the a arguments, plus one ioctl(2).
    pub(crate) fn proc_async_resize_pty(
        &mut self,
        attrs: &mut crate::value::AttrMap,
        args: &[crate::value::Value],
    ) -> Result<crate::value::Value, crate::value::RuntimeError> {
        use crate::value::{Value, ValueView};
        let Some(mut size) = pty_size_of(attrs) else {
            return Ok(Value::NIL);
        };
        for arg in args {
            if let ValueView::Pair(key, v) = arg.view() {
                let n = v.to_f64().clamp(0.0, f64::from(u16::MAX)) as u16;
                match key.as_str() {
                    "rows" => size.rows = n,
                    "cols" => size.cols = n,
                    _ => {}
                }
            }
        }
        attrs.insert(PTY_ROWS.to_string(), Value::int(i64::from(size.rows)));
        attrs.insert(PTY_COLS.to_string(), Value::int(i64::from(size.cols)));
        if let Some(ValueView::Int(pid)) = attrs.get("pid").map(Value::view)
            && let Ok(map) = crate::runtime::native_methods::proc_stdin_map().lock()
            && let Some(sink) = map.get(&(pid as u32)).cloned()
        {
            drop(map);
            if let Ok(guard) = sink.lock()
                && let Some(master) = guard.as_ref().and_then(|s| s.pty_master())
            {
                resize(master, size).map_err(|e| {
                    crate::value::RuntimeError::new(format!("Failed to resize the pty: {e}"))
                })?;
            }
        }
        Ok(Value::NIL)
    }
}
