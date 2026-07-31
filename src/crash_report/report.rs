//! Writing the report itself, from inside the signal handler.
//!
//! Every routine here down to and including the raw backtrace sticks to
//! `open`/`mkdir`/`write` and fixed stack buffers: no allocation, no locks, no
//! `core::fmt`. That is what makes the report land even when the fault was in
//! the allocator. The symbolized backtrace at the very end breaks that rule on
//! purpose and is written last, after everything that matters is on disk.

use std::backtrace::Backtrace;
use std::ffi::c_int;

use super::handler::{PREAMBLE, REPORT_DIR};

/// Header buffer size. Anything longer is truncated rather than allocated.
const BUF_SIZE: usize = 2048;

/// Path buffer size — `PATH_MAX` on Linux.
const PATH_SIZE: usize = 4096;

/// # Safety
/// `info` must be null or the kernel-supplied `siginfo_t` for `sig`.
pub(super) unsafe fn write_report(sig: c_int, info: *mut libc::siginfo_t) {
    let Some(dir) = REPORT_DIR.get() else { return };
    // SAFETY: getpid takes no arguments and cannot fail.
    let pid = unsafe { libc::getpid() };

    // <dir>/<pid>.txt, built in place: no allocation, no formatting machinery,
    // nothing that can fail if the heap is corrupt.
    let mut path = [0u8; PATH_SIZE];
    let mut len = 0;
    append(&mut path, &mut len, dir);
    // SAFETY: `append` always leaves spare bytes past `len`.
    unsafe { mkdir_p(&mut path, len) };
    append(&mut path, &mut len, b"/");
    let mut num = [0u8; 24];
    append(&mut path, &mut len, dec(&mut num, pid as i64));
    append(&mut path, &mut len, b".txt\0");

    // SAFETY: `path` is NUL-terminated in place.
    let fd = unsafe {
        libc::open(
            path.as_ptr().cast(),
            libc::O_WRONLY | libc::O_CREAT | libc::O_TRUNC,
            0o644 as libc::c_uint,
        )
    };
    if fd < 0 {
        return;
    }

    let mut buf = Buf::new();
    buf.s("mutsu crash report\nsignal: ");
    buf.dec(sig as i64);
    buf.s(" (");
    buf.s(signal_name(sig));
    buf.s(")\nsi_code: ");
    // SAFETY: `info` is the caller's kernel-supplied siginfo.
    let code = unsafe { si_code(info) };
    buf.dec(code as i64);
    // A null fault address is a null deref; anything else is a freed or
    // garbage pointer. That distinction is visible at a glance here. For a
    // signal that was *sent* (`kill`, `raise`, `abort`) the union holds the
    // sender's pid/uid instead, so printing it as an address would mislead.
    buf.s("\nfault-addr: ");
    if code > 0 {
        buf.s("0x");
        // SAFETY: as above.
        buf.hex(unsafe { fault_addr(info) } as u64);
    } else {
        buf.s("n/a (signal was sent, not a fault)");
    }
    buf.s("\npid: ");
    buf.dec(pid as i64);
    buf.s("\nppid: ");
    // SAFETY: getppid takes no arguments and cannot fail.
    buf.dec(unsafe { libc::getppid() } as i64);
    buf.s("\ntid: ");
    buf.dec(thread_id());
    buf.s("\nthread: ");
    let mut name = [0u8; 32];
    buf.b(thread_name(&mut name));
    buf.s("\ntime: ");
    // SAFETY: a null argument means "return the time, store nothing".
    buf.dec(unsafe { libc::time(std::ptr::null_mut()) } as i64);
    buf.s("\n");
    buf.flush(fd);
    if let Some(pre) = PREAMBLE.get() {
        write_all(fd, pre);
    }

    write_all(fd, b"--- backtrace (raw) ---\n");
    write_raw_backtrace(fd);

    // Everything above is async-signal-safe and already on disk. What follows
    // allocates and takes locks, so it can deadlock outright if the fault was
    // inside the allocator: bound it with an alarm and accept losing it.
    // SAFETY: arming an alarm cannot fail.
    unsafe { libc::alarm(10) };
    write_all(fd, b"--- backtrace (symbolized, best effort) ---\n");
    let bt = Backtrace::force_capture().to_string();
    write_all(fd, bt.as_bytes());
    write_all(fd, b"\n");

    // SAFETY: `fd` is the descriptor opened above.
    unsafe { libc::close(fd) };
}

/// Append `bytes` to `path[..*len]`, truncating rather than overflowing and
/// always leaving room for the `/<pid>.txt\0` suffix.
fn append(path: &mut [u8; PATH_SIZE], len: &mut usize, bytes: &[u8]) {
    let n = bytes.len().min(path.len().saturating_sub(*len + 32));
    path[*len..*len + n].copy_from_slice(&bytes[..n]);
    *len += n;
}

/// `mkdir -p` over `path[..len]` using only `mkdir(2)`, so the report
/// directory costs nothing until a process actually crashes. Errors (including
/// `EEXIST`) are ignored; a real failure just means `open` fails next.
///
/// # Safety
/// `path` must have at least one spare byte past `len`.
unsafe fn mkdir_p(path: &mut [u8], len: usize) {
    for i in 1..=len {
        if i == len || path[i] == b'/' {
            let saved = path[i];
            path[i] = 0;
            // SAFETY: `path` is NUL-terminated at `i` for the duration.
            unsafe { libc::mkdir(path.as_ptr().cast(), 0o755) };
            path[i] = saved;
        }
    }
}

/// Frame addresses via glibc's `backtrace_symbols_fd`, which is documented
/// async-signal-safe (it does not allocate). Names are poor without
/// `-rdynamic`, but `addr2line -f -e <binary>` resolves the addresses offline.
#[cfg(all(target_os = "linux", target_env = "gnu"))]
fn write_raw_backtrace(fd: c_int) {
    let mut frames = [std::ptr::null_mut::<libc::c_void>(); 64];
    // SAFETY: `frames` is a valid array of the length passed in.
    unsafe {
        let n = libc::backtrace(frames.as_mut_ptr(), frames.len() as c_int);
        if n > 0 {
            libc::backtrace_symbols_fd(frames.as_ptr(), n, fd);
        }
    }
}

#[cfg(not(all(target_os = "linux", target_env = "gnu")))]
fn write_raw_backtrace(fd: c_int) {
    write_all(fd, b"(unavailable on this platform)\n");
}

fn write_all(fd: c_int, mut data: &[u8]) {
    while !data.is_empty() {
        // SAFETY: writing `data.len()` bytes from a live slice.
        let n = unsafe { libc::write(fd, data.as_ptr().cast(), data.len()) };
        if n <= 0 {
            return;
        }
        data = &data[n as usize..];
    }
}

fn signal_name(sig: c_int) -> &'static str {
    match sig {
        libc::SIGSEGV => "SIGSEGV",
        libc::SIGBUS => "SIGBUS",
        libc::SIGILL => "SIGILL",
        libc::SIGFPE => "SIGFPE",
        libc::SIGABRT => "SIGABRT",
        _ => "?",
    }
}

/// # Safety
/// `info` must be null or a valid `siginfo_t`.
#[cfg(any(target_os = "linux", target_os = "macos"))]
unsafe fn fault_addr(info: *mut libc::siginfo_t) -> usize {
    if info.is_null() {
        return 0;
    }
    #[cfg(target_os = "linux")]
    // SAFETY: caller guarantees `info` is a valid siginfo_t.
    unsafe {
        (*info).si_addr() as usize
    }
    #[cfg(target_os = "macos")]
    // SAFETY: caller guarantees `info` is a valid siginfo_t.
    unsafe {
        (*info).si_addr as usize
    }
}

#[cfg(not(any(target_os = "linux", target_os = "macos")))]
unsafe fn fault_addr(_info: *mut libc::siginfo_t) -> usize {
    0
}

/// # Safety
/// `info` must be null or a valid `siginfo_t`.
#[cfg(any(target_os = "linux", target_os = "macos"))]
pub(super) unsafe fn si_code(info: *mut libc::siginfo_t) -> c_int {
    if info.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `info` is a valid siginfo_t.
    unsafe { (*info).si_code }
}

#[cfg(not(any(target_os = "linux", target_os = "macos")))]
pub(super) unsafe fn si_code(_info: *mut libc::siginfo_t) -> c_int {
    0
}

#[cfg(target_os = "linux")]
fn thread_id() -> i64 {
    // SAFETY: gettid takes no arguments and cannot fail.
    unsafe { libc::syscall(libc::SYS_gettid) as i64 }
}

#[cfg(not(target_os = "linux"))]
fn thread_id() -> i64 {
    // SAFETY: pthread_self cannot fail.
    unsafe { libc::pthread_self() as i64 }
}

/// The faulting thread's name, via `prctl(PR_GET_NAME)` — a bare syscall,
/// unlike `pthread_getname_np`, which reads `/proc`.
#[cfg(target_os = "linux")]
fn thread_name(buf: &mut [u8; 32]) -> &[u8] {
    // SAFETY: PR_GET_NAME writes at most 16 bytes into the buffer.
    unsafe { libc::prctl(libc::PR_GET_NAME, buf.as_mut_ptr()) };
    let end = buf.iter().position(|&b| b == 0).unwrap_or(buf.len());
    &buf[..end]
}

#[cfg(not(target_os = "linux"))]
fn thread_name(_buf: &mut [u8; 32]) -> &[u8] {
    b"?"
}

/// Fixed-capacity, allocation-free formatting buffer for the report header.
struct Buf {
    data: [u8; BUF_SIZE],
    len: usize,
}

impl Buf {
    fn new() -> Buf {
        Buf {
            data: [0; BUF_SIZE],
            len: 0,
        }
    }
    fn b(&mut self, bytes: &[u8]) {
        let n = bytes.len().min(self.data.len() - self.len);
        self.data[self.len..self.len + n].copy_from_slice(&bytes[..n]);
        self.len += n;
    }
    fn s(&mut self, s: &str) {
        self.b(s.as_bytes());
    }
    fn dec(&mut self, v: i64) {
        let mut num = [0u8; 24];
        self.b(dec(&mut num, v));
    }
    fn hex(&mut self, v: u64) {
        let mut num = [0u8; 16];
        for (i, slot) in num.iter_mut().enumerate() {
            *slot = b"0123456789abcdef"[((v >> (60 - i * 4)) & 0xf) as usize];
        }
        self.b(&num);
    }
    fn flush(&mut self, fd: c_int) {
        write_all(fd, &self.data[..self.len]);
        self.len = 0;
    }
}

/// Render `v` in decimal into `num`, returning the filled tail.
fn dec(num: &mut [u8; 24], v: i64) -> &[u8] {
    let neg = v < 0;
    let mut mag = v.unsigned_abs();
    let mut i = num.len();
    loop {
        i -= 1;
        num[i] = b'0' + (mag % 10) as u8;
        mag /= 10;
        if mag == 0 {
            break;
        }
    }
    if neg {
        i -= 1;
        num[i] = b'-';
    }
    &num[i..]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dec_renders_signed_values() {
        let mut n = [0u8; 24];
        assert_eq!(dec(&mut n, 0), b"0");
        assert_eq!(dec(&mut n, 12345), b"12345");
        assert_eq!(dec(&mut n, -42), b"-42");
        assert_eq!(dec(&mut n, i64::MIN), b"-9223372036854775808");
    }

    #[test]
    fn hex_is_fixed_width() {
        let mut buf = Buf::new();
        buf.hex(0);
        assert_eq!(&buf.data[..buf.len], b"0000000000000000");
        let mut buf = Buf::new();
        buf.hex(0xdead_beef);
        assert_eq!(&buf.data[..buf.len], b"00000000deadbeef");
    }

    #[test]
    fn buf_truncates_instead_of_overflowing() {
        let mut buf = Buf::new();
        let big = vec![b'x'; BUF_SIZE * 2];
        buf.b(&big);
        assert_eq!(buf.len, BUF_SIZE);
    }

    #[test]
    fn append_leaves_room_for_the_suffix() {
        let mut path = [0u8; PATH_SIZE];
        let mut len = 0;
        append(&mut path, &mut len, &vec![b'a'; PATH_SIZE * 2]);
        assert_eq!(len, PATH_SIZE - 32);
    }

    #[test]
    fn mkdir_p_creates_every_level() {
        let base = std::env::temp_dir().join(format!("mutsu-mkdir-p-{}", std::process::id()));
        let target = base.join("a/b/c");
        let mut path = [0u8; PATH_SIZE];
        let bytes = std::os::unix::ffi::OsStrExt::as_bytes(target.as_os_str());
        path[..bytes.len()].copy_from_slice(bytes);
        // SAFETY: `path` is far longer than the path written into it.
        unsafe { mkdir_p(&mut path, bytes.len()) };
        assert!(target.is_dir(), "{} was not created", target.display());
        let _ = std::fs::remove_dir_all(&base);
    }
}
