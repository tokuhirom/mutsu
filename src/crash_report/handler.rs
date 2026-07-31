//! Installing the fatal-signal handler, and what that handler does before it
//! hands the signal back to whoever owned it.

use std::ffi::c_int;
use std::sync::OnceLock;
use std::sync::atomic::{AtomicBool, Ordering};

/// The signals a crash report is written for. SIGABRT is included because a
/// Rust double-panic and `std`'s stack-overflow reporter both land there.
pub(super) const FATAL: [c_int; 5] = [
    libc::SIGSEGV,
    libc::SIGBUS,
    libc::SIGILL,
    libc::SIGFPE,
    libc::SIGABRT,
];

/// Alternate signal stack size. Larger than the platform `SIGSTKSZ` (8-16 KiB)
/// because a stack-overflow SIGSEGV leaves no room anywhere else and the
/// symbolized backtrace is stack-hungry.
const ALT_STACK_SIZE: usize = 256 * 1024;

/// Report directory: absolute, with no trailing slash. Resolved at install
/// time because a Raku program may `chdir` before it crashes.
pub(super) static REPORT_DIR: OnceLock<Box<[u8]>> = OnceLock::new();
/// Pre-rendered `version:`/`cwd:`/`argv:` lines. Reading them needs
/// allocation, which the handler must not do before the report has landed.
pub(super) static PREAMBLE: OnceLock<Box<[u8]>> = OnceLock::new();

/// Previous disposition of each signal in [`FATAL`], so the signal ends up
/// wherever it would have gone without us — in particular `std`'s
/// stack-overflow reporter, which owns SIGSEGV before we do.
struct OldActions([libc::sigaction; FATAL.len()]);
// SAFETY: published once through `OnceLock` before any handler reads it, and
// never mutated afterwards.
unsafe impl Send for OldActions {}
unsafe impl Sync for OldActions {}
static OLD_ACTIONS: OnceLock<OldActions> = OnceLock::new();

static INSTALLED: AtomicBool = AtomicBool::new(false);
static IN_HANDLER: AtomicBool = AtomicBool::new(false);

pub(super) fn install() {
    if std::env::var("MUTSU_CRASH_REPORT").as_deref() == Ok("0") {
        return;
    }
    // Per-thread: every caller gets its own alternate stack.
    install_alt_stack();
    if INSTALLED.swap(true, Ordering::SeqCst) {
        return;
    }
    REPORT_DIR.get_or_init(report_dir);
    PREAMBLE.get_or_init(preamble);
    // SAFETY: `sa` is a zeroed sigaction carrying a valid handler, and each
    // `old[i]` is a live, writable `sigaction`.
    let old = unsafe {
        let mut old: [libc::sigaction; FATAL.len()] = std::mem::zeroed();
        let mut sa: libc::sigaction = std::mem::zeroed();
        sa.sa_sigaction = handler as *const () as usize;
        sa.sa_flags = libc::SA_SIGINFO | libc::SA_ONSTACK;
        libc::sigemptyset(&mut sa.sa_mask);
        for (i, &sig) in FATAL.iter().enumerate() {
            libc::sigaction(sig, &sa, &mut old[i]);
        }
        old
    };
    // A crash inside this window falls back to SIG_DFL, which is still the
    // right exit status — only `std`'s stack-overflow message would be lost.
    let _ = OLD_ACTIONS.set(OldActions(old));
}

/// Give the calling thread a large alternate stack, so the handler still has
/// somewhere to run when the fault *is* the stack running out. Without this a
/// stack-overflow SIGSEGV would simply never reach the handler.
fn install_alt_stack() {
    let mut stack = Vec::<u8>::with_capacity(ALT_STACK_SIZE);
    let ptr = stack.as_mut_ptr();
    std::mem::forget(stack); // lives for the rest of the process
    // SAFETY: `ptr` owns ALT_STACK_SIZE bytes that are never freed.
    unsafe {
        let ss = libc::stack_t {
            ss_sp: ptr.cast(),
            ss_flags: 0,
            ss_size: ALT_STACK_SIZE,
        };
        libc::sigaltstack(&ss, std::ptr::null_mut());
    }
}

fn report_dir() -> Box<[u8]> {
    let dir = std::env::var_os("MUTSU_CRASH_DIR")
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|| std::path::PathBuf::from("tmp/crash"));
    let abs = if dir.is_absolute() {
        dir
    } else {
        match std::env::current_dir() {
            Ok(cwd) => cwd.join(dir),
            Err(_) => std::env::temp_dir().join("mutsu-crash"),
        }
    };
    let mut bytes = std::os::unix::ffi::OsStrExt::as_bytes(abs.as_os_str()).to_vec();
    while bytes.len() > 1 && bytes.last() == Some(&b'/') {
        bytes.pop();
    }
    bytes.into_boxed_slice()
}

fn preamble() -> Box<[u8]> {
    let mut out = Vec::new();
    out.extend_from_slice(b"version: ");
    out.extend_from_slice(env!("CARGO_PKG_VERSION").as_bytes());
    out.push(b'\n');
    out.extend_from_slice(b"cwd: ");
    if let Ok(cwd) = std::env::current_dir() {
        out.extend_from_slice(std::os::unix::ffi::OsStrExt::as_bytes(cwd.as_os_str()));
    }
    out.push(b'\n');
    // argv is half the point of the report: it is what distinguishes the
    // interpreter running the `.t` file from a subprocess `is_run` spawned.
    out.extend_from_slice(b"argv:");
    for arg in std::env::args_os() {
        out.push(b' ');
        out.extend_from_slice(std::os::unix::ffi::OsStrExt::as_bytes(arg.as_os_str()));
    }
    out.push(b'\n');
    out.into_boxed_slice()
}

extern "C" fn handler(sig: c_int, info: *mut libc::siginfo_t, _ctx: *mut libc::c_void) {
    // Exactly one report per process. A fault *inside* the handler must not
    // loop, and a second thread faulting concurrently must not interleave its
    // writes into the same file — both go straight to the hand-off so the
    // process still dies with the right status. (A second thread's hand-off
    // can kill the process while the first is still writing; that costs at
    // most the tail of the report, which is why the fields that matter are
    // written first.)
    if !IN_HANDLER.swap(true, Ordering::SeqCst) {
        // SAFETY: `info` is the kernel-supplied siginfo for `sig`.
        unsafe { super::report::write_report(sig, info) };
    }
    // SAFETY: `info` is the kernel-supplied siginfo for `sig`.
    hand_off(sig, unsafe { super::report::si_code(info) });
}

/// Put the previous disposition back and let the signal through, so the exit
/// status, `std`'s stack-overflow message and core-dump behaviour are all
/// unchanged by our presence.
///
/// For a genuine hardware fault (`si_code > 0`) that means *returning*: the
/// faulting instruction re-executes and faults again, and the restored handler
/// then sees the true `si_addr`. Re-raising instead would hand it an
/// `SI_TKILL` siginfo with a null address, which is exactly what makes `std`'s
/// guard-page check miss a stack overflow and swallow its "has overflowed its
/// stack" message. A signal that was *sent* to us (`kill`, `raise`, `abort`)
/// has no instruction to re-execute, so that one really does have to be
/// re-raised.
fn hand_off(sig: c_int, si_code: c_int) {
    // SAFETY: restoring a sigaction we saved, then unblocking and raising the
    // very signal we are handling.
    unsafe {
        match (OLD_ACTIONS.get(), FATAL.iter().position(|&s| s == sig)) {
            (Some(old), Some(i)) => {
                libc::sigaction(sig, &old.0[i], std::ptr::null_mut());
            }
            _ => {
                libc::signal(sig, libc::SIG_DFL);
            }
        }
        if si_code > 0 {
            return;
        }
        let mut set: libc::sigset_t = std::mem::zeroed();
        libc::sigemptyset(&mut set);
        libc::sigaddset(&mut set, sig);
        libc::pthread_sigmask(libc::SIG_UNBLOCK, &set, std::ptr::null_mut());
        libc::raise(sig);
    }
}

pub(super) fn selftest_if_requested() {
    match std::env::var("MUTSU_CRASH_SELFTEST").as_deref() {
        Ok("segv") => {
            // A real fault, so the report's si_addr is a real fault address.
            // SAFETY: none — deliberately dereferencing null.
            unsafe { std::ptr::null_mut::<u8>().write_volatile(1) };
        }
        Ok("abort") => std::process::abort(),
        _ => {}
    }
}
