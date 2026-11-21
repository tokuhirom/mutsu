//! The faulting thread's registers, the top of its stack, and the mappings
//! those addresses fall in.
//!
//! A backtrace cannot say who made a call through a bad function pointer: the
//! faulting `pc` *is* the bad address, it has no unwind info, and a caller
//! built without frame pointers (libcrypto, libc) leaves nothing in `rbp` to
//! walk. The caller's address is still there, though: `call` pushed it at
//! `[rsp]` (x86-64), and `bl`/`blr` left it in `lr` (aarch64). #16811's one CI
//! SIGSEGV was exactly this shape — a pool thread inside `ffi_call` jumped to a
//! heap address — and its report could not name the caller. This section is
//! what answers that the next time.
//!
//! Everything here is async-signal-safe: the registers come from the
//! kernel-supplied `ucontext`, the stack words are read with
//! `process_vm_readv` (an unreadable `sp` yields `EFAULT`, not a second
//! fault), and `/proc/self/maps` is read with `open`/`read` into a fixed
//! buffer.

use std::ffi::c_int;

use super::report::{Buf, write_all};

/// Stack words dumped from `sp` upward.
const STACK_WORDS: usize = 8;

/// The registers worth reporting, as (name, value) pairs, plus the stack
/// pointer and the link register (the caller's return address) when the
/// architecture has one.
#[derive(Default)]
struct Regs {
    named: [(&'static str, u64); 12],
    len: usize,
    pc: u64,
    sp: u64,
    lr: Option<u64>,
}

impl Regs {
    fn push(&mut self, name: &'static str, v: u64) {
        if self.len < self.named.len() {
            self.named[self.len] = (name, v);
            self.len += 1;
        }
    }
}

/// # Safety
/// `ctx` must be null or the `ucontext_t` the kernel passed to an
/// `SA_SIGINFO` handler.
#[cfg(all(target_os = "linux", target_env = "gnu", target_arch = "x86_64"))]
unsafe fn regs(ctx: *mut libc::c_void) -> Option<Regs> {
    if ctx.is_null() {
        return None;
    }
    // SAFETY: the caller guarantees `ctx` is the kernel's ucontext_t.
    let g = unsafe { &(*(ctx as *const libc::ucontext_t)).uc_mcontext.gregs };
    let r = |i: c_int| g[i as usize] as u64;
    let mut out = Regs {
        pc: r(libc::REG_RIP),
        sp: r(libc::REG_RSP),
        ..Regs::default()
    };
    for (name, i) in [
        ("rip", libc::REG_RIP),
        ("rsp", libc::REG_RSP),
        ("rbp", libc::REG_RBP),
        ("rax", libc::REG_RAX),
        ("rbx", libc::REG_RBX),
        ("rdi", libc::REG_RDI),
        ("rsi", libc::REG_RSI),
        ("rdx", libc::REG_RDX),
        ("rcx", libc::REG_RCX),
        ("r8", libc::REG_R8),
        ("r9", libc::REG_R9),
        ("r12", libc::REG_R12),
    ] {
        out.push(name, r(i));
    }
    Some(out)
}

/// # Safety
/// As for the x86-64 variant.
#[cfg(all(target_os = "linux", target_env = "gnu", target_arch = "aarch64"))]
unsafe fn regs(ctx: *mut libc::c_void) -> Option<Regs> {
    if ctx.is_null() {
        return None;
    }
    // SAFETY: the caller guarantees `ctx` is the kernel's ucontext_t.
    let m = unsafe { &(*(ctx as *const libc::ucontext_t)).uc_mcontext };
    let mut out = Regs {
        pc: m.pc,
        sp: m.sp,
        lr: Some(m.regs[30]),
        ..Regs::default()
    };
    out.push("pc", m.pc);
    out.push("sp", m.sp);
    out.push("lr", m.regs[30]);
    out.push("fp", m.regs[29]);
    for (name, i) in [
        ("x0", 0),
        ("x1", 1),
        ("x2", 2),
        ("x3", 3),
        ("x4", 4),
        ("x5", 5),
    ] {
        out.push(name, m.regs[i]);
    }
    Some(out)
}

#[cfg(not(all(
    target_os = "linux",
    target_env = "gnu",
    any(target_arch = "x86_64", target_arch = "aarch64")
)))]
unsafe fn regs(_ctx: *mut libc::c_void) -> Option<Regs> {
    None
}

/// Read `STACK_WORDS` words at `sp` without risking a fault: a bad `sp` makes
/// the syscall fail instead. Returns how many words were read.
#[cfg(target_os = "linux")]
fn read_stack(sp: u64, words: &mut [u64; STACK_WORDS]) -> usize {
    let local = libc::iovec {
        iov_base: words.as_mut_ptr().cast(),
        iov_len: std::mem::size_of_val(words),
    };
    let remote = libc::iovec {
        iov_base: sp as usize as *mut libc::c_void,
        iov_len: std::mem::size_of_val(words),
    };
    // SAFETY: `local` describes `words`, which is live and writable; the
    // remote range is only read by the kernel, which reports an unmapped one
    // as an error rather than faulting.
    let n = unsafe { libc::process_vm_readv(libc::getpid(), &local, 1, &remote, 1, 0) };
    if n <= 0 {
        0
    } else {
        n as usize / std::mem::size_of::<u64>()
    }
}

#[cfg(not(target_os = "linux"))]
fn read_stack(_sp: u64, _words: &mut [u64; STACK_WORDS]) -> usize {
    0
}

/// Write the register, stack and mapping sections for the signal context
/// `ctx`.
///
/// # Safety
/// `ctx` must be null or the `ucontext_t` the kernel passed to an
/// `SA_SIGINFO` handler.
pub(super) unsafe fn write_context(fd: c_int, ctx: *mut libc::c_void) {
    // SAFETY: forwarded from this function's contract.
    let Some(regs) = (unsafe { regs(ctx) }) else {
        write_all(fd, b"--- registers ---\n(unavailable on this platform)\n");
        return;
    };
    let mut buf = Buf::new();
    buf.s("--- registers ---\n");
    for &(name, v) in &regs.named[..regs.len] {
        buf.s(name);
        buf.s(": 0x");
        buf.hex(v);
        buf.s("\n");
    }
    // `[sp]` is the return address when the fault is a `call` through a bad
    // pointer; the following words are what the caller left above it.
    let mut words = [0u64; STACK_WORDS];
    let n = read_stack(regs.sp, &mut words);
    buf.s("--- stack (from sp) ---\n");
    if n == 0 {
        buf.s("(unreadable)\n");
    }
    for (i, w) in words[..n].iter().enumerate() {
        buf.s("[sp+");
        buf.dec((i * 8) as i64);
        buf.s("]: 0x");
        buf.hex(*w);
        buf.s("\n");
    }
    buf.flush(fd);

    // The mappings the interesting addresses fall in, so `[sp]` reads as
    // "libcrypto.so.3 + offset" instead of a bare number.
    let mut addrs = [0u64; STACK_WORDS + 2];
    addrs[0] = regs.pc;
    let mut count = 1;
    if let Some(lr) = regs.lr {
        addrs[count] = lr;
        count += 1;
    }
    for &w in &words[..n] {
        addrs[count] = w;
        count += 1;
    }
    write_all(fd, b"--- mappings of pc, lr and the stack words ---\n");
    write_maps(fd, &addrs[..count]);
}

/// Copy every `/proc/self/maps` line whose range contains one of `addrs`.
#[cfg(target_os = "linux")]
fn write_maps(fd: c_int, addrs: &[u64]) {
    // SAFETY: a NUL-terminated literal path; the descriptor is closed below.
    let maps = unsafe {
        libc::open(
            c"/proc/self/maps".as_ptr(),
            libc::O_RDONLY | libc::O_CLOEXEC,
        )
    };
    if maps < 0 {
        write_all(fd, b"(unreadable)\n");
        return;
    }
    let mut chunk = [0u8; 4096];
    let mut line = [0u8; 512];
    let mut len = 0usize;
    loop {
        // SAFETY: reading into a live buffer of the length passed.
        let got = unsafe { libc::read(maps, chunk.as_mut_ptr().cast(), chunk.len()) };
        if got <= 0 {
            break;
        }
        for &b in &chunk[..got as usize] {
            if b == b'\n' {
                emit_if_hit(fd, &line[..len], addrs);
                len = 0;
            } else if len < line.len() {
                line[len] = b;
                len += 1;
            }
        }
    }
    emit_if_hit(fd, &line[..len], addrs);
    // SAFETY: `maps` is the descriptor opened above.
    unsafe { libc::close(maps) };
}

#[cfg(not(target_os = "linux"))]
fn write_maps(fd: c_int, _addrs: &[u64]) {
    write_all(fd, b"(unavailable on this platform)\n");
}

fn emit_if_hit(fd: c_int, line: &[u8], addrs: &[u64]) {
    if let Some((start, end)) = map_range(line)
        && addrs.iter().any(|&a| a >= start && a < end)
    {
        write_all(fd, line);
        write_all(fd, b"\n");
    }
}

/// The `start-end` range a `/proc/self/maps` line opens with.
fn map_range(line: &[u8]) -> Option<(u64, u64)> {
    let dash = line.iter().position(|&b| b == b'-')?;
    let rest = &line[dash + 1..];
    let space = rest.iter().position(|&b| b == b' ')?;
    Some((parse_hex(&line[..dash])?, parse_hex(&rest[..space])?))
}

fn parse_hex(s: &[u8]) -> Option<u64> {
    if s.is_empty() || s.len() > 16 {
        return None;
    }
    let mut v = 0u64;
    for &c in s {
        let d = match c {
            b'0'..=b'9' => c - b'0',
            b'a'..=b'f' => c - b'a' + 10,
            b'A'..=b'F' => c - b'A' + 10,
            _ => return None,
        };
        v = (v << 4) | d as u64;
    }
    Some(v)
}

#[cfg(test)]
mod tests {
    use super::map_range;

    #[test]
    fn a_maps_line_yields_its_range() {
        let line = b"7f7174000000-7f7174021000 rw-p 00000000 00:00 0 ";
        assert_eq!(map_range(line), Some((0x7f71_7400_0000, 0x7f71_7402_1000)));
    }

    #[test]
    fn a_malformed_line_yields_nothing() {
        assert_eq!(map_range(b"garbage"), None);
        assert_eq!(map_range(b"zz-10 r--p"), None);
    }
}
