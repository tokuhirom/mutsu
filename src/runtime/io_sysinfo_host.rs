//! Host identity (kernel release, machine, hostname), probed once per process
//! without spawning a subprocess.
//!
//! `$*KERNEL` and `$*DISTRO` need `uname -r`, `uname -m` and `hostname`. Shelling
//! out for them cost three `fork`/`exec`s on every `mutsu` start — paid by every
//! `prove` test process and every `mzef` subprocess, for data most programs never
//! read. All three fields come from a single `uname(2)` call (`hostname` prints
//! the same `nodename` the struct carries), so one syscall replaces the three
//! processes.
//!
//! The fallback path reads `/proc` instead, which is what makes this usable under
//! Miri: Miri can neither spawn a process nor call a foreign function, but with
//! `-Zmiri-disable-isolation` it can read a file. That is what lets
//! `gc::soundness_smoke` build a real `Interpreter` under Miri.

use std::sync::OnceLock;

/// The three host-identity strings, as `uname(2)` reports them.
pub(crate) struct HostInfo {
    /// `uname -r` — e.g. `6.18.7-76061807-generic`.
    pub(crate) release: String,
    /// `uname -m` — e.g. `x86_64`.
    pub(crate) machine: String,
    /// `uname -n` — the same value the `hostname` command prints.
    pub(crate) hostname: String,
}

/// Probe the host once and cache it for the life of the process. These are
/// process constants, so a repeat read must never re-run the syscall.
pub(crate) fn host_info() -> &'static HostInfo {
    static HOST_INFO: OnceLock<HostInfo> = OnceLock::new();
    HOST_INFO.get_or_init(probe)
}

#[cfg(all(
    unix,
    feature = "native",
    not(miri),
    not(target_arch = "wasm32"),
    not(target_os = "macos")
))]
fn probe() -> HostInfo {
    uname_probe().unwrap_or_else(file_probe)
}

// macOS `uname(2)` is available too, but `machine` there reports the hardware
// name the same way `uname -m` does, so the same code serves both. Kept as a
// separate arm only because the non-macOS arm can fall back to `/proc`, which
// does not exist on macOS.
#[cfg(all(
    unix,
    feature = "native",
    not(miri),
    not(target_arch = "wasm32"),
    target_os = "macos"
))]
fn probe() -> HostInfo {
    uname_probe().unwrap_or_else(empty_probe)
}

#[cfg(not(all(unix, feature = "native", not(miri), not(target_arch = "wasm32"))))]
fn probe() -> HostInfo {
    file_probe()
}

/// One `uname(2)`: no fork, no exec, no allocation beyond the three strings.
#[cfg(all(unix, feature = "native", not(miri), not(target_arch = "wasm32")))]
fn uname_probe() -> Option<HostInfo> {
    // SAFETY: `utsname` is a plain C struct of byte arrays, so an all-zero value
    // is a valid initialization, and `uname` only writes into the pointee.
    unsafe {
        let mut uts: libc::utsname = std::mem::zeroed();
        if libc::uname(&mut uts) != 0 {
            return None;
        }
        Some(HostInfo {
            release: c_field(&uts.release),
            machine: c_field(&uts.machine),
            hostname: c_field(&uts.nodename),
        })
    }
}

/// Decode a NUL-terminated `utsname` field. `c_char` is `i8` on x86_64 and `u8`
/// on aarch64, so cast rather than assume either.
#[cfg(all(unix, feature = "native", not(miri), not(target_arch = "wasm32")))]
fn c_field(buf: &[libc::c_char]) -> String {
    let bytes: Vec<u8> = buf
        .iter()
        .take_while(|&&c| c != 0)
        .map(|&c| c as u8)
        .collect();
    String::from_utf8_lossy(&bytes).into_owned()
}

/// The no-syscall path: `/proc` carries the same two strings on Linux (verified
/// equal to `uname -r` / `uname -n`), and `ARCH` is a build-time constant that
/// matches `uname -m` for the targets mutsu ships.
fn file_probe() -> HostInfo {
    fn read_trimmed(path: &str) -> Option<String> {
        std::fs::read_to_string(path)
            .ok()
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
    }
    HostInfo {
        release: read_trimmed("/proc/sys/kernel/osrelease").unwrap_or_default(),
        machine: std::env::consts::ARCH.to_string(),
        hostname: read_trimmed("/proc/sys/kernel/hostname")
            .or_else(|| std::env::var("HOSTNAME").ok().filter(|s| !s.is_empty()))
            .unwrap_or_default(),
    }
}

#[cfg(all(
    unix,
    feature = "native",
    not(miri),
    not(target_arch = "wasm32"),
    target_os = "macos"
))]
fn empty_probe() -> HostInfo {
    HostInfo {
        release: String::new(),
        machine: std::env::consts::ARCH.to_string(),
        hostname: String::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::host_info;

    /// The point of the module: the values must stay *correct*, not just cheap.
    /// `roast/S02-magicals/KERNEL.t` requires every field to be truthy and
    /// `roast/S32-io/IO-Socket-INET.t` matches `.release` against `/Microsoft/`
    /// for WSL detection.
    #[test]
    #[cfg(target_os = "linux")]
    fn host_info_matches_proc() {
        let info = host_info();
        let osrelease = std::fs::read_to_string("/proc/sys/kernel/osrelease").unwrap();
        assert_eq!(info.release, osrelease.trim());
        let hostname = std::fs::read_to_string("/proc/sys/kernel/hostname").unwrap();
        assert_eq!(info.hostname, hostname.trim());
        assert!(!info.machine.is_empty());
    }

    /// Cached: a second call must hand back the same static, not re-probe.
    #[test]
    fn host_info_is_cached() {
        assert!(std::ptr::eq(host_info(), host_info()));
    }
}
