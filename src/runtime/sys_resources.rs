//! Host resources the runtime reports: CPU cores, memory and the local time
//! zone. Each is one routine shared by the Raku-level API (`Kernel.cpu-cores`,
//! `Kernel.free-memory`, `Kernel.total-memory`, `DateTime.now`'s offset) and
//! the `nqp::` op Rakudo builds that API on (`nqp::cpucores`,
//! `nqp::freemem`, `nqp::totalmem`, `nqp::decodelocaltime`).

/// The number of CPU cores available to this process (at least 1).
// Cost: O(1) plus one `sched_getaffinity(2)` / sysconf.
pub(crate) fn cpu_core_count() -> i64 {
    std::thread::available_parallelism().map_or(1, |n| n.get() as i64)
}

/// Total physical memory in bytes, as libuv's `uv_get_total_memory` reports
/// it (`MemTotal` on Linux, `hw.memsize` on macOS); 0 when unknown.
// Cost: O(f), f = size of /proc/meminfo (a few KB) on Linux; one sysctl on macOS.
pub(crate) fn total_memory() -> i64 {
    platform::total_memory().unwrap_or(0)
}

/// Memory available for new allocations in bytes, as libuv's
/// `uv_get_free_memory` reports it (`MemAvailable` on Linux, free pages on
/// macOS); 0 when unknown.
// Cost: O(f), f = size of /proc/meminfo (a few KB) on Linux; two sysctls on macOS.
pub(crate) fn free_memory() -> i64 {
    platform::free_memory().unwrap_or(0)
}

/// The local time zone's offset from UTC at `epoch` (seconds east), and
/// whether daylight saving time is in effect then. UTC (0, false) when the
/// zone cannot be determined.
// Cost: O(1) plus one `localtime_r(3)`.
pub(crate) fn local_offset_at(epoch: i64) -> (i64, bool) {
    // Miri cannot call a foreign function, so it takes the documented
    // "could not be determined" arm.
    #[cfg(all(not(target_arch = "wasm32"), not(miri), feature = "native"))]
    {
        let t = epoch as libc::time_t;
        // SAFETY: `tm` is a plain C struct, so an all-zero value is a valid
        // initialization; `localtime_r` only reads `t` and writes `tm`, both
        // live locals, and is the thread-safe variant.
        let tm = unsafe {
            let mut tm: libc::tm = std::mem::zeroed();
            if libc::localtime_r(&t, &mut tm).is_null() {
                return (0, false);
            }
            tm
        };
        (tm.tm_gmtoff, tm.tm_isdst > 0)
    }
    #[cfg(not(all(not(target_arch = "wasm32"), not(miri), feature = "native")))]
    {
        let _ = epoch;
        (0, false)
    }
}

/// `epoch` broken down in the local time zone, in the order
/// `nqp::decodelocaltime` answers: second, minute, hour, day of month,
/// month (1-12), year, day of week (0 = Sunday), day of year (0-based), DST
/// flag. The calendar arithmetic is DateTime's (`value::temporal_core`).
// Cost: O(1) plus one `localtime_r(3)`.
pub(crate) fn decode_local_time(epoch: i64) -> [i64; 9] {
    use crate::value::temporal_core::{civil_to_epoch_days, epoch_days_to_civil};
    let (offset, dst) = local_offset_at(epoch);
    let local = epoch + offset;
    let days = local.div_euclid(86_400);
    let secs = local.rem_euclid(86_400);
    let (year, month, day) = epoch_days_to_civil(days);
    [
        secs % 60,
        secs / 60 % 60,
        secs / 3600,
        day,
        month,
        year,
        // 1970-01-01 was a Thursday.
        (days + 4).rem_euclid(7),
        days - civil_to_epoch_days(year, 1, 1),
        i64::from(dst),
    ]
}

#[cfg(target_os = "linux")]
mod platform {
    /// One `/proc/meminfo` field, in bytes.
    fn meminfo_bytes(field: &str) -> Option<i64> {
        let text = std::fs::read_to_string("/proc/meminfo").ok()?;
        let line = text.lines().find(|l| l.starts_with(field))?;
        let kib: i64 = line[field.len()..]
            .trim_start_matches(':')
            .split_whitespace()
            .next()?
            .parse()
            .ok()?;
        Some(kib * 1024)
    }

    pub(super) fn total_memory() -> Option<i64> {
        meminfo_bytes("MemTotal")
    }

    pub(super) fn free_memory() -> Option<i64> {
        meminfo_bytes("MemAvailable").or_else(|| meminfo_bytes("MemFree"))
    }
}

#[cfg(all(target_os = "macos", feature = "native"))]
mod platform {
    /// An unsigned integer sysctl, read into a zeroed u64 (a 32-bit value
    /// fills its low half, which is the value on a little-endian host).
    fn sysctl_u64(name: &std::ffi::CStr) -> Option<u64> {
        let mut value: u64 = 0;
        let mut len = std::mem::size_of::<u64>();
        // SAFETY: `name` is NUL-terminated; `value`/`len` are live locals
        // describing a buffer of exactly `len` bytes, which `sysctlbyname`
        // writes at most `len` bytes into; no new value is set (null, 0).
        let rc = unsafe {
            libc::sysctlbyname(
                name.as_ptr(),
                (&mut value as *mut u64).cast(),
                &mut len,
                std::ptr::null_mut(),
                0,
            )
        };
        (rc == 0).then_some(value)
    }

    pub(super) fn total_memory() -> Option<i64> {
        sysctl_u64(c"hw.memsize").map(|v| v as i64)
    }

    pub(super) fn free_memory() -> Option<i64> {
        let pages = sysctl_u64(c"vm.page_free_count")?;
        let page_size = sysctl_u64(c"hw.pagesize")?;
        Some((pages * page_size) as i64)
    }
}

#[cfg(not(any(target_os = "linux", all(target_os = "macos", feature = "native"))))]
mod platform {
    pub(super) fn total_memory() -> Option<i64> {
        None
    }

    pub(super) fn free_memory() -> Option<i64> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decode_local_time_matches_the_calendar() {
        let (offset, _) = local_offset_at(1_700_000_000);
        // 2023-11-14T22:13:20Z shifted into the local zone.
        let parts = decode_local_time(1_700_000_000 - offset);
        assert_eq!(&parts[..8], &[20, 13, 22, 14, 11, 2023, 2, 317]);
    }

    #[test]
    fn memory_is_reported() {
        assert!(cpu_core_count() >= 1);
        if cfg!(target_os = "linux") {
            assert!(total_memory() > 0);
            assert!(free_memory() > 0);
        }
    }
}
