/// The process's own `getrusage(RUSAGE_SELF)` fields, in MoarVM's
/// `MVM_proc_getrusage` order: user seconds, user microseconds, system
/// seconds, system microseconds, then maxrss, ixrss, idrss, isrss, minflt,
/// majflt, nswap, inblock, oublock, msgsnd, msgrcv, nsignals, nvcsw, nivcsw.
/// `None` where the platform has no `getrusage` (or it failed).
///
/// The one reading of rusage, used by `nqp::getrusage`.
///
/// Cost: O(1) + one syscall.
pub(crate) fn process_rusage() -> Option<[i64; 18]> {
    #[cfg(all(unix, feature = "native"))]
    {
        let mut usage = std::mem::MaybeUninit::<libc::rusage>::uninit();
        let ret = unsafe { libc::getrusage(libc::RUSAGE_SELF, usage.as_mut_ptr()) };
        if ret != 0 {
            return None;
        }
        let u = unsafe { usage.assume_init() };
        Some([
            widen(u.ru_utime.tv_sec),
            widen(u.ru_utime.tv_usec),
            widen(u.ru_stime.tv_sec),
            widen(u.ru_stime.tv_usec),
            widen(u.ru_maxrss),
            widen(u.ru_ixrss),
            widen(u.ru_idrss),
            widen(u.ru_isrss),
            widen(u.ru_minflt),
            widen(u.ru_majflt),
            widen(u.ru_nswap),
            widen(u.ru_inblock),
            widen(u.ru_oublock),
            widen(u.ru_msgsnd),
            widen(u.ru_msgrcv),
            widen(u.ru_nsignals),
            widen(u.ru_nvcsw),
            widen(u.ru_nivcsw),
        ])
    }
    #[cfg(not(all(unix, feature = "native")))]
    {
        None
    }
}

/// Widen an rusage field to i64. The fields' C types differ per platform
/// (`tv_usec` is an `i32` on macOS, the counters are `c_long`), so a plain
/// `as i64` is an identity cast on 64-bit Linux and a lossless widening
/// elsewhere; going through `Into` states that without either.
#[cfg(all(unix, feature = "native"))]
fn widen<T: Into<i64>>(v: T) -> i64 {
    v.into()
}
